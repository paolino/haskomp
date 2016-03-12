



{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
import System.Random

import Control.Monad
import Control.Lens
import Control.Lens.TH
import Control.Concurrent.STM
import Control.Concurrent

import Midi 
import Subd
import Score 


-- zipping pitches and rythm to events
notes :: [Int] -> Subd -> Events
notes ps r =  concat (zipWith note ps (scanl (\(_,k) y -> (y,k+y)) (0,0) $ flattenSubd r)) >>= convert
  where
  note 0 (x,y) = [] -- pause hack, fix  with Subd = Subd [Maybe Subd] | Tab 
  note k (x,y) = [E (N 0 k (floor $ (sqrt(sqrt x)*128)) 0.17) y]

-- phrase editing state
newtype Strength = Strength Double deriving Read

data TransS = TransS {
  _tag :: Key,
  _shift :: Double,
  _width :: Double,
  _strength :: Double,
  _mute :: Bool
  }
  deriving Show
makeLenses ''TransS
  

newtype Offset  = Offset {
  fromOffset :: Int
  } deriving (Show,Read,Eq)


data PhraseS = PhraseS {
  _phrase :: Events,
  _stack :: Maybe Offset,
  _assoc :: [(Offset,TransS)]
  } deriving Show

makeLenses ''PhraseS

bootPhraseS = PhraseS [] Nothing []

transProj :: TransS -> E a -> E a
transProj t = projectE (Linear ((view width t) +1) (view shift t))

transEvents :: Events -> Offset -> TransS -> Events
transEvents xs k t = over traverse (transProj t) . over (traverse . event) (modifyVelOn (floor . (*128) . (* view strength t) . (/128) . fromIntegral ) . offset (fromOffset k `mod` 12)) $ xs

fullEvents :: PhraseS -> [(Key,Events)]
fullEvents s = map (\(k,t) -> (view tag t,transEvents (view phrase s) k t)) (filter (not . view mute . snd) . view assoc $ s)

-- phrase editing protocol language

data Editing  = New  -- new phrase
        | Play Strength Offset -- start to over play the phrase now at the given transposition
        | Stop Offset -- stop all playing for the given transposition
        | Kill -- stop the editing ?
        | DoNothing  -- boo hack
        | Shift Double -- set the time shift
        | Width Double -- correct the time width
        | Save deriving Read

-- midi events to language, hard coded!, fix me
midi2L :: NE -> Editing
midi2L (On _ n v) = Play (Strength $ fromIntegral v/128) $ Offset n 
midi2L (Off _ n) = Stop $ Offset n 
midi2L (Control _ 1 0) = New
midi2L (Control _ 16 v) = Shift (fromIntegral v/128)
midi2L (Control _ 17 v) = Width (fromIntegral v/128)
midi2L _ = DoNothing

flipset t s p = set t p s

updateAssoc o t =  (((o,t):) . filter ((/=) o . fst))
---- parsing editing events in async Interface and update
assocLens i = lens (lookup i) (flip t) where
  t (Just v) = (:) (i,v) . filter ((/=) i . fst) 
  t Nothing = filter ((/=) i . fst)

reset :: Interface -> PhraseS -> IO ()
reset int s = atomically $ do
      resetI int
      forM_ (fullEvents s) . uncurry $ insertI int


edit  :: IO Events  -- a random event creator
      -> (Events -> IO a) -- save action
      -> Interface -- engine side effect (STM)
      -> PhraseS -- actual state
      -> Editing -- action
      -> IO PhraseS -- new state

edit new save int s New = do
    s' <- flipset phrase s <$> new 
    reset int s'
    return s'
    
edit new save int s (Play (Strength v) o) = do
    s' <- case lookup o (view assoc s) of
      Just _ -> return $ set stack (Just o) s
      Nothing -> do
        (te,k) <- atomically $ newI int
        let   t = TransS k te 1 1 False
        return $ set stack (Just o) . over assoc (updateAssoc o t)  $ s
    let s'' = over (assoc . assocLens o) (fmap $ set strength v . set mute False) $ s'
    reset int s''
    return s''

edit new save int s' (Stop o) = do
    let s'' = over (assoc . assocLens o) (fmap $ set mute True) $ s'
    reset int s''
    return s''
    
edit new save int s Kill = 
    atomically (resetI int) >> return bootPhraseS

edit new save int s@(view assoc -> as) (Shift sh) = do
    let s' = maybe s (flipset assoc s) $ do   
          o <- view stack s
          return $ over (assocLens o) (fmap $ set shift sh) as
    print (length $ view assoc s')
    reset int s'
    return s'

edit new save int s@(view assoc -> as) (Width w) = do
    let s' = maybe s (flipset assoc s) $ do   
          o <- view stack s
          t <- set width w <$> lookup o as
          return $ updateAssoc o t as
    reset int s'
    return s'

edit new save _ s Save = 
  save (concatMap snd . fullEvents $ s) >> return s

edit _ _ _ s _ = return s



-- hard coded, fix me
new = fmap concat . replicateM 4 $ liftM2 notes (stream $ (replicate 18 0) ++ [36..47]) $ randomSubd 5 [1,1,1,1,1,2,2,2,2,2]



main = do
   t <- newTVarIO $ Tempus 125 4 16
   store <- newTChanIO
   ne <- newTChanIO 
   i <- midiUp (readTVar t) ne
   keyb <- newTChanIO
   let loop s = do
        a <- atomically $ readTChan keyb `orElse` (midi2L <$> readTChan ne)
        edit new (atomically . writeTChan store) i s a >>= loop
   forkIO $ loop bootPhraseS
   forkIO . forever $ atomically (readTChan store) >>= print
   forever $ do
       l <- getLine
       case reads  l of
          [(a,_)] -> atomically $ writeTChan keyb a
          _ -> putStr "garbled"

{-
 -} 
