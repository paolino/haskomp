



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
import Data.List (partition)

-- zipping pitches and rythm to events
notes :: [Int] -> Subd -> Events
notes ps r =  concat (zipWith note ps (scanl (\(_,k) y -> (y,k+y)) (0,0) $ flattenSubd r)) >>= convert
  where
  note 0 (x,y) = [] -- pause hack, fix  with Subd = Subd [Maybe Subd] | Tab 
  note k (x,y) = [E (N 0 k (floor $ (sqrt(sqrt x)*128)) 0.17) y]

-- phrase editing state
newtype Strength = Strength Double deriving (Read, Show)


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
transEvents xs k t = over traverse (transProj t) . over (traverse . event) (modifyVelOn (floor . (*128) . (* view strength t) . (/128) . fromIntegral ) . offset (fromOffset k) 12) $ xs

fullEvents :: PhraseS -> [(Key,Events)]
fullEvents s = map (\(k,t) -> (view tag t,transEvents (view phrase s) k t)) (filter (not . view mute . snd) . view assoc $ s)

-- phrase editing protocol language

data Editing  = New  -- new phrase
        | Play Strength Offset -- start to over play the phrase now at the given transposition
        | Stop Offset -- stop all playing for the given transposition
        | Kill -- stop the editing ?
        | DoNothing  -- boo hack
        | Shift Offset Double -- set the time shift
        | Width Offset Double -- correct the time width
        | Save deriving (Read, Show)

-- midi events to language, hard coded!, fix me
midi2L :: NE -> Editing
midi2L (On _ n v) = 
  if n >=9 && n <=12 then Play (Strength $ fromIntegral v/128) $ Offset (n - 9)
  else 
  if n >=25 && n <=28 then Play (Strength $ fromIntegral v/128) $ Offset (n - 21) else Play (Strength $ fromIntegral v/128) $ Offset (n) 
midi2L (Off _ n) =   
  if n >=9 && n <=12 then Stop $ Offset (n - 9)
  else 
  if n >=25 && n <=28 then Stop $ Offset (n - 21) else Stop $ Offset (n) 
midi2L (Control _ 114 127) = New
midi2L (Control _ k v) = 
  if k >= 21 && k <= 28 then  Shift (Offset $ k - 21) (fromIntegral v/128)
  else if k >= 41 && k <= 48 then  Width (Offset $ k - 41) (fromIntegral v/128)
  else DoNothing

midi2L _ = DoNothing

flipset t s p = set t p s

updateAssoc o t =  (((o,t):) . filter ((/=) o . fst))
---- parsing editing events in async Interface and update
assocLens i = lens (lookup i) (flip t) where
  t (Just v) = (:) (i,v) . filter ((/=) i . fst) 
  t Nothing = filter ((/=) i . fst)
assocLensN f = lens (map snd . filter (f . fst)) (flip t) where
  t vs xs = let 
     (ys,zs) = partition (f . fst) xs
     in zipWith (\(i,_) v -> (i,v)) ys vs ++ zs

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
edit new save int s (Shift (Offset o) sh) = do
    let s' = over (assoc . assocLensN (\(Offset o') -> o' `mod` 8 == o)) (fmap $ set shift sh) s
    reset int s'
    return s'

edit new save int s (Width (Offset o) w) = do
    let s' = over (assoc . assocLensN (\(Offset o') -> o' `mod` 8 == o)) (fmap $ set width w) s
    reset int s'
    return s'
edit new save _ s Save = 
  save (concatMap snd . fullEvents $ s) >> return s

edit _ _ _ s DoNothing = print s >> return s
edit _ _ _ s _ = return s



-- hard coded, fix me
new = fmap concat . replicateM 4 $ liftM2 notes (stream $ (replicate 200 0) ++ [36..47]) $ randomSubd 3 [1..5]


main = do
   t <- newTVarIO $ Tempus 125 4
   store <- newTChanIO
   ne <- newTChanIO 
   i <- midiUp (readTVar t) ne
   keyb <- newTChanIO
   let loop s = do
        a <- atomically $ readTChan keyb `orElse` (midi2L <$> readTChan ne)
        print a
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
