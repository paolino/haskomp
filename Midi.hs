{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

-- midi operations around NE events, see Score

module Midi (Tempus(..),Interface, Key, resetI, killI, insertI, deleteI,newI,midiUp) where

import qualified Sound.ALSA.Exception as AlsaExc
import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port.InfoMonad as PortInfo
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event.RemoveMonad as Remove
import Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer.Queue as Queue
import qualified Sound.ALSA.Sequencer.RealTime as RealTime
import qualified Sound.ALSA.Sequencer.Time as Time
import qualified Sound.ALSA.Sequencer as SndSeq

import Control.Monad.Trans.Cont (ContT(ContT), runContT)
import Control.Monad.IO.Class (liftIO, )
import Control.Monad (mplus,forM_,forever,when)

import Sound.OSC (time,sleepThreadUntil,Time)

import Control.Concurrent.STM
import Control.Concurrent
import Control.Lens


import Score
type Scheduler = [Data] -> Time -> IO ()

handleExceptionCont :: ContT () IO () -> IO ()
handleExceptionCont = handleException . flip runContT return

handleException :: IO () -> IO ()
handleException act =  act `AlsaExc.catch` \e -> putStrLn $ "alsa_exception: " ++ AlsaExc.show e


mainLoop :: (Scheduler -> IO ()) -> (Data -> IO ()) -> IO ()
mainLoop run acq = handleExceptionCont $ do
      h <- ContT $ SndSeq.withDefault SndSeq.Block
      liftIO $ Client.setName h "Haskell-Beat"
      (public :: Port.T)<-
        ContT $ Port.withSimple h "inout"
          (Port.caps [Port.capRead, Port.capSubsRead,
                      Port.capWrite, Port.capSubsWrite])
          (Port.types [Port.typeMidiGeneric, Port.typeApplication])
      -- the time accurate queue from alsa seq
      q <- ContT $ Queue.with h
      -- initialize for using time 
      liftIO $ do
        PortInfo.modify h public $ do
           PortInfo.setTimestamping True
           PortInfo.setTimestampReal True
           PortInfo.setTimestampQueue q

        addr <- flip Addr.Cons public <$> Client.getId h 

        Queue.control h q Event.QueueStart Nothing
        forkIO . forever $ (body `fmap` Event.input h) >>= acq
        let   schedule :: Scheduler
              schedule xs t = do
                let   mkEv e = (Event.simple addr e) {
                             Event.queue = q,
                             Event.time = Time.consAbs . Time.Real . RealTime.fromDouble $ t
                             }
                mapM_ (Event.output (h:: SndSeq.T SndSeq.DuplexMode) . mkEv) xs
                _ <- Event.drainOutput h
                return ()
        run schedule

-- tag for events
type Key = Int

-- interacting with the midi component based on STM
data Interface = Interface {
  resetI :: STM (), -- set events, fullstop
  insertI :: Key -> Events -> STM (), -- add a layer of events and getthe key for it
  deleteI :: Key -> STM (), -- remove a layer from its key
  newI :: STM (Time,Key),
  killI :: IO () -- stop midi component
  }

-- tagged events
type KBoard = [(Key,E NE)]

zeroKey = 0

mkInterface :: TVar KBoard -> STM Time -> STM (IO () -> Interface)
mkInterface tb tt = do
  tn <- newTVar 0
  let   resetI = writeTVar tb []
        insertI n xs = modifyTVar tb $ (++) (zip (repeat n) xs) . filter ((/=) n . fst) 
        deleteI n = modifyTVar tb $ filter ((/=) n . fst)
        newI = do
          t <- tt
          n <- readTVar tn
          writeTVar tn $ n +1
          return (t,n)
  return $ Interface resetI insertI deleteI newI 

-- timing environment
data Tempus = Tempus {
  bpm :: Int, -- beats per minute
  lines :: Int -- number of lines, quantization
  }

parseNote :: NE -> Data
parseNote note = case note of 
    On c p v -> event c NoteOn (Pitch $ fromIntegral p) (Velocity $ fromIntegral v)
    Off c p -> event c NoteOff (Pitch $ fromIntegral p) (Velocity $ fromIntegral 0)
    Control c p v -> CtrlEv Controller $ 
      Ctrl (Channel $ fromIntegral c) (Parameter $ fromIntegral p) (Value $ fromIntegral v)
   where event c e p v = NoteEv e $ simpleNote (Channel $ fromIntegral c) p v

parseEvent :: Data -> Maybe NE 
parseEvent (NoteEv NoteOn (Note (Channel c) (Pitch p) (Velocity v) _ _)) 
    = Just $ On (fromIntegral c) (fromIntegral p) (fromIntegral v)
parseEvent (NoteEv NoteOff (Note (Channel c) (Pitch p) _ _ _)) = Just $ Off (fromIntegral c) (fromIntegral p)
parseEvent (CtrlEv Controller (Ctrl (Channel c) (Parameter p) (Value v))) = Just $ Control (fromIntegral c) (fromIntegral p)(fromIntegral v)
parseEvent _ = Nothing

controller :: STM Tempus -> STM KBoard -> IO (STM Time, Scheduler -> IO ())
controller tt tb = do
    t0 <- Sound.OSC.time
    tti <- newTVarIO 0
    let s schedule = 
          let go n  = do
                Tempus bpm lines <- atomically tt
                let   q = 15 / fromIntegral bpm
                      c = q * fromIntegral n -- absolute quantized time
                      m = n `mod` lines
                      d = q * fromIntegral m  -- relative
                      w0 = fromIntegral m / fromIntegral lines -- normalized window start
                      w1 = fromIntegral (m + 1) / fromIntegral lines -- normalized window ends

                atomically $ writeTVar tti w1 -- expose next deadline
                sleepThreadUntil $ c + t0 
                xs <- map parseNote <$> pickBoard (Span w0 w1) <$> map snd <$> atomically tb
                schedule xs (d + q)
                go (n + 1) 
            in go 0
    return (readTVar tti, s)

-- start midi operations
midiUp  :: STM Tempus  -- a Tempus shared structure
        -> TChan NE  -- a chan to notify midi events
        -> IO Interface -- an interface to interact with the score
midiUp tt cn = do
    tb <- newTVarIO []
    (tti,sch) <- controller tt $ readTVar tb
    t <- forkIO $ mainLoop sch (maybe (return ()) (atomically . writeTChan cn) . parseEvent)
    ($ killThread t) <$> atomically (mkInterface tb tti) 
    
  
  






