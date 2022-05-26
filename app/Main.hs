{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Control.Monad.Trans.Reader
import LiveCoding
import LiveCoding.PortMidi
import LiveCoding.Vivid
import Vivid

main :: IO ()
main = putStrLn "hello"

sineDef :: SDBody' '["freq", "gate", "fadeSecs"] [Signal]
sineDef = do
  s <- sinOsc (freq_ (V :: V "freq"))
  s' <- envGate ~* s ~* 0.1
  out 0 [s', s']

sineCell ::
  Cell
    (HandlingStateT IO)
    Float
    ()
sineCell = proc frequency -> do
  liveSynth
    -<
      ( (realToFrac frequency :: I "freq", 1 :: I "gate", 2 :: I "fadeSecs"),
        sineDef,
        Started
      )
  returnA -< ()

portMidiCell1 :: Cell (HandlingStateT IO) () ()
portMidiCell1 = loopPortMidiC $ proc () -> do
  readEventsC "name" -< ()
  liftHandlingState sineCell -< 440

portMidiCell2 :: Cell (HandlingStateT IO) () ()
portMidiCell2 = loopPortMidiC $
  foreverE True $ proc () -> do
    b <- arrM (const ask) -< ()
    if b
      then liftCell $ liftCell $ readEventsC "name" -< ()
      else returnA -< []
    liftCell $ liftCell $ liftHandlingState sineCell -< 440
    liftCell throwC -< False
    returnA -< ()
