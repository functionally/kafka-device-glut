{-|
Module      :  Main
Copyright   :  (c) 2016 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Experimental
Portability :  Stable

Simple producer that transfers GLUT events \<<https://hackage.haskell.org/package/GLUT-2.7.0.10/docs/Graphics-UI-GLUT-Callbacks-Window.html>\> on a Kafka topcis.
-}


module Main (
-- * Main entry.
  main
) where


import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.String (IsString(fromString))
import Graphics.Rendering.OpenGL (ClearBuffer(..), ($=!), clear)
import Graphics.UI.GLUT (createWindow, displayCallback, getArgsAndInitialize, mainLoop, swapBuffers)
import Network.UI.Kafka.GLUT (glutLoop)


-- | The main action.
main :: IO ()
main =
  do
    (_, args) <- getArgsAndInitialize
    case args of
      [client, host, port, topic, sensor] ->
        do
          let
            callbacks = [minBound..maxBound]
          putStrLn $ "Kafka client:   " ++ client
          putStrLn $ "Kafka address:  (" ++ host ++ "," ++ port ++ ")"
          putStrLn $ "Kafka topic:    " ++ topic
          putStrLn $ "Sensor name:    " ++ sensor
          putStrLn $ "GLUT callbacks: " ++ show callbacks
          void $ createWindow "GLUT Events for Kafka"
          (_, loop) <-
            glutLoop
              (fromString client)
              (fromString host, toEnum $ read port)
              (fromString topic)
              sensor
              callbacks
          void . forkIO $ void loop
          displayCallback $=!
            do
              clear [ColorBuffer]
              swapBuffers
          mainLoop
      _ -> putStrLn "USAGE: kafka-device-glut client host port topic senosr"
