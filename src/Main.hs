module Main (
  main
) where


import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.String (IsString(fromString))
import Graphics.Rendering.OpenGL (ClearBuffer(..), ($=!), clear)
import Graphics.UI.GLUT (createWindow, displayCallback, getArgsAndInitialize, mainLoop, swapBuffers)
import Network.UI.Kafka.GLUT (glutLoop)


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
