{-# LANGUAGE RecordWildCards #-}


module Network.UI.Kafka.GLUT (
  GlutCallback(..)
, glutLoop
) where


import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (when)
import Graphics.Rendering.OpenGL (($=!))
import Graphics.UI.GLUT as G (Key(..), KeyState(..), JoystickButtons(..), JoystickPosition(..), Modifiers(..), MouseButton(..), Position(..), SpaceballInput(..), SpecialKey(..), joystickCallback, keyboardMouseCallback, motionCallback, passiveMotionCallback, spaceballCallback)
import Network.Kafka (KafkaAddress, KafkaClientId)
import Network.Kafka.Protocol (TopicName)
import Network.UI.Kafka as K (ExitAction, LoopAction, Sensor, producerLoop)
import Network.UI.Kafka.Types as K (Event(..), Modifiers(..), Button(..), SpecialKey(..))


data GlutCallback =
    KeyboardMouse
  | Motion
  | PassiveMotion
  | Spaceball
  | Joystick
  deriving (Bounded, Enum, Eq, Ord, Read, Show)


glutLoop :: KafkaClientId -> KafkaAddress -> TopicName -> Sensor -> [GlutCallback] -> IO (ExitAction, LoopAction)
glutLoop clientId address topic sensor callbacks =
  do
    nextEvent <- newEmptyMVar
    mapM_
      (uncurry (when . (`elem` callbacks)))
      [
        (KeyboardMouse, keyboardMouseCallback $=! Just ((((putMVar nextEvent .) .) .) . interpretKeyboardMouse   ))
      , (Motion       , motionCallback        $=! Just (   putMVar nextEvent          . interpretMotion          ))
      , (PassiveMotion, passiveMotionCallback $=! Just (   putMVar nextEvent          . interpretMotion          ))
      , (Spaceball    , spaceballCallback     $=! Just (   putMVar nextEvent          . interpretSpaceball       ))
      , (Joystick     , joystickCallback      $=! Just (  (putMVar nextEvent .)       . interpretJoystick     , 0))
      ]
    (exit, loop) <-
      producerLoop clientId address topic sensor
        $ (: [])
        <$> takeMVar nextEvent
    return
      (
        exit
      , do
          result <- loop
          mapM_
            (uncurry (when . (`elem` callbacks)))
            [
              (KeyboardMouse, keyboardMouseCallback $=! Nothing)
            , (Motion       , motionCallback        $=! Nothing)
            , (PassiveMotion, passiveMotionCallback $=! Nothing)
            , (Spaceball    , spaceballCallback     $=! Nothing)
            , (Joystick     , joystickCallback      $=! Nothing)
            ]
          return result
      )


interpretKeyboardMouse :: Key -> KeyState -> G.Modifiers -> Position -> Event
interpretKeyboardMouse key' state modifiers' position =
  let
    toggle        = Just $ translateKeyState  state
    modifiers     = Just $ translateModifiers modifiers'
    mousePosition = Just $ translatePosition  position
  in
    case key' of
      Char key            -> KeyEvent{..}
      G.SpecialKey key    -> let
                               specialKey = translateSpecialKey key
                             in
                               SpecialKeyEvent{..}
      MouseButton button' -> let
                               button = (translateMouseButton button', translateKeyState state)
                             in
                               MouseEvent{..}


interpretMotion :: Position -> Event
interpretMotion position =
  let
    mousePosition = Just $ translatePosition position
  in
    PositionEvent{..}


interpretSpaceball :: SpaceballInput -> Event
interpretSpaceball (SpaceballMotion rightward upward backward) =
  let
    motionRightward = fromIntegral rightward / 1000
    motionUpward    = fromIntegral upward    / 1000
    motionBackward  = fromIntegral backward  / 1000
  in
    MotionEvent{..}
interpretSpaceball (SpaceballRotation forward clockwise rightward) =
  let
    rotationForward   = fromIntegral forward   / 1800
    rotationClockwise = fromIntegral clockwise / 1800
    rotationRightward = fromIntegral rightward / 1800
  in
    RotationEvent{..}
interpretSpaceball (SpaceballButton button' state) =
  let
    button = (IndexButton button', translateKeyState state)
  in
    ButtonEvent{..}


interpretJoystick :: JoystickButtons -> JoystickPosition -> Event
interpretJoystick JoystickButtons{..} (JoystickPosition rightward forward upward) =
  let
    joystickRightward = fromIntegral rightward / 1000
    joystickForward   = fromIntegral forward   / 1000
    joystickUpward    = fromIntegral upward    / 1000
    buttons =
      [
        (LetterButton 'A', translateKeyState joystickButtonA)
      , (LetterButton 'B', translateKeyState joystickButtonB)
      , (LetterButton 'C', translateKeyState joystickButtonC)
      , (LetterButton 'D', translateKeyState joystickButtonD)
      ]
  in
    JoystickEvent{..}


translateKeyState :: Enum a => KeyState -> a
translateKeyState G.Down = toEnum 0
translateKeyState G.Up   = toEnum 1


translateSpecialKey :: G.SpecialKey -> K.SpecialKey
translateSpecialKey G.KeyF1          = K.KeyF1
translateSpecialKey G.KeyF2          = K.KeyF2
translateSpecialKey G.KeyF3          = K.KeyF3
translateSpecialKey G.KeyF4          = K.KeyF4
translateSpecialKey G.KeyF5          = K.KeyF5
translateSpecialKey G.KeyF6          = K.KeyF6
translateSpecialKey G.KeyF7          = K.KeyF7
translateSpecialKey G.KeyF8          = K.KeyF8
translateSpecialKey G.KeyF9          = K.KeyF9
translateSpecialKey G.KeyF10         = K.KeyF10
translateSpecialKey G.KeyF11         = K.KeyF11
translateSpecialKey G.KeyF12         = K.KeyF12
translateSpecialKey G.KeyLeft        = K.KeyLeft
translateSpecialKey G.KeyUp          = K.KeyUp
translateSpecialKey G.KeyRight       = K.KeyRight
translateSpecialKey G.KeyDown        = K.KeyDown
translateSpecialKey G.KeyPageUp      = K.KeyPageUp
translateSpecialKey G.KeyPageDown    = K.KeyPageDown
translateSpecialKey G.KeyHome        = K.KeyHome
translateSpecialKey G.KeyEnd         = K.KeyEnd
translateSpecialKey G.KeyInsert      = K.KeyInsert
translateSpecialKey G.KeyNumLock     = K.KeyNumLock
translateSpecialKey G.KeyBegin       = K.KeyBegin
translateSpecialKey G.KeyDelete      = K.KeyDelete
translateSpecialKey G.KeyShiftL      = K.KeyShiftL
translateSpecialKey G.KeyShiftR      = K.KeyShiftR
translateSpecialKey G.KeyCtrlL       = K.KeyCtrlL
translateSpecialKey G.KeyCtrlR       = K.KeyCtrlR
translateSpecialKey G.KeyAltL        = K.KeyAltL
translateSpecialKey G.KeyAltR        = K.KeyAltR
translateSpecialKey (G.KeyUnknown x) = K.KeyUnknown x


translateModifiers :: G.Modifiers -> K.Modifiers
translateModifiers G.Modifiers{..} =
  let
    shiftModifier = not $ translateKeyState shift
    ctrlModifier  = not $ translateKeyState ctrl
    altModifier   = not $ translateKeyState alt
  in
    K.Modifiers{..}


translatePosition :: Position -> (Double, Double)
translatePosition (Position x y) = (fromIntegral x, fromIntegral y)


translateMouseButton :: G.MouseButton -> K.Button
translateMouseButton G.LeftButton           = K.LeftButton 
translateMouseButton G.MiddleButton         = K.MiddleButton 
translateMouseButton G.RightButton          = K.RightButton 
translateMouseButton G.WheelUp              = K.WheelUp 
translateMouseButton G.WheelDown            = K.WheelDown 
translateMouseButton (G.AdditionalButton x) = K.IndexButton x
