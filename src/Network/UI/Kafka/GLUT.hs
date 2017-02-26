{-|
Module      :  Network.UI.Kafka.GLUT
Copyright   :  (c) 2016 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Experimental
Portability :  Stable

Produce events on a Kafka topic from GLUT callbacks \<<https://hackage.haskell.org/package/GLUT-2.7.0.10/docs/Graphics-UI-GLUT-Callbacks-Window.html>\>.
-}


{-# LANGUAGE RecordWildCards #-}


module Network.UI.Kafka.GLUT (
-- * Event handling.
  GlutCallback(..)
, glutLoop
) where


import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (when)
import Graphics.Rendering.OpenGL (($=!))
import Graphics.UI.GLUT as G (Key(..), KeyState(..), JoystickButtons(..), JoystickPosition(..), Modifiers(..), MouseButton(..), Position(..), SpaceballInput(..), SpecialKey(..), joystickCallback, keyboardMouseCallback, motionCallback, passiveMotionCallback, spaceballCallback)
import Network.UI.Kafka as K (ExitAction, LoopAction, Sensor, TopicConnection, producerLoop)
import Network.UI.Kafka.Types as K (Event(..), Modifiers(..), Button(..), SpecialKey(..))


-- | Types of GLUT callbacks.  See \<<https://hackage.haskell.org/package/GLUT-2.7.0.10/docs/Graphics-UI-GLUT-Callbacks-Window.html>\> for more details.
data GlutCallback =
    -- | Key presses and mouse button clicks.
    KeyboardMouse
    -- | Mouse motion while a button is depressed.
  | Motion
    -- | Mouse motion.
  | PassiveMotion
    -- | Spaceball tracking and button clicks.
  | Spaceball
    -- | Joystick tracking and button clicks.
  | Joystick
  deriving (Bounded, Enum, Eq, Ord, Read, Show)


-- | Produce events for a Kafka topic from GLUT callbacks \<<https://hackage.haskell.org/package/GLUT-2.7.0.10/docs/Graphics-UI-GLUT-Callbacks-Window.html>\>.
glutLoop :: TopicConnection             -- ^ The Kafka topic name and connection information.
         -> Sensor                      -- ^ The name of the sensor producing events.
         -> [GlutCallback]              -- ^ Which callbacks to enable.
         -> IO (ExitAction, LoopAction) -- ^ Action to create the exit and loop actions.
glutLoop topicConnection sensor callbacks =
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
      producerLoop topicConnection sensor
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


-- | Interpret key presses and mouse clickes.
interpretKeyboardMouse :: Key         -- ^ The key.
                       -> KeyState    -- ^ The state of the key.
                       -> G.Modifiers -- ^ The modifier keys.
                       -> Position    -- ^ The mouse position.
                       -> Event       -- ^ The corresponding event.
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


-- | Interpret mouse motion.
interpretMotion :: Position -- ^ The mouse position.
                -> Event    -- ^ The corresponding event.
interpretMotion position =
  let
    mousePosition = Just $ translatePosition position
  in
    PositionEvent{..}


-- | Interpret a spaceball event.
interpretSpaceball :: SpaceballInput -- ^ The the spaceball input.
                   -> Event          -- ^ The corresponding event.
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


-- | Interpret a joystick event.
interpretJoystick :: JoystickButtons  -- ^ The state of joystick buttons.
                  -> JoystickPosition -- ^ The joystick position.
                  -> Event            -- ^ The corresponding event.
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


-- | Translate the state of a key.
translateKeyState :: Enum a
                  => KeyState -- ^ The GLUT state.
                  -> a        -- ^ The corresponding enumeration.
translateKeyState G.Down = toEnum 0
translateKeyState G.Up   = toEnum 1


-- | Translate a special key.
translateSpecialKey :: G.SpecialKey -- ^ The GLUT special key.
                    -> K.SpecialKey -- ^ The corresponding special key.
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


-- | Translate key modifiers.
translateModifiers :: G.Modifiers -- ^ The GLUT key modifiers.
                   -> K.Modifiers -- ^ The corresponding key modifiers.
translateModifiers G.Modifiers{..} =
  let
    shiftModifier = not $ translateKeyState shift
    ctrlModifier  = not $ translateKeyState ctrl
    altModifier   = not $ translateKeyState alt
  in
    K.Modifiers{..}


-- | Translate a position.
translatePosition :: Position         -- ^ The position.
                  -> (Double, Double) -- ^ The corresponding position.
translatePosition (Position x y) = (fromIntegral x, fromIntegral y)


-- | Translate a mouse button.
translateMouseButton :: G.MouseButton -- ^ The GLUT button.
                     -> K.Button      -- ^ The corresponding button.
translateMouseButton G.LeftButton           = K.LeftButton 
translateMouseButton G.MiddleButton         = K.MiddleButton 
translateMouseButton G.RightButton          = K.RightButton 
translateMouseButton G.WheelUp              = K.WheelUp 
translateMouseButton G.WheelDown            = K.WheelDown 
translateMouseButton (G.AdditionalButton x) = K.IndexButton x
