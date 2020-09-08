> {-# LANGUAGE DuplicateRecordFields #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE FlexibleInstances #-}

> module Show.Event (
>     ShowEvent(..)
> ) where
> 
> -- core dependencies
> import qualified Data.ByteString.Char8 as C8
> import           Data.Int
> import qualified Data.Text as T
> import qualified Data.Text.Encoding as T
> import qualified Data.Vector as V
> import           Data.Word
> import           Foreign.C.Types (CFloat)
> 
> -- ecosystem dependencies
> import qualified SDL
> import qualified SDL.Raw.Types as SDL (JoystickID, FingerID, TouchID, GestureID)
> import qualified SDL.Internal.Types as SDL (Window(..))
> import qualified SDL.Input.Joystick
> import qualified SDL.Input.GameController as SDL
> import qualified Vulkan as VK





> class ShowEvent t where
>   showEvent :: t -> C8.ByteString

> instance ShowEvent Int where
>   showEvent = C8.pack . show
> 
> instance ShowEvent Int32 where
>   showEvent = C8.pack . show





Vulkan
======

> instance ShowEvent VK.DebugUtilsMessengerCallbackDataEXT where
>   showEvent x =
>     let
>       VK.DebugUtilsMessengerCallbackDataEXT
>         { VK.flags           = VK.DebugUtilsMessengerCallbackDataFlagsEXT flags
>         , VK.messageIdName   = mname
>         , VK.messageIdNumber = num
>         , VK.message         = msg
>         , VK.queueLabels     = queues
>         , VK.cmdBufLabels    = cmdBufs
>         , VK.objects         = objs
>         } = x
> 
>     in C8.intercalate " "
>       [ "flags:" <> (C8.pack $ show flags)
>       , "msgIdName:" <> case mname of
>           Nothing   -> "none"
>           Just name -> name
>       , "msgIdNo:" <> showEvent num
>       , "queues:" <> showEvent queues
>       , "cmdBuffers:" <> showEvent cmdBufs
>       , "objs:" <> showEvent objs
>       , "msg:'" <> msg <> "'"
>       ]

> instance ShowEvent (V.Vector VK.DebugUtilsLabelEXT) where
>   showEvent xs = case V.toList xs of
>     [] -> "none"
>     ys -> C8.intercalate "," $ map VK.labelName ys

> instance ShowEvent (V.Vector VK.DebugUtilsObjectNameInfoEXT) where
>   showEvent xs =
>     let
>       s :: VK.DebugUtilsObjectNameInfoEXT -> C8.ByteString
>       s x =
>         let
>           VK.DebugUtilsObjectNameInfoEXT
>             { VK.objectName = name
>             , VK.objectType = typ
>             } = x
>         in case name of
>           Nothing -> "null." <> (C8.pack $ show typ)
>           Just str -> str <> "." <> (C8.pack $ show typ)
>     in case V.toList xs of
>       [] -> "none"
>       ys -> C8.intercalate "," $ map s ys





SDL
===

> instance ShowEvent SDL.Window where
>   showEvent win =
>     "win:" <> showWindow win
> 
> instance ShowEvent (Maybe (SDL.Window)) where
>   showEvent x = case x of
>     Nothing  -> "win:none"
>     Just win -> showEvent win
> 
> showWindow :: SDL.Window -> C8.ByteString
> showWindow (SDL.Window win) = C8.pack $ show win

> instance ShowEvent SDL.MouseDevice where
>   showEvent x = case x of
>     SDL.Mouse k -> "mouse:" <> showEvent k
>     SDL.Touch   -> "mouse:touch"
> 
> instance ShowEvent SDL.MouseButton where
>   showEvent x = "button:" <> showButton x
> 
> instance ShowEvent [SDL.MouseButton] where
>   showEvent xs = if null xs
>     then "buttons:none"
>     else "buttons:" <> (C8.intercalate "," $ map showButton xs)
> 
> showButton :: SDL.MouseButton -> C8.ByteString
> showButton x = case x of
>   SDL.ButtonLeft    -> "left"
>   SDL.ButtonMiddle  -> "middle"
>   SDL.ButtonRight   -> "right"
>   SDL.ButtonX1      -> "x1"
>   SDL.ButtonX2      -> "x2"
>   SDL.ButtonExtra k -> "extra" <> showEvent k
> 
> instance ShowEvent SDL.InputMotion where
>   showEvent x = case x of
>     SDL.Released -> "action:released"
>     SDL.Pressed  -> "action:pressed"
> 
> showClicks :: Word8 -> C8.ByteString
> showClicks k = "clicks:" <> (C8.pack $ show k)

> instance ShowEvent SDL.MouseScrollDirection where
>   showEvent x = case x of
>     SDL.ScrollNormal  -> "scroll:normal"
>     SDL.ScrollFlipped -> "scroll:flipped"

> instance ShowEvent (SDL.Point SDL.V2 Int32) where
>   showEvent (SDL.P (SDL.V2 x y)) =
>     "x:" <> (C8.pack $ show x) <> " " <> "y:" <> (C8.pack $ show y)
> 
> relPos :: SDL.V2 Int32 -> C8.ByteString
> relPos (SDL.V2 dx dy) =
>   "dx:" <> (C8.pack $ show dx) <> " " <> "dy:" <> (C8.pack $ show dy)
> 
> showSize :: SDL.V2 Int32 -> C8.ByteString
> showSize (SDL.V2 w h) =
>   "w:" <> (C8.pack $ show w) <> " " <> "h:" <> (C8.pack $ show h)
> 
> showScrollPos :: SDL.V2 Int32 -> C8.ByteString
> showScrollPos (SDL.V2 x y) =
>   "x:" <> (C8.pack $ show x) <> " " <> "y:" <> (C8.pack $ show y)

> showRepeat :: Bool -> C8.ByteString
> showRepeat p = case p of
>   True  -> "repeat:true"
>   False -> "repeat:false"
> 
> showScancode :: SDL.Scancode -> C8.ByteString
> showScancode (SDL.Scancode k) = "scancode:" <> (C8.pack $ show k)
> 
> showKeycode :: SDL.Keycode -> C8.ByteString
> showKeycode (SDL.Keycode k) = "keycode:" <> (C8.pack $ show k)
> 
> instance ShowEvent SDL.KeyModifier where
>   showEvent x =
>     let
>       SDL.KeyModifier
>         { SDL.keyModifierLeftShift  = pLS
>         , SDL.keyModifierRightShift = pRS
>         , SDL.keyModifierLeftCtrl   = pLC
>         , SDL.keyModifierRightCtrl  = pRC
>         , SDL.keyModifierLeftAlt    = pLA
>         , SDL.keyModifierRightAlt   = pRA
>         , SDL.keyModifierLeftGUI    = pLG
>         , SDL.keyModifierRightGUI   = pRG
>         , SDL.keyModifierNumLock    = pNL
>         , SDL.keyModifierCapsLock   = pCL
>         , SDL.keyModifierAltGr      = pAG
>         } = x
> 
>       labels = mconcat
>         [ if not pLS then [] else [ "lshift" ]
>         , if not pRS then [] else [ "rshift" ]
>         , if not pLC then [] else [ "lctrl" ]
>         , if not pRC then [] else [ "rctrl" ]
>         , if not pLA then [] else [ "lalt" ]
>         , if not pRA then [] else [ "ralt" ]
>         , if not pLG then [] else [ "lgui" ]
>         , if not pRG then [] else [ "rgui" ]
>         , if not pNL then [] else [ "numlock" ]
>         , if not pCL then [] else [ "capslock" ]
>         , if not pAG then [] else [ "altgr" ]
>         ]
> 
>       mods = if null labels
>         then "none"
>         else C8.intercalate "+" labels
> 
>     in "modifiers:" <> mods

> showStart :: Int32 -> C8.ByteString
> showStart k = "start:" <> (C8.pack $ show k)
> 
> showLength :: Int32 -> C8.ByteString
> showLength k = "length:" <> (C8.pack $ show k)
> 
> showText :: T.Text -> C8.ByteString
> showText text = "text:<<<" <> (T.encodeUtf8 text) <> ">>>"

> showJoystickId :: SDL.JoystickID -> C8.ByteString
> showJoystickId which = "joystick:" <> (C8.pack $ show which)
> 
> showAxisId :: Word8 -> C8.ByteString
> showAxisId idx = "axis:" <> (C8.pack $ show idx)
> 
> showAxisValue :: Int16 -> C8.ByteString
> showAxisValue val = "value:" <> (C8.pack $ show val)
> 
> showBallId :: Word8 -> C8.ByteString
> showBallId idx = "ball:" <> (C8.pack $ show idx)
> 
> showBallRelMotion :: SDL.V2 Int16 -> C8.ByteString
> showBallRelMotion (SDL.V2 x y) =
>   "x:" <> (C8.pack $ show x) <> " " <> "y:" <> (C8.pack $ show y)
> 
> showHatId :: Word8 -> C8.ByteString
> showHatId idx = "hat:" <> (C8.pack $ show idx)
> 
> instance ShowEvent SDL.JoyHatPosition where
>   showEvent x = "position:" <> case x of
>     SDL.HatCentered  -> "centered"
>     SDL.HatUp        -> "up"
>     SDL.HatRight     -> "right"
>     SDL.HatDown      -> "down"
>     SDL.HatLeft      -> "left"
>     SDL.HatRightUp   -> "right-up"
>     SDL.HatRightDown -> "right-down"
>     SDL.HatLeftUp    -> "left-up"
>     SDL.HatLeftDown  -> "left-down"
> 
> showJoyButtonId :: Word8 -> C8.ByteString
> showJoyButtonId idx = "joystick_button:" <> (C8.pack $ show idx)
> 
> instance ShowEvent SDL.JoyButtonState where
>   showEvent x = "state:" <> case x of
>     SDL.JoyButtonPressed  -> "pressed"
>     SDL.JoyButtonReleased -> "released"
> 
> instance ShowEvent SDL.JoyDeviceConnection where
>   showEvent x = "source:" <> case x of
>     SDL.JoyDeviceAdded   -> "added"
>     SDL.JoyDeviceRemoved -> "removed"

> showControllerId :: Int32 -> C8.ByteString
> showControllerId idx = "controller:" <> (C8.pack $ show idx)
> 
> instance ShowEvent SDL.ControllerButton where
>   showEvent x = "button:" <> case x of
>     SDL.ControllerButtonInvalid       -> "invalid"
>     SDL.ControllerButtonA             -> "A"
>     SDL.ControllerButtonB             -> "B"
>     SDL.ControllerButtonX             -> "X"
>     SDL.ControllerButtonY             -> "Y"
>     SDL.ControllerButtonBack          -> "back"
>     SDL.ControllerButtonGuide         -> "guide"
>     SDL.ControllerButtonStart         -> "start"
>     SDL.ControllerButtonLeftStick     -> "left-stick"
>     SDL.ControllerButtonRightStick    -> "right-stick"
>     SDL.ControllerButtonLeftShoulder  -> "left-shoulder"
>     SDL.ControllerButtonRightShoulder -> "right-shoulder"
>     SDL.ControllerButtonDpadUp        -> "dpad-up"
>     SDL.ControllerButtonDpadDown      -> "dpad-down"
>     SDL.ControllerButtonDpadLeft      -> "dpad-left"
>     SDL.ControllerButtonDpadRight     -> "dpad-right"
> 
> instance ShowEvent SDL.ControllerButtonState where
>   showEvent x = "state:" <> case x of
>     SDL.ControllerButtonPressed      -> "pressed"
>     SDL.ControllerButtonReleased     -> "released"
>     SDL.ControllerButtonInvalidState -> "invalid"
> 
> instance ShowEvent SDL.ControllerDeviceConnection where
>   showEvent x = "connection:" <> case x of
>     SDL.ControllerDeviceAdded    -> "added"
>     SDL.ControllerDeviceRemoved  -> "removed"
>     SDL.ControllerDeviceRemapped -> "remapped"

> showAudioId :: Word32 -> C8.ByteString
> showAudioId idx = "audio_id:" <> (C8.pack $ show idx)
> 
> showIsAddition :: Bool -> C8.ByteString
> showIsAddition p = if p
>   then "is_addition:true"
>   else "is_addition:false"
> 
> showIsCapture :: Bool -> C8.ByteString
> showIsCapture p = if p
>   then "is_capture:true"
>   else "is_capture:false"

> showTouchId :: SDL.TouchID -> C8.ByteString
> showTouchId idx = "touch_id:" <> (C8.pack $ show idx)
> 
> showFingerId :: SDL.FingerID -> C8.ByteString
> showFingerId idx = "finger_id:" <> (C8.pack $ show idx)
> 
> showFingerPos :: SDL.Point SDL.V2 CFloat -> C8.ByteString
> showFingerPos (SDL.P (SDL.V2 x y)) =
>   "x:" <> (C8.pack $ show x) <> " " <> "y:" <> (C8.pack $ show y)
> 
> showPressure :: CFloat -> C8.ByteString
> showPressure pressure = "pressure:" <> (C8.pack $ show pressure)
> 
> showFingerRelMotion :: SDL.V2 CFloat -> C8.ByteString
> showFingerRelMotion (SDL.V2 dx dy) =
>   "dx:" <> (C8.pack $ show dx) <> " " <> "dy:" <> (C8.pack $ show dy)

> showDTheta :: CFloat -> C8.ByteString
> showDTheta dt = "dtheta:" <> (C8.pack $ show dt)
> 
> showDDist :: CFloat -> C8.ByteString
> showDDist dd = "ddist:" <> (C8.pack $ show dd)
> 
> showFingerCount :: (Integral n, Show n) => n -> C8.ByteString
> showFingerCount n = "fingers:" <> (C8.pack $ show n)
> 
> showGestureId :: SDL.GestureID -> C8.ByteString
> showGestureId idx = "gesture:" <> (C8.pack $ show idx)
> 
> showGestureError :: CFloat -> C8.ByteString
> showGestureError err = "error:" <> (C8.pack $ show err)




> instance ShowEvent SDL.Event where
>   showEvent event =
>     let
>       SDL.Event
>         { SDL.eventTimestamp = timestamp
>         , SDL.eventPayload = payload
>         } = event
> 
>       pre = "time:" <> (C8.pack $ show timestamp) <> " "
> 
>       unwords :: [C8.ByteString] -> C8.ByteString
>       unwords = C8.intercalate " "
> 
>     in pre <> case payload of
> 
>       SDL.WindowExposedEvent
>         (SDL.WindowExposedEventData
>           { SDL.windowExposedEventWindow = win
>           }) -> unwords
>             [ showEvent win, "window_exposed" ]
> 
>       SDL.WindowGainedMouseFocusEvent
>         (SDL.WindowGainedMouseFocusEventData
>           { SDL.windowGainedMouseFocusEventWindow = win
>           }) -> unwords
>             [ showEvent win, "window_gained_mouse_focus" ]
> 
>       SDL.WindowLostMouseFocusEvent
>         (SDL.WindowLostMouseFocusEventData
>           { SDL.windowLostMouseFocusEventWindow = win
>           }) -> unwords
>             [ showEvent win, "window_lost_mouse_focus" ]
> 
>       SDL.WindowGainedKeyboardFocusEvent
>         (SDL.WindowGainedKeyboardFocusEventData
>           { SDL.windowGainedKeyboardFocusEventWindow = win
>           }) -> unwords
>             [ showEvent win, "window_gained_keyboard_focus" ]
> 
>       SDL.WindowLostKeyboardFocusEvent
>         (SDL.WindowLostKeyboardFocusEventData
>           { SDL.windowLostKeyboardFocusEventWindow = win
>           }) -> unwords
>             [ showEvent win, "window_lost_keyboard_focus" ]
> 
>       SDL.WindowResizedEvent
>         (SDL.WindowResizedEventData
>           { SDL.windowResizedEventWindow = win
>           , SDL.windowResizedEventSize   = size
>           }) -> unwords
>             [ showEvent win, "window_resized", showSize size ]
> 
>       SDL.WindowSizeChangedEvent
>         (SDL.WindowSizeChangedEventData
>           { SDL.windowSizeChangedEventWindow = win
>           , SDL.windowSizeChangedEventSize   = size
>           }) -> unwords
>             [ showEvent win, "window_size_changed", showSize size ]
> 
>       SDL.WindowMinimizedEvent
>         (SDL.WindowMinimizedEventData
>           { SDL.windowMinimizedEventWindow = win
>           }) -> unwords
>             [ showEvent win, "window_minimized" ]
> 
>       SDL.WindowMaximizedEvent
>         (SDL.WindowMaximizedEventData
>           { SDL.windowMaximizedEventWindow = win
>           }) -> unwords
>             [ showEvent win, "window_maximized" ]
> 
>       SDL.WindowHiddenEvent
>         (SDL.WindowHiddenEventData
>           { SDL.windowHiddenEventWindow = win
>           }) -> unwords
>             [ showEvent win, "window_hidden" ]
> 
>       SDL.WindowShownEvent
>         (SDL.WindowShownEventData
>           { SDL.windowShownEventWindow = win
>           }) -> unwords
>             [ showEvent win, "window_shown" ]
> 
>       SDL.WindowMovedEvent
>         (SDL.WindowMovedEventData
>           { SDL.windowMovedEventWindow   = win
>           , SDL.windowMovedEventPosition = pos
>           }) -> unwords
>             [ showEvent win, "window_moved", showEvent pos ]
> 
>       SDL.WindowClosedEvent
>         (SDL.WindowClosedEventData
>           { SDL.windowClosedEventWindow = win
>           }) -> unwords
>             [ showEvent win, "window_closed" ]
> 
>       SDL.WindowRestoredEvent
>         (SDL.WindowRestoredEventData
>           { SDL.windowRestoredEventWindow = win
>           }) -> unwords
>             [ showEvent win, "window_restored" ]
> 
>       SDL.MouseMotionEvent
>         (SDL.MouseMotionEventData
>           { SDL.mouseMotionEventWindow    = mwin
>           , SDL.mouseMotionEventWhich     = dev
>           , SDL.mouseMotionEventPos       = pos
>           , SDL.mouseMotionEventRelMotion = dpos
>           , SDL.mouseMotionEventState     = buttons
>           }) -> unwords
>             [ showEvent mwin, "mouse_move", showEvent dev
>             , showEvent pos, relPos dpos, showEvent buttons ]
> 
>       SDL.MouseButtonEvent
>         (SDL.MouseButtonEventData
>           { SDL.mouseButtonEventWindow  = mwin
>           , SDL.mouseButtonEventWhich   = dev
>           , SDL.mouseButtonEventPos     = pos
>           , SDL.mouseButtonEventButton  = button
>           , SDL.mouseButtonEventMotion  = action
>           , SDL.mouseButtonEventClicks  = num
>           }) -> unwords
>             [ showEvent mwin, "mouse_button", showEvent dev
>             , showEvent pos, showEvent button, showEvent action
>             , showClicks num ]
> 
>       SDL.MouseWheelEvent
>         (SDL.MouseWheelEventData
>           { SDL.mouseWheelEventWindow    = mwin
>           , SDL.mouseWheelEventWhich     = dev
>           , SDL.mouseWheelEventPos       = pos
>           , SDL.mouseWheelEventDirection = dir
>           }) -> unwords
>             [ showEvent mwin, "mouse_wheel", showEvent dev
>             , showScrollPos pos, showEvent dir ]
> 
>       SDL.KeyboardEvent
>         (SDL.KeyboardEventData
>           { SDL.keyboardEventWindow    = mwin
>           , SDL.keyboardEventKeyMotion = motion
>           , SDL.keyboardEventRepeat    = rpt
>           , SDL.keyboardEventKeysym = SDL.Keysym
>             { SDL.keysymScancode = scancode
>             , SDL.keysymKeycode  = keycode
>             , SDL.keysymModifier = modifier
>             }
>           }) -> unwords
>             [ showEvent mwin, "keyboard", showEvent motion
>             , showRepeat rpt, showScancode scancode
>             , showKeycode keycode, showEvent modifier ]
> 
>       SDL.TextEditingEvent
>         (SDL.TextEditingEventData
>           { SDL.textEditingEventWindow = mwin
>           , SDL.textEditingEventText   = text
>           , SDL.textEditingEventStart  = start
>           , SDL.textEditingEventLength = len
>           }) -> unwords
>             [ showEvent mwin, "text_edit", showStart start
>             , showLength len, showText text ]
> 
>       SDL.TextInputEvent
>         (SDL.TextInputEventData
>           { SDL.textInputEventWindow = mwin
>           , SDL.textInputEventText   = text
>           }) -> unwords
>             [ showEvent mwin, "text_input", showText text ]
> 
>       SDL.KeymapChangedEvent -> "keymap_changed"
> 
>       SDL.JoyAxisEvent
>         (SDL.JoyAxisEventData
>           { SDL.joyAxisEventWhich = which
>           , SDL.joyAxisEventAxis  = idx
>           , SDL.joyAxisEventValue = val
>           }) -> unwords
>             [ "joystick_axis", showJoystickId which
>             , showAxisId idx, showAxisValue val ]
> 
>       SDL.JoyBallEvent
>         (SDL.JoyBallEventData
>           { SDL.joyBallEventWhich     = which
>           , SDL.joyBallEventBall      = idx
>           , SDL.joyBallEventRelMotion = rel
>           }) -> unwords
>             [ "trackball", showJoystickId which
>             , showBallId idx, showBallRelMotion rel ]
> 
>       SDL.JoyHatEvent
>         (SDL.JoyHatEventData
>           { SDL.joyHatEventWhich = which
>           , SDL.joyHatEventHat   = idx
>           , SDL.joyHatEventValue = pos
>           }) -> unwords
>             [ "joystick_hat", showJoystickId which
>             , showHatId idx, showEvent pos ]
> 
>       SDL.JoyButtonEvent
>         (SDL.JoyButtonEventData
>           { SDL.joyButtonEventWhich  = which
>           , SDL.joyButtonEventButton = idx
>           , SDL.joyButtonEventState  = st
>           }) -> unwords
>             [ "joystick_button", showJoystickId which
>             , showJoyButtonId idx, showEvent st ]
> 
>       SDL.JoyDeviceEvent
>         (SDL.JoyDeviceEventData
>           { SDL.joyDeviceEventConnection = conn
>           , SDL.joyDeviceEventWhich      = which
>           }) -> unwords
>             [ "joystick_device", showJoystickId which
>             , showEvent conn ]
> 
>       SDL.ControllerAxisEvent
>         (SDL.ControllerAxisEventData
>           { SDL.controllerAxisEventWhich = which
>           , SDL.controllerAxisEventAxis  = idx
>           , SDL.controllerAxisEventValue = val
>           }) -> unwords
>             [ "controller_axis", showControllerId which
>             , showAxisId idx, showAxisValue val ]
> 
>       SDL.ControllerButtonEvent
>         (SDL.ControllerButtonEventData
>           { SDL.controllerButtonEventWhich  = which
>           , SDL.controllerButtonEventButton = button
>           , SDL.controllerButtonEventState  = st
>           }) -> unwords
>             [ "controller_button", showControllerId which
>             , showEvent button, showEvent st ]
> 
>       SDL.ControllerDeviceEvent
>         (SDL.ControllerDeviceEventData
>           { SDL.controllerDeviceEventConnection = conn
>           , SDL.controllerDeviceEventWhich      = which
>           }) -> unwords
>             [ "controller_device", showControllerId which
>             , showEvent conn ]
> 
>       SDL.AudioDeviceEvent
>         (SDL.AudioDeviceEventData
>           { SDL.audioDeviceEventIsAddition = isAddition
>           , SDL.audioDeviceEventWhich      = idx
>           , SDL.audioDeviceEventIsCapture  = isCapture
>           }) -> unwords
>             [ "audio_device", showAudioId idx
>             , showIsAddition isAddition, showIsCapture isCapture ]
> 
>       SDL.TouchFingerEvent
>         (SDL.TouchFingerEventData
>           { SDL.touchFingerEventTouchID  = touchId
>           , SDL.touchFingerEventFingerID = fingerId
>           , SDL.touchFingerEventMotion   = motion
>           , SDL.touchFingerEventPos      = pos
>           , SDL.touchFingerEventPressure = pressure
>           }) -> unwords
>             [ "touch_finger", showTouchId touchId, showFingerId fingerId
>             , showEvent motion, showFingerPos pos, showPressure pressure ]
> 
>       SDL.TouchFingerMotionEvent
>         (SDL.TouchFingerMotionEventData
>           { SDL.touchFingerMotionEventTouchID   = touchId
>           , SDL.touchFingerMotionEventFingerID  = fingerId
>           , SDL.touchFingerMotionEventPos       = pos
>           , SDL.touchFingerMotionEventRelMotion = rel
>           , SDL.touchFingerMotionEventPressure  = pressure
>           }) -> unwords
>             [ "touch_finger_motion", showTouchId touchId, showFingerId fingerId
>             , showFingerPos pos, showPressure pressure, showFingerRelMotion rel ]
> 
>       SDL.MultiGestureEvent
>         (SDL.MultiGestureEventData
>           { SDL.multiGestureEventTouchID    = touchId
>           , SDL.multiGestureEventDTheta     = dtheta
>           , SDL.multiGestureEventDDist      = ddist
>           , SDL.multiGestureEventPos        = pos
>           , SDL.multiGestureEventNumFingers = count
>           }) -> unwords
>             [ "multi_gesture", showTouchId touchId, showFingerPos pos
>             , showDTheta dtheta, showDDist ddist, showFingerCount count ]
> 
>       SDL.DollarGestureEvent
>         (SDL.DollarGestureEventData
>           { SDL.dollarGestureEventTouchID    = touchId
>           , SDL.dollarGestureEventGestureID  = gestureId
>           , SDL.dollarGestureEventNumFingers = count
>           , SDL.dollarGestureEventError      = err
>           , SDL.dollarGestureEventPos        = pos
>           }) -> unwords
>             [ "dollar_gesture", showTouchId touchId, showGestureId gestureId
>             , showFingerCount count, showFingerPos pos, showGestureError err ]
> 
>       SDL.DropEvent
>         (SDL.DropEventData
>           { SDL.dropEventFile = file
>           }) -> unwords
>             [ "drop_event" ]
> 
>       SDL.ClipboardUpdateEvent -> "clipboard_update"
> 
>       SDL.UserEvent
>         (SDL.UserEventData
>           { SDL.userEventType   = t
>           , SDL.userEventWindow = mwin
>           , SDL.userEventCode   = code
>           }) -> unwords
>             [ "user_event", showEvent mwin
>             , "type:" <> (C8.pack $ show t)
>             , "code:" <> (C8.pack $ show code) ]
> 
>       SDL.SysWMEvent
>         (SDL.SysWMEventData
>           { SDL.sysWMEventMsg = msg
>           }) -> unwords
>             [ "sys_event" ]
> 
>       SDL.QuitEvent -> "quit"
> 
>       SDL.UnknownEvent
>         (SDL.UnknownEventData
>           { SDL.unknownEventType = t
>           }) -> unwords
>             [ "unknown_event", "type:" <> (C8.pack $ show t) ]
