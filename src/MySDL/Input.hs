module MySDL.Input (input) where

import SDL.Event (EventPayload(KeyboardEvent),eventPayload,keyboardEventKeyMotion
                 ,InputMotion(Pressed),keyboardEventKeysym,pollEvents)
import SDL.Input.Keyboard (Keysym(keysymKeycode))
import SDL.Input.Keyboard.Codes
import Data.Maybe (fromMaybe)
import Data.List (find)

input :: IO Keycode
input = do
  events <- pollEvents
  let fkc event = case eventPayload event of
                    KeyboardEvent keyboardEvent ->
                      if keyboardEventKeyMotion keyboardEvent == Pressed
                         then let kbeSim = keyboardEventKeysym keyboardEvent
                               in keysymKeycode kbeSim
                         else KeycodeUnknown
                    _ -> KeycodeUnknown
      kc = fromMaybe KeycodeUnknown $ find (/=KeycodeUnknown) (map fkc events)
  return kc
  
