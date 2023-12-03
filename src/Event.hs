module Event (inputEvent) where

import SDL.Input.Keyboard.Codes
import MySDL.Input (input)
import MyData (World,Input(..))

inputEvent :: IO Input
inputEvent = do
  kc <- input
  let isQuit = kc == KeycodeEscape
      ninp
        | isQuit = QIT
        | otherwise = NON
  return ninp
