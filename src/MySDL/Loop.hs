module MySDL.Loop where

import Control.Monad.State.Strict (StateT,get,put)
import Control.Monad.IO.Class (liftIO)
import SDL.Video.Renderer (Renderer,Surface)
import SDL.Time (delay)
import MySDL.Draw (draw)
import MyData (World(..), Input(..),  delayTime)
import Event (inputEvent)

loop :: Renderer -> [Surface] -> StateT World IO ()
loop re imageS = do
  inp <- liftIO inputEvent
  wld <- get
  let cntWld = cnt wld
  let newWorld = wld{cnt=cntWld + 1}
  --liftIO (print (cnt wld))
  put newWorld
  liftIO $ draw re imageS cntWld
  delay delayTime
  if inp==QIT then return () else loop re imageS
