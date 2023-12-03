module App(appMain) where

import Control.Monad.State.Strict (runStateT)
import MySDL.Load (loadFiles)
import MySDL.Loop (loop)
import MySDL.Init (withInit)
import MySDL.Video (withVideo)
import MyData (initWorld)

appMain :: IO ()
appMain =
  withInit $ do
    imageS <- loadFiles
    withVideo $
      \renderer -> runStateT (loop renderer imageS) initWorld
