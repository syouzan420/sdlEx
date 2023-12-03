module MySDL.Video (withVideo) where

import SDL.Video (createWindow, defaultWindow, windowInitialSize
                 ,createRenderer, defaultRenderer, destroyWindow)

import SDL.Video.Renderer (Renderer,present)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import MySDL.Draw (initDraw)
import MyData (windowSize, title)

withVideo :: (MonadIO m) => (Renderer -> m a) -> m ()
withVideo op = do
  window <- createWindow title (defaultWindow {windowInitialSize = windowSize})
  renderer <- createRenderer window (-1) defaultRenderer
  initDraw renderer
  present renderer
  void $ op renderer
  destroyWindow window
