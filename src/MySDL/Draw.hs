module MySDL.Draw (initDraw) where

import SDL.Video (Renderer)
import SDL.Video.Renderer (rendererDrawColor,clear)
import SDL (($=))
import Control.Monad.IO.Class (MonadIO)
import MyData (backColor)

initDraw :: MonadIO m => Renderer -> m ()
initDraw re = do
  rendererDrawColor re $= backColor
  clear re
