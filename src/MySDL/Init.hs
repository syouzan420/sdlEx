module MySDL.Init (withInit) where

import SDL (quit)
import SDL.Init (initializeAll)
import qualified SDL.Image as I
import Control.Monad.IO.Class (MonadIO)

withInit :: MonadIO m => m a -> m ()
withInit op = do
  initializeAll
  I.initialize []
  _ <- op
  quit
