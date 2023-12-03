module MySDL.Draw (initDraw,draw) where

import SDL.Video (Renderer)
import SDL.Video.Renderer (Surface,Texture,SurfacePixelFormat(..),Rectangle(..),PixelFormat(..)
                          ,rendererDrawColor,clear,destroyTexture
                          ,createTextureFromSurface,copyEx,present
                          ,lockSurface,unlockSurface,surfacePixels,surfaceFormat,createRGBSurfaceFrom)
import SDL (($=))
import SDL.Vect (Point(P),V2(..))
import Foreign.Ptr (castPtr)
import Foreign.ForeignPtr (newForeignPtr_)
import Data.Vector.Storable.Mutable (unsafeFromForeignPtr0)
import Control.Monad.IO.Class (MonadIO)
import MyData (backColor)

initDraw :: MonadIO m => Renderer -> m ()
initDraw re = do
  rendererDrawColor re $= backColor
  clear re

draw :: Renderer -> [Surface] -> IO ()
draw re imageS = do
  imageTO <- mapM (createTextureFromSurface re) imageS
  let imageS0 = head imageS
  lockSurface imageS0
  let pixelFormat = RGBA8888 
  pointer0 <- surfacePixels imageS0
  frPointer <- newForeignPtr_ (castPtr pointer0)
  let mvector = unsafeFromForeignPtr0 frPointer (64*64)
  newImageS0 <- createRGBSurfaceFrom mvector (V2 64 64) 64 pixelFormat
  unlockSurface imageS0
  initDraw re
  imageDraw re imageTO
  mapM_ destroyTexture imageTO
  present re

imageDraw :: Renderer -> [Texture] -> IO ()
imageDraw re imageT = 
  mapM_ (\x -> copyEx re (imageT!!x) (Just (Rectangle (P (V2 0 0)) (V2 64 64)))
                                     (Just (Rectangle (P (V2 (fromIntegral x*70+100) 100)) (V2 64 64)))
                                     0 Nothing (V2 False False)) [0..(length imageT-1)]


