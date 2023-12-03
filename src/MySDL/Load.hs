module MySDL.Load (loadFiles) where

import qualified SDL.Image as I
import SDL.Video.Renderer (Surface)
import MyData (imageFiles)

loadFiles :: IO [Surface]
loadFiles = do
  imageS <- loadImages imageFiles
  return imageS

loadImages :: [FilePath] -> IO [Surface]
loadImages = mapM I.load
