{-# LANGUAGE OverloadedStrings #-}
module MyData where

import qualified Data.Text as T
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import Data.Word (Word8,Word32)

data World = World {cnt :: Int, abc :: Int}
data Input = NON | QIT deriving (Eq, Show)

type Color = V4 Word8

title :: T.Text
title = "SDL Experiment"

initWorld :: World
initWorld = World {cnt = 0, abc = 0}

imageFiles :: [FilePath]
imageFiles = ["images/image0.png"]

winSizeX, winSizeY :: CInt
winSizeX = 900; winSizeY = 600

windowSize :: V2 CInt
windowSize = V2 winSizeX winSizeY

backColor :: Color
backColor = V4 182 100 255 255

delayTime :: Word32
delayTime = 50
