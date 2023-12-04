module MySDL.Draw (initDraw,draw) where

import SDL.Video (Renderer)
import SDL.Video.Renderer (Surface,Texture,SurfacePixelFormat(..),Rectangle(..),PixelFormat(..)
                          ,rendererDrawColor,clear,destroyTexture
                          ,createTextureFromSurface,copyEx,present
                          ,lockSurface,unlockSurface,surfacePixels,surfaceFormat,createRGBSurfaceFrom)
import SDL (($=))
import SDL.Vect (Point(P),V2(..),V4(..))
import SDL.Internal.Numbered (fromNumber)
import qualified SDL.Raw.Types as SDLT
import Foreign.Ptr (castPtr)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Storable (peek)
import qualified Data.Vector.Storable.Mutable  as VM
import Control.Monad.IO.Class (MonadIO)
import System.Random.Shuffle (shuffleM)
import Data.Word (Word8)
import MyData (backColor)

initDraw :: MonadIO m => Renderer -> m ()
initDraw re = do
  rendererDrawColor re $= backColor
  clear re

draw :: Renderer -> [Surface] -> IO ()
draw re imageS = do
  imageTO <- mapM (createTextureFromSurface re) imageS
  let imageS0 = head imageS
  -- メモリ操作のため surfaceをロックする
  lockSurface imageS0
  -- surfaceのPixelForat情報があるポインタを取得
  SurfacePixelFormat pointerPixFormat <- surfaceFormat imageS0
  -- ポインタ（アドレス）の情報を読む
  surPixFormat <- peek pointerPixFormat
  -- このPixelFormatは SDL.Raw.Type で定義されてゐるもので
  -- pixelFormatFormat といふデータに SDL.Video.Renderer で定義される PixelFormat の情報が
  -- Word32といふ型で格納されてゐる
  -- これは print で表示すると 整数なのだが これを fromNumber で PixelFormatの形式に變換する
  let sPixFormat = fromNumber (SDLT.pixelFormatFormat surPixFormat) :: PixelFormat
  -- イメージのsurfaceのピクセル情報が格納されてゐるアドレスを得る（ポインタ）
  pointer0 <- surfacePixels imageS0
  -- データからsurfaceをつくるためには IOVector Word8 といふ型でデータを保持しなければならない
  -- IOVector は MVector (PrimState IO) と同義であり
  -- surfacePixels函數で得られる Ptr () を castPtr で Ptr Word8 へ變換し
  -- Ptr Word8 を newForeignPtr_ で ForeignPtr Word8 へ變換する
  frPointer <- newForeignPtr_ (castPtr pointer0)
  -- イメージの1ピクセルは 4つのWord8（0〜255）のデータで表される
  -- Word8の総数は 4*64*64 となる (64*64ピクセルだから)
  -- この長さを ポインタを起点としたアドレスから讀み込み MVector型のデータを得る
  let mvector = VM.unsafeFromForeignPtr0 frPointer (4*64*64) :: (VM.MVector (VM.PrimState IO) Word8)
  -- ロックを解除
  unlockSurface imageS0
  mvector2 <- VM.clone mvector
  mapM_ (\y -> mapM_ (\x -> sfl4x4 mvector2 (V2 x y)) [0..15]) [0..15]

--  four <- take4x4 mvector2 (V2 1 1)
--  four2 <- shuffleList four
--  put4x4 mvector2 four2 (V2 1 1)
--  print four

  -- MVector Word8 型のデータ, surfaceのサイズ, ピッチ(1行のピクセル(64px)のバイト數),
  -- そして 先程求めたsPixFormat (このイメージはABGR8888だった)を使い 新たなsurfaceをつくる
  newImageS0 <- createRGBSurfaceFrom mvector2 (V2 64 64) (4*64) sPixFormat
  newImageT <- createTextureFromSurface re newImageS0
  let imageTextures = imageTO ++ [newImageT]
  initDraw re
  imageDraw re imageTextures 
  mapM_ destroyTexture imageTextures 
  present re

sfl4x4 :: VM.IOVector Word8 -> V2 Int -> IO () 
sfl4x4 vect pos = do
  four <- take4x4 vect pos 
  four2 <- shuffleList four
  put4x4 vect four2 pos 

shuffleList :: [[V4 Word8]] -> IO [[V4 Word8]]
shuffleList tgt = do
  newList <- shuffleM tgt
  mapM shuffleM newList

pToI :: V2 Int -> Int
pToI (V2 p q) = 4*(64*q + p)

put4x4 :: VM.IOVector Word8 -> [[V4 Word8]] -> V2 Int -> IO () 
put4x4 vect list (V2 a b) = writeList vect list (16*(64*b + a)) 

writeList :: VM.IOVector Word8 -> [[V4 Word8]] -> Int -> IO ()
writeList _ [] _ = return ()
writeList vect (y:ys) si = do
  writeListX vect y si
  writeList vect ys (si+4*64)

writeListX :: VM.IOVector Word8 -> [V4 Word8] -> Int -> IO () 
writeListX _ [] _ = return ()
writeListX vect ((V4 a b c d):xs) si = do
  mapM_ (uncurry (VM.write vect)) (zip (map (+si) [0,1,2,3]) [a,b,c,d])
  writeListX vect xs (si+4)

take4x4 :: VM.IOVector Word8 -> V2 Int -> IO [[V4 Word8]]
take4x4 vect (V2 a b) =
  let si = 16*(64*b + a)
   in makeList vect si (V2 0 0) 

makeList :: VM.IOVector Word8 -> Int -> V2 Int -> IO [[V4 Word8]]
makeList _ _ (V2 _ 4) = return []
makeList vect si (V2 _ q) = do
  x <- makeListX vect si (V2 0 q)
  xs <- makeList vect si (V2 0 (q+1))
  return (x : xs)

makeListX :: VM.IOVector Word8 -> Int -> V2 Int -> IO [V4 Word8]
makeListX _ _ (V2 4 _) = return []
makeListX vect si (V2 p q) = do
  x <- makeV4 vect (si+pToI (V2 p q))
  xs <- makeListX vect si (V2 (p+1) q)
  return (x : xs)

makeV4 :: VM.IOVector Word8 -> Int -> IO (V4 Word8)
makeV4 vect i = do 
  [a,b,c,d] <- mapM (VM.read vect) [i..(i+3)]
  return (V4 a b c d)

imageDraw :: Renderer -> [Texture] -> IO ()
imageDraw re imageT = 
  mapM_ (\x -> copyEx re (imageT!!x) (Just (Rectangle (P (V2 0 0)) (V2 64 64)))
                                     (Just (Rectangle (P (V2 (fromIntegral x*70+100) 100)) (V2 64 64)))
                                     0 Nothing (V2 False False)) [0..(length imageT-1)]


