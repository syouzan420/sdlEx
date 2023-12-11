module Lib where

type Len = Int
type Amp = Int
type Count = Int
type Shift = Int

cosList :: Len -> Amp -> Count -> [Int]
cosList l a c = map (\x -> round (fromIntegral a * cos ((2*pi/fromIntegral l)*(fromIntegral x-fromIntegral c)))) [0..(l-1)] 

shiftList :: Shift -> [a] -> [a]
shiftList _ [] = []
shiftList s lst 
  | absS > lng = lst
  | otherwise = if s>0 then replicate s (head lst) ++ take (lng-s) lst
                       else drop absS lst ++ replicate absS (last lst)
       where lng = length lst
             absS = abs s
