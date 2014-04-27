import qualified DyadicMap as DM

import Data.Serialize
import Data.Word
import qualified Data.ByteString as BS

-- Const Tree of height 8
data DP a b = DP b (DM.Map8 (DP a b))

fromMap :: (Serialize a) => (a -> b) -> DP a b
fromMap f = fromMap' (f . fromRight . decode . BS.pack)
   where
     fromRight (Right x) = x
     fromRight _ = error "decoded illegal value"
     fromMap' :: ([Word8] -> b) -> DP a b
     fromMap' f' = DP (f' []) $ DM.create (\i -> fromMap' (f' . (i:)))

eval :: (Serialize a) => DP a b -> a -> b
eval dp x = eval' dp (BS.unpack $ encode x)
   where
     eval' (DP r _) [] = r
     eval' (DP _ a) (b:bs) = eval' (DM.lookup b a) bs


dpfib :: DP Integer Integer
dpfib = fromMap fib

fib' :: Integer -> Integer
fib' x = eval dpfib x

fib :: Integer -> Integer
fib x
  | x == 0 = 1
  | x == 1 = 2
  | otherwise = fib' (x-1) + fib' (x-2)

main :: IO ()
main = putStrLn $ show $ fib 180
