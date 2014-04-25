import Data.List
import Data.Maybe
import Control.Monad
import Data.Serialize
import qualified DyadicMap as DM
import qualified Data.ByteString as BS
import Data.Array.IArray
import Data.Word

-- Const Tree of height 8
data DP a b = DP b (DM.Map8 (DP a b))

fromMap :: (Serialize a) => (a -> b) -> DP a b
fromMap f = fromMap' (f . fromRight . decode . BS.pack)
   where
     fromRight (Right x) = x
     fromMap' :: ([Word8] -> b) -> DP a b
     fromMap' f' = DP (f' []) $ DM.create (\i -> fromMap' (f' . (i:)))

eval :: (Serialize a) => DP a b -> a -> b
eval dp x = eval' dp (BS.unpack $ encode x)
   where
     eval' (DP r _) [] = r
     eval' (DP _ a) (b:bs) = eval' (DM.lookup b a) bs


dp = fromMap f
f' x = eval dp x

f :: Integer -> Integer
f x
  | x == 0 = 1
  | x == 1 = 2
  | otherwise = f' (x-1) + f' (x-2)

main :: IO ()
main = putStrLn $ show $ f 180
