module Main where


import qualified Data.Bitstream.Lazy as Bi
import           Data.List.Split
import           System.Environment

main :: IO()
main = getArgs >>= parse
parse ["-v"] = putStrLn "genesisPal2Act tool 0.1"
parse [name] = convert name
parse _ = putStrLn "Usage: genesisPal2Act [-v] [FILE]"

convert :: String -> IO()
convert name = do
  i <-   Bi.readFile name--load and divide by nybbles
  let
    input = chunksOf 4 $ Bi.unpack (i :: Bi.Bitstream Bi.Right)
    out = conv input
  Bi.writeFile "pal.act" (Bi.pack out :: Bi.Bitstream Bi.Right)

conv :: [[Bool]] -> [Bool]
conv [] = []
conv (nul: b: g: r: rest) = r ++ tailAlign ++ g ++ tailAlign ++ b ++ tailAlign ++ conv rest
  where tailAlign = [False, False, False, False] --align value with zeros
