module Main where
import           Control.Arrow
import           Control.Monad.Except
import           Data.Binary.Put      (putWord16be, runPut)
import           Data.Bits
import qualified Data.ByteString.Lazy as Bs
import           Data.List
import           Data.List.Split
import           Data.Word            (Word16, Word8)
import           Text.Printf

data RleEntry = Raw [Word8] | Code {len:: Int, val:: Word8}
  deriving Show


listToHex :: [Word8] -> String
listToHex xs = "["++ concatMap (printf "0x%02X,") xs ++ "]"

lutEncodedOffset = 0x22CFE
idxsEncodedOffset = 0x22B00

getLutEncoded :: [Word8] -> [Word8]
getLutEncoded [] = error "Unexpected end of copy stream"
getLutEncoded (0:_) = [] --copy stop command
getLutEncoded (countByte:xs) = if countByte >= 0x80
  then take  count xs ++ getLutEncoded (drop count xs) --hibit is set - copy
  else error "Unknown command byte in copy command" --not set, unknown command
  where count = (fromIntegral countByte .&. 0x7F) :: Int --lower 7 bits are copy count

deserialize :: [Word8] -> [RleEntry]
deserialize [] = error "Unexpected end of RLE stream"
deserialize (0:_) = [] --end of RLE stream
deserialize (countByte:xs) = if countByte >= 0x80
  then Raw (take count xs) : deserialize (drop count xs)--it's Raw
  else Code count (head xs) : deserialize (tail xs)
  where count = (fromIntegral countByte .&. 0x7F) :: Int --lower 7 bits are RLE count

decode :: [RleEntry] -> [Word8]
decode [] = []
decode (Raw bs : xs) = bs ++ decode xs
decode (Code l v : xs) = replicate l v ++ decode xs

lookUpIdxs :: [Int] -> [[Word16]] -> [Word16]
lookUpIdxs idxs lut = concatMap (lut !!) idxs

mergeToWord16 :: Word8 -> Word8 -> Word16 --Merge 2 bytes in one word
mergeToWord16 x y = (fromIntegral x `shiftL` 8) .|. fromIntegral y


main :: IO ()
main = do
  input <- Bs.readFile "../Jewel Master (UE) [!].bin"
  let
    lutEncoded = getLutEncoded $ Bs.unpack $ Bs.drop lutEncodedOffset input
    entriesAttr = deserialize lutEncoded
    decodedAttr = decode entriesAttr
    entriesTileIdx = deserialize (drop 0xF0 lutEncoded) --skip encoded attributes
    decodedTileIdx= decode entriesTileIdx
    decodedLut = zipWith mergeToWord16 decodedAttr decodedTileIdx --merge attributes and indexes in proper Genesis NT entry
    lutScanHi = chunksOf 2 decodedLut --lut is 2 tiles wide, that's a lut for hi scanline
    lutScanLo = tail lutScanHi --low scanline is 2 tiles further in serialized lut

    idxsEncoded = Bs.unpack $ Bs.drop idxsEncodedOffset input
    entriesIdxs = deserialize idxsEncoded --encoded with Rle right in ROM - no copy needed
    decodedIdxs = decode entriesIdxs
    idxScanlines :: [[Int]]
    idxScanlines = chunksOf 20 (map ((*2).fromIntegral) decodedIdxs) --title screen is 40 tiles wide and 2x7 tiles in height

    decodeMetaScan idxs = lookUpIdxs idxs lutScanHi ++ lookUpIdxs idxs lutScanLo
    decoded = concatMap decodeMetaScan idxScanlines



  --print $ length (decodeMetaScan (head idxScanlines))
  --Bs.writeFile "decodedAttr.bin" (Bs.pack decodedAttr)
  Bs.writeFile "decodedLut.bin" $ runPut $ mapM_ putWord16be decodedLut
  Bs.writeFile "decodedIndexes.bin" (Bs.pack decodedIdxs)
  Bs.writeFile "decoded.bin" $ runPut $ mapM_ putWord16be decoded
