
module Main where
import           Data.Bits
import qualified Data.Bitstream.Lazy  as Bi
import qualified Data.ByteString.Lazy as Bs
import           Data.List            (foldl')
import qualified Data.Map.Strict      as M
import           Data.Maybe           (fromJust)
import           Data.Word
import Data.Binary.Get
import Data.List.Split

data LutEntry = LutEntry {bitLen:: Word8, rleCount:: Word8, rleData:: Word8}
  deriving Show
type LutMap = M.Map Word8 LutEntry --map with offsets in LUT as keys and LutEntries as entries
data RleEntry = Code {len:: Int, val:: Word8}
  deriving Show

gfxSizeOffset = 0x3A600
lutEncodedOffset = 0x3A602
gfxEncodedOffset = 0x3A653

fillLut :: Word8 -> [Word8] -> LutMap
fillLut _ (0xFF:_) = M.empty
fillLut pixVal (a:b:c:stream)
  | a < 0x80 = M.insert (offs b (loNybble a)) LutEntry {bitLen = loNybble a, rleCount = (a `shiftR` 4) .&. 0xF, rleData = pixVal} $ fillLut pixVal (c:stream) --dont read new pix value
  | otherwise = M.insert (offs c (loNybble b)) LutEntry {bitLen = loNybble b, rleCount = (b `shiftR` 4) .&. 0xF, rleData = loNybble a} $ fillLut (loNybble a) stream
      where
        loNybble x = x .&. 0xF
        offs x bl = x `shiftL` fromIntegral (8 - bl) --each lut entry takes 2^bitlen bytes in LUT

-----------------------------------------DECODING-----------------------------------------------------------------------------------
toBoolStream :: Bs.ByteString -> [Bool] --convert to bytestream and then to bool via bitstream
toBoolStream inputString = Bi.unpack (Bi.fromByteString inputString :: Bi.Bitstream Bi.Right)

bitsToNum :: (Num a) => [Bool] -> a
bitsToNum = foldl' (\byte b -> byte*2 + if b then 1 else 0) 0

deserialize :: LutMap -> [Bool] -> [RleEntry]
deserialize lutMap xs = if lutIdx < 0xFC
  then Code (fromIntegral (rleCount lutEntry)) (rleData lutEntry): deserialize lutMap (drop (fromIntegral(bitLen lutEntry)) xs) --usual RLE entry found in lut
  else Code (bitsToNum (take 3 (drop 6 xs))) (bitsToNum (take 4 (drop (6+3) xs))) : deserialize lutMap (drop (6+7) xs) --raw literal case - extended last 6-bit entry in lut
  where --take 8 bits, check bitcount for current entry and utiilze only that bits, then check another 8 bits
    lutIdx = bitsToNum (take 8 xs)
    lutEntry = snd.fromJust $ M.lookupLE lutIdx lutMap

decode :: Int -> [RleEntry] -> [Word8]
decode count (Code l v : xs) = if count <= 0 then []
                               else replicate (l + 1) v ++ decode (count - l - 1) xs

mergeNybbles :: [Word8] -> [Word8] --merge 2 4-bit pixels in one word8 for storage
mergeNybbles [] = []
mergeNybbles [x] = [x] --if list length is even, should not happen
mergeNybbles (a:b:xs) = ((a `shiftL` 4) .|. b) : mergeNybbles xs

xorWords :: [Word8] -> [Word8] --xor 2 32bit words
xorWords xs = concat $ xorLists [0,0,0,0] $ chunksOf 4 xs
  where
    xorLists :: [Word8] -> [[Word8]] -> [[Word8]]
    xorLists _ [] = []
    xorLists prev (cur:xs) = xored : xorLists xored xs
      where xored = zipWith xor prev cur


main :: IO ()
main = do
  input <- Bs.readFile "../Jewel Master (UE) [!].bin"
  let
    lutEncoded = Bs.unpack $ Bs.drop lutEncodedOffset input
    lut = fillLut 0 lutEncoded
    gfxPixSize =
      if sizeWord < 0x8000
        then error "Not-encrypted scheme not implemented" --hibit is a xor-encryption flag
        else fromIntegral (sizeWord .&. 0x7FFF) * 8 * 8 --tile count * tile size in pixels
      where sizeWord = runGet getWord16be $ Bs.drop gfxSizeOffset input
    encodedStream = toBoolStream $ Bs.drop gfxEncodedOffset input
    rleEntries = deserialize lut encodedStream
    decoded = decode gfxPixSize rleEntries
    decodedSerialized = xorWords $ mergeNybbles decoded
  --print $ length decodedEncrypted
  Bs.writeFile "decoded.bin" $ Bs.pack decodedSerialized
