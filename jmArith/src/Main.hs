module Main where
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.Bitstream.Lazy  as Bi
import qualified Data.ByteString.Lazy as Bs
import           Data.Int
import           Data.List            (foldl', group, sortBy)
import           Data.List.Split
import qualified Data.Map.Strict      as M
import           Data.Maybe
import           Data.Ord             (comparing)
import           Data.Word
import           System.Environment

data LutEntry = LutEntry  {bitLen:: Word8, rleCount:: Word8, rleData:: Word8}
  deriving (Show)
type LutMap = M.Map Word8 LutEntry --map with offsets in LUT as keys and LutEntries as entries
type EncodeDictionary = M.Map RleEntry [Bool] --reverse map for encoding: value is actual entry code

data RleEntry = Code {len:: Int, val:: Word8} --zero len means 1 raw byte
  deriving (Show, Eq, Ord)

gfxOffset :: Int64
gfxOffset = 0x3A600 --0x3B728 --for sprite strings gfx
lutEntriesCount :: Int
lutEntriesCount = 252 --252 entries are in LUT: last 4 entries for extra entries


----------------------------MAIN------------------------------------------------
main :: IO()
main = getArgs >>= parse
  where
    parse ["-v"] = putStrLn "jmArith compression tool 0.1"
    parse ["-d"] = decodeFile
    parse ["-e"] = encodeFile
    parse _ = putStrLn "Usage: jmArith [-vde]"

------------------------------DECODE--------------------------------------------
decodeFile :: IO()
decodeFile = do
  input <- Bs.readFile "Jewel Master (UE) [!].bin"
  let
    gfxPixSize = runGet getSize input
    rleEntries = runGet getRleEntries input
    decoded = decode gfxPixSize rleEntries
    decodedDecrypted = xorDecrypt $ mergeNybbles decoded
  Bs.writeFile "decoded.bin" $ Bs.pack decodedDecrypted

getSize :: Get Int
getSize = do
  skip $ fromIntegral gfxOffset
  size <- getWord16be
  if size < 0x8000 then fail "Not-encrypted scheme not implemented" --hibit is a xor-encryption flag
    else return $ fromIntegral (size .&. 0x7FFF) * 8 * 8  --tile count * tile size in pixels

getRleEntries :: Get [RleEntry]
getRleEntries = do
  skip $ fromIntegral $ gfxOffset + 2 --LUT is right after size word
  lut <- getLut
  encodedStream <- getRemainingLazyByteString --compressed stream right after LUT
  return $ deserialize lut $ toBoolStream encodedStream

getLut :: Get LutMap
getLut = getLut' 0 --init shareable pixValue
getLut' :: Word8 -> Get LutMap
getLut' pixVal = do
  let
    hiNybble x = (x `shiftR` 4) .&. 0xF
    loNybble x = x .&. 0xF
    offs x bl = x `shiftL` fromIntegral (8 - bl) --each lut entry takes 2^bitlen bytes in LUT
  command <- getWord8
  case command of
    0xFF -> return M.empty--end of LUT byte reached
    a | a < 0x80 -> do --old pixel value will be used
          b <- getWord8
          rest <- getLut' pixVal
          return $ M.insert (offs b (loNybble a)) LutEntry {bitLen = loNybble a, rleCount = hiNybble a, rleData = pixVal} rest
      | otherwise -> do  --read and use new pixel value
          b <- getWord8
          c <- getWord8
          rest <- getLut' $ loNybble a
          return $ M.insert (offs c (loNybble b)) LutEntry {bitLen = loNybble b, rleCount = hiNybble b, rleData = loNybble a} rest

toBoolStream :: Bs.ByteString -> [Bool] --convert to bytestream and then to bool via bitstream
toBoolStream inputString = Bi.unpack (Bi.fromByteString inputString :: Bi.Bitstream Bi.Right)

toBitStream :: [Bool] -> Bi.Bitstream Bi.Right
toBitStream = Bi.pack

bitsToNum :: (Num a) => [Bool] -> a
bitsToNum = foldl' (\byte b -> byte*2 + if b then 1 else 0) 0

numToBits :: (Bits a, Num a) => Int -> a -> [Bool] --number with a given bitfield width to bits list
numToBits n b = map (testBit b) [n-1, n-2..0]

deserialize :: LutMap -> [Bool] -> [RleEntry]
deserialize _ [] = []
deserialize lutMap xs = if lutIdx < 0xFC
  then Code (fromIntegral (rleCount lutEntry)) (rleData lutEntry): deserialize lutMap (drop (fromIntegral(bitLen lutEntry)) xs) --usual RLE entry found in lut
  else Code (bitsToNum (take 3 (drop 6 xs))) (bitsToNum (take 4 (drop (6+3) xs))) : deserialize lutMap (drop (6+7) xs) --raw literal case - extended last 6-bit entry in lut
  where --take 8 bits, check bitcount for current entry and utiilze only that bits, then check another 8 bits
    lutIdx = bitsToNum (safeTake 8 xs)
    safeTake n is = if length (take n is) < n
      then is ++ replicate (n - length is) False -- in case of remainder is shorter than n bits
      else take n is
    lutEntry = snd.fromJust $ M.lookupLE lutIdx lutMap

decode :: Int -> [RleEntry] -> [Word8] --decode RLE to bytes, stop at pixel count achieve
decode _ [] = []
decode count (Code l v : xs) = if count <= 0 then []
                               else replicate (l + 1) v ++ decode (count - l - 1) xs

mergeNybbles :: [Word8] -> [Word8] --merge 2 4-bit pixels in one word8 for storage
mergeNybbles [x] = [x] --if list length is odd, should not happen
mergeNybbles xs = map (\[a, b] -> (a `shiftL` 4) .|. b) $ chunksOf 2 xs

xorDecrypt :: [Word8] -> [Word8] --first word32 is plain. XOR next encrypted word32 with previous plain word32, you get next plain word32
xorDecrypt xs = concat $ scanl1 (zipWith xor) $ chunksOf 4 xs

------------------------------ENCODE--------------------------------------------
encodeFile :: IO()
encodeFile = do
  input <- Bs.readFile "decoded.bin"
  let
    decodedEncrypted = xorEncrypt $ Bs.unpack input
    decodedEncryptedPixels = splitNybbles decodedEncrypted
    encoded = trimRlesLen $ encode decodedEncryptedPixels
    histogramSorted = sortBy (flip compare) $ histogram encoded
    --convert frequencies in histogram to range probabilities:
    --252 entries in LUT (last 4 entries for expansion). Total entries count is length of encoded entries list.
    histogramRanges = map (\(a,b) -> (getProbability a (length encoded), b)) histogramSorted
    lutEncode = buildLutMapEncode histogramRanges --build new lut for encoding
    sizeWord = (Bs.length input `shiftR` 5) .|. 0x8000 -- get tiles count from gfx size OR encryption flag
    sizeWordBs = runPut $ putWord16be (fromIntegral sizeWord)
    lutMapSerialized = serializeLutMap lutEncode
    encodeDictionary = buildEncodeDictionary lutEncode
    encodedStream = serialize encodeDictionary encoded
  --append lut and encoded stream in one block for further ROM insert
  Bs.writeFile "encoded.bin" $ Bs.concat [sizeWordBs, Bs.pack lutMapSerialized, Bi.toByteString $ toBitStream encodedStream]

xorEncrypt :: [Word8] -> [Word8] -- just XOR previous plain word32 with next plain word32.
xorEncrypt xs = concat $ zipWith (zipWith xor) plain shifted
  where
     plain = chunksOf 4 xs
     shifted = [0,0,0,0] : plain --prepend with one 0, so first word is decrypted

encode :: [Word8] -> [RleEntry] --RLE encode
encode = map (\xs -> Code (length xs - 1) (head xs)) . group

trimRlesLen :: [RleEntry] -> [RleEntry] --RLE length can be 8 pixels max due to serialization scheme
trimRlesLen = concatMap trimRleLen
  where
    trimRleLen x @ (Code l v) = if l > 7 then Code 7 v : trimRleLen (Code (l-8) v)
                                         else [x]

splitNybbles :: [Word8] -> [Word8] --split merged 4bit pixels values to list of pixels
splitNybbles = concatMap (\x -> [x `shiftR` 4, x .&. 0xF])

-- tuples are swapped, as map had fst as Key, and we should have [(weight, entry)] tuples
histogram :: Ord a => [a] -> [(Int,a)]
histogram xs = swap . M.toList $ M.fromListWith (+) [(c, 1) | c <- xs]
  where swap = map (\(a,b)->(b,a))

getProbability :: Int -> Int -> Int --nearest 2^n value for lut entry size in given entry probability weight
getProbability weight total
  | p < 1 = 0 --less than 1 entry will be encoded by raw entry bits in encoded strm
  | roundedP <= 2 = roundedP --for 1 and 2
  | otherwise = roundToPower2 p
  where
    p = (fromIntegral weight / fromIntegral total) * fromIntegral lutEntriesCount
    roundedP = round p :: Int
    roundToPower2 :: Double -> Int --rounds number to closest 2^n
    roundToPower2 x = if (x - fromIntegral pre) > (fromIntegral suc - x) then suc else pre
      where
        bitSz = floor (logBase 2 x) :: Int--calc how much bits int value takes
        pre = 2^bitSz
        suc = 2^(bitSz + 1)

buildLutMapEncode :: [(Int, RleEntry)] -> LutMap --build a proper lut map from histogarm for seriaization
buildLutMapEncode xs = go lutEntries 0 --second param for offset tracking
  where
    go [] _ = M.empty
    go (e@(LutEntry b _ _) :es) count
      | offs <= fromIntegral lutEntriesCount = M.insert count e $ go es offs
      | otherwise                  = M.empty --index overflow - too many Rle entries
      where offs = count + (2 ^ (8-b))
    lutEntries = map histToLutEntry xs
    histToLutEntry :: (Int, RleEntry) -> LutEntry
    histToLutEntry (c, Code l v) = LutEntry {bitLen = 8 - round bitsFractional, rleCount = fromIntegral l, rleData = v}
      where bitsFractional = logBase 2 (fromIntegral c) :: Double


serializeLutMap :: LutMap -> [Word8] --convert LUT to game's format
serializeLutMap m = serializeMap 0xFF sortedByData --sort entries by pixvalues and serialize
  where -- 0xFF is a pixvalue, which just intis and never equal to real pixvalue, which is 4 bits
    sortedByData = sortBy (comparing (rleData.snd)) $ M.toList m
    serializeMap :: Word8 -> [(Word8, LutEntry)] -> [Word8]
    serializeMap _ [] = [0xFF] --lut is terminated by FF
    serializeMap pixVal ((offs, LutEntry bLen rCount rData) : es)
      | rData /= pixVal = [0x80 .|. rData, (rCount `shiftL` 4) .|. bLen, slotNum] ++ serializeMap rData es --new pix value
      | otherwise = [(rCount `shiftL` 4) .|. bLen, slotNum] ++ serializeMap pixVal es
      where slotNum = offs `shiftR` fromIntegral (8 - bLen)


buildEncodeDictionary ::  LutMap -> EncodeDictionary --convert lut to encoding dictionary
buildEncodeDictionary m = go (M.toList m)
  where
    go :: [(Word8, LutEntry)] -> EncodeDictionary
    go [] = M.empty
    go ((offs, LutEntry bLen rCount rData) : es) = M.insert (Code (fromIntegral rCount) rData) bitCode $ go es
      where
        bitCode = numToBits (fromIntegral bLen) slotNum
        slotNum = offs `shiftR` fromIntegral (8 - bLen)

serialize :: EncodeDictionary -> [RleEntry] -> [Bool] --build a bool stream by the dictionary
serialize dict = concatMap getCode
  where
  getCode e@(Code l v) = fromMaybe
    ([True, True, True, True, True, True] ++ numToBits 3 l ++ numToBits 4 v) --code not found, inject raw in bitstream
    (M.lookup e dict) --code found, return it
