module Main where
import           Control.Arrow
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString.Lazy as Bs
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Word            (Word16, Word8)
import           System.Environment
import           Text.Printf

data RleEntry = Raw [Word8] | Code {len:: Int, val:: Word8}
  deriving (Eq, Show)

data StreamCounter a = StreamCounter {stream :: [a], inVal :: Word8, outVal :: Int} --stream with statistics embedded

listToHex :: [Word8] -> String
listToHex xs = "["++ concatMap (printf "0x%02X,") xs ++ "]"

------------------------------------------------------------------------

lutEncodedOffset = 0x22CFE
idxsEncodedOffset = 0x22B00 --0x22BF2 for background map

unpackCopyCommands :: [Word8] -> [Word8] --data is wrappped in copy commands, null-terminated
unpackCopyCommands [] = error "Unexpected end of copy stream"
unpackCopyCommands (0:_) = [] --copy stop command
unpackCopyCommands (countByte:xs) = if countByte >= 0x80
  then take  count xs ++ unpackCopyCommands (drop count xs) --hibit is set - copy
  else error "Unknown command byte in copy command" --not set, unknown command
  where count = (fromIntegral countByte .&. 0x7F) :: Int --lower 7 bits are copy count

deserialize :: State (StreamCounter Word8) [RleEntry]
deserialize = do --get RLE codes from bytes and return read size in StreamCounter
  s <- get
  case stream s of
    [] -> error "Unexpected end of RLE stream"
    (0:_) -> do --end of RLE packed block reached
      put $ StreamCounter [] 0 (outVal s + 1) --only outval counter needed in return state
      return []
    (countByte:xs) -> do
      let
        count = (fromIntegral countByte .&. 0x7F) :: Int --lower 7 bits are count
        outCount = outVal s
      if countByte >= 0x80 then do --it's Raw
        put $ StreamCounter (drop count xs) 0 (outCount + count + 1)
        rest <- deserialize
        return $ Raw (take count xs) : rest
      else do --it's RLE code
        put $ StreamCounter (tail xs) 0 (outCount + 2)
        rest <- deserialize
        return $ Code count (head xs) : rest


decode :: [RleEntry] -> [Word8]
decode [] = []
decode (Raw bs : xs) = bs ++ decode xs
decode (Code l v : xs) = replicate l v ++ decode xs

lookUpIdxs :: [Int] -> [[Word16]] -> [Word16]
lookUpIdxs idxs lut = concatMap (lut !!) idxs

mergeToWord16 :: Word8 -> Word8 -> Word16 --Merge 2 bytes in one word
mergeToWord16 x y = (fromIntegral x `shiftL` 8) .|. fromIntegral y


-----------------------------------------------------------------------
readWords16 :: Get [Word16]
readWords16 = do
  empty <- isEmpty
  if empty
     then return []
     else do v <- getWord16be
             rest <- readWords16
             return (v : rest)

type HalfMeta =  [Word16]
type Scanline = [HalfMeta]
type Title = [Scanline]

getMetaTiles :: [Scanline] -> [Scanline] --build full metatile scanline from 2 scanlines
getMetaTiles [] = []
getMetaTiles [scan] = error "Scanline count is odd, can't build metatile"
getMetaTiles (scan1 : scan2 : scans) = zipWith (++) scan1 scan2 : getMetaTiles scans

divideOnMetatiles :: Bs.ByteString -> Scanline --get metatiles from raw map
divideOnMetatiles input = concat $ getMetaTiles scanlines
 where
  rawMap = runGet readWords16 input
  scanlines = chunksOf 20 $ chunksOf 2 rawMap --divide on half-metatiles and scanlines


splitFromWord16 :: [Word16] -> ([Word8], [Word8]) --split on hi and lo bytes
splitFromWord16 [] = ([], [])
splitFromWord16 (x:xs) = (fromIntegral (x `shiftR` 8):hs, fromIntegral (x .&. 0xFF):ls)
  where (hs, ls) = splitFromWord16 xs

encode :: [Word8] -> [RleEntry] --RLE encode
encode xs = go $ group xs
go :: [[Word8]] -> [RleEntry]
go [] = []
go s@([x] : xs) = Raw singles : go (drop (length singles) s)
  where singles = concat $ takeWhile (\a -> length a == 1) s
go (x : xs) = Code (length x) (head x) : go xs

trimRlesLen :: [RleEntry] -> [RleEntry] --RLE length can be 0x7F max due to serialization scheme
trimRlesLen = concatMap trimRleLen
  where
    trimRleLen x@ (Code l v) = if l > 0x7F then Code 0x7F v : trimRleLen (Code (l-0x7F) v)
                                            else [x]
    trimRleLen x@ (Raw vs) = if length vs > 0x7F then Raw (take 0x7F vs) : trimRleLen (Raw (drop 0x7F vs))
                                            else [x]

serialize ::  [RleEntry] -> [Word8]
serialize [] = [0] --null terimnated RLE block
serialize (Raw x : xs) = (fromIntegral(length x) .|. 0x80) : x ++ serialize xs
serialize (Code count val : xs) = (fromIntegral count .&. 0x7F) : val : serialize xs

packCopyCommands :: [Word8] -> [Word8] --data is wrappped in copy commands, null-terminated
packCopyCommands xs = if length xs > 0x7F
                        then 0xFF: take 0x7F xs ++ packCopyCommands (drop 0x7F xs)
                        else fromIntegral (length xs .|. 0x80) : xs ++ [0]--last copy block null-terminated


-----------------------MAIN-----------------------------------------------------
main :: IO()
main = getArgs >>= parse
  where
    parse ["-v"] = putStrLn "jmRle compression tool 0.1"
    parse ["-d"] = decodeFile
    parse ["-e"] = encodeFile
    parse _ = putStrLn "Usage: jmRle [-vde]"

    decodeFile = do
      input <- Bs.readFile "Jewel Master (UE) [!].bin"
      let
        lutEncoded = unpackCopyCommands $ Bs.unpack $ Bs.drop lutEncodedOffset input
        (deserializedAttr, finalState) = runState deserialize $ StreamCounter lutEncoded 0 0
        attrSize = outVal finalState --packed index goes right after packed attributes, just unpack 2 times
        decodedAttr = decode deserializedAttr
        deserializedIdx = evalState deserialize $ StreamCounter (drop attrSize lutEncoded) 0 0 --skip encoded attributes
        decodedTileIdx = decode deserializedIdx
        decodedLut = zipWith mergeToWord16 decodedAttr decodedTileIdx --merge attributes and indexes in proper Genesis NT entry
        lutScanHi = chunksOf 2 decodedLut --lut is 2 tiles wide, that's a lut for hi scanline
        lutScanLo = tail lutScanHi --low scanline is 2 tiles further in serialized lut

        idxsEncoded = Bs.unpack $ Bs.drop idxsEncodedOffset input
        entriesIdxs = evalState deserialize $ StreamCounter idxsEncoded 0 0 --encoded with Rle right in ROM - no copy needed
        decodedIdxs = decode entriesIdxs
        titleScanlines:: [[Int]]
        titleScanlines = chunksOf 20 (map ((*2).fromIntegral) decodedIdxs) --title screen is 280 metatiles in size: 20 x 14 metatiles or 40x28 tiles
        decoded = concatMap (\scanlineIdxs -> lookUpIdxs scanlineIdxs lutScanHi ++ lookUpIdxs scanlineIdxs lutScanLo) titleScanlines --lookup 2 scanlines at time


      Bs.writeFile "decodedLut.bin" $ runPut $ mapM_ putWord16be decodedLut
      Bs.writeFile "decoded.bin" $ runPut $ mapM_ putWord16be decoded

    encodeFile = do
      frontInput <- Bs.readFile "decoded.bin"
      bkgInput <- Bs.readFile "decodedBkg.bin"
      let
        input = Bs.append frontInput bkgInput
        metaTiles = divideOnMetatiles input
        lutList = sort.nub $ metaTiles --list of all unique metatiles in title
        (lutAttr, lutTileIdx) = splitFromWord16 (concat (lutList ++ replicate (0x100 - length lutList) [0,0,0,0]))
        lutSerialized = serialize (trimRlesLen (encode lutAttr)) ++ serialize (trimRlesLen (encode lutTileIdx))
        lutPackedInCopy = packCopyCommands lutSerialized

        lut = zip lutList [0..]

        frontMetaTiles = divideOnMetatiles frontInput
        frontIndexes = map (\mTile -> fromJust (lookup mTile lut)) frontMetaTiles
        frontIndexesSerialized = serialize (trimRlesLen (encode frontIndexes))

        bkgMetaTiles = divideOnMetatiles bkgInput
        bkgIndexes = map (\mTile -> fromJust (lookup mTile lut)) bkgMetaTiles
        bkgIndexesSerialized = serialize (trimRlesLen (encode bkgIndexes))

      Bs.writeFile "encodedLut.bin" $ Bs.pack lutPackedInCopy
      Bs.writeFile "encodedFront.bin" $ Bs.pack frontIndexesSerialized
      Bs.writeFile "encodedBkg.bin" $ Bs.pack bkgIndexesSerialized
