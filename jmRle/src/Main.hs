module Main where

import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString.Lazy as Bs
import           Data.Int
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Word            (Word16, Word8)
import           System.Environment
import           Text.Printf

data RleEntry = Raw [Word8] | Code {len:: Int, val:: Word8}
  deriving (Eq, Show)

listToHex :: [Word8] -> String
listToHex xs = "["++ concatMap (printf "0x%02X,") xs ++ "]"

------------------------------------------------------------------------
lutEncodedOffset :: Int64
lutEncodedOffset = 0x22CFE
idxsEncodedOffset :: Int64
idxsEncodedOffset = 0x22B00 --0x22BF2 for background map

type RleBlock = [RleEntry]
data LutRleEntries = LutRleEntries {hiByteRleEntries :: RleBlock, loByteRleEntries :: RleBlock}
type HalfMeta =  [Word16]
type Scanline = [HalfMeta]
type Title = [Scanline]
type Lut = [LutEntry]
-- each lut entry is a 2x2 metatile, stored as
--AB
--CD
-- AB are hiTiles, CD are loTiles
data LutEntry = LutEntry {hiTiles :: HalfMeta, loTiles :: HalfMeta}
  deriving Show

-----------------------MAIN-----------------------------------------------------
main :: IO()
main = getArgs >>= parse
  where
    parse ["-v"] = putStrLn "jmRle compression tool 0.1"
    parse ["-d"] = decodeFile
    parse ["-e"] = encodeFile
    parse _ = putStrLn "Usage: jmRle [-vde]"

-------------------------------DECODE-------------------------------------------
getLutEntries :: Get LutRleEntries --packed index goes right after packed attributes
getLutEntries = LutRleEntries <$> getRleBlock <*> getRleBlock

getRleBlock :: Get RleBlock
getRleBlock  = do --get RLE codes from bytes
  command <- getWord8
  let count = (fromIntegral command .&. 0x7F) :: Int --lower 7 bits are count
      count64 = fromIntegral count :: Int64
  case command of
    0 -> return [] --end of RLE packed block reached
    x | x >= 0x80 -> do --Raw
        rawBlock <- getLazyByteString count64
        rest <- getRleBlock
        return $ Raw (Bs.unpack rawBlock) : rest
    _ -> do --ohterwise RLE
      iterateByte <- getWord8
      rest <- getRleBlock
      return $ Code count iterateByte : rest

getLutCopyBlock :: Get Bs.ByteString
getLutCopyBlock = do
  command <- getWord8 --data is wrappped in copy commands, null-terminated
  case command of
     0 -> return Bs.empty --copy stop command
     x | x < 0x80 -> fail "Unknown command byte in copy command"
     _ -> do --hibit is set - copy
        copyBlock <- getLazyByteString $ fromIntegral $ command .&. 0x7F --lower 7 bits are copy count
        rest <- getLutCopyBlock
        return $ Bs.append copyBlock rest

decode :: RleBlock -> [Word8]
decode [] = []
decode (Raw bs : xs) = bs ++ decode xs
decode (Code l v : xs) = replicate l v ++ decode xs


mergeToWord16 :: Word8 -> Word8 -> Word16 --Merge 2 bytes in one word
mergeToWord16 x y = (fromIntegral x `shiftL` 8) .|. fromIntegral y


deserializeLut :: [Word16] -> Lut
deserializeLut xs = deserializeLut' $ chunksOf 2 xs
  where
    deserializeLut' (hi:lo:rest) = LutEntry hi lo : deserializeLut' rest
    deserializeLut' _ = []

renderScanline :: Lut -> [Word8] -> [Word16] --emit 2 scanlines from LUT and indexes of Scanline
renderScanline lut idxs = concatMap (hiTiles . (lut !!)) idxsInt ++ concatMap (loTiles . (lut !!)) idxsInt
  where idxsInt = map fromIntegral idxs


decodeFile :: IO()
decodeFile = do
  input <- Bs.readFile "Jewel Master (UE) [!].bin"
  let
    lutEncoded = runGet getLutCopyBlock $  Bs.drop lutEncodedOffset input
    lutEntries = runGet getLutEntries lutEncoded
    decodedHiBytes = decode $ hiByteRleEntries lutEntries
    decodedLoBytes = decode $ loByteRleEntries lutEntries
    decodedLut = zipWith mergeToWord16 decodedHiBytes decodedLoBytes --merge attributes and indexes in proper Genesis NT entry
    lut = deserializeLut decodedLut

    idxsEncoded = Bs.drop idxsEncodedOffset input
    entriesIdxs = runGet getRleBlock idxsEncoded
    decodedIdxs = decode entriesIdxs
    titleScanlines = chunksOf 20 decodedIdxs --title screen is 280 metatiles in size: 20 x 14 metatiles or 40x28 tiles
    decoded = concatMap (renderScanline lut) titleScanlines

  Bs.writeFile "decodedLut.bin" $ runPut $ mapM_ putWord16be decodedLut
  Bs.writeFile "decoded.bin" $ runPut $ mapM_ putWord16be decoded

-------------------------------ENCODE-------------------------------------------
readWords16 :: Get [Word16]
readWords16 = do
  empty <- isEmpty
  if empty
     then return []
     else do v <- getWord16be
             rest <- readWords16
             return (v : rest)


getMetaTiles :: [Scanline] -> [Scanline] --build full metatile scanline from 2 scanlines
getMetaTiles [] = []
getMetaTiles [_] = error "Scanline count is odd, can't build metatile"
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

encode :: [Word8] -> RleBlock --RLE encode
encode xs = go $ group xs
go :: [[Word8]] -> RleBlock
go [] = []
go s@([_] : _) = Raw singles : go (drop (length singles) s)
  where singles = concat $ takeWhile (\a -> length a == 1) s
go (x : xs) = Code (length x) (head x) : go xs

trimRlesLen :: RleBlock -> RleBlock --RLE length can be 0x7F max due to serialization scheme
trimRlesLen = concatMap trimRleLen
  where
    trimRleLen x@ (Code l v) = if l > 0x7F then Code 0x7F v : trimRleLen (Code (l-0x7F) v)
                                            else [x]
    trimRleLen x@ (Raw vs) = if length vs > 0x7F then Raw (take 0x7F vs) : trimRleLen (Raw (drop 0x7F vs))
                                            else [x]

serialize ::  RleBlock -> [Word8]
serialize [] = [0] --null terimnated RLE block
serialize (Raw x : xs) = (fromIntegral(length x) .|. 0x80) : x ++ serialize xs
serialize (Code count value : xs) = (fromIntegral count .&. 0x7F) : value : serialize xs

packCopyCommands :: [Word8] -> [Word8] --data is wrappped in copy commands, null-terminated
packCopyCommands xs = if length xs > 0x7F
                        then 0xFF: take 0x7F xs ++ packCopyCommands (drop 0x7F xs)
                        else fromIntegral (length xs .|. 0x80) : xs ++ [0]--last copy block null-terminated

encodeFile :: IO()
encodeFile = do
  frontInput <- Bs.readFile "decoded.bin"
  bkgInput <- Bs.readFile "decodedBkg.bin"
  let
    input = Bs.append frontInput bkgInput
    metaTiles = divideOnMetatiles input
    lutList = sort.nub $ metaTiles --list of all unique metatiles in title
    (lutAttr, lutTileIdx) = splitFromWord16 (concat (lutList ++ replicate (0x100 - length lutList) [0,0,0,0]))
    lutSerialized = (serialize.trimRlesLen.encode) lutAttr ++ (serialize.trimRlesLen.encode) lutTileIdx
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
