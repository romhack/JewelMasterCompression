jmRle - compression tool for tile maps in Sega Genesis game "Jewel Master"
=========
 
Tile maps are divided in metatiles (2x2 tiles). Metatiles sets are stored in LUT, which is encoded with [RLE](https://nl.wikipedia.org/wiki/Run-length_encoding). LUT indexes are also encoded with RLE.
The following steps are used for decoding: 

1. 22CFE-230E5 (0x3E7 size) LUT of NT entries. (22CFE has ptr at 0x1970) For SEGA its 2 bytes: Attribute byte and tile index byte.
First, it's copied using copy commands: LUT is already compressed with LUT, so second compression just copies raw bytes all the time and even increases block size: only Copy 0x7F bytes command is used (0xFF byte each 0x7F bytes). 00 byte is a stop signal.

2. After copy, 1st RLE block is unpacked as attributes LUT, and next RLE block is unpacked as indexes LUT.
RLE scheme:
   1YYY YYYY - it's RAW: copy next Y bytes as raw.
   0YYY YYYY ZZZZ ZZZZ - it's RLE: repeat Z byte Y times, if Y = 0 end of packed data, 

LUT is a metatiles entries set, stored in AA BB YY ZZ scheme. On screen this metatile has layout: 
AA BB
YY ZZ
Whole decoded LUT is 0x400 tiles long (0x100 meta tiles), so each metatile can be addressed by single byte.

3. 22b00-22bf1 (0xF2 size) (22b00 has ptr at 0x1974): offsets of LUT table (indexes of metatiles in LUT) for front screen.
It's unpacked straight as in item 2.

4. 22bf2 - 22CFD (0x10C size) (22bF2 has ptr at 0x1976): indexes for background of title screen.  
It's unpacked straight as in item 2.

5. Take metatile index, search it in LUT, copy to Name Table 2x2 metatile, until the end of indexes.


```
Usage: jmRle [-vde]
  -d      decode from ROM
  -e      encode from 2 binary maps (front and background)
  -v      show version number
```

Offsets and file names are hardcoded in source, use [Haskell stack](http://docs.haskellstack.org/en/stable/install_and_upgrade/) to compile the tool.
