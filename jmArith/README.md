jmArith - compression tool for tile sets in Sega Genesis game "Jewel Master"
=========
 
Tile sets are compressed with [range encoding](https://en.wikipedia.org/wiki/Range_encoding).
The following steps are used for decoding: 

1. Read XOR flag and Size of unpacked data:
3A600-3A601 first word (prt is stored at 0x1806): first bit is a xor or not-xor flag, rest 15 bits is a plain size of data / 8: counter is for longwords: 4 bytes, so minimal size is 4*8*1 = 32, i.e. 64 pixels - exactly 1 tile in 4bpp. 8118 means it will be xored unpack, 118 tiles will be unpacked

2. Read and unpack LUT from 3A602-0x3A652. 
Plain LUT structure: 16 bit word 
hibyte is used bits of entry (bitcount): (8 - bitcount) is unused bits in entry to decompress, so LUT entry is repeated 2^(8-bitcount) times: 
so, if bitcount = 8, 08XX word is in LUT 1 time and takes exatly 2 bytes. 
07XX word should be put in LUT 2 times, as low bit is unused and can be any bit - we will still read proper 07XX entry from LUT, so entries like 03XX should be put 0x20 times. Note, that it's a word count, so 03XX will use 0x40 bytes of LUT. 
lobyte XY is a RLE entry of pixels: X is repeat counter (0 means single pixel copy), Y is actually 4 bits for concrete pixel value. 
last 6-bit entry in lut is reserved (so last 8 bytes in LUT are zeroes): if you hit it, you just read next 7 bits from stream as a raw RLE entry (0CCC PPPP). For rare entries, which didn't fit the lut size. 
LUT serialization scheme:
1XXX PPPP CCCC LLLL DDDD DDDD: it's a new pixel value set: PPPP-new pixval. C - RLE entry counter, L - bitlen of entry. D is slot number of entry in plain LUT: offset = slotNum * (2^(8-L))
0CCC LLLL DDDD DDDD: just use old pixval, rest are the same.
RLE Length can be maximum 8 pixes (zero-based 7 takes 3 bits, as it can be stored in LUT). Serialized LUT is terminated by FF

3. Decode RLE entries: 0x3A653 - 0x3B727 compressed bitstream of lut indexes: read a byte from compressed stream and check a LUT entry at this byte offset. The entry will show in a hi byte bits, which this entry takes (higher the probability of RLE entry, less bits takes to encode and more place it takes in LUT table to store (analog of probability interval on axis)). All other bits, we've read in the beginning LUT offset byte are just another entry bits: shiftout used bits and check next 8 bits from compressed stream.

4. Unpack RLE until the target read in item 1 size is reached.

5. XOR-decrypt unpacked data to plain tile map: first word32 is plain. XOR next encrypted word32 with previous plain word32, you get next plain word32. To XOR encrypt plain tile map, you just XOR previous plain word32 with next plain word32.

```
Usage: jmArith [-vde]
  -d      decode from ROM
  -e      encode from binary file
  -v      show version number
```

Decoding offset is hardcoded, use [Haskell stack](http://docs.haskellstack.org/en/stable/install_and_upgrade/) to compile the tool.
