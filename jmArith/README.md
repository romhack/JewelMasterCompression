jmArith - compression tool for tile sets in Sega Genesis game "Jewel Master"
=========
 
Tile sets are compressed with RLE, with some prefix coding applied after that. Prefix coding, used by original packer is very much alike [Shannon coding](https://en.wikipedia.org/wiki/Shannon_coding). So prefix code building stage can be more optimal, if Huffman coding approach is used, thus improving compression ratio.  
This tool uses original Sahnnon approach. Please check Marat's codec at chief-net forum [here](http://chief-net.ru/forum/topic.php?forum=4&topic=114&p=3) for working implementation of Huffman coding for this algorithm.  

The following steps are used for decoding: 

1. Read XOR flag and Size of unpacked data:
3A600-3A601 first word (ptr is stored at 0x1806): first bit is a xor or not-xor flag, rest 15 bits is a plain size of data / 8: counter is for longwords: 4 bytes, so minimal size is 4*8*1 = 32, i.e. 64 pixels - exactly 1 tile in 4bpp. 8118 means it will be xored unpack, 118 tiles will be unpacked

2. Read and unpack LUT from 3A602-0x3A652. 
Plain LUT structure: 16 bit word 
Hibyte is used bits of entry or prefix code length (bitcount): (8 - bitcount) is unused bits in entry to decompress. LUT entry will be repeated 2^(8-bitcount) times in LUT table. This is used during decompression. Decoder reads always 1 byte from compressed stream and take this byte as index in LUT table. If some entry has prefix code with length less than 8 bits, low bits of offset byte, which was read recently are bits from the next entry (i.e. garbage). But that doesn't matter, you will read proper entry from table anyway, as this entry duplicated in table necessary number of times.  
So, if bitcount = 8, 08XY word is put in LUT 1 time. 07XY word should be put in LUT 2 times, as low bit is unused and can be any bit - we will still read proper 07XY entry from LUT, so entries like 03XY should be put 0x20 times. Note, that it's a word count, so 03XY will use 0x40 bytes of LUT.  
Lobyte XY is a RLE entry of pixels: X is repeat counter (0 means single pixel copy), Y is actually 4 bits for concrete pixel value. 
Last 6-bit entry in lut is reserved (so last 8 bytes in LUT are zeroes): if you hit it, you just read next 7 bits from stream as a raw RLE entry (0CCC PPPP). For rare entries, which didn't fit the LUT size. 
LUT serialization scheme:
1XXX PPPP CCCC LLLL DDDD DDDD: it's a new pixel value set: PPPP-new pixval. C - RLE entry counter, L - bitlen of entry. D is slot number of entry in plain LUT: offset = slotNum * (2^(8-L))
0CCC LLLL DDDD DDDD: just use old pixval, rest are the same.
RLE Length can be maximum 8 pixes (zero-based 7 takes 3 bits, as it can be stored in LUT). Serialized LUT is terminated by FF

3. Decode RLE entries: 0x3A653 - 0x3B727 compressed bitstream of lut indexes: read a byte from compressed stream and check a LUT entry at this byte offset. The entry will show in a hi byte bits, which this entry takes (higher the probability of RLE entry, less bits takes to encode and more place it takes in LUT table to store (analog of probability interval on axis)). All other bits, we've read in the beginning LUT offset byte are just another entry bits: shiftout used bits and check next 8 bits from compressed stream.

4. Unpack RLE until the target read in item 1 size is reached.

5. XOR-decrypt unpacked data to plain tile map: first word32 is plain. XOR next encrypted word32 with previous plain word32, you get next plain word32. To XOR encrypt plain tile map, you just XOR previous plain word32 with next plain word32.

Sprite labels graphics is stored in the same scheme at 0x3B728 - 0x3BE45 (ptr is stored at 0x181D)

```
Usage: jmArith [-vde]
  -d      decode from ROM
  -e      encode from binary file
  -v      show version number
```

Decoding offset is hardcoded, use [Haskell stack](http://docs.haskellstack.org/en/stable/install_and_upgrade/) to compile the tool.
