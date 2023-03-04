# Base58Flickr

## Essential Stats

| essential               | stat                                                                                                   |
| ----------------------- | ------------------------------------------------------------------------------------------------------ |
| codec                   | Base58Flickr                                                                                           |
| codec home page         | [manufacturing flic.kr style photo URLs](https://www.flickr.com/groups/api/discuss/72157616713786392/) |
| multibase name          | base58flickr                                                                                           |
| multibase code          | Z                                                                                                      |
| preferred pipeline      | by                                                                                                     |
| reference node          | zaphod                                                                                                 |
| time to encode 10 kB    | 32.27 ms                                                                                               |
| time to decode to 10 kB | 33.78 ms                                                                                               |
| 'foobar' encodes to     | "ZT1yV2Yzy"                                                                                            |


## Codec Overview

This codec encodes an octet stream using a standard base-58 codec using the
following alphabet:
| input | encoding |
| ----- | -------- |
| 0x00  | '1'      |
| 0x01  | '2'      |
| 0x02  | '3'      |
| 0x03  | '4'      |
| 0x04  | '5'      |
| 0x05  | '6'      |
| 0x06  | '7'      |
| 0x07  | '8'      |
| 0x08  | '9'      |
| 0x09  | 'a'      |
| 0x0a  | 'b'      |
| 0x0b  | 'c'      |
| 0x0c  | 'd'      |
| 0x0d  | 'e'      |
| 0x0e  | 'f'      |
| 0x0f  | 'g'      |
| 0x10  | 'h'      |
| 0x11  | 'i'      |
| 0x12  | 'j'      |
| 0x13  | 'k'      |
| 0x14  | 'm'      |
| 0x15  | 'n'      |
| 0x16  | 'o'      |
| 0x17  | 'p'      |
| 0x18  | 'q'      |
| 0x19  | 'r'      |
| 0x1a  | 's'      |
| 0x1b  | 't'      |
| 0x1c  | 'u'      |
| 0x1d  | 'v'      |
| 0x1e  | 'w'      |
| 0x1f  | 'x'      |
| 0x20  | 'y'      |
| 0x21  | 'z'      |
| 0x22  | 'A'      |
| 0x23  | 'B'      |
| 0x24  | 'C'      |
| 0x25  | 'D'      |
| 0x26  | 'E'      |
| 0x27  | 'F'      |
| 0x28  | 'G'      |
| 0x29  | 'H'      |
| 0x2a  | 'J'      |
| 0x2b  | 'K'      |
| 0x2c  | 'L'      |
| 0x2d  | 'M'      |
| 0x2e  | 'N'      |
| 0x2f  | 'P'      |
| 0x30  | 'Q'      |
| 0x31  | 'R'      |
| 0x32  | 'S'      |
| 0x33  | 'T'      |
| 0x34  | 'U'      |
| 0x35  | 'V'      |
| 0x36  | 'W'      |
| 0x37  | 'X'      |
| 0x38  | 'Y'      |
| 0x39  | 'Z'      |



## API

The API for this codec is on Hackage at [Data.Multibase.Types.Codecs.Base58Flickr](https://hackage.haskell.org/package/polymede-0.0.0.1/docs/Data-Multibase-Types-Codecs-Base58Flickr.html).

## A Simple Example

This example program shows the codec as a pair of functions: one to encode an octet stream, 
and another to decode the resulting multibase encoding to recover it.

```haskell
{{#include ../hs/src/PolymedeSimpleBase58Flickr.hs}}
```

## A More Realistic Example

In reality we expect the binary to be encoded to be embedded in a larger structure that will need
to be in (most likely) a JSON text. This next example shows how this can be done with the polymede
JSON API.

```haskell
{{#include ../hs/src/PolymedeJSONBase58Flickr.hs}}
```

## Benchmarks


This section provides summary information only &mdash; see the [Base58Flickr benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base58Flickr.html) for the full
breakdown, attached notes, etc.

### Key Results

Compiled with GHC 9.4.4 (x86_64), on the **zaphod** reference node running macOS 13.2.1 (22D68):

* There are no highlighted benchmarks.

### Release Constraints

The following benchmarks are constrained to run inside the following bounds for each release:

* there are no release constraints for this codec/report

### Details

See the [Base58Flickr benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base58Flickr.html) for the full breakdown, attached notes, etc.


## Remarks on Expected Codec Performance

This codec has not been optimised but should perform moderatly well


## Test Vectors

Most of the test coverage for this codecs is to be found in the test suite for the underlying
codec but we do maintain some simple test vectors to ensure basic functionality and ensure that 
the correct codec has been integrated.

| encoded-input       | expected             |
| ------------------- | -------------------- |
| "Z"                 | ""                   |
| "Z7Pznk19XTTzBtx"   | "yes mani !"         |
| "Z17Pznk19XTTzBtx"  | "\NULyes mani !"     |
| "Z117Pznk19XTTzBtx" | "\NUL\NULyes mani !" |
| "Z@"                | **decode failure**   |


In addition to these test vectors we test that the codec can successfully roundtrip a selection of 
random byte streams.

## Test Output

The codec tests generate the following output.

```
<{{#include ../test/Base58Flickr-out.txt}}
```
