# Base58Btc

## Essential Stats

| essential              | stat                                                                                     |
| ---------------------- | ---------------------------------------------------------------------------------------- |
| codec                  | Base58Btc                                                                                |
| codec home page        | [The Base58 Encoding Scheme](https://datatracker.ietf.org/doc/html/draft-msporny-base58) |
| multibase name         | base58btc                                                                                |
| multibase code         | z                                                                                        |
| preferred pipeline     | by                                                                                       |
| reference node         | zaphod                                                                                   |
| time to encode 10KB    | ??                                                                                       |
| time to decode to 10KB | ??                                                                                       |
| 'foobar' encodes to    | "zt1Zv2yaZ"                                                                              |


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
| 0x09  | 'A'      |
| 0x0a  | 'B'      |
| 0x0b  | 'C'      |
| 0x0c  | 'D'      |
| 0x0d  | 'E'      |
| 0x0e  | 'F'      |
| 0x0f  | 'G'      |
| 0x10  | 'H'      |
| 0x11  | 'J'      |
| 0x12  | 'K'      |
| 0x13  | 'L'      |
| 0x14  | 'M'      |
| 0x15  | 'N'      |
| 0x16  | 'P'      |
| 0x17  | 'Q'      |
| 0x18  | 'R'      |
| 0x19  | 'S'      |
| 0x1a  | 'T'      |
| 0x1b  | 'U'      |
| 0x1c  | 'V'      |
| 0x1d  | 'W'      |
| 0x1e  | 'X'      |
| 0x1f  | 'Y'      |
| 0x20  | 'Z'      |
| 0x21  | 'a'      |
| 0x22  | 'b'      |
| 0x23  | 'c'      |
| 0x24  | 'd'      |
| 0x25  | 'e'      |
| 0x26  | 'f'      |
| 0x27  | 'g'      |
| 0x28  | 'h'      |
| 0x29  | 'i'      |
| 0x2a  | 'j'      |
| 0x2b  | 'k'      |
| 0x2c  | 'm'      |
| 0x2d  | 'n'      |
| 0x2e  | 'o'      |
| 0x2f  | 'p'      |
| 0x30  | 'q'      |
| 0x31  | 'r'      |
| 0x32  | 's'      |
| 0x33  | 't'      |
| 0x34  | 'u'      |
| 0x35  | 'v'      |
| 0x36  | 'w'      |
| 0x37  | 'x'      |
| 0x38  | 'y'      |
| 0x39  | 'z'      |



## API

The API for this codec is on Hackage at [Data.Multibase.Types.Codecs.Base58Btc](https://hackage.haskell.org/package/polymede-0.0.0.1/docs/Data-Multibase-Types-Codecs-Base58Btc.html).

## A Simple Example

This example program shows the codec as a pair of functions: one to encode an octet stream, 
and another to decode the resulting multibase encoding to recover it.

```haskell
{{#include ../hs/src/PolymedeSimpleBase58Btc.hs}}
```

## A More Realistic Example

In reality we expect the binary to be encoded to be embedded in a larger structure that will need
to be in (most likely) a JSON text. This next example shows how this can be done with the polymede
JSON API.

```haskell
{{#include ../hs/src/PolymedeJSONBase58Btc.hs}}
```

## Benchmarks


This section provides summary information only &mdash; see the [Base58Btc benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base58Btc.html) for the full
breakdown, attached notes, etc.

### Key Results

Compiled with GHC 9.4.4 (aarch64), on the **marvin** reference node running macOS 13.2.1 (22D68):

* There are no highlighted benchmarks.

### Release Constraints

The following benchmarks are constrained to run inside the following bounds for each release:

* there are no release constraints for this codec/report

### Details

See the [Base58Btc benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base58Btc.html) for the full breakdown, attached notes, etc.


## Remarks on Expected Codec Performance

This codec has not been optimised but should perform moderatly well


## Test Vectors

Most of the test coverage for this codecs is to be found in the test suite for the underlying
codec but we do maintain some simple test vectors to ensure basic functionality and ensure that 
the correct codec has been integrated.

| encoded-input       | expected             |
| ------------------- | -------------------- |
| "z"                 | ""                   |
| "z7paNL19xttacUY"   | "yes mani !"         |
| "z17paNL19xttacUY"  | "\NULyes mani !"     |
| "z117paNL19xttacUY" | "\NUL\NULyes mani !" |
| "z@"                | **decode failure**   |


In addition to these test vectors we test that the codec can successfully roundtrip a selection of 
random byte streams.

## Test Output

The codec tests generate the following output.

```
<{{#include ../test/Base58Btc-out.txt}}
```
