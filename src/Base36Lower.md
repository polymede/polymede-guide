# Base36Lower

## Essential Stats

| essential              | stat                                                   |
| ---------------------- | ------------------------------------------------------ |
| codec                  | Base36Lower                                            |
| codec home page        | [multibase](https://github.com/multiformats/multibase) |
| multibase name         | base36                                                 |
| multibase code         | k                                                      |
| preferred pipeline     | by                                                     |
| reference node         | zaphod                                                 |
| time to encode 10KB    | ??                                                     |
| time to decode to 10KB | ??                                                     |
| 'foobar' encodes to    | "k13x8yd7ywi"                                          |


## Codec Overview

This codec encodes an octet stream using a standard base-36 codec using the
following alphabet:
| input | encoding |
| ----- | -------- |
| 0x00  | '0'      |
| 0x01  | '1'      |
| 0x02  | '2'      |
| 0x03  | '3'      |
| 0x04  | '4'      |
| 0x05  | '5'      |
| 0x06  | '6'      |
| 0x07  | '7'      |
| 0x08  | '8'      |
| 0x09  | '9'      |
| 0x0a  | 'a'      |
| 0x0b  | 'b'      |
| 0x0c  | 'c'      |
| 0x0d  | 'd'      |
| 0x0e  | 'e'      |
| 0x0f  | 'f'      |
| 0x10  | 'g'      |
| 0x11  | 'h'      |
| 0x12  | 'i'      |
| 0x13  | 'j'      |
| 0x14  | 'k'      |
| 0x15  | 'l'      |
| 0x16  | 'm'      |
| 0x17  | 'n'      |
| 0x18  | 'o'      |
| 0x19  | 'p'      |
| 0x1a  | 'q'      |
| 0x1b  | 'r'      |
| 0x1c  | 's'      |
| 0x1d  | 't'      |
| 0x1e  | 'u'      |
| 0x1f  | 'v'      |
| 0x20  | 'w'      |
| 0x21  | 'x'      |
| 0x22  | 'y'      |
| 0x23  | 'z'      |



## API

The API for this codec is on Hackage at [Data.Multibase.Types.Codecs.Base36Lower](https://hackage.haskell.org/package/polymede-0.0.0.1/docs/Data-Multibase-Types-Codecs-Base36Lower.html).

## A Simple Example

This example program shows the codec as a pair of functions: one to encode an octet stream, 
and another to decode the resulting multibase encoding to recover it.

```haskell
{{#include ../hs/src/PolymedeSimpleBase36Lower.hs}}
```

## A More Realistic Example

In reality we expect the binary to be encoded to be embedded in a larger structure that will need
to be in (most likely) a JSON text. This next example shows how this can be done with the polymede
JSON API.

```haskell
{{#include ../hs/src/PolymedeJSONBase36Lower.hs}}
```

## Benchmarks


This section provides summary information only &mdash; see the [Base36Lower benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base36Lower.html) for the full
breakdown, attached notes, etc.

### Key Results

Compiled with GHC 9.4.4 (aarch64), on the **marvin** reference node running macOS 13.2.1 (22D68):

* There are no highlighted benchmarks.

### Release Constraints

The following benchmarks are constrained to run inside the following bounds for each release:

* there are no release constraints for this codec/report

### Details

See the [Base36Lower benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base36Lower.html) for the full breakdown, attached notes, etc.


## Remarks on Expected Codec Performance

This codec has not been optimised but should perform moderatly well


## Test Vectors

Most of the test coverage for this codecs is to be found in the test suite for the underlying
codec but we do maintain some simple test vectors to ensure basic functionality and ensure that 
the correct codec has been integrated.

| encoded-input         | expected             |
| --------------------- | -------------------- |
| "k"                   | ""                   |
| "k2lcpzo5yikidynfl"   | "yes mani !"         |
| "k02lcpzo5yikidynfl"  | "\NULyes mani !"     |
| "k002lcpzo5yikidynfl" | "\NUL\NULyes mani !" |
| "k@"                  | **decode failure**   |


In addition to these test vectors we test that the codec can successfully roundtrip a selection of 
random byte streams.

## Test Output

The codec tests generate the following output.

```
<{{#include ../test/Base36Lower-out.txt}}
```
