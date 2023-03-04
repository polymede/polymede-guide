# Base36Upper

## Essential Stats

| essential               | stat                                                   |
| ----------------------- | ------------------------------------------------------ |
| codec                   | Base36Upper                                            |
| codec home page         | [multibase](https://github.com/multiformats/multibase) |
| multibase name          | base36upper                                            |
| multibase code          | K                                                      |
| preferred pipeline      | by                                                     |
| reference node          | zaphod                                                 |
| time to encode 10 kB    | 35.93 ms                                               |
| time to decode to 10 kB | 37.97 ms                                               |
| 'foobar' encodes to     | "K13X8YD7YWI"                                          |


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
| 0x0a  | 'A'      |
| 0x0b  | 'B'      |
| 0x0c  | 'C'      |
| 0x0d  | 'D'      |
| 0x0e  | 'E'      |
| 0x0f  | 'F'      |
| 0x10  | 'G'      |
| 0x11  | 'H'      |
| 0x12  | 'I'      |
| 0x13  | 'J'      |
| 0x14  | 'K'      |
| 0x15  | 'L'      |
| 0x16  | 'M'      |
| 0x17  | 'N'      |
| 0x18  | 'O'      |
| 0x19  | 'P'      |
| 0x1a  | 'Q'      |
| 0x1b  | 'R'      |
| 0x1c  | 'S'      |
| 0x1d  | 'T'      |
| 0x1e  | 'U'      |
| 0x1f  | 'V'      |
| 0x20  | 'W'      |
| 0x21  | 'X'      |
| 0x22  | 'Y'      |
| 0x23  | 'Z'      |



## API

The API for this codec is on Hackage at [Data.Multibase.Types.Codecs.Base36Upper](https://hackage.haskell.org/package/polymede-0.0.0.1/docs/Data-Multibase-Types-Codecs-Base36Upper.html).

## A Simple Example

This example program shows the codec as a pair of functions: one to encode an octet stream, 
and another to decode the resulting multibase encoding to recover it.

```haskell
{{#include ../hs/src/PolymedeSimpleBase36Upper.hs}}
```

## A More Realistic Example

In reality we expect the binary to be encoded to be embedded in a larger structure that will need
to be in (most likely) a JSON text. This next example shows how this can be done with the polymede
JSON API.

```haskell
{{#include ../hs/src/PolymedeJSONBase36Upper.hs}}
```

## Benchmarks


This section provides summary information only &mdash; see the [Base36Upper benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base36Upper.html) for the full
breakdown, attached notes, etc.

### Key Results

Compiled with GHC 9.4.4 (x86_64), on the **zaphod** reference node running macOS 13.2.1 (22D68):

* There are no highlighted benchmarks.

### Release Constraints

The following benchmarks are constrained to run inside the following bounds for each release:

* there are no release constraints for this codec/report

### Details

See the [Base36Upper benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base36Upper.html) for the full breakdown, attached notes, etc.


## Remarks on Expected Codec Performance

This codec has not been optimised but should perform moderatly well


## Test Vectors

Most of the test coverage for this codecs is to be found in the test suite for the underlying
codec but we do maintain some simple test vectors to ensure basic functionality and ensure that 
the correct codec has been integrated.

| encoded-input         | expected             |
| --------------------- | -------------------- |
| "K"                   | ""                   |
| "K2LCPZO5YIKIDYNFL"   | "yes mani !"         |
| "K02LCPZO5YIKIDYNFL"  | "\NULyes mani !"     |
| "K002LCPZO5YIKIDYNFL" | "\NUL\NULyes mani !" |
| "K@"                  | **decode failure**   |


In addition to these test vectors we test that the codec can successfully roundtrip a selection of 
random byte streams.

## Test Output

The codec tests generate the following output.

```
<{{#include ../test/Base36Upper-out.txt}}
```
