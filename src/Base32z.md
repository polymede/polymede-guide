# Base32z

## Essential Stats

| essential               | stat                                                                                                  |
| ----------------------- | ----------------------------------------------------------------------------------------------------- |
| codec                   | Base32z                                                                                               |
| codec home page         | [human-oriented base-32 encoding](http://philzimmermann.com/docs/human-oriented-base-32-encoding.txt) |
| multibase name          | base32z                                                                                               |
| multibase code          | h                                                                                                     |
| preferred pipeline      | by                                                                                                    |
| reference node          | zaphod                                                                                                |
| time to encode 10 kB    | 7.500 μs                                                                                              |
| time to decode to 10 kB | 48.93 μs                                                                                              |
| 'foobar' encodes to     | "hc3zs6aubqe"                                                                                         |


## Codec Overview

Is a base 32 encoding that omits padding and uses the following alphabet:
| input | encoding |
| ----- | -------- |
| 0x00  | 'y'      |
| 0x01  | 'b'      |
| 0x02  | 'n'      |
| 0x03  | 'd'      |
| 0x04  | 'r'      |
| 0x05  | 'f'      |
| 0x06  | 'g'      |
| 0x07  | '8'      |
| 0x08  | 'e'      |
| 0x09  | 'j'      |
| 0x0a  | 'k'      |
| 0x0b  | 'm'      |
| 0x0c  | 'c'      |
| 0x0d  | 'p'      |
| 0x0e  | 'q'      |
| 0x0f  | 'x'      |
| 0x10  | 'o'      |
| 0x11  | 't'      |
| 0x12  | '1'      |
| 0x13  | 'u'      |
| 0x14  | 'w'      |
| 0x15  | 'i'      |
| 0x16  | 's'      |
| 0x17  | 'z'      |
| 0x18  | 'a'      |
| 0x19  | '3'      |
| 0x1a  | '4'      |
| 0x1b  | '5'      |
| 0x1c  | 'h'      |
| 0x1d  | '7'      |
| 0x1e  | '6'      |
| 0x1f  | '9'      |


## API

The API for this codec is on Hackage at [Data.Multibase.Types.Codecs.Base32z](https://hackage.haskell.org/package/polymede-0.0.0.1/docs/Data-Multibase-Types-Codecs-Base32z.html).

## A Simple Example

This example program shows the codec as a pair of functions: one to encode an octet stream, 
and another to decode the resulting multibase encoding to recover it.

```haskell
{{#include ../hs/src/PolymedeSimpleBase32z.hs}}
```

## A More Realistic Example

In reality we expect the binary to be encoded to be embedded in a larger structure that will need
to be in (most likely) a JSON text. This next example shows how this can be done with the polymede
JSON API.

```haskell
{{#include ../hs/src/PolymedeJSONBase32z.hs}}
```

## Benchmarks


This section provides summary information only &mdash; see the [Base32z benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base32z.html) for the full
breakdown, attached notes, etc.

### Key Results

Compiled with GHC 9.4.4 (x86_64), on the **zaphod** reference node running macOS 13.2.1 (22D68):

* There are no highlighted benchmarks.

### Release Constraints

The following benchmarks are constrained to run inside the following bounds for each release:

* there are no release constraints for this codec/report

### Details

See the [Base32z benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base32z.html) for the full breakdown, attached notes, etc.


## Remarks on Expected Codec Performance

This codec has been optimised and should perform well.


## Test Vectors

Most of the test coverage for this codecs is to be found in the test suite for the underlying
codec but we do maintain some simple test vectors to ensure basic functionality and ensure that 
the correct codec has been integrated.

| encoded-input       | expected           |
| ------------------- | ------------------ |
| "h"                 | ""                 |
| "hca"               | "f"                |
| "hc3zo"             | "fo"               |
| "hc3zs6"            | "foo"              |
| "hc3zs6ao"          | "foob"             |
| "hc3zs6aub"         | "fooba"            |
| "hc3zs6aubqe"       | "foobar"           |
| "h0======="         | **decode failure** |
| "hAAAAAAAA0=======" | **decode failure** |


In addition to these test vectors we test that the codec can successfully roundtrip a selection of 
random byte streams.

## Test Output

The codec tests generate the following output.

```
<{{#include ../test/Base32z-out.txt}}
```
