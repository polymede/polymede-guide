# Base2

## Essential Stats

| essential               | stat                                                   |
| ----------------------- | ------------------------------------------------------ |
| codec                   | Base2                                                  |
| codec home page         | [multibase](https://github.com/multiformats/multibase) |
| multibase name          | base2                                                  |
| multibase code          | 0                                                      |
| preferred pipeline      | by                                                     |
| reference node          | zaphod                                                 |
| time to encode 10 kB    | 860.4 μs                                               |
| time to decode to 10 kB | 10.59 ms                                               |
| 'foobar' encodes to     | "0011001100110111101101111011000100110000101110010"    |


## Codec Overview

This codec encodes an octet stream using a standard base-2 codec using the
following alphabet:
| input | encoding |
| ----- | -------- |
| 0x00  | '0'      |
| 0x01  | '1'      |



## API

The API for this codec is on Hackage at [Data.Multibase.Types.Codecs.Base2](https://hackage.haskell.org/package/polymede-0.0.0.1/docs/Data-Multibase-Types-Codecs-Base2.html).

## A Simple Example

This example program shows the codec as a pair of functions: one to encode an octet stream, 
and another to decode the resulting multibase encoding to recover it.

```haskell
{{#include ../hs/src/PolymedeSimpleBase2.hs}}
```

## A More Realistic Example

In reality we expect the binary to be encoded to be embedded in a larger structure that will need
to be in (most likely) a JSON text. This next example shows how this can be done with the polymede
JSON API.

```haskell
{{#include ../hs/src/PolymedeJSONBase2.hs}}
```

## Benchmarks


This section provides summary information only &mdash; see the [Base2 benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base2.html) for the full
breakdown, attached notes, etc.

### Key Results

Compiled with GHC 9.4.4 (x86_64), on the **zaphod** reference node running macOS 13.2.1 (22D68):

* There are no highlighted benchmarks.

### Release Constraints

The following benchmarks are constrained to run inside the following bounds for each release:

* there are no release constraints for this codec/report

### Details

See the [Base2 benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base2.html) for the full breakdown, attached notes, etc.


## Remarks on Expected Codec Performance

This codec has not been optimised but should perform moderatly well


## Test Vectors

Most of the test coverage for this codecs is to be found in the test suite for the underlying
codec but we do maintain some simple test vectors to ensure basic functionality and ensure that 
the correct codec has been integrated.

| encoded-input                                                                                       | expected             |
| --------------------------------------------------------------------------------------------------- | -------------------- |
| "0"                                                                                                 | ""                   |
| "001111001011001010111001100100000011011010110000101101110011010010010000000100001"                 | "yes mani !"         |
| "00000000001111001011001010111001100100000011011010110000101101110011010010010000000100001"         | "\NULyes mani !"     |
| "0000000000000000001111001011001010111001100100000011011010110000101101110011010010010000000100001" | "\NUL\NULyes mani !" |
| "02"                                                                                                | **decode failure**   |


In addition to these test vectors we test that the codec can successfully roundtrip a selection of 
random byte streams.

## Test Output

The codec tests generate the following output.

```
<{{#include ../test/Base2-out.txt}}
```
