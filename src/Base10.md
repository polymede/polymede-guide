# Base10

## Essential Stats

| essential              | stat                                                   |
| ---------------------- | ------------------------------------------------------ |
| codec                  | Base10                                                 |
| codec home page        | [multibase](https://github.com/multiformats/multibase) |
| multibase name         | base10                                                 |
| multibase code         | 9                                                      |
| preferred pipeline     | by                                                     |
| reference node         | zaphod                                                 |
| time to encode 10KB    | ??                                                     |
| time to decode to 10KB | ??                                                     |
| 'foobar' encodes to    | "9112628796121458"                                     |


## Codec Overview

This codec encodes an octet stream using a standard base-10 codec using the
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



## API

The API for this codec is on Hackage at [Data.Multibase.Types.Codecs.Base10](https://hackage.haskell.org/package/polymede-0.0.0.1/docs/Data-Multibase-Types-Codecs-Base10.html).

## A Simple Example

This example program shows the codec as a pair of functions: one to encode an octet stream, 
and another to decode the resulting multibase encoding to recover it.

```haskell
{{#include ../hs/src/PolymedeSimpleBase10.hs}}
```

## A More Realistic Example

In reality we expect the binary to be encoded to be embedded in a larger structure that will need
to be in (most likely) a JSON text. This next example shows how this can be done with the polymede
JSON API.

```haskell
{{#include ../hs/src/PolymedeJSONBase10.hs}}
```

## Benchmarks


This section provides summary information only &mdash; see the [Base10 benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base10.html) for the full
breakdown, attached notes, etc.

### Key Results

Compiled with GHC 9.4.4 (aarch64), on the **marvin** reference node running macOS 13.2.1 (22D68):

* There are no highlighted benchmarks.

### Release Constraints

The following benchmarks are constrained to run inside the following bounds for each release:

* there are no release constraints for this codec/report

### Details

See the [Base10 benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base10.html) for the full breakdown, attached notes, etc.


## Remarks on Expected Codec Performance

This codec has not been optimised but should perform moderatly well


## Test Vectors

Most of the test coverage for this codecs is to be found in the test suite for the underlying
codec but we do maintain some simple test vectors to ensure basic functionality and ensure that 
the correct codec has been integrated.

| encoded-input                 | expected             |
| ----------------------------- | -------------------- |
| "9"                           | ""                   |
| "9573277761329450583662625"   | "yes mani !"         |
| "90573277761329450583662625"  | "\NULyes mani !"     |
| "900573277761329450583662625" | "\NUL\NULyes mani !" |
| "9A"                          | **decode failure**   |


In addition to these test vectors we test that the codec can successfully roundtrip a selection of 
random byte streams.

## Test Output

The codec tests generate the following output.

```
<{{#include ../test/Base10-out.txt}}
```
