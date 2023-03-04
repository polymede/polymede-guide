# Base32HexNoPadLower

## Essential Stats

| essential              | stat                                                   |
| ---------------------- | ------------------------------------------------------ |
| codec                  | Base32HexNoPadLower                                    |
| codec home page        | [multibase](https://github.com/multiformats/multibase) |
| multibase name         | base32hex                                              |
| multibase code         | v                                                      |
| preferred pipeline     | bl                                                     |
| reference node         | zaphod                                                 |
| time to encode 10KB    | 49.87 μs                                               |
| time to decode to 10KB | 15.30 μs                                               |
| 'foobar' encodes to    | "vcpnmuoj1e8"                                          |


## Codec Overview

This codec is based on the RF4648 base32hex codec with the following ajustments:
<<Ajustments>>


## API

The API for this codec is on Hackage at [Data.Multibase.Types.Codecs.Base32HexNoPadLower](https://hackage.haskell.org/package/polymede-0.0.0.1/docs/Data-Multibase-Types-Codecs-Base32HexNoPadLower.html).

## A Simple Example

This example program shows the codec as a pair of functions: one to encode an octet stream, 
and another to decode the resulting multibase encoding to recover it.

```haskell
{{#include ../hs/src/PolymedeSimpleBase32HexNoPadLower.hs}}
```

## A More Realistic Example

In reality we expect the binary to be encoded to be embedded in a larger structure that will need
to be in (most likely) a JSON text. This next example shows how this can be done with the polymede
JSON API.

```haskell
{{#include ../hs/src/PolymedeJSONBase32HexNoPadLower.hs}}
```

## Benchmarks


This section provides summary information only &mdash; see the [Base32HexNoPadLower benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base32HexNoPadLower.html) for the full
breakdown, attached notes, etc.

### Key Results

Compiled with GHC 9.4.4 (aarch64), on the **marvin** reference node running macOS 13.2.1 (22D68):

* There are no highlighted benchmarks.

### Release Constraints

The following benchmarks are constrained to run inside the following bounds for each release:

* there are no release constraints for this codec/report

### Details

See the [Base32HexNoPadLower benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base32HexNoPadLower.html) for the full breakdown, attached notes, etc.


## Remarks on Expected Codec Performance

This codec does not conform to RFC 4648 and will perform poorly in comparison to
its corresponding RFC 4648 codec.


## Test Vectors

Most of the test coverage for this codecs is to be found in the test suite for the underlying
codec but we do maintain some simple test vectors to ensure basic functionality and ensure that 
the correct codec has been integrated.

| encoded-input | expected          |
| ------------- | ----------------- |
| "v"           | ""                |
| "vadqms"      | "Sun"             |
| "vco"         | "f"               |
| "vcpng"       | "fo"              |
| "vcpnmu"      | "foo"             |
| "vcpnmuog"    | "foob"            |
| "vcpnmuoj1"   | "fooba"           |
| "vcpnmuoj1e8" | "foobar"          |
| "v=cco======" | **invalid input** |


In addition to these test vectors we test that the codec can successfully roundtrip a selection of 
random byte streams.

## Test Output

The codec tests generate the following output.

```
<{{#include ../test/Base32HexNoPadLower-out.txt}}
```
