# Base16Lower

## Essential Stats

| essential              | stat                                                   |
| ---------------------- | ------------------------------------------------------ |
| codec                  | Base16Lower                                            |
| codec home page        | [multibase](https://github.com/multiformats/multibase) |
| multibase name         | base16                                                 |
| multibase code         | f                                                      |
| preferred pipeline     | bl                                                     |
| reference node         | zaphod                                                 |
| time to encode 10KB    | 9.611 μs                                               |
| time to decode to 10KB | 12.57 μs                                               |
| 'foobar' encodes to    | "f666f6f626172"                                        |


## Codec Overview

This codec is based on the RF4648 base16 codec with the following ajustments:
<<Ajustments>>


## API

The API for this codec is on Hackage at [Data.Multibase.Types.Codecs.Base16Lower](https://hackage.haskell.org/package/polymede-0.0.0.1/docs/Data-Multibase-Types-Codecs-Base16Lower.html).

## A Simple Example

This example program shows the codec as a pair of functions: one to encode an octet stream, 
and another to decode the resulting multibase encoding to recover it.

```haskell
{{#include ../hs/src/PolymedeSimpleBase16Lower.hs}}
```

## A More Realistic Example

In reality we expect the binary to be encoded to be embedded in a larger structure that will need
to be in (most likely) a JSON text. This next example shows how this can be done with the polymede
JSON API.

```haskell
{{#include ../hs/src/PolymedeJSONBase16Lower.hs}}
```

## Benchmarks


This section provides summary information only &mdash; see the [Base16Lower benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base16Lower.html) for the full
breakdown, attached notes, etc.

### Key Results

Compiled with GHC 9.4.4 (aarch64), on the **marvin** reference node running macOS 13.2.1 (22D68):

* There are no highlighted benchmarks.

### Release Constraints

The following benchmarks are constrained to run inside the following bounds for each release:

* there are no release constraints for this codec/report

### Details

See the [Base16Lower benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base16Lower.html) for the full breakdown, attached notes, etc.


## Remarks on Expected Codec Performance

This codec does not conform to RFC 4648 and will perform poorly in comparison to
its corresponding RFC 4648 codec.


## Test Vectors

Most of the test coverage for this codecs is to be found in the test suite for the underlying
codec but we do maintain some simple test vectors to ensure basic functionality and ensure that 
the correct codec has been integrated.

| encoded-input   | expected |
| --------------- | -------- |
| "f"             | ""       |
| "f53756e"       | "Sun"    |
| "f66"           | "f"      |
| "f666f"         | "fo"     |
| "f666f6f"       | "foo"    |
| "f666f6f62"     | "foob"   |
| "f666f6f6261"   | "fooba"  |
| "f666f6f626172" | "foobar" |


In addition to these test vectors we test that the codec can successfully roundtrip a selection of 
random byte streams.

## Test Output

The codec tests generate the following output.

```
<{{#include ../test/Base16Lower-out.txt}}
```
