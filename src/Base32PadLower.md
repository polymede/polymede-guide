# Base32PadLower

## Essential Stats

| essential              | stat                                                   |
| ---------------------- | ------------------------------------------------------ |
| codec                  | Base32PadLower                                         |
| codec home page        | [multibase](https://github.com/multiformats/multibase) |
| multibase name         | base32pad                                              |
| multibase code         | c                                                      |
| preferred pipeline     | bl                                                     |
| reference node         | zaphod                                                 |
| time to encode 10KB    | 36.39 μs                                               |
| time to decode to 10KB | 15.13 μs                                               |
| 'foobar' encodes to    | "cmzxw6ytboi======"                                    |


## Codec Overview

This codec is based on the RF4648 base32 codec with the following ajustments:
<<Ajustments>>


## API

The API for this codec is on Hackage at [Data.Multibase.Types.Codecs.Base32PadLower](https://hackage.haskell.org/package/polymede-0.0.0.1/docs/Data-Multibase-Types-Codecs-Base32PadLower.html).

## A Simple Example

This example program shows the codec as a pair of functions: one to encode an octet stream, 
and another to decode the resulting multibase encoding to recover it.

```haskell
{{#include ../hs/src/PolymedeSimpleBase32PadLower.hs}}
```

## A More Realistic Example

In reality we expect the binary to be encoded to be embedded in a larger structure that will need
to be in (most likely) a JSON text. This next example shows how this can be done with the polymede
JSON API.

```haskell
{{#include ../hs/src/PolymedeJSONBase32PadLower.hs}}
```

## Benchmarks


This section provides summary information only &mdash; see the [Base32PadLower benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base32PadLower.html) for the full
breakdown, attached notes, etc.

### Key Results

Compiled with GHC 9.4.4 (aarch64), on the **marvin** reference node running macOS 13.2.1 (22D68):

* There are no highlighted benchmarks.

### Release Constraints

The following benchmarks are constrained to run inside the following bounds for each release:

* there are no release constraints for this codec/report

### Details

See the [Base32PadLower benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base32PadLower.html) for the full breakdown, attached notes, etc.


## Remarks on Expected Codec Performance

This codec does not conform to RFC 4648 and will perform poorly in comparison to
its corresponding RFC 4648 codec.


## Test Vectors

Most of the test coverage for this codecs is to be found in the test suite for the underlying
codec but we do maintain some simple test vectors to ensure basic functionality and ensure that 
the correct codec has been integrated.

| encoded-input       | expected           |
| ------------------- | ------------------ |
| "c"                 | ""                 |
| "ckn2w4==="         | "Sun"              |
| "cmy======"         | "f"                |
| "cmzxq===="         | "fo"               |
| "cmzxw6==="         | "foo"              |
| "cmzxw6yq="         | "foob"             |
| "cmzxw6ytb"         | "fooba"            |
| "cmzxw6ytboi======" | "foobar"           |
| "c=cmzxw6==="       | **invalid input**  |
| "cze=="             | **decode failure** |


In addition to these test vectors we test that the codec can successfully roundtrip a selection of 
random byte streams.

## Test Output

The codec tests generate the following output.

```
<{{#include ../test/Base32PadLower-out.txt}}
```
