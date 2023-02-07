# Base64Pad

## Essential Stats

| essential             | stat                                               |
| --------------------- | -------------------------------------------------- |
| codec                 | Base64Pad                                          |
| codec home page       | [RFC 4648](https://www.rfc-editor.org/rfc/rfc4648) |
| multibase name        | base64pad                                          |
| multibase code        | M                                                  |
| preferred pipeline    | ByteLazy                                           |
| reference node        | marvin                                             |
| time to encode 1MB    | 457.2 μs                                           |
| time to decode to 1MB | 1.894 ms                                           |
| 'foobar' encodes to   | "MZm9vYmFy"                                        |


## Codec Overview

This codec is part of a family of codecs standardised by the IETF in 
[RFC 4648](https://www.rfc-editor.org/rfc/rfc4648). The 
[Base 64 Wikipedia page](https://en.wikipedia.org/wiki/Base64) has a good overview.


## API

The API for this codec is on Hackage at [Data.Multibase.Types.Codecs.Base64Pad](https://hackage.haskell.org/package/polymede-0.0.0.1/docs/Data-Multibase-Types-Codecs-Base64Pad.html).

## A Simple Example

This example program shows the codec as a pair of functions: one to encode an octet stream, 
and another to decode the resulting multibase encoding to recover it.

```haskell
{{#include ../hs/src/PolymedeSimpleBase64Pad.hs}}
```

## A More Realistic Example

In reality we expect the binary to be encoded to be embedded in a larger structure that will need
to be in (most likely) a JSON text. This next example shows how this can be done with the polymede
JSON API.

```haskell
{{#include ../hs/src/PolymedeJSONBase64Pad.hs}}
```

## Benchmarks


This section provides summary information only &mdash; see the [Base64Pad benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base64Pad.html) for the full
breakdown, attached notes, etc.

### Key Results

Compiled with GHC 9.4.4 (aarch64), on the **marvin** reference node running macOS 13.2 (22D49):

* encodes 1,000,000 bytes in **457.2 μs** using the **ByteLazy** pipeline
* decodes to 1,000,000 bytes in **1.894 ms** using the **ByteLazy** pipeline

### Release Constraints

The following benchmarks are constrained to run inside the following bounds for each release:

| benchmark                         | nodes                      | current  | bound        |
| --------------------------------- | -------------------------- | -------- | ------------ |
| **encode-1MB-Base64Pad-ByteLazy** | **marvin** zaphod trillian | 457.2 μs | **485.0 μs** |
| encode-1MB-Base64Pad-ByteLazy     | dat                        | 457.2 μs | 352.0 μs     |

### Details

See the [Base64Pad benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base64Pad.html) for the full breakdown, attached notes, etc.


## Remarks on Expected Codec Performance

This RFC 4648 codec should perform well, especially over `MultibaseBytesLazy`.


## Test Vectors

Most of the test coverage for this codecs is to be found in the test suite for the underlying
codec but we do maintain some simple test vectors to ensure basic functionality and ensure that 
the correct codec has been integrated.

| encoded-input | expected           |
| ------------- | ------------------ |
| "M"           | ""                 |
| "MZg=="       | "f"                |
| "MZm8="       | "fo"               |
| "MZm9v"       | "foo"              |
| "MZm9vYg=="   | "foob"             |
| "MZm9vYmE="   | "fooba"            |
| "MZm9vYmFy"   | "foobar"           |
| "M=eAoeAo="   | **invalid input**  |
| "Me=AoeAo="   | **invalid input**  |
| "M=eAoeAo="   | **invalid input**  |
| "MeAoe=Ao="   | **invalid input**  |
| "MeAoeA=o="   | **invalid input**  |
| "MZE=="       | **decode failure** |


In addition to these test vectors we test that the codec can successfully roundtrip a selection of 
random byte streams.

## Test Output

The codec tests generate the following output.

```
<{{#include ../test/Base64Pad-out.txt}}
```
