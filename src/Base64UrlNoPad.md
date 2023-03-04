# Base64UrlNoPad

## Essential Stats

| essential              | stat                                               |
| ---------------------- | -------------------------------------------------- |
| codec                  | Base64UrlNoPad                                     |
| codec home page        | [RFC 4648](https://www.rfc-editor.org/rfc/rfc4648) |
| multibase name         | base64url                                          |
| multibase code         | u                                                  |
| preferred pipeline     | bl                                                 |
| reference node         | zaphod                                             |
| time to encode 10KB    | 4.693 μs                                           |
| time to decode to 10KB | 19.84 μs                                           |
| 'foobar' encodes to    | "uZm9vYmFy"                                        |


## Codec Overview

This codec is part of a family of codecs standardised by the IETF in 
[RFC 4648](https://www.rfc-editor.org/rfc/rfc4648). The 
[Base 64 Wikipedia page](https://en.wikipedia.org/wiki/Base64) has a good overview.


## API

The API for this codec is on Hackage at [Data.Multibase.Types.Codecs.Base64UrlNoPad](https://hackage.haskell.org/package/polymede-0.0.0.1/docs/Data-Multibase-Types-Codecs-Base64UrlNoPad.html).

## A Simple Example

This example program shows the codec as a pair of functions: one to encode an octet stream, 
and another to decode the resulting multibase encoding to recover it.

```haskell
{{#include ../hs/src/PolymedeSimpleBase64UrlNoPad.hs}}
```

## A More Realistic Example

In reality we expect the binary to be encoded to be embedded in a larger structure that will need
to be in (most likely) a JSON text. This next example shows how this can be done with the polymede
JSON API.

```haskell
{{#include ../hs/src/PolymedeJSONBase64UrlNoPad.hs}}
```

## Benchmarks


This section provides summary information only &mdash; see the [Base64UrlNoPad benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base64UrlNoPad.html) for the full
breakdown, attached notes, etc.

### Key Results

Compiled with GHC 9.4.4 (aarch64), on the **marvin** reference node running macOS 13.2.1 (22D68):

* There are no highlighted benchmarks.

### Release Constraints

The following benchmarks are constrained to run inside the following bounds for each release:

| benchmark                   | nodes      | current  | bound        |
| --------------------------- | ---------- | -------- | ------------ |
| **enc-bl-001MB-Base64UrlB** | **marvin** | 452.1 μs | **485.0 μs** |
| enc-bl-001MB-Base64UrlB     | zaphod     | 452.1 μs | 352.0 μs     |

### Details

See the [Base64UrlNoPad benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base64UrlNoPad.html) for the full breakdown, attached notes, etc.


## Remarks on Expected Codec Performance

This RFC 4648 codec should perform well, especially over `MultibaseBytesLazy`.


## Test Vectors

Most of the test coverage for this codecs is to be found in the test suite for the underlying
codec but we do maintain some simple test vectors to ensure basic functionality and ensure that 
the correct codec has been integrated.

| encoded-input | expected           |
| ------------- | ------------------ |
| "u"           | ""                 |
| "uPA"         | "<"                |
| "uPDw"        | "<<"               |
| "uPDw_"       | "<<?"              |
| "uPDw_Pw"     | "<<??"             |
| "uPDw_Pz4"    | "<<??>"            |
| "uPDw_Pz4-"   | "<<??>>"           |
| "uZg"         | "f"                |
| "uZm8"        | "fo"               |
| "uZm9v"       | "foo"              |
| "uZm9vYg"     | "foob"             |
| "uZm9vYmE"    | "fooba"            |
| "uZm9vYmFy"   | "foobar"           |
| "u=eAoeAo="   | **invalid input**  |
| "ue=AoeAo="   | **invalid input**  |
| "u=eAoeAo="   | **invalid input**  |
| "ueAoe=Ao="   | **invalid input**  |
| "ueAoeA=o="   | **invalid input**  |
| "uZE=="       | **decode failure** |


In addition to these test vectors we test that the codec can successfully roundtrip a selection of 
random byte streams.

## Test Output

The codec tests generate the following output.

```
<{{#include ../test/Base64UrlNoPad-out.txt}}
```
