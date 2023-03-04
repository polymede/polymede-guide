# Base32PadUpper

## Essential Stats

| essential               | stat                                               |
| ----------------------- | -------------------------------------------------- |
| codec                   | Base32PadUpper                                     |
| codec home page         | [RFC 4648](https://www.rfc-editor.org/rfc/rfc4648) |
| multibase name          | base32padupper                                     |
| multibase code          | C                                                  |
| preferred pipeline      | bl                                                 |
| reference node          | zaphod                                             |
| time to encode 10 kB    | 7.026 μs                                           |
| time to decode to 10 kB | 12.98 μs                                           |
| 'foobar' encodes to     | "CMZXW6YTBOI======"                                |


## Codec Overview

This codec is part of a family of codecs standardised by the IETF in 
[RFC 4648](https://www.rfc-editor.org/rfc/rfc4648). The 
[Base 64 Wikipedia page](https://en.wikipedia.org/wiki/Base64) has a good overview.


## API

The API for this codec is on Hackage at [Data.Multibase.Types.Codecs.Base32PadUpper](https://hackage.haskell.org/package/polymede-0.0.0.1/docs/Data-Multibase-Types-Codecs-Base32PadUpper.html).

## A Simple Example

This example program shows the codec as a pair of functions: one to encode an octet stream, 
and another to decode the resulting multibase encoding to recover it.

```haskell
{{#include ../hs/src/PolymedeSimpleBase32PadUpper.hs}}
```

## A More Realistic Example

In reality we expect the binary to be encoded to be embedded in a larger structure that will need
to be in (most likely) a JSON text. This next example shows how this can be done with the polymede
JSON API.

```haskell
{{#include ../hs/src/PolymedeJSONBase32PadUpper.hs}}
```

## Benchmarks


This section provides summary information only &mdash; see the [Base32PadUpper benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base32PadUpper.html) for the full
breakdown, attached notes, etc.

### Key Results

Compiled with GHC 9.4.4 (x86_64), on the **zaphod** reference node running macOS 13.2.1 (22D68):

* There are no highlighted benchmarks.

### Release Constraints

The following benchmarks are constrained to run inside the following bounds for each release:

* there are no release constraints for this codec/report

### Details

See the [Base32PadUpper benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base32PadUpper.html) for the full breakdown, attached notes, etc.


## Remarks on Expected Codec Performance

This RFC 4648 codec should perform well, especially over `MultibaseBytesLazy`.


## Test Vectors

Most of the test coverage for this codecs is to be found in the test suite for the underlying
codec but we do maintain some simple test vectors to ensure basic functionality and ensure that 
the correct codec has been integrated.

| encoded-input       | expected           |
| ------------------- | ------------------ |
| "C"                 | ""                 |
| "CKN2W4==="         | "Sun"              |
| "CMY======"         | "f"                |
| "CMZXQ===="         | "fo"               |
| "CMZXW6==="         | "foo"              |
| "CMZXW6YQ="         | "foob"             |
| "CMZXW6YTB"         | "fooba"            |
| "CMZXW6YTBOI======" | "foobar"           |
| "C=CMZXW6==="       | **invalid input**  |
| "CZE=="             | **decode failure** |


In addition to these test vectors we test that the codec can successfully roundtrip a selection of 
random byte streams.

## Test Output

The codec tests generate the following output.

```
<{{#include ../test/Base32PadUpper-out.txt}}
```
