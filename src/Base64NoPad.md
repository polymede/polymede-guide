# Base64NoPad

## Essential Stats

| essential               | stat                                                   |
| ----------------------- | ------------------------------------------------------ |
| codec                   | Base64NoPad                                            |
| codec home page         | [multibase](https://github.com/multiformats/multibase) |
| multibase name          | base64                                                 |
| multibase code          | m                                                      |
| preferred pipeline      | bl                                                     |
| reference node          | zaphod                                                 |
| time to encode 10 kB    | 3.413 μs                                               |
| time to decode to 10 kB | 16.77 μs                                               |
| 'foobar' encodes to     | "mZm9vYmFy"                                            |


## Codec Overview

This codec is based on the RF4648 base64 codec with the following ajustments:
<<Ajustments>>


## API

The API for this codec is on Hackage at [Data.Multibase.Types.Codecs.Base64NoPad](https://hackage.haskell.org/package/polymede-0.0.0.1/docs/Data-Multibase-Types-Codecs-Base64NoPad.html).

## A Simple Example

This example program shows the codec as a pair of functions: one to encode an octet stream, 
and another to decode the resulting multibase encoding to recover it.

```haskell
{{#include ../hs/src/PolymedeSimpleBase64NoPad.hs}}
```

## A More Realistic Example

In reality we expect the binary to be encoded to be embedded in a larger structure that will need
to be in (most likely) a JSON text. This next example shows how this can be done with the polymede
JSON API.

```haskell
{{#include ../hs/src/PolymedeJSONBase64NoPad.hs}}
```

## Benchmarks


This section provides summary information only &mdash; see the [Base64NoPad benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base64NoPad.html) for the full
breakdown, attached notes, etc.

### Key Results

Compiled with GHC 9.4.4 (x86_64), on the **zaphod** reference node running macOS 13.2.1 (22D68):

* There are no highlighted benchmarks.

### Release Constraints

The following benchmarks are constrained to run inside the following bounds for each release:

| benchmark                | nodes      | current  | bound        |
| ------------------------ | ---------- | -------- | ------------ |
| enc-bl-001MB-Base64B     | marvin     | 311.5 μs | 485.0 μs     |
| **enc-bl-001MB-Base64B** | **zaphod** | 311.5 μs | **352.0 μs** |

### Details

See the [Base64NoPad benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base64NoPad.html) for the full breakdown, attached notes, etc.


## Remarks on Expected Codec Performance

This codec does not conform to RFC 4648 and will perform poorly in comparison to
its corresponding RFC 4648 codec.


## Test Vectors

Most of the test coverage for this codecs is to be found in the test suite for the underlying
codec but we do maintain some simple test vectors to ensure basic functionality and ensure that 
the correct codec has been integrated.

| encoded-input | expected           |
| ------------- | ------------------ |
| "m"           | ""                 |
| "mPA"         | "<"                |
| "mPDw"        | "<<"               |
| "mPDw/"       | "<<?"              |
| "mPDw/Pw"     | "<<??"             |
| "mPDw/Pz4"    | "<<??>"            |
| "mPDw/Pz4+"   | "<<??>>"           |
| "mZg"         | "f"                |
| "mZm8"        | "fo"               |
| "mZm9v"       | "foo"              |
| "mZm9vYg"     | "foob"             |
| "mZm9vYmE"    | "fooba"            |
| "mZm9vYmFy"   | "foobar"           |
| "mZE=="       | **decode failure** |


In addition to these test vectors we test that the codec can successfully roundtrip a selection of 
random byte streams.

## Test Output

The codec tests generate the following output.

```
<{{#include ../test/Base64NoPad-out.txt}}
```
