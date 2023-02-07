# Base64Pad

## Essential Stats

| essential             | stat                                               |
| --------------------- | -------------------------------------------------- |
| codec                 | Base64Pad                                          |
| codec home page       | [RFC 4648](https://www.rfc-editor.org/rfc/rfc4648) |
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

# Base64Pad

This folder contains the benchmark results for the Base64Pad compiled with GHC 9.4.4 (aarch64), on the 
**marvin** reference node running macOS 13.2 (22D49).

## Key Results

* encodes 1,000,000 bytes in **457.2 μs** using the **ByteLazy** pipeline
* decodes to 1,000,000 bytes in **1.894 ms** using the **ByteLazy** pipeline

## Results Summary

| benchmark                      | duration |
| ------------------------------ | -------- |
| encode-1MB-Base64Pad-Text      | 542.4 μs |
| encode-1MB-Base64Pad-TextLazy  | 497.9 μs |
| encode-1MB-Base64Pad-TextShort | 513.5 μs |
| encode-1MB-Base64Pad-Byte      | 479.6 μs |
| encode-1MB-Base64Pad-ByteLazy  | 457.2 μs |
| encode-1MB-Base64Pad-ByteShort | 509.8 μs |
| decode-1MB-Base64Pad-Text      | 1.922 ms |
| decode-1MB-Base64Pad-TextLazy  | 1.922 ms |
| decode-1MB-Base64Pad-TextShort | 1.948 ms |
| decode-1MB-Base64Pad-Byte      | 1.925 ms |
| decode-1MB-Base64Pad-ByteLazy  | 1.894 ms |
| decode-1MB-Base64Pad-ByteShort | 1.950 ms |
| decode-1MB-N#Base64Pad-Byte    | 1.911 ms |
| decode-1MB-X#Base64Pad-Byte    | 1.972 ms |

## Release Constraints

The following benchmarks are constrained to run inside the following bounds for each release:

| benchmark                         | nodes                      | current  | bound        |
| --------------------------------- | -------------------------- | -------- | ------------ |
| **encode-1MB-Base64Pad-ByteLazy** | **marvin** zaphod trillian | 457.2 μs | **485.0 μs** |
| encode-1MB-Base64Pad-ByteLazy     | dat                        | 457.2 μs | 352.0 μs     |

## Notes

The following observations concerning these results have been logged:

### [Unreleased]

#### Base64Pad-decode

* _decode times appear to be somewhat greater than expected_ ([@cdornan], 2023-02-01)

    This needs further investigation.

### [0.0.0.1]

#### Base64Pad-encode

* _lazy encode pipelines slightly better -- for now_ ([@cdornan], 2023-02-01)

    This should only be significant on small inputs.

    We hope to fix this soon in a coming release.

[Unreleased]: <https://github.com/cdornan/polymede-benchmarks>
[0.0.0.1]: <https://github.com/cdornan/polymede-benchmarks>
[@cdornan]: <https://github.com/cdornan>

## Breakdown

See the following individual reports for a breakdown with methodological notes, etc.

* [Base64Pad-decode]
* [Base64Pad-encode]
* [Base64Pad-hack]
* [Base64Pad-misc]

[Base64Pad-encode]: <./Base64Pad-encode/index.html>
[Base64Pad-misc]: <./Base64Pad-misc/index.html>
[Base64Pad-hack]: <./Base64Pad-hack/index.html>
[Base64Pad-decode]: <./Base64Pad-decode/index.html>



## Remarks on Expected Codec Performance

This RFC 4648 codec should perform well, especially over `MultibaseBytesLazy`.


## Tests

Most of the test coverage for this codecs is to be found in the test suite for the underlying
codec but we do maintain some simple test vectors to ensure basic functionality and ensure that 
the correct codec has been integrated.

| encoded-input | expected           |
| ------------- | ------------------ |
| "M"           | ""                 |
| "MZg=="       | "f"                |
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
