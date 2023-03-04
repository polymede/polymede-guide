# Base256Emoji

## Essential Stats

| essential               | stat                                                   |
| ----------------------- | ------------------------------------------------------ |
| codec                   | Base256Emoji                                           |
| codec home page         | [multibase](https://github.com/multiformats/multibase) |
| multibase name          | base256emoji                                           |
| multibase code          | 🚀                                                      |
| preferred pipeline      | by                                                     |
| reference node          | zaphod                                                 |
| time to encode 10 kB    | 887.0 μs                                               |
| time to decode to 10 kB | 7.671 ms                                               |
| 'foobar' encodes to     | "\128640\128538\128531\128531\128073\129316\129402"    |


## Codec Overview

This codec is a base-256 codec with a custom alphabet using variable-sized 
codepoints. It is intended to make it easier to recognise long, familiar sequences of octets.


## API

The API for this codec is on Hackage at [Data.Multibase.Types.Codecs.Base256Emoji](https://hackage.haskell.org/package/polymede-0.0.0.1/docs/Data-Multibase-Types-Codecs-Base256Emoji.html).

## A Simple Example

This example program shows the codec as a pair of functions: one to encode an octet stream, 
and another to decode the resulting multibase encoding to recover it.

```haskell
{{#include ../hs/src/PolymedeSimpleBase256Emoji.hs}}
```

## A More Realistic Example

In reality we expect the binary to be encoded to be embedded in a larger structure that will need
to be in (most likely) a JSON text. This next example shows how this can be done with the polymede
JSON API.

```haskell
{{#include ../hs/src/PolymedeJSONBase256Emoji.hs}}
```

## Benchmarks


This section provides summary information only &mdash; see the [Base256Emoji benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base256Emoji.html) for the full
breakdown, attached notes, etc.

### Key Results

Compiled with GHC 9.4.4 (x86_64), on the **zaphod** reference node running macOS 13.2.1 (22D68):

* There are no highlighted benchmarks.

### Release Constraints

The following benchmarks are constrained to run inside the following bounds for each release:

* there are no release constraints for this codec/report

### Details

See the [Base256Emoji benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base256Emoji.html) for the full breakdown, attached notes, etc.


## Remarks on Expected Codec Performance

This codec has not been optimized and should proobably not well suited to large data sets.


## Test Vectors

Most of the test coverage for this codecs is to be found in the test suite for the underlying
codec but we do maintain some simple test vectors to ensure basic functionality and ensure that 
the correct codec has been integrated.

| encoded-input                                       | expected           |
| --------------------------------------------------- | ------------------ |
| "\128640"                                           | ""                 |
| "\128640\128538\128531\128531\128073\129316\129402" | "foobar"           |
| "\128640x"                                          | **decode failure** |


In addition to these test vectors we test that the codec can successfully roundtrip a selection of 
random byte streams.

## Test Output

The codec tests generate the following output.

```
<{{#include ../test/Base256Emoji-out.txt}}
```
