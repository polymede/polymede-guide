# Proquint

## Essential Stats

| essential              | stat                                                                                                                      |
| ---------------------- | ------------------------------------------------------------------------------------------------------------------------- |
| codec                  | Proquint                                                                                                                  |
| codec home page        | [A Proposal for Proquints: Identifiers that are Readable, Spellable, and Pronounceable](https://arxiv.org/html/0901.4016) |
| multibase name         | proquint                                                                                                                  |
| multibase code         | p                                                                                                                         |
| preferred pipeline     | by                                                                                                                        |
| reference node         | zaphod                                                                                                                    |
| time to encode 10KB    | 694.5 μs                                                                                                                  |
| time to decode to 10KB | 760.3 μs                                                                                                                  |
| 'foobar' encodes to    | "pro-kinoz-kutof-kajuf"                                                                                                   |


## Codec Overview

Proquints are a proposal for encoding 16-bit words that are readable, spellable and
pronounceable. For example, 127.0.0.1 would be encoded as "lusab-babad" (absent any identifying 
prefix). It isn't clear how octet streams with an odd number of octets should be encoded with
proquints so we will use an encoder that extends odd-sized streams with an extra zero octet onto
the end of the stream.


## API

The API for this codec is on Hackage at [Data.Multibase.Types.Codecs.Proquint](https://hackage.haskell.org/package/polymede-0.0.0.1/docs/Data-Multibase-Types-Codecs-Proquint.html).

## A Simple Example

This example program shows the codec as a pair of functions: one to encode an octet stream, 
and another to decode the resulting multibase encoding to recover it.

```haskell
{{#include ../hs/src/PolymedeSimpleProquint.hs}}
```

## A More Realistic Example

In reality we expect the binary to be encoded to be embedded in a larger structure that will need
to be in (most likely) a JSON text. This next example shows how this can be done with the polymede
JSON API.

```haskell
{{#include ../hs/src/PolymedeJSONProquint.hs}}
```

## Benchmarks


This section provides summary information only &mdash; see the [Proquint benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Proquint.html) for the full
breakdown, attached notes, etc.

### Key Results

Compiled with GHC 9.4.4 (aarch64), on the **marvin** reference node running macOS 13.2.1 (22D68):

* There are no highlighted benchmarks.

### Release Constraints

The following benchmarks are constrained to run inside the following bounds for each release:

* there are no release constraints for this codec/report

### Details

See the [Proquint benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Proquint.html) for the full breakdown, attached notes, etc.


## Remarks on Expected Codec Performance

This codec has not been optimized and should proobably not well suited to large data sets.


## Test Vectors

Most of the test coverage for this codecs is to be found in the test suite for the underlying
codec but we do maintain some simple test vectors to ensure basic functionality and ensure that 
the correct codec has been integrated.

| encoded-input           | expected           |
| ----------------------- | ------------------ |
| "pro"                   | ""                 |
| "pro-lusab-babad"       | "\DEL\NUL\NUL\SOH" |
| "pro-kinoz-kutof-kajuf" | "foobar"           |
| "pro-"                  | **decode failure** |


In addition to these test vectors we test that the codec can successfully roundtrip a selection of 
random byte streams.

## Test Output

The codec tests generate the following output.

```
<{{#include ../test/Proquint-out.txt}}
```
