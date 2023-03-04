# Base8

## Essential Stats

| essential               | stat                                                   |
| ----------------------- | ------------------------------------------------------ |
| codec                   | Base8                                                  |
| codec home page         | [multibase](https://github.com/multiformats/multibase) |
| multibase name          | base8                                                  |
| multibase code          | 7                                                      |
| preferred pipeline      | by                                                     |
| reference node          | zaphod                                                 |
| time to encode 10 kB    | 563.9 μs                                               |
| time to decode to 10 kB | 43.45 ms                                               |
| 'foobar' encodes to     | "73146755730460562"                                    |


## Codec Overview

This codec encodes an octet stream using a standard base-8 codec using the
following alphabet:
| input | encoding |
| ----- | -------- |
| 0x00  | '0'      |
| 0x01  | '1'      |
| 0x02  | '2'      |
| 0x03  | '3'      |
| 0x04  | '4'      |
| 0x05  | '5'      |
| 0x06  | '6'      |
| 0x07  | '7'      |



## API

The API for this codec is on Hackage at [Data.Multibase.Types.Codecs.Base8](https://hackage.haskell.org/package/polymede-0.0.0.1/docs/Data-Multibase-Types-Codecs-Base8.html).

## A Simple Example

This example program shows the codec as a pair of functions: one to encode an octet stream, 
and another to decode the resulting multibase encoding to recover it.

```haskell
{{#include ../hs/src/PolymedeSimpleBase8.hs}}
```

## A More Realistic Example

In reality we expect the binary to be encoded to be embedded in a larger structure that will need
to be in (most likely) a JSON text. This next example shows how this can be done with the polymede
JSON API.

```haskell
{{#include ../hs/src/PolymedeJSONBase8.hs}}
```

## Benchmarks


This section provides summary information only &mdash; see the [Base8 benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base8.html) for the full
breakdown, attached notes, etc.

### Key Results

Compiled with GHC 9.4.4 (x86_64), on the **zaphod** reference node running macOS 13.2.1 (22D68):

* There are no highlighted benchmarks.

### Release Constraints

The following benchmarks are constrained to run inside the following bounds for each release:

* there are no release constraints for this codec/report

### Details

See the [Base8 benchmarks](https://cdornan.github.io/polymede-benchmarks/benchmarks/0.0.0.1/Base8.html) for the full breakdown, attached notes, etc.


## Remarks on Expected Codec Performance

This codec has not been optimised but should perform moderatly well


## Test Vectors

Most of the test coverage for this codecs is to be found in the test suite for the underlying
codec but we do maintain some simple test vectors to ensure basic functionality and ensure that 
the correct codec has been integrated.

| encoded-input                       | expected             |
| ----------------------------------- | -------------------- |
| "7"                                 | ""                   |
| "7362625631006654133464440102"      | "yes mani !"         |
| "7000745453462015530267151100204"   | "\NULyes mani !"     |
| "700000171312714403326055632220041" | "\NUL\NULyes mani !" |
| "79"                                | **decode failure**   |


In addition to these test vectors we test that the codec can successfully roundtrip a selection of 
random byte streams.

## Test Output

The codec tests generate the following output.

```
<{{#include ../test/Base8-out.txt}}
```
