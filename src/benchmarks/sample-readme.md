# README

This folder contains the benchmark data for the polymede-0.0.0.1 **Base64Pad** codec
compiled with GHC 9.4.4, on the **marvin** aarch64 reference node
running macOS 13.2 (22D49).

## Key Results

* encodes 1,000,000 bytes in **481.8 μs** using the **ByteLazy** pipeline

## Results Summary

| benchmark                      | duration |
| ------------------------------ | -------- |
| encode-1MB-Base64Pad-Text      | 602.6 μs |
| encode-1MB-Base64Pad-TextLazy  | 544.8 μs |
| encode-1MB-Base64Pad-TextShort | 584.4 μs |
| encode-1MB-Base64Pad-Byte      | 527.6 μs |
| encode-1MB-Base64Pad-ByteLazy  | 481.8 μs |
| encode-1MB-Base64Pad-ByteShort | 583.7 μs |

See <index.html> for the full breakdown with detailed methodological notes.

## Release Constraints

The following benchmarks are constrained to run inside the following bounds for each release:

| benchmark                         | nodes                      | current  | bound        |
| --------------------------------- | -------------------------- | -------- | ------------ |
| **encode-1MB-Base64Pad-ByteLazy** | **marvin** zaphod trillian | 481.8 μs | **485.0 μs** |
| encode-1MB-Base64Pad-ByteLazy     | dat                        | 481.8 μs | 352.0 μs     |
