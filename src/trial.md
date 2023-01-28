# base64Pad

|                      |         |
|----------------------|---------|
| multibase name       | base64pad
| multibase character  | `M`
| home page            | <https://www.rfc-editor.org/rfc/rfc4648>
| polymede version     | 1.2.0.0


## Overview

From [Wikipedia](https://en.wikipedia.org/wiki/Base64) :

> In computer programming, Base64 is a group of binary-to-text encoding schemes that represent binary data (more specifically, a sequence of 8-bit bytes) in sequences of 24 bits that can be represented by four 6-bit Base64 digits.

> Common to all binary-to-text encoding schemes, Base64 is designed to carry data stored in binary formats across channels that only reliably support text content. Base64 is particularly prevalent on the World Wide Web[1] where one of its uses is the ability to embed image files or other binary assets inside textual assets such as HTML and CSS files.[2]

> Base64 is also widely used for sending e-mail attachments. This is required because SMTP – in its original form – was designed to transport 7-bit ASCII characters only. This encoding causes an overhead of 33–37% (33% by the encoding itself; up to 4% more by the inserted line breaks).

This codec has been thoroughly standardised by [RFC4648](https://en.wikipedia.org/wiki/Base64) and is well understood.


## API

The codec has been designed to work well with all types of input, though it works optimally with ByteString types.

```haskell
import Mb

myEncodeBase64 :: ByteString -> Md
myEncodeBase64 = encodeMd MdBase64Pad

myDecode :: Md -> Either String ByteString
myDecode = mdDecode

newtype MyBytes = MyBytes { getMyBytes :: ByteString }
  deriving (Show)
  deriving (ToJSON,FromJSON)
    via UsingMd MyBytes
```


## Performance and Benchmarks

On our reference workstation 

| Benchmark            | Description |
|----------------------|-------------|
| name-with-link       | encoder benchmarks, all stream types
| name-with-link       | decoder benchmarks, all stream types
| name-with-link       | comparison with native codec on select stream types

Link to the full benchmarks site.

## Test Vectors

| Input                | Output
|----------------------|-------------|
| name-with-link       | encoder benchmarks, various stream types
| name-with-link       | decoder benchmarks, various stream types

## Test Output

```
DVTestSuites
  all tests present:                                                                      OK
  no tests duplicated:                                                                    OK
  DVTestSuites
    pkt
      42/s-ref:                                                                           OK
      42/s-ext:                                                                           OK
      42/s-spc:                                                                           OK
      42/s-acc:                                                                           OK
      42/c-ref:                                                                           OK
      42/c-ext:                                                                           OK
      42/c-spc:                                                                           OK
      42/c-acc:                                                                           OK
      .42/s-ref:                                                                          OK
      .42/s-ext:                                                                          OK
      .42/s-spc:                                                                          OK
      .42/s-acc:                                                                          OK
      .42/c-ref:                                                                          OK
      .42/c-ext:                                                                          OK
      .42/c-spc:                                                                          OK
      .42/c-acc:                                                                          OK
      42./s-ref:                                                                          OK
      42./s-ext:                                                                          OK
      42./s-spc:                                                                          OK
      42./s-acc:                                                                          OK
      42./c-ref:                                                                          OK
      42./c-ext:                                                                          OK
      42./c-spc:                                                                          OK
      42./c-acc:                                                                          OK
      .42./s-ref:                                                                         OK
      .42./s-ext:                                                                         OK
      .42./s-spc:                                                                         OK
      .42./s-acc:                                                                         OK
      .42./c-ref:                                                                         OK
      .42./c-ext:                                                                         OK
      .42./c-spc:                                                                         OK
      .42./c-acc:                                                                         OK
      .42.2/s-ref:                                                                        OK
      .42.2/s-ext:                                                                        OK
      .42.2/s-spc:                                                                        OK
      .42.2/s-acc:                                                                        OK
      .42.2/c-ref:                                                                        OK
      .42.2/c-ext:                                                                        OK
      .42.2/c-spc:                                                                        OK
      .42.2/c-acc:                                                                        OK
      42,/s-ref:                                                                          OK
      42,/s-ext:                                                                          OK
      42,/s-spc:                                                                          OK
      42,/s-acc:                                                                          OK
      42,/c-ref:                                                                          OK
      42,/c-ext:                                                                          OK
      42,/c-spc:                                                                          OK
      42,/c-acc:                                                                          OK
      42.,/s-ref:                                                                         OK
      42.,/s-ext:                                                                         OK
      42.,/s-spc:                                                                         OK
      42.,/s-acc:                                                                         OK
      42.,/c-ref:                                                                         OK
      42.,/c-ext:                                                                         OK
      42.,/c-spc:                                                                         OK
      42.,/c-acc:                                                                         OK
      .42.,/s-ref:                                                                        OK
      .42.,/s-ext:                                                                        OK
      .42.,/s-spc:                                                                        OK
      .42.,/s-acc:                                                                        OK
      .42.,/c-ref:                                                                        OK
      .42.,/c-ext:                                                                        OK
      .42.,/c-spc:                                                                        OK
      .42.,/c-acc:                                                                        OK
      /s-ref:                                                                             OK
      /s-ext:                                                                             OK
      /s-spc:                                                                             OK
      /s-acc:                                                                             OK
      /c-ref:                                                                             OK
      /c-ext:                                                                             OK
      /c-spc:                                                                             OK
      /c-acc:                                                                             OK
      ./s-ref:                                                                            OK
      ./s-ext:                                                                            OK
      ./s-spc:                                                                            OK
      ./s-acc:                                                                            OK
      ./c-ref:                                                                            OK
      ./c-ext:                                                                            OK
      ./c-spc:                                                                            OK
      ./c-acc:                                                                            OK
      -42/s-ref:                                                                          OK
      -42/s-ext:                                                                          OK
      -42/s-spc:                                                                          OK
      -42/s-acc:                                                                          OK
      -42/c-ref:                                                                          OK
      -42/c-ext:                                                                          OK
      -42/c-spc:                                                                          OK
      -42/c-acc:                                                                          OK
      .-42/s-ref:                                                                         OK
      .-42/s-ext:                                                                         OK
      .-42/s-spc:                                                                         OK
      .-42/s-acc:                                                                         OK
      .-42/c-ref:                                                                         OK
      .-42/c-ext:                                                                         OK
      .-42/c-spc:                                                                         OK
      .-42/c-acc:                                                                         OK
      -.42/s-ref:                                                                         OK
      -.42/s-ext:                                                                         OK
      -.42/s-spc:                                                                         OK
      -.42/s-acc:                                                                         OK
      -.42/c-ref:                                                                         OK
      -.42/c-ext:                                                                         OK
      -.42/c-spc:                                                                         OK
      -.42/c-acc:                                                                         OK
    utc
      types
        Date:                                                                             OK
          +++ OK, passed 100 tests.
        LocalizedTime:                                                                    OK
          +++ OK, passed 100 tests.
        LocalizedTime_:                                                                   OK
          +++ OK, passed 100 tests.
        FlexiTimestamp:                                                                   OK
          +++ OK, passed 100 tests.
        FlexiTimestamp_:                                                                  OK
          +++ OK, passed 100 tests.
        Timestamp:                                                                        OK
          +++ OK, passed 100 tests.
        Timestamp_:                                                                       OK
          +++ OK, passed 100 tests.
        Timestampms:                                                                      OK
          +++ OK, passed 100 tests.
        Timestamp_ms:                                                                     OK
          +++ OK, passed 100 tests.
        Timestampps:                                                                      OK
          +++ OK, passed 100 tests.
        Timestamp_ps:                                                                     OK
          +++ OK, passed 100 tests.
        Timestampf:                                                                       OK
          +++ OK, passed 100 tests.
        Timestamp_f:                                                                      OK
          +++ OK, passed 100 tests.
        TimestampS:                                                                       OK
          +++ OK, passed 100 tests.
        Timestamp_S:                                                                      OK
          +++ OK, passed 100 tests.
        TimestampmsS:                                                                     OK
          +++ OK, passed 100 tests.
        Timestamp_msS:                                                                    OK
          +++ OK, passed 100 tests.
        Timestampl:                                                                       OK
          +++ OK, passed 100 tests.
        Timestamp_l:                                                                      OK
          +++ OK, passed 100 tests.
        UTC:                                                                              OK
          +++ OK, passed 100 tests.
        UTC_:                                                                             OK
          +++ OK, passed 100 tests.
        UTCms:                                                                            OK
          +++ OK, passed 100 tests.
        UTC_ms:                                                                           OK
          +++ OK, passed 100 tests.
        UTCps:                                                                            OK
          +++ OK, passed 100 tests.
        UTC_ps:                                                                           OK
          +++ OK, passed 100 tests.
        UTCf:                                                                             OK
          +++ OK, passed 100 tests.
        UTC_f:                                                                            OK
          +++ OK, passed 100 tests.
        UTCS:                                                                             OK
          +++ OK, passed 100 tests.
        UTC_S:                                                                            OK
          +++ OK, passed 100 tests.
        UTCmsS:                                                                           OK
          +++ OK, passed 100 tests.
        UTC_msS:                                                                          OK
          +++ OK, passed 100 tests.
        TSc:                                                                              OK
          +++ OK, passed 100 tests.
        TSd:                                                                              OK
          +++ OK, passed 100 tests.
        TSu:                                                                              OK
          +++ OK, passed 100 tests.
        TSv:                                                                              OK
          +++ OK, passed 100 tests.
      GBU Grid
        Date
          good : 2022-05-21:                                                              OK
          bad  : 2022-05-21T00:00:00:                                                     OK
          bad  : 2022-05-21T00:00:00Z:                                                    OK
        LocalizedTime
          good : 2022-05-21T00:00:00:                                                     OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21T00:00:00Z:                                                    OK
          ugly : 2022-05-21 00:00:00:                                                     OK
          ugly : 2022-05-21T00:00:00.123:                                                 OK
        LocalizedTime_
          good : 2022-05-21 00:00:00:                                                     OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21 00:00:00Z:                                                    OK
          ugly : 2022-05-21T00:00:00:                                                     OK
          ugly : 2022-05-21 00:00:00.123:                                                 OK
        FlexiTimestamp
          good : 2022-05-21T00:00:00Z:                                                    OK
          good : 2022-05-21T00:00:00:                                                     OK
          good : 2022-05-21T10:10:10+0100:                                                OK
          ugly : 2022-05-21:                                                              OK
          ugly : 2022-05-21 10:10:10Z:                                                    OK
          ugly : 2022-05-21T23:59:59.123:                                                 OK
        FlexiTimestamp_
          good : 2022-05-21 00:00:00Z:                                                    OK
          good : 2022-05-21 00:00:00:                                                     OK
          good : 2022-05-21 10:10:10+0100:                                                OK
          ugly : 2022-05-21:                                                              OK
          ugly : 2022-05-21T10:10:10Z:                                                    OK
          ugly : 2022-05-21 23:59:59.123:                                                 OK
        Timestamp
          good : 2022-05-21T00:00:00Z:                                                    OK
          good : 2022-05-21T00:00:00+0100:                                                OK
          good : 2022-05-21T00:00:00+2300:                                                OK
          good : 2022-05-21T00:00:00-2300:                                                OK
          good : 2022-05-21T00:00:00+2359:                                                OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21T00:00:00:                                                     OK
          bad  : 2022-05-21T00:00:00+2400:                                                OK
          bad  : 2022-05-21T00:00:00+2360:                                                OK
          ugly : 2022-05-21 00:00:00Z:                                                    OK
          ugly : 2022-05-21T00:00:00.123Z:                                                OK
          ugly : 2022-05-21T00:00:00+0000:                                                OK
        Timestamp_
          good : 2022-05-21 00:00:00Z:                                                    OK
          good : 2022-05-21 00:00:00+0100:                                                OK
          good : 2022-05-21 00:00:00+2300:                                                OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21 00:00:00:                                                     OK
          ugly : 2022-05-21T00:00:00Z:                                                    OK
          ugly : 2022-05-21 00:00:00.123Z:                                                OK
          ugly : 2022-05-21 00:00:00+0000:                                                OK
        Timestampf
          good : 2022-05-21T00:00:00Z:                                                    OK
          good : 2022-05-21T00:00:00.999Z:                                                OK
          good : 2022-05-21T00:00:00.123456789012+0100:                                   OK
          good : 2022-05-21T00:00:00.9+2300:                                              OK
          good : 2022-05-21T00:00:00.12-2300:                                             OK
          good : 2022-05-21T00:00:00.4567+2359:                                           OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21T00:00:00:                                                     OK
          bad  : 2022-05-21T00:00:00+2400:                                                OK
          bad  : 2022-05-21T00:00:00+2360:                                                OK
          ugly : 2022-05-21 00:00:00Z:                                                    OK
          ugly : 2022-05-21T00:00:00.123+0000:                                            OK
        Timestamp_f
          good : 2022-05-21 00:00:00Z:                                                    OK
          good : 2022-05-21 00:00:00.999Z:                                                OK
          good : 2022-05-21 00:00:00.123456789012+0100:                                   OK
          good : 2022-05-21 00:00:00.9+2300:                                              OK
          good : 2022-05-21 00:00:00.12-2300:                                             OK
          good : 2022-05-21 00:00:00.4567+2359:                                           OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21 00:00:00:                                                     OK
          bad  : 2022-05-21 00:00:00+2400:                                                OK
          bad  : 2022-05-21 00:00:00+2360:                                                OK
          ugly : 2022-05-21T00:00:00Z:                                                    OK
          ugly : 2022-05-21 00:00:00.123+0000:                                            OK
        Timestampms
          good : 2022-05-21T00:00:00.999Z:                                                OK
          good : 2022-05-21T00:00:00.123+0100:                                            OK
          good : 2022-05-21T00:00:00.988+2300:                                            OK
          good : 2022-05-21T00:00:00.123-2300:                                            OK
          good : 2022-05-21T00:00:00.456+2359:                                            OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21T00:00:00:                                                     OK
          bad  : 2022-05-21T00:00:00+2400:                                                OK
          bad  : 2022-05-21T00:00:00+2360:                                                OK
          ugly : 2022-05-21T00:00:00Z:                                                    OK
          ugly : 2022-05-21 00:00:00Z:                                                    OK
          ugly : 2022-05-21T00:00:00.123+0000:                                            OK
        Timestamp_ms
          good : 2022-05-21 00:00:00.999Z:                                                OK
          good : 2022-05-21 00:00:00.123+0100:                                            OK
          good : 2022-05-21 00:00:00.988+2300:                                            OK
          good : 2022-05-21 00:00:00.123-2300:                                            OK
          good : 2022-05-21 00:00:00.456+2359:                                            OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21 00:00:00:                                                     OK
          bad  : 2022-05-21 00:00:00+2400:                                                OK
          bad  : 2022-05-21 00:00:00+2360:                                                OK
          ugly : 2022-05-21 00:00:00Z:                                                    OK
          ugly : 2022-05-21T00:00:00Z:                                                    OK
          ugly : 2022-05-21 00:00:00.123+0000:                                            OK
        Timestampps
          good : 2022-05-21T00:00:00.999999999999Z:                                       OK
          good : 2022-05-21T00:00:00.123456789012+0100:                                   OK
          good : 2022-05-21T00:00:00.988988988988+2300:                                   OK
          good : 2022-05-21T00:00:00.123456789012-2300:                                   OK
          good : 2022-05-21T00:00:00.456789012345+2359:                                   OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21T00:00:00:                                                     OK
          bad  : 2022-05-21T00:00:00+2400:                                                OK
          bad  : 2022-05-21T00:00:00+2360:                                                OK
          ugly : 2022-05-21T00:00:00Z:                                                    OK
          ugly : 2022-05-21 00:00:00Z:                                                    OK
          ugly : 2022-05-21T00:00:00.123456789012+0000:                                   OK
        Timestamp_ps
          good : 2022-05-21 00:00:00.999999999999Z:                                       OK
          good : 2022-05-21 00:00:00.123456789012+0100:                                   OK
          good : 2022-05-21 00:00:00.988988988988+2300:                                   OK
          good : 2022-05-21 00:00:00.123456789012-2300:                                   OK
          good : 2022-05-21 00:00:00.456789012345+2359:                                   OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21 00:00:00:                                                     OK
          bad  : 2022-05-21 00:00:00+2400:                                                OK
          bad  : 2022-05-21 00:00:00+2360:                                                OK
          ugly : 2022-05-21 00:00:00Z:                                                    OK
          ugly : 2022-05-21T00:00:00Z:                                                    OK
          ugly : 2022-05-21 00:00:00.123456789012+0000:                                   OK
        TimestampS
          good : 2022-05-21T00:00:00Z:                                                    OK
          good : 2022-05-21T00:00:00+0100:                                                OK
          good : 2022-05-21T00:00:00+2300:                                                OK
          good : 2022-05-21T00:00:00-2300:                                                OK
          good : 2022-05-21T00:00:00+2359:                                                OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21T00:00:00:                                                     OK
          bad  : 2022-05-21T00:00:00+2400:                                                OK
          bad  : 2022-05-21T00:00:00+2360:                                                OK
          bad  : 2022-05-21T00:00:00.1Z:                                                  OK
          ugly : 2022-05-21 00:00:00Z:                                                    OK
          ugly : 2022-05-21T00:00:00+0000:                                                OK
        Timestamp_S
          good : 2022-05-21 00:00:00Z:                                                    OK
          good : 2022-05-21 00:00:00+0100:                                                OK
          good : 2022-05-21 00:00:00+2300:                                                OK
          good : 2022-05-21 00:00:00-2300:                                                OK
          good : 2022-05-21 00:00:00+2359:                                                OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21 00:00:00:                                                     OK
          bad  : 2022-05-21 00:00:00+2400:                                                OK
          bad  : 2022-05-21 00:00:00+2360:                                                OK
          bad  : 2022-05-21 00:00:00.123Z:                                                OK
          ugly : 2022-05-21T00:00:00Z:                                                    OK
          ugly : 2022-05-21 00:00:00+0000:                                                OK
        TimestampmsS
          good : 2022-05-21T00:00:00.123Z:                                                OK
          good : 2022-05-21T00:00:00.123+0100:                                            OK
          good : 2022-05-21T00:00:00.999+2300:                                            OK
          good : 2022-05-21T00:00:00.999-2300:                                            OK
          good : 2022-05-21T00:00:00.999+2359:                                            OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21T00:00:00:                                                     OK
          bad  : 2022-05-21T00:00:00+2400:                                                OK
          bad  : 2022-05-21T00:00:00+2360:                                                OK
          bad  : 2022-05-21T00:00:00Z:                                                    OK
          ugly : 2022-05-21 00:00:00.988Z:                                                OK
          ugly : 2022-05-21T00:00:00.123+0000:                                            OK
        Timestampl
          good : 2022-05-21T00:00:00Z:                                                    OK
          good : 2022-05-21T00:00:00:                                                     OK
          good : 2022-05-21T00:00:00+2300:                                                OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21T00:00:00+2400:                                                OK
          bad  : 2022-05-21T00:00:00+2360:                                                OK
          ugly : 2022-05-21 00:00:00Z:                                                    OK
          ugly : 2022-05-21T00:00:00+0000:                                                OK
          ugly : 2022-05-21T00:00:00-0000:                                                OK
        Timestamp_l
          good : 2022-05-21 00:00:00Z:                                                    OK
          good : 2022-05-21 00:00:00:                                                     OK
          good : 2022-05-21 00:00:00+2300:                                                OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21 00:00:00+2400:                                                OK
          bad  : 2022-05-21 00:00:00+2360:                                                OK
          ugly : 2022-05-21T00:00:00Z:                                                    OK
          ugly : 2022-05-21 00:00:00+0000:                                                OK
          ugly : 2022-05-21 00:00:00-0000:                                                OK
        UTC
          good : 2022-05-21T00:00:00Z:                                                    OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21T00:00:00:                                                     OK
          ugly : 2022-05-21 00:00:00Z:                                                    OK
          ugly : 2022-05-21T00:00:00.123Z:                                                OK
          ugly : 2022-05-21T00:00:00+0000:                                                OK
          ugly : 2022-05-21T00:00:00+0100:                                                OK
        UTC_
          good : 2022-05-21 00:00:00Z:                                                    OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21 00:00:00:                                                     OK
          ugly : 2022-05-21T00:00:00Z:                                                    OK
          ugly : 2022-05-21 00:00:00.123Z:                                                OK
          ugly : 2022-05-21 00:00:00+0000:                                                OK
          ugly : 2022-05-21 00:00:00+0100:                                                OK
        UTCms
          good : 2022-05-21T00:00:00.123Z:                                                OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21T00:00:00:                                                     OK
          ugly : 2022-05-21 00:00:00.123Z:                                                OK
          ugly : 2022-05-21T00:00:00Z:                                                    OK
          ugly : 2022-05-21T00:00:00.1Z:                                                  OK
          ugly : 2022-05-21T00:00:00.1234Z:                                               OK
          ugly : 2022-05-21T00:00:00.123+0000:                                            OK
          ugly : 2022-05-21T00:00:00.123+0100:                                            OK
        UTC_ms
          good : 2022-05-21 00:00:00.123Z:                                                OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21 00:00:00:                                                     OK
          ugly : 2022-05-21T00:00:00Z:                                                    OK
          ugly : 2022-05-21 00:00:00Z:                                                    OK
          ugly : 2022-05-21 00:00:00.1Z:                                                  OK
          ugly : 2022-05-21 00:00:00.1234Z:                                               OK
          ugly : 2022-05-21 00:00:00+0000:                                                OK
          ugly : 2022-05-21 00:00:00+0100:                                                OK
        UTCps
          good : 2022-05-21T00:00:00.123456789012Z:                                       OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21T00:00:00:                                                     OK
          ugly : 2022-05-21 00:00:00Z:                                                    OK
          ugly : 2022-05-21T00:00:00Z:                                                    OK
          ugly : 2022-05-21T00:00:00+0000:                                                OK
          ugly : 2022-05-21T00:00:00+0100:                                                OK
        UTC_ps
          good : 2022-05-21 00:00:00.123456789012Z:                                       OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21 00:00:00:                                                     OK
          ugly : 2022-05-21T00:00:00Z:                                                    OK
          ugly : 2022-05-21 00:00:00Z:                                                    OK
          ugly : 2022-05-21 00:00:00+0000:                                                OK
          ugly : 2022-05-21 00:00:00+0100:                                                OK
        UTCf
          good : 2022-05-21T00:00:00Z:                                                    OK
          good : 2022-05-21T00:00:00.1Z:                                                  OK
          good : 2022-05-21T00:00:00.1234Z:                                               OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21T00:00:00:                                                     OK
          ugly : 2022-05-21 00:00:00Z:                                                    OK
          ugly : 2022-05-21T00:00:00+0000:                                                OK
          ugly : 2022-05-21T00:00:00+0100:                                                OK
        UTC_f
          good : 2022-05-21 00:00:00Z:                                                    OK
          good : 2022-05-21 00:00:00.12Z:                                                 OK
          good : 2022-05-21 00:00:00.123456789012Z:                                       OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21 00:00:00:                                                     OK
          ugly : 2022-05-21T00:00:00Z:                                                    OK
          ugly : 2022-05-21 00:00:00+0000:                                                OK
          ugly : 2022-05-21 00:00:00+0100:                                                OK
        UTCS
          good : 2022-05-21T00:00:00Z:                                                    OK
          bad  : 2022-05-21T00:00:00.1Z:                                                  OK
          bad  : 2022-05-21T00:00:00.1234Z:                                               OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21T00:00:00:                                                     OK
          ugly : 2022-05-21 00:00:00Z:                                                    OK
          ugly : 2022-05-21T00:00:00+0000:                                                OK
          ugly : 2022-05-21T00:00:00+0100:                                                OK
        UTC_S
          good : 2022-05-21 00:00:00Z:                                                    OK
          bad  : 2022-05-21 00:00:00.12Z:                                                 OK
          bad  : 2022-05-21 00:00:00.123456789012Z:                                       OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21 00:00:00:                                                     OK
          ugly : 2022-05-21T00:00:00Z:                                                    OK
          ugly : 2022-05-21 00:00:00+0000:                                                OK
          ugly : 2022-05-21 00:00:00+0100:                                                OK
        UTCmsS
          good : 2022-05-21T00:00:00.123Z:                                                OK
          bad  : 2022-05-21T00:00:00Z:                                                    OK
          bad  : 2022-05-21T00:00:00.1234Z:                                               OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21T00:00:00:                                                     OK
          ugly : 2022-05-21 00:00:00.123Z:                                                OK
          ugly : 2022-05-21T00:00:00.123+0000:                                            OK
          ugly : 2022-05-21T00:00:00.123+0100:                                            OK
        UTC_msS
          good : 2022-05-21 00:00:00.123Z:                                                OK
          bad  : 2022-05-21 00:00:00.1:                                                   OK
          bad  : 2022-05-21 00:00:00.123456789012Z:                                       OK
          bad  : 2022-05-21:                                                              OK
          bad  : 2022-05-21 00:00:00:                                                     OK
          ugly : 2022-05-21T00:00:00.123Z:                                                OK
          ugly : 2022-05-21 00:00:00.123+0000:                                            OK
          ugly : 2022-05-21 00:00:00.123+0100:                                            OK
      Special GBU Grid
        TSc
          good : 2022-05-21T10:10:10Z:                                                    OK
          good : 2022-05-21T10:10:10+01:                                                  OK
          good : 2022-05-21T10:10:10+0115:                                                OK
          good : 2022-05-21T10:10:10:                                                     OK
          bad  : 2022-05-21:                                                              OK
          ugly : 2022-05-21T10:10:10+00:                                                  OK
          ugly : 2022-05-21T10:10:10+0100:                                                OK
          ugly : 2022-05-21T10:10:10-00:                                                  OK
          ugly : 2022-05-21T10:10:10-0000:                                                OK
        TSd
          good : 2022-05-21T10:10:10+00:                                                  OK
          good : 2022-05-21T10:10:10+01:                                                  OK
          good : 2022-05-21T10:10:10+0115:                                                OK
          good : 2022-05-21T10:10:10-00:                                                  OK
          bad  : 2022-05-21:                                                              OK
          ugly : 2022-05-21T10:10:10:                                                     OK
          ugly : 2022-05-21T10:10:10+0100:                                                OK
          ugly : 2022-05-21T10:10:10+01:00:                                               OK
          ugly : 2022-05-21T10:10:10-0000:                                                OK
          ugly : 2022-05-21T10:10:10-00:00:                                               OK
        TSu
          good : 2022-05-21T10:10:10+0000:                                                OK
          good : 2022-05-21T10:10:10+0100:                                                OK
          good : 2022-05-21T10:10:10+0115:                                                OK
          good : 2022-05-21T10:10:10-0000:                                                OK
          bad  : 2022-05-21:                                                              OK
          ugly : 2022-05-21T10:10:10:                                                     OK
          ugly : 2022-05-21T10:10:10+01:                                                  OK
          ugly : 2022-05-21T10:10:10+01:00:                                               OK
          ugly : 2022-05-21T10:10:10-00:                                                  OK
          ugly : 2022-05-21T10:10:10-00:00:                                               OK
        TSv
          good : 2022-05-21T10:10:10+00:00:                                               OK
          good : 2022-05-21T10:10:10+01:00:                                               OK
          good : 2022-05-21T10:10:10+01:15:                                               OK
          good : 2022-05-21T10:10:10-00:00:                                               OK
          bad  : 2022-05-21:                                                              OK
          ugly : 2022-05-21T10:10:10:                                                     OK
          ugly : 2022-05-21T10:10:10+01:                                                  OK
          ugly : 2022-05-21T10:10:10+0100:                                                OK
          ugly : 2022-05-21T10:10:10-00:                                                  OK
          ugly : 2022-05-21T10:10:10-0000:                                                OK
      UTCRecord
        UTCRecord:                                                                        OK
          +++ OK, passed 100 tests.
        UTCRecordReadingUTC:                                                              OK
          +++ OK, passed 100 tests.
        UTCRecordReadingZT:                                                               OK (0.01s)
          +++ OK, passed 100 tests.
        UTCRecordShowingUTC:                                                              OK
          +++ OK, passed 100 tests.
        UTCRecordShowingTZ:                                                               OK
          +++ OK, passed 100 tests.
    exc
      trapdef-plain:                                                                      OK (0.03s)
        +++ OK, passed 3 tests.
      trapdef-smain:                                                                      OK (0.03s)
        +++ OK, passed 3 tests.
      trapdef-amain:                                                                      OK (0.03s)
        +++ OK, passed 3 tests.
      trapdef-xtidy:                                                                      OK (0.05s)
        +++ OK, passed 3 tests.
      trapdef-xmnty:                                                                      OK (0.04s)
        +++ OK, passed 3 tests.
      trapall-plain:                                                                      OK (0.05s)
        +++ OK, passed 3 tests.
      trapall-xmain:                                                                      OK (0.05s)
        +++ OK, passed 3 tests.
      trapall-xtidy:                                                                      OK (0.07s)
        +++ OK, passed 3 tests.
      trapall-xmnty:                                                                      OK (0.04s)
        +++ OK, passed 3 tests.
      handle--plain:                                                                      OK (0.05s)
        +++ OK, passed 3 tests.
      handle--xaamn:                                                                      OK (0.06s)
        +++ OK, passed 3 tests.
      handle--xssmn:                                                                      OK (0.04s)
        +++ OK, passed 3 tests.
      handle--xasmn:                                                                      OK (0.04s)
        +++ OK, passed 3 tests.
      handle--xsamn:                                                                      OK (0.08s)
        +++ OK, passed 3 tests.
      handle--xmnhd:                                                                      OK (0.05s)
        +++ OK, passed 3 tests.
      bracket-plain:                                                                      OK (0.02s)
        +++ OK, passed 1 test.
      bracket-xmain:                                                                      OK (0.04s)
        +++ OK, passed 3 tests.
      bracket-xsetp:                                                                      OK (0.08s)
        +++ OK, passed 3 tests.
      bracket-xtidy:                                                                      OK (0.04s)
        +++ OK, passed 3 tests.
      bracket-xmnty:                                                                      OK (0.05s)
        +++ OK, passed 3 tests.
      brktone-plain:                                                                      OK (0.08s)
        +++ OK, passed 3 tests.
      brktone-xmain:                                                                      OK (0.06s)
        +++ OK, passed 3 tests.
      brktone-xsetp:                                                                      OK (0.05s)
        +++ OK, passed 3 tests.
      brktone-xtidy:                                                                      OK (0.05s)
        +++ OK, passed 3 tests.
      brktone-xmnty:                                                                      OK (0.05s)
        +++ OK, passed 3 tests.
      withfnl-plain:                                                                      OK (0.07s)
        +++ OK, passed 3 tests.
      withfnl-xmain:                                                                      OK (0.06s)
        +++ OK, passed 3 tests.
      withfnl-xtidy:                                                                      OK (0.05s)
        +++ OK, passed 3 tests.
      withfnl-xmnty:                                                                      OK (0.07s)
        +++ OK, passed 3 tests.
      onabort-plain:                                                                      OK (0.06s)
        +++ OK, passed 3 tests.
      onabort-xmain:                                                                      OK (0.06s)
        +++ OK, passed 3 tests.
      onabort-xtidy:                                                                      OK (0.05s)
        +++ OK, passed 3 tests.
      onabort-xmnty:                                                                      OK (0.04s)
        +++ OK, passed 3 tests.
      withhdr-plain:                                                                      OK (0.06s)
        +++ OK, passed 3 tests.
      withhdr-xaamn:                                                                      OK (0.06s)
        +++ OK, passed 3 tests.
      withhdr-xssmn:                                                                      OK (0.06s)
        +++ OK, passed 3 tests.
      withhdr-xasmn:                                                                      OK (0.05s)
        +++ OK, passed 3 tests.
      withhdr-xsamn:                                                                      OK (0.05s)
        +++ OK, passed 3 tests.
      withhdg-plain:                                                                      OK (0.05s)
        +++ OK, passed 3 tests.
      withhdg-xmain:                                                                      OK (0.04s)
        +++ OK, passed 3 tests.
      withhdg-xtidy:                                                                      OK (0.04s)
        +++ OK, passed 3 tests.
      withhdg-xmnty:                                                                      OK (0.02s)
        +++ OK, passed 3 tests.
    fex:                                                                                  OK
      +++ OK, passed 100 tests.
    fwl:                                                                                  OK (0.29s)
      +++ OK, passed 100 tests.
    pgs
      0:                                                                                  OK
      1:                                                                                  OK
    pgp
      PGPPacketQC
        test_du_jour:                                                                     OK
        QuickCheck
          AEADAlgorithm:                                                                  OK
            +++ OK, passed 100 tests.
          AEADChunkSize:                                                                  OK
            +++ OK, passed 100 tests.
          AEADEncryptedDataPacket:                                                        OK
            +++ OK, passed 100 tests.
          AEADEncryptedDataPacketData:                                                    OK
            +++ OK, passed 100 tests.
          BitLength:                                                                      OK
            +++ OK, passed 100 tests.
          Bytes160:                                                                       OK
            +++ OK, passed 100 tests.
          CheckData:                                                                      OK
            +++ OK, passed 100 tests.
          ClearTextData:                                                                  OK
            +++ OK, passed 100 tests.
          CompressionAlgorithm:                                                           OK
            +++ OK, passed 100 tests.
          CompressedData:                                                                 OK
            +++ OK, passed 100 tests.
          CompressedDataPacket:                                                           OK
            +++ OK, passed 100 tests.
          EncryptedData:                                                                  OK
            +++ OK, passed 100 tests.
          EncryptedDataPacket:                                                            OK
            +++ OK, passed 100 tests.
          Features:                                                                       OK
            +++ OK, passed 100 tests.
          HashAlgorithm:                                                                  OK
            +++ OK, passed 100 tests.
          Issuer:                                                                         OK
            +++ OK, passed 100 tests.
          IssuerFingerprint:                                                              OK
            +++ OK, passed 100 tests.
          KeyFlags:                                                                       OK
            +++ OK, passed 100 tests.
          KeyID:                                                                          OK
            +++ OK, passed 100 tests.
          KeyServerPreferences:                                                           OK
            +++ OK, passed 100 tests.
          KeyVersionNumber:                                                               OK
            +++ OK, passed 100 tests.
          LiteralDataPacket:                                                              OK
            +++ OK, passed 100 tests.
          LiteralDataPacketEncoding:                                                      OK
            +++ OK, passed 100 tests.
          LiteralDataPacketFilename:                                                      OK
            +++ OK, passed 100 tests.
          MarkerPacket:                                                                   OK
            +++ OK, passed 100 tests.
          ModificationDetectionCode:                                                      OK
            +++ OK, passed 100 tests.
          MPI:                                                                            OK
            +++ OK, passed 100 tests.
          NumOctets16:                                                                    OK
            +++ OK, passed 100 tests.
          NumberOfDays16:                                                                 OK
            +++ OK, passed 100 tests.
          OldPacketLen0:                                                                  OK
            +++ OK, passed 100 tests.
          OldPacketLen1:                                                                  OK
            +++ OK, passed 100 tests.
          OldPacketLen2:                                                                  OK
            +++ OK, passed 100 tests.
          OldPacketLen3:                                                                  OK
            +++ OK, passed 100 tests.
          OnePassSignaturePackets:                                                        OK
            +++ OK, passed 100 tests.
          PGPVersion:                                                                     OK
            +++ OK, passed 100 tests.
          Packet:                                                                         OK
            +++ OK, passed 100 tests.
          PacketHeader:                                                                   OK
            +++ OK, passed 100 tests.
          PacketLen:                                                                      OK
            +++ OK, passed 100 tests.
          Packets:                                                                        OK (0.01s)
            +++ OK, passed 100 tests.
          PacketType:                                                                     OK
            +++ OK, passed 100 tests.
          PGPOctetFlag:                                                                   OK
            +++ OK, passed 100 tests.
          PreferredAEADAlgorithms:                                                        OK
            +++ OK, passed 100 tests.
          PreferredCompressionAlgorithms:                                                 OK
            +++ OK, passed 100 tests.
          PreferredHashAlgorithms:                                                        OK
            +++ OK, passed 100 tests.
          PreferredSymmetricAlgorithms:                                                   OK
            +++ OK, passed 100 tests.
          PublicKeyAlgorithm:                                                             OK
            +++ OK, passed 100 tests.
          PublicKeyData:                                                                  OK
            +++ OK, passed 100 tests.
          PublicKeyEncryptedSessionKey:                                                   OK
            +++ OK, passed 100 tests.
          PublicKeyEncryptedSessionKeyData:                                               OK
            +++ OK, passed 100 tests.
          PublicKeyPacket:                                                                OK
            +++ OK, passed 100 tests.
          PublicKeyAlgorithmData:                                                         OK
            +++ OK, passed 100 tests.
          PublicKeyV3Data:                                                                OK
            +++ OK, passed 100 tests.
          Reserved-00-Packet:                                                             OK
            +++ OK, passed 100 tests.
          S2KCount:                                                                       OK
            +++ OK, passed 100 tests.
          S2KSpecifier:                                                                   OK
            +++ OK, passed 100 tests.
          S2KUsage:                                                                       OK
            +++ OK, passed 100 tests.
          Salt8:                                                                          OK
            +++ OK, passed 100 tests.
          SecretKeyPacket:                                                                OK
            +++ OK, passed 100 tests.
          SecretKeyPacketData:                                                            OK
            +++ OK, passed 100 tests.
          ShortSignature:                                                                 OK
            +++ OK, passed 100 tests.
          SignatureCreationTime:                                                          OK
            +++ OK, passed 100 tests.
          SignatureExpirationTime:                                                        OK
            +++ OK, passed 100 tests.
          SignaturePacket:                                                                OK
            +++ OK, passed 100 tests.
          SignatureSubpacket:                                                             OK
            +++ OK, passed 100 tests.
          SignatureSubpacketData:                                                         OK
            +++ OK, passed 100 tests.
          SignatureSubpackets:                                                            OK
            +++ OK, passed 100 tests.
          SignatureSubpacketType:                                                         OK
            +++ OK, passed 100 tests.
          SignatureType:                                                                  OK
            +++ OK, passed 100 tests.
          SymEncryptedIntegrityProtectedBytes:                                            OK
            +++ OK, passed 100 tests.
          SymEncryptedIntegrityProtectedData:                                             OK
            +++ OK, passed 100 tests.
          SymmetricKeyAlgorithm:                                                          OK
            +++ OK, passed 100 tests.
          SymmetricKeyEncryptedSessionKey:                                                OK
            +++ OK, passed 100 tests.
          SymmetricKeyEncryptedSessionKeyData:                                            OK
            +++ OK, passed 100 tests.
          TimeStamp:                                                                      OK
            +++ OK, passed 100 tests.
          TrustPacket:                                                                    OK
            +++ OK, passed 100 tests.
          Undefined-15-Packet:                                                            OK
            +++ OK, passed 100 tests.
          Undefined-16-Packet:                                                            OK
            +++ OK, passed 100 tests.
          UnknownPacket:                                                                  OK
            +++ OK, passed 100 tests.
          UserAttributeData:                                                              OK
            +++ OK, passed 100 tests.
          UserAttributePacket:                                                            OK
            +++ OK, passed 100 tests.
          UserAttributeSubpacket:                                                         OK
            +++ OK, passed 100 tests.
          UserAttributeType:                                                              OK
            +++ OK, passed 100 tests.
          UserID:                                                                         OK
            +++ OK, passed 100 tests.
          V3SigData:                                                                      OK
            +++ OK, passed 100 tests.
      Files
        000001-006.public_key:                                                            OK
        000002-013.user_id:                                                               OK
        000003-002.sig:                                                                   OK (0.01s)
        000004-012.ring_trust:                                                            OK (0.01s)
        000005-002.sig:                                                                   OK
        000006-012.ring_trust:                                                            OK (0.01s)
        000007-002.sig:                                                                   OK
        000008-012.ring_trust:                                                            OK (0.01s)
        000009-002.sig:                                                                   OK (0.02s)
        000010-012.ring_trust:                                                            OK
        000011-002.sig:                                                                   OK (0.02s)
        000012-012.ring_trust:                                                            OK (0.01s)
        000013-014.public_subkey:                                                         OK (0.01s)
        000014-002.sig:                                                                   OK (0.01s)
        000015-012.ring_trust:                                                            OK (0.01s)
        000016-006.public_key:                                                            OK (0.01s)
        000017-002.sig:                                                                   OK (0.02s)
        000018-012.ring_trust:                                                            OK
        000019-013.user_id:                                                               OK (0.02s)
        000020-002.sig:                                                                   OK
        000021-012.ring_trust:                                                            OK (0.01s)
        000022-002.sig:                                                                   OK (0.02s)
        000023-012.ring_trust:                                                            OK (0.01s)
        000024-014.public_subkey:                                                         OK
        000025-002.sig:                                                                   OK (0.01s)
        000026-012.ring_trust:                                                            OK (0.01s)
        000027-006.public_key:                                                            OK (0.01s)
        000028-002.sig:                                                                   OK (0.01s)
        000029-012.ring_trust:                                                            OK (0.01s)
        000030-013.user_id:                                                               OK (0.01s)
        000031-002.sig:                                                                   OK (0.01s)
        000032-012.ring_trust:                                                            OK (0.01s)
        000033-002.sig:                                                                   OK (0.02s)
        000034-012.ring_trust:                                                            OK (0.02s)
        000035-006.public_key:                                                            OK (0.01s)
        000036-013.user_id:                                                               OK (0.01s)
        000037-002.sig:                                                                   OK (0.01s)
        000038-012.ring_trust:                                                            OK (0.01s)
        000039-002.sig:                                                                   OK (0.02s)
        000040-012.ring_trust:                                                            OK (0.01s)
        000041-017.attribute:                                                             OK (0.01s)
        000042-002.sig:                                                                   OK (0.02s)
        000043-012.ring_trust:                                                            OK (0.01s)
        000044-014.public_subkey:                                                         OK
        000045-002.sig:                                                                   OK (0.01s)
        000046-012.ring_trust:                                                            OK
        000047-005.secret_key:                                                            OK (0.01s)
        000048-013.user_id:                                                               OK (0.01s)
        000049-002.sig:                                                                   OK (0.02s)
        000050-012.ring_trust:                                                            OK (0.01s)
        000051-007.secret_subkey:                                                         OK (0.02s)
        000052-002.sig:                                                                   OK (0.02s)
        000053-012.ring_trust:                                                            OK
        000054-005.secret_key:                                                            OK (0.01s)
        000055-002.sig:                                                                   OK (0.01s)
        000056-012.ring_trust:                                                            OK (0.01s)
        000057-013.user_id:                                                               OK
        000058-002.sig:                                                                   OK (0.02s)
        000059-012.ring_trust:                                                            OK (0.01s)
        000060-007.secret_subkey:                                                         OK (0.01s)
        000061-002.sig:                                                                   OK (0.01s)
        000062-012.ring_trust:                                                            OK (0.01s)
        000063-005.secret_key:                                                            OK (0.01s)
        000064-002.sig:                                                                   OK (0.02s)
        000065-012.ring_trust:                                                            OK (0.01s)
        000066-013.user_id:                                                               OK (0.01s)
        000067-002.sig:                                                                   OK
        000068-012.ring_trust:                                                            OK (0.01s)
        000069-005.secret_key:                                                            OK (0.01s)
        000070-013.user_id:                                                               OK (0.01s)
        000071-002.sig:                                                                   OK (0.01s)
        000072-012.ring_trust:                                                            OK (0.02s)
        000073-017.attribute:                                                             OK
        000074-002.sig:                                                                   OK (0.02s)
        000075-012.ring_trust:                                                            OK (0.02s)
        000076-007.secret_subkey:                                                         OK (0.02s)
        000077-002.sig:                                                                   OK
        000078-012.ring_trust:                                                            OK (0.01s)
        002182-002.sig:                                                                   OK (0.01s)
        3F5BBA0B0694BEB6000005-002.sig:                                                   OK (0.01s)
        3F5BBA0B0694BEB6000017-002.sig:                                                   OK (0.01s)
        camina.gpg:                                                                       OK
        cd.gpg:                                                                           OK (0.01s)
        compressedsig-bzip2.gpg:                                                          OK (0.01s)
        compressedsig-zlib.gpg:                                                           OK
        compressedsig.gpg:                                                                OK (0.01s)
        drummer.gpg:                                                                      OK (0.01s)
        onepass_sig:                                                                      OK (0.01s)
        pubring.gpg:                                                                      OK (0.04s)
        secring.gpg:                                                                      OK (0.01s)
        symmetrically_encrypted:                                                          OK
        uncompressed-ops-dsa-sha384.txt.gpg:                                              OK
        uncompressed-ops-dsa.gpg:                                                         OK
        uncompressed-ops-rsa.gpg:                                                         OK
    phb
      build-fail
        simple-cycle:                                                                     OK
        complex-cycle:                                                                    OK
        child-policy:                                                                     OK
        context-policy:                                                                   OK
      page-filter
        base
          BlakePosts.second:                                                              OK
          ModelPersonalBlog.ellis:                                                        OK
          JordanAlexViralResponseComments.0-alex:                                         OK
          EllisPosts.first:                                                               OK
          ModelPersonalBlog.jo:                                                           OK
          ModelPersonalAbout.drew:                                                        OK
          JordanAlexViralResponseComments.1-jordan:                                       OK
          ModelPersonalBlog.jamie:                                                        OK
          BlakePosts.first:                                                               OK
          ModelPersonalBlog.chris:                                                        OK
          AlexViralComments.2-jordan:                                                     OK
          ModelPersonalBlog.emery:                                                        OK
          ChrisPosts.first:                                                               OK
          ModelPersonalAbout.alex:                                                        OK
          JordanPosts.first:                                                              OK
          EmeryPosts.first:                                                               OK
          AlexPosts.viral:                                                                OK
          AlexViralComments.7-jordan:                                                     OK
          DrewPosts.first:                                                                OK
          AlexViralComments.6-jo:                                                         OK
          AlexViralComments.5-emery:                                                      OK
          AlexInitialComments.0-alex:                                                     OK
          ModelPersonalAbout.blake:                                                       OK
          JordanAlexViralResponseComments.2-blake:                                        OK
          AlexInitialComments.1-blake:                                                    OK
          ModelPersonalBlog.jordan:                                                       OK
          AlexViralComments.1-blake:                                                      OK
          JamiePosts.first:                                                               OK
          JoPosts.first:                                                                  OK
          JordanPosts.alex-viral-response:                                                OK
          ModelPersonalAbout.emery:                                                       OK
          ModelPersonalBlog.drew:                                                         OK
          JordanAlexViralResponseComments.3-jordan:                                       OK
          AlexViralComments.4-ellis:                                                      OK
          AlexViralComments.8-chris:                                                      OK
          AlexViralComments.0-alex:                                                       OK
          ModelPersonalBlogDir.persona-blog-dir:                                          OK
          ModelPersonalAbout.jo:                                                          OK
          AlexViralComments.3-drew:                                                       OK
          ModelPersonalAbout.jamie:                                                       OK
          AlexPosts.first:                                                                OK
          ModelPersonalAbout.chris:                                                       OK
          ModelPersonalBlog.alex:                                                         OK
          ModelPersonalBlog.blake:                                                        OK
          AlexInitialComments.2-jordan:                                                   OK
          ModelPersonalAbout.jordan:                                                      OK
          ModelPersonalAbout.ellis:                                                       OK
        star
          BlakePosts.second:                                                              OK
          ModelPersonalBlog.ellis:                                                        OK
          JordanAlexViralResponseComments.0-alex:                                         OK
          EllisPosts.first:                                                               OK
          ModelPersonalBlog.jo:                                                           OK
          ModelPersonalAbout.drew:                                                        OK
          JordanAlexViralResponseComments.1-jordan:                                       OK
          ModelPersonalBlog.jamie:                                                        OK
          BlakePosts.first:                                                               OK
          ModelPersonalBlog.chris:                                                        OK
          AlexViralComments.2-jordan:                                                     OK
          ModelPersonalBlog.emery:                                                        OK
          ChrisPosts.first:                                                               OK
          ModelPersonalAbout.alex:                                                        OK
          JordanPosts.first:                                                              OK
          EmeryPosts.first:                                                               OK
          AlexPosts.viral:                                                                OK
          AlexViralComments.7-jordan:                                                     OK
          DrewPosts.first:                                                                OK
          AlexViralComments.6-jo:                                                         OK
          AlexViralComments.5-emery:                                                      OK
          AlexInitialComments.0-alex:                                                     OK
          ModelPersonalAbout.blake:                                                       OK
          JordanAlexViralResponseComments.2-blake:                                        OK
          AlexInitialComments.1-blake:                                                    OK
          ModelPersonalBlog.jordan:                                                       OK
          AlexViralComments.1-blake:                                                      OK
          JamiePosts.first:                                                               OK
          JoPosts.first:                                                                  OK
          JordanPosts.alex-viral-response:                                                OK
          ModelPersonalAbout.emery:                                                       OK
          ModelPersonalBlog.drew:                                                         OK
          JordanAlexViralResponseComments.3-jordan:                                       OK
          AlexViralComments.4-ellis:                                                      OK
          AlexViralComments.8-chris:                                                      OK
          AlexViralComments.0-alex:                                                       OK
          ModelPersonalBlogDir.persona-blog-dir:                                          OK
          ModelPersonalAbout.jo:                                                          OK
          AlexViralComments.3-drew:                                                       OK
          ModelPersonalAbout.jamie:                                                       OK
          AlexPosts.first:                                                                OK
          ModelPersonalAbout.chris:                                                       OK
          ModelPersonalBlog.alex:                                                         OK
          ModelPersonalBlog.blake:                                                        OK
          AlexInitialComments.2-jordan:                                                   OK
          ModelPersonalAbout.jordan:                                                      OK
          ModelPersonalAbout.ellis:                                                       OK
        plus
          BlakePosts.second:                                                              OK
          ModelPersonalBlog.ellis:                                                        OK
          JordanAlexViralResponseComments.0-alex:                                         OK
          EllisPosts.first:                                                               OK
          ModelPersonalBlog.jo:                                                           OK
          ModelPersonalAbout.drew:                                                        OK
          JordanAlexViralResponseComments.1-jordan:                                       OK
          ModelPersonalBlog.jamie:                                                        OK
          BlakePosts.first:                                                               OK
          ModelPersonalBlog.chris:                                                        OK
          AlexViralComments.2-jordan:                                                     OK
          ModelPersonalBlog.emery:                                                        OK
          ChrisPosts.first:                                                               OK
          ModelPersonalAbout.alex:                                                        OK
          JordanPosts.first:                                                              OK
          EmeryPosts.first:                                                               OK
          AlexPosts.viral:                                                                OK
          AlexViralComments.7-jordan:                                                     OK
          DrewPosts.first:                                                                OK
          AlexViralComments.6-jo:                                                         OK
          AlexViralComments.5-emery:                                                      OK
          AlexInitialComments.0-alex:                                                     OK
          ModelPersonalAbout.blake:                                                       OK
          JordanAlexViralResponseComments.2-blake:                                        OK
          AlexInitialComments.1-blake:                                                    OK
          ModelPersonalBlog.jordan:                                                       OK
          AlexViralComments.1-blake:                                                      OK
          JamiePosts.first:                                                               OK
          JoPosts.first:                                                                  OK
          JordanPosts.alex-viral-response:                                                OK
          ModelPersonalAbout.emery:                                                       OK
          ModelPersonalBlog.drew:                                                         OK
          JordanAlexViralResponseComments.3-jordan:                                       OK
          AlexViralComments.4-ellis:                                                      OK
          AlexViralComments.8-chris:                                                      OK
          AlexViralComments.0-alex:                                                       OK
          ModelPersonalBlogDir.persona-blog-dir:                                          OK
          ModelPersonalAbout.jo:                                                          OK
          AlexViralComments.3-drew:                                                       OK
          ModelPersonalAbout.jamie:                                                       OK
          AlexPosts.first:                                                                OK
          ModelPersonalAbout.chris:                                                       OK
          ModelPersonalBlog.alex:                                                         OK
          ModelPersonalBlog.blake:                                                        OK
          AlexInitialComments.2-jordan:                                                   OK
          ModelPersonalAbout.jordan:                                                      OK
          ModelPersonalAbout.ellis:                                                       OK
      tag-expr
        TagExprVectorTests
          model
            empty:                                                                        OK
          phoebe
            empty:                                                                        OK
        TagExprFunctionTests
          model
            mtc-alex-blog:                                                                OK
            mtc-jrdn-blog:                                                                OK
            msm-alex-blog:                                                                OK
            msm-jrdn-blog:                                                                OK
            msg-jmie-blog:                                                                OK
            cyc-drew-blog:                                                                OK
            cyc-ells-blog:                                                                OK
            cyc-emry-blog:                                                                OK
          phoebe
            mtc-alex-blog:                                                                OK
            mtc-jrdn-blog:                                                                OK
            msm-alex-blog:                                                                OK
            msm-jrdn-blog:                                                                OK
            msg-jmie-blog:                                                                OK
            cyc-drew-blog:                                                                OK
            cyc-ells-blog:                                                                OK
            cyc-emry-blog:                                                                OK
        TagExprComparisonTests
          model
            pna-eq-z:                                                                     OK
            pna-ne-z:                                                                     OK
            pna-eq-1:                                                                     OK
            pna-ne-1:                                                                     OK
            pna-eq-2-9:                                                                   OK
            pna-ne-2-9:                                                                   OK
            pna-eq-12:                                                                    OK
            pna-ne-12:                                                                    OK
            pge-eq-z:                                                                     OK
            pge-ne-z:                                                                     OK
            pge-eq-1:                                                                     OK
            pge-ne-1:                                                                     OK
            pge-eq-2-9:                                                                   OK
            pge-ne-2-9:                                                                   OK
            pge-eq-12:                                                                    OK
            pge-ne-12:                                                                    OK
            pna-eq-foo-z:                                                                 OK
            pna-ne-foo-z:                                                                 OK
            pna-eq-foo-bar:                                                               OK
            pna-ne-foo-bar:                                                               OK
            pge-eq-foo-z:                                                                 OK
            pge-ne-foo-z:                                                                 OK
            pge-eq-foo-bar:                                                               OK
            pge-ne-foo-bar:                                                               OK
          phoebe
            pna-eq-z:                                                                     OK
            pna-ne-z:                                                                     OK
            pna-eq-1:                                                                     OK
            pna-ne-1:                                                                     OK
            pna-eq-2-9:                                                                   OK
            pna-ne-2-9:                                                                   OK
            pna-eq-12:                                                                    OK
            pna-ne-12:                                                                    OK
            pge-eq-z:                                                                     OK
            pge-ne-z:                                                                     OK
            pge-eq-1:                                                                     OK
            pge-ne-1:                                                                     OK
            pge-eq-2-9:                                                                   OK
            pge-ne-2-9:                                                                   OK
            pge-eq-12:                                                                    OK
            pge-ne-12:                                                                    OK
            pna-eq-foo-z:                                                                 OK
            pna-ne-foo-z:                                                                 OK
            pna-eq-foo-bar:                                                               OK
            pna-ne-foo-bar:                                                               OK
            pge-eq-foo-z:                                                                 OK
            pge-ne-foo-z:                                                                 OK
            pge-eq-foo-bar:                                                               OK
            pge-ne-foo-bar:                                                               OK
        TagExprLogicTests
          model
            trivial-pass:                                                                 OK
            trivial-fail:                                                                 OK
            black-empty:                                                                  OK
            white-empty:                                                                  OK
            black-t:                                                                      OK
            white-t:                                                                      OK
            black-f:                                                                      OK
            white-f:                                                                      OK
            black-ff:                                                                     OK
            white-ff:                                                                     OK
            black-ft:                                                                     OK
            white-ft:                                                                     OK
            black-tf:                                                                     OK
            white-tf:                                                                     OK
            black-tt:                                                                     OK
            white-tt:                                                                     OK
            and-empty:                                                                    OK
            nand-empty:                                                                   OK
            and-t:                                                                        OK
            nand-t:                                                                       OK
            and-f:                                                                        OK
            nand-f:                                                                       OK
            and-tf:                                                                       OK
            nand-tf:                                                                      OK
            implies-empty:                                                                OK
            implies-t:                                                                    OK
            implies-f:                                                                    OK
            implies-ff:                                                                   OK
            implies-tf:                                                                   OK
            implies-ft:                                                                   OK
            implies-tt:                                                                   OK
            implies-tft:                                                                  OK
            implies-ftt:                                                                  OK
            implies-ttt:                                                                  OK
            implies-tff:                                                                  OK
            implies-ftf:                                                                  OK
            implies-ttf:                                                                  OK
          phoebe
            trivial-pass:                                                                 OK
            trivial-fail:                                                                 OK
            black-empty:                                                                  OK
            white-empty:                                                                  OK
            black-t:                                                                      OK
            white-t:                                                                      OK
            black-f:                                                                      OK
            white-f:                                                                      OK
            black-ff:                                                                     OK
            white-ff:                                                                     OK
            black-ft:                                                                     OK
            white-ft:                                                                     OK
            black-tf:                                                                     OK
            white-tf:                                                                     OK
            black-tt:                                                                     OK
            white-tt:                                                                     OK
            and-empty:                                                                    OK
            nand-empty:                                                                   OK
            and-t:                                                                        OK
            nand-t:                                                                       OK
            and-f:                                                                        OK
            nand-f:                                                                       OK
            and-tf:                                                                       OK
            nand-tf:                                                                      OK
            implies-empty:                                                                OK
            implies-t:                                                                    OK
            implies-f:                                                                    OK
            implies-ff:                                                                   OK
            implies-tf:                                                                   OK
            implies-ft:                                                                   OK
            implies-tt:                                                                   OK
            implies-tft:                                                                  OK
            implies-ftt:                                                                  OK
            implies-ttt:                                                                  OK
            implies-tff:                                                                  OK
            implies-ftf:                                                                  OK
            implies-ttf:                                                                  OK
        Phoebe property tests:                                                            OK (0.01s)
          +++ OK, passed 100 tests.
      text-edit
        blog:                                                                             OK
        %page:                                                                            OK
        TextEditDelete @Blog:                                                             OK
        TextEditDelete @Page:                                                             OK
      restore
        production:                                                                       OK (0.09s)
        testing:                                                                          OK
      thorough-model
        update-page-thorough:                                                             OK
        update-page-fail-self-not-admin:                                                  OK
        update-page-unauthorized:                                                         OK
        update-page-missing:                                                              OK
        delete-page-unauthorized:                                                         OK
        delete-page-missing:                                                              OK
        delete-blog:                                                                      OK
        delete-page:                                                                      OK
        delete-persona:                                                                   OK
        delete-session:                                                                   OK
        delete-tpersona:                                                                  OK
        delete-tpage:                                                                     OK
        delete-pfn:                                                                       OK
      et
        simple-fail:                                                                      OK
        foreign-fail:                                                                     OK
        no-fail:                                                                          OK
    dia
      scan
        pragma
          positive
            "{-# foo #-}"/"":                                                             OK
            "{-# #- -} #-}"/"#-}":                                                        OK
            "{-##-}"/" ":                                                                 OK
          negative
            {-# foo :                                                                     OK
            {-#:                                                                          OK
            {-#-}:                                                                        OK
          zero
            foo:                                                                          OK
            :                                                                             OK
            #-}:                                                                          OK
            {- foo -}:                                                                    OK
        dialect
          positive
            "{-@ foo @-}"/"":                                                             OK
            "{-@ @- -} @-}"/"@-}":                                                        OK
            "{-@@-}"/" ":                                                                 OK
          negative
            {-@ foo :                                                                     OK
            {-@:                                                                          OK
            {-@-}:                                                                        OK
          zero
            foo:                                                                          OK
            :                                                                             OK
            @-}:                                                                          OK
            {- foo -}:                                                                    OK
        whitespace
          positive
            " \n\t\v"/"foo ":                                                             OK
            " "/"":                                                                       OK
            "\n"/".":                                                                     OK
          zero
            foo:                                                                          OK
            :                                                                             OK
        body
          positive
            "foo"/"":                                                                     OK
            "("/"":                                                                       OK
          zero
            :                                                                             OK
            {-:                                                                           OK
        line comments
          positive
            "-- cmt "/"":                                                                 OK
            "----"/"\n":                                                                  OK
            "-- cmt "/"\n trailing":                                                      OK
            "-- {-"/"":                                                                   OK
          zero
            :                                                                             OK
            {-:                                                                           OK
            baz:                                                                          OK
            - --:                                                                         OK
        block comments
          positive
            "{- comment -}"/"":                                                           OK
            "{- {--} {--} -}"/"{-":                                                       OK
            "{- {-@ LANGUAGE @-} -}"/"--":                                                OK
            "{-$ comment $-}"/"-}":                                                       OK
            "{-$ -- $-}"/"-}":                                                            OK
          negative
            {-:                                                                           OK
            {-}:                                                                          OK
            {- {- comment -}:                                                             OK
            -}:                                                                           OK
          zero
            :                                                                             OK
            {-# #-}:                                                                      OK
            {-@ @-}:                                                                      OK
      module
        contiguous
          ""/"":                                                                          OK
          "{-##-}"/"":                                                                    OK
          "{-##-}\n{-##-}"/"\n\n":                                                        OK
          "{-##-} \t\v\n \t\v--baz "/"\n\n":                                              OK
          "{-##-}\n{-##-}"/"\n \t\v\n":                                                   OK
        dialects
          {-@  @-}:                                                                       OK
          {-@ foo/bar @-}:                                                                OK
          {-@ foo/bar foo'-._1/foo'-._1 @-}:                                              OK
        LANGUAGE
          {-# LANGUAGE BangPatterns #-}:                                                  OK
          {-# LANGUAGE BangPatterns, TypeFamilies #-}:                                    OK
          {-# LANGUAGE BangPatterns, TemplateHaskell, TupleSections #-}:                  OK
          {-# LANGUAGE testx1 #-}:                                                        OK
          {-# LANGUAGE FlexibleContexts, testx1 #-}:                                      OK
          {-# LANGUAGE testx1, FlexibleContexts, testx2 #-}:                              OK
        Items
          "{-# LANGUAGE BangPatterns #-}\n{-# LANGUAGE StrictData #-}\n-- comment\n":     OK
          "":                                                                             OK
        DialectsWithExtensions
          "{-@ foo/bar @-}\n\n":                                                          OK
          "{-@ foo/bar @-}\n{-# LANGUAGE BangPatterns #-}\n\n":                           OK
          "{-@ foo/bar baz/wibble @-}\n{-# LANGUAGE BangPatterns, TupleSections #-}\n\n": OK
        ModuleTest
          first:                                                                          OK
          second:                                                                         OK
          dialect-00:                                                                     OK
          dialect-01:                                                                     OK
          dialect-02:                                                                     OK
          dialect-03:                                                                     OK
          dialect-04:                                                                     OK
          dialect-05:                                                                     OK
          dialect-06:                                                                     OK
          dialect-07:                                                                     OK
          dialect-08:                                                                     OK
          dialect-09:                                                                     OK
          dialect-10:                                                                     OK
      dialect
        analyseModule
          tm1:                                                                            OK
          tm2:                                                                            OK
          tm3:                                                                            OK
          tm4:                                                                            OK
        applyEdit
          te1:                                                                            OK
          te1a:                                                                           OK
          te1b:                                                                           OK
          te2:                                                                            OK
          te4:                                                                            OK

All 1073 tests passed (0.57s)

```