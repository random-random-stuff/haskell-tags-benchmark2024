# conduit-parse

The `conduit-extra` package provides utilities to turn a `Parser` into a `Consumer`, but only for streams of `ByteString`s of `Text`s (cf `Data.Conduit.Attoparsec` module).

This library makes it possible to work with any kind of input by providing a general-purpose parsing framework based on `conduit`.
