# gsrinfer

[![Build Status](https://travis-ci.org/ymyzk/gsrinfer.svg?branch=master)](https://travis-ci.org/ymyzk/gsrinfer)

A type reconstruction algorithm implementation for the gradual implicitly typed language with shift and reset.

## Usage
### Compile & Run
```shell
omake
./src/main/main
```

### Run unit tests
```shell
omake test
```

## Requirements
- OCaml 4.01+
- OMake
- Menhir
- OUnit2

## References
- [Principal Type Schemes for Gradual Programs. In Proc. of ACM POPL, 2015.](http://www.cs.ubc.ca/~rxg/ptsgp.pdf)
