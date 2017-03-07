# gsrinfer

[![Build Status](https://travis-ci.org/ymyzk/gsrinfer.svg?branch=master)](https://travis-ci.org/ymyzk/gsrinfer)

A type reconstruction algorithm implementation for the gradual implicitly typed language with shift and reset.

**Try it online!! https://gsrinfer.ymyzk.com**

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
- OCaml 4.02+
- OMake
- Menhir
- OUnit2
- js_of_ocaml

## References
- Yusuke Miyazaki and Atsushi Igarashi. A type reconstruction algorithm for gradually typed delimited continuations. In 第19回プログラミングおよびプログラミング言語ワークショップ (PPL2017) 論文集, 2017.
- [Ronald Garcia and Matteo Cimini. Principal Type Schemes for Gradual Programs. In Proc. of ACM POPL, 2015.](http://www.cs.ubc.ca/~rxg/ptsgp.pdf)
