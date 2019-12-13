# haskell-server-example

Simple Haskell server with the backend from qfpl's [applied-fp-course](https://github.com/qfpl/applied-fp-course) and an Elm frontend

## Prerequisites

- SQLite setup on your machine

## Usage

`$ stack setup`

Then either:
```
$ stack build
$ stack exec haskell-server-example-exe
```

or

`$ stack run`

To build the front end:

```
$ cd frontend
$ elm make src/Main.elm
```

## Interactive mode

`$ stack ghci`
