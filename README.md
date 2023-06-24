# monkey-rust

An implementation of a [Monkey](https://monkeylang.org/) interpreter in Rust.

## Usage

```console
$ cargo run
>> 1 + 2
3
>> let factorial = fn(n) { if (n == 1) { 1 } else { n * factorial(n - 1) }}
>> factorial(10)
3628800
```
