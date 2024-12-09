# party

[![Package Version](https://img.shields.io/hexpm/v/party)](https://hex.pm/packages/party)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/party/)

A little parser combinator library for Gleam!

This is great for simple tasks. If you need something more heavyweight and capable, check out [Atto](https://hexdocs.pm/atto)! It's a great project, and it may be more complicated but it's got more features and better errors than Party, including some features that Party will intentionally not add for the foreseeable future.

A little demo:
```gleam
fn identstring() -> Parser(String, e) {
  use first <- do(lowercase_letter())
  use rest <- do(many_concat(either(alphanum(), char("_"))))
  return(first <> rest)
}
```

## Installation

If available on Hex this package can be added to your Gleam project:

```sh
gleam add party
```

and its documentation can be found at <https://hexdocs.pm/party>.
