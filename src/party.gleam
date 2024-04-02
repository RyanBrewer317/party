//// A simple parser combinator library.
//// Party is stable, though breaking changes might come in a far-future 2.0 or 3.0 release.
//// The way this works is that Party parsers operate on a stream of tokens.
//// The user can use any type of token they want, 
//// but every combinator has a version of it specialized to list-of-character streams.
//// If you're parsing over a string, don't use types starting with `General`.
//// Your life will be easier. 
//// Using type synonyms, Party also makes much of this code agnostic to the token type,
//// so switching to a custom token type later won't be *too* painful.

// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import gleam/string
import gleam/result

/// The generalized error type for the parser,
/// for any token type `t` and user-defined error type `e`.
/// Only use this type if you're parsing over a custom list of tokens;
/// if you're just parsing over a string, use `ParseError` instead.
pub type GeneralParseError(t, e) {
  /// The error for an unexpected token encountered when parsing.
  Unexpected(pos: Position, error: t)
  /// The error for a user-defined operation failing during parsing, such as `int.parse`.
  UserError(pos: Position, error: e)
}

/// The custom error type for the parser, 
/// which can itself be parameterized by a user-defined error type.
/// The user-defined error type is useful for, for example, 
/// adding a `int.parse` call into your parser pipeline.
/// See `try` for using this feature.
pub type ParseError(e) =
  GeneralParseError(String, e)

/// The type for positions within a string.
pub type Position {
  Position(row: Int, col: Int)
}

/// The parser type, parameterized by the type of
/// tokens in the stream, the type of the value parses 
/// and the user-defined error type it can return.
pub opaque type GeneralParser(t, a, e) {
  Parser(
    parse: fn(List(t), Position) ->
      Result(#(a, List(t), Position), GeneralParseError(t, e)),
  )
}

/// The parser type, parameterized by the type  of the value 
/// it parses and the user-defined error type it can return.
pub type Parser(a, e) =
  GeneralParser(String, a, e)

/// Apply a parser to a list of graphemes 
/// (holding on to extra result info that is hidden from the library user).
fn run(
  p: GeneralParser(t, a, e),
  src: List(t),
  pos: Position,
) -> Result(#(a, List(t), Position), GeneralParseError(t, e)) {
  case p {
    Parser(f) -> f(src, pos)
  }
}

/// Apply a parser to a stream of tokens, returning the parser's result or failure.
/// Only use this function if you're parsing over a custom list of tokens;
/// if you're parsing over a string, use `go` instead.
pub fn go_on_tokens(
  p: GeneralParser(t, a, e),
  src: List(t),
) -> Result(a, GeneralParseError(t, e)) {
  case run(p, src, Position(1, 1)) {
    Ok(#(x, _, _)) -> Ok(x)
    Error(e) -> Error(e)
  }
}

/// Apply a parser to a string, returning the parser's result or failure.
pub fn go(p: Parser(a, e), src: String) -> Result(a, ParseError(e)) {
  go_on_tokens(p, string.to_graphemes(src))
}

/// Get the current parser position.
/// Useful for reporting errors from later stages in 
/// the project, like static analysis.
pub fn pos() -> GeneralParser(t, Position, e) {
  Parser(fn(source, p) { Ok(#(p, source, p)) })
}

/// Parse a token if it matches the predicate.
/// Only use this function if you're parsing over a custom list of tokens;
/// if you're parsing over a string, use `satisfy` instead.
pub fn tokens_satisfy(
  pred: fn(t) -> Bool,
  newline_pred: fn(t) -> Bool,
  eof_token: t,
) -> GeneralParser(t, t, e) {
  Parser(fn(source, pos) {
    let assert Position(row, col) = pos
    case source {
      [h, ..t] ->
        case pred(h) {
          True ->
            case newline_pred(h) {
              True -> Ok(#(h, t, Position(row + 1, 0)))
              False -> Ok(#(h, t, Position(row, col + 1)))
            }
          False -> Error(Unexpected(pos, h))
        }
      [] -> Error(Unexpected(pos, eof_token))
    }
  })
}

/// Parse a character if it matches the predicate.
pub fn satisfy(when pred: fn(String) -> Bool) -> Parser(String, e) {
  tokens_satisfy(pred, fn(c) { c == "\n" }, "EOF")
}

/// Parse a lowercase letter.
pub fn lowercase_letter() -> Parser(String, e) {
  satisfy(when: fn(c) { string.contains("abcdefghijklmnopqrstuvwxyz", c) })
}

/// Parse an uppercase letter.
pub fn uppercase_letter() -> Parser(String, e) {
  satisfy(when: fn(c) { string.contains("ABCDEFGHIJKLMNOPQRSTUVWXYZ", c) })
}

/// Parse a lowercase or uppercase letter.
pub fn letter() -> Parser(String, e) {
  either(lowercase_letter(), uppercase_letter())
}

/// Parse a specific character.
pub fn char(c) -> Parser(String, e) {
  satisfy(when: fn(c2) { c == c2 })
}

/// Parse a digit.
pub fn digit() -> Parser(String, e) {
  satisfy(fn(c) { string.contains("0123456789", c) })
}

/// Parse a sequence of digits.
pub fn digits() -> Parser(String, e) {
  many1_concat(digit())
}

/// Parse the first parser, or the second if the first fails.
pub fn either(
  p: GeneralParser(t, a, e),
  q: GeneralParser(t, a, e),
) -> GeneralParser(t, a, e) {
  Parser(fn(source, pos) { result.or(run(p, source, pos), run(q, source, pos)) })
}

/// Parse with the first parser in the list that doesn't fail.
pub fn choice(ps: List(GeneralParser(t, a, e))) -> GeneralParser(t, a, e) {
  Parser(fn(source, pos) {
    case ps {
      [] -> panic as "choice doesn't accept an empty list of parsers"
      [p] -> run(p, source, pos)
      [p, ..t] ->
        case run(p, source, pos) {
          Ok(#(x, r, pos2)) -> Ok(#(x, r, pos2))
          Error(_) -> run(choice(t), source, pos)
        }
    }
  })
}

/// Parse an alphanumeric character.
pub fn alphanum() -> Parser(String, e) {
  either(digit(), letter())
}

/// Parse zero or more whitespace characters.
pub fn whitespace() -> Parser(String, e) {
  many_concat(choice([char(" "), char("\t"), char("\n")]))
}

/// Parse one or more whitespace characters.
pub fn whitespace1() -> Parser(String, e) {
  many1_concat(choice([char(" "), char("\t"), char("\n")]))
}

/// Keep trying the parser until it fails, and return the array of parsed results.
/// This cannot fail because it parses zero or more times!
pub fn many(p: GeneralParser(t, a, e)) -> GeneralParser(t, List(a), e) {
  Parser(fn(source, pos) {
    case run(p, source, pos) {
      Error(_) -> Ok(#([], source, pos))
      Ok(#(x, r, pos2)) ->
        result.map(run(many(p), r, pos2), fn(res) {
          #([x, ..res.0], res.1, res.2)
        })
    }
  })
}

/// Parse a certain string as many times as possible, returning everything that was parsed.
/// This cannot fail because it parses zero or more times!
pub fn many_concat(
  p: GeneralParser(t, String, e),
) -> GeneralParser(t, String, e) {
  many(p)
  |> map(string.concat)
}

/// Keep trying the parser until it fails, and return the array of parsed results.
/// This can fail, because it must parse successfully at least once!
pub fn many1(p: GeneralParser(t, a, e)) -> GeneralParser(t, List(a), e) {
  Parser(fn(source, pos) {
    case run(p, source, pos) {
      Error(e) -> Error(e)
      Ok(#(x, r, pos2)) ->
        result.map(run(many(p), r, pos2), fn(res) {
          #([x, ..res.0], res.1, res.2)
        })
    }
  })
}

/// Parse a certain string as many times as possible, returning everything that was parsed.
/// This can fail, because it must parse successfully at least once!
pub fn many1_concat(
  p: GeneralParser(t, String, e),
) -> GeneralParser(t, String, e) {
  many1(p)
  |> map(string.concat)
}

/// Do the first parser, ignore its result, then do the second parser.
pub fn seq(
  p: GeneralParser(t, a, e),
  q: GeneralParser(t, b, e),
) -> GeneralParser(t, b, e) {
  use _ <- do(p)
  q
}

/// Parse a sequence separated by the given separator parser.
pub fn sep(
  parser: GeneralParser(t, a, e),
  by s: GeneralParser(t, b, e),
) -> GeneralParser(t, List(a), e) {
  use res <- do(perhaps(sep1(parser, by: s)))
  case res {
    Ok(sequence) -> return(sequence)
    Error(Nil) -> return([])
  }
}

/// Parse a sequence separated by the given separator parser.
/// This only succeeds if at least one element of the sequence was parsed.
pub fn sep1(
  parser: GeneralParser(t, a, e),
  by s: GeneralParser(t, b, e),
) -> GeneralParser(t, List(a), e) {
  use first <- do(parser)
  use rest <- do(many(seq(s, parser)))
  return([first, ..rest])
}

/// Do `p`, then apply `f` to the result if it succeeded.
pub fn map(p: GeneralParser(t, a, e), f: fn(a) -> b) -> GeneralParser(t, b, e) {
  Parser(fn(source, pos) {
    case run(p, source, pos) {
      Ok(#(x, r, pos2)) -> Ok(#(f(x), r, pos2))
      Error(e) -> Error(e)
    }
  })
}

/// Do `p`, the apply `f` to the result if it succeeded.
/// `f` itself can fail with the user-defined error type, 
/// and if it does the result is a `UserError` with the error.
pub fn try(
  p: GeneralParser(t, a, e),
  f: fn(a) -> Result(b, e),
) -> GeneralParser(t, b, e) {
  Parser(fn(source, pos) {
    case run(p, source, pos) {
      Ok(#(x, r, pos2)) ->
        case f(x) {
          Ok(a) -> Ok(#(a, r, pos2))
          Error(e) -> Error(UserError(pos2, e))
        }
      Error(e) -> Error(e)
    }
  })
}

/// Transform the user-defined error type 
/// with a user-provided conversion function.
pub fn error_map(
  p: GeneralParser(t, a, e),
  f: fn(e) -> f,
) -> GeneralParser(t, a, f) {
  Parser(fn(source, pos) {
    case run(p, source, pos) {
      Ok(res) -> Ok(res)
      Error(e) ->
        case e {
          UserError(pos, e) -> Error(UserError(pos, f(e)))
          Unexpected(pos, s) -> Error(Unexpected(pos, s))
        }
    }
  })
}

/// Try running a parser, but still succeed (with `Error(Nil)`) if it failed.
pub fn perhaps(p: GeneralParser(t, a, e)) -> GeneralParser(t, Result(a, Nil), e) {
  Parser(fn(source, pos) {
    case run(p, source, pos) {
      Ok(#(x, r, pos2)) -> Ok(#(Ok(x), r, pos2))
      Error(_) -> Ok(#(Error(Nil), source, pos))
    }
  })
}

/// Do each parser in the list, returning the result of the last parser.
pub fn all(ps: List(GeneralParser(t, a, e))) -> GeneralParser(t, a, e) {
  case ps {
    [p] -> p
    [h, ..t] -> {
      use _ <- do(h)
      all(t)
    }
    _ -> panic as "all(parsers) doesn't accept an empty list of parsers"
  }
  // TODO: should this be an Unexpected instead?
}

/// Parse an exact string of characters.
pub fn string(s: String) -> Parser(String, e) {
  case string.pop_grapheme(s) {
    Ok(#(h, t)) -> {
      use c <- do(char(h))
      use rest <- do(string(t))
      return(c <> rest)
    }
    Error(_) -> return("")
  }
}

/// Negate a parser: if it succeeds, this fails, and vice versa.
/// Currently, the unexpected token must be provided so there's 
/// something to return when the given parser succeeds.
/// Only use this function if you're parsing over a custom list of tokens;
/// if you're parsing over a string, use `not` instead.
pub fn generalized_not(
  p: GeneralParser(t, a, e),
  unexpected: t,
) -> GeneralParser(t, Nil, e) {
  Parser(fn(source, pos) {
    case run(p, source, pos) {
      Ok(_) -> Error(Unexpected(pos, unexpected))
      // todo: better error message here (add a label system)
      Error(_) -> Ok(#(Nil, source, pos))
    }
  })
}

/// Negate a parser: if it succeeds, this fails, and vice versa.
/// Example: `seq(string("if"), not(either(alphanum(), char("_"))))`
pub fn not(p: Parser(a, e)) -> Parser(Nil, e) {
  // TODO: add label system for better error messages here, 
  // or some other such nicer way of doing this
  generalized_not(p, "")
}

/// Parses successfully only when at the end of the input string.
pub fn end() -> GeneralParser(t, Nil, e) {
  Parser(fn(source, pos) {
    case source {
      [] -> Ok(#(Nil, source, pos))
      [h, ..] -> Error(Unexpected(pos, h))
    }
  })
}

/// Run a parser as normal, but the parser itself isn't evaluated until it is used.
/// This is needed for recursive grammars, such as `E := n | E + E` where `n` is a number.
/// Example: `lazy(digit)` instead of `digit()`.
pub fn lazy(p: fn() -> GeneralParser(t, a, e)) -> GeneralParser(t, a, e) {
  Parser(fn(source, pos) { run(p(), source, pos) })
}

/// A monadic bind for pleasant interplay with gleam's `use` syntax.
/// example: 
/// ```
/// fn identifier() -> Parser(String, e) {
///     use pos <- do(pos())
///     use first <- do(lowercase_letter())
///     use rest <- do(many(either(alphanum(), char("_"))))
///     return(Ident(pos, first <> string.concat(rest)))
/// }
/// ```
pub fn do(
  p: GeneralParser(t, a, e),
  f: fn(a) -> GeneralParser(t, b, e),
) -> GeneralParser(t, b, e) {
  Parser(fn(source, pos) {
    case run(p, source, pos) {
      Ok(#(x, r, pos2)) -> run(f(x), r, pos2)
      Error(e) -> Error(e)
    }
  })
}

/// A monadic return for pleasant interplay with gleam's `use` syntax.
/// see `do` for more details and an example.
/// This is redundant if the last `do` is a `map` instead.
/// But I prefer using it, stylistically.
pub fn return(x: a) -> GeneralParser(t, a, e) {
  Parser(fn(source, pos) { Ok(#(x, source, pos)) })
}

/// Immediately fail regardles of the next input.
/// Takes a token to return as the unexpected token if the token stream is empty.
/// Only use this function if you're parsing over a custom list of tokens;
/// if you're parsing over a string, use `generalized_fail` instead.
pub fn generalized_fail(eof_token: t) -> GeneralParser(t, a, e) {
  Parser(fn(source, pos) {
    case source {
      [] -> Error(Unexpected(pos, eof_token))
      [h, ..] -> Error(Unexpected(pos, h))
    }
  })
}

/// Immediately fail regardless of the next input.
pub fn fail() -> Parser(a, e) {
  generalized_fail("EOF")
}
