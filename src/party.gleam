import gleam/string
import gleam/result

//// Party: A simple parser combinator library
//// Party is pre-alpha and breaking changes should be expected in the near future.

/// The custom error type for the parser, 
/// which can itself be parameterized by a user-defined error type
/// The user-defined error type is useful for, for example, 
/// adding a `int.parse` call into your parser pipeline
pub type ParseError(e) {
    Unexpected(error: String)
    UserError(error: e)
}

/// The parser type, parameterized by the type it parses and 
/// the user-defined error type it can return
pub opaque type Parser(a, e) {
    Parser(parse: fn(String, Int) -> Result(#(a, String, Int), ParseError(e)))
}

/// apply a parser to a string
fn run(p: Parser(a, e), src: String, pos: Int) -> Result(#(a, String, Int), ParseError(e)) {
    case p {
        Parser(f) -> f(src, pos)
    }
}

/// user-facing `run`
pub fn go(p: Parser(a, e), src: String) -> Result(a, ParseError(e)) {
    case run(p, src, 0) {
        Ok(#(x, _, _)) -> Ok(x)
        Error(e) -> Error(e)
    }
}

pub fn pos() -> Parser(Int, e) {
    Parser(fn(source, i) {
        Ok(#(i, source, i))
    })
}

/// parse a character if it matches the predicate
pub fn satisfy(when pred: fn(String)->Bool) -> Parser(String, e) {
    Parser(fn(source, i) {
        case string.pop_grapheme(source) {
            Ok(#(h, t)) -> 
                case pred(h) {
                    True -> Ok(#(h, t, i + 1))
                    False -> Error(Unexpected(h))
                }
            Error(Nil) -> Error(Unexpected("EOF"))
        }
    })
}

/// parse a lowercase letter
pub fn lowercase_letter() -> Parser(String, e) {
    satisfy(when: fn(c) {
        string.contains("abcdefghijklmnopqrstuvwxyz", c)
    })
}

/// parse an uppercase letter
pub fn uppercase_letter() -> Parser(String, e) {
    satisfy(when: fn(c) {
        string.contains("ABCDEFGHIJKLMNOPQRSTUVWXYZ", c)
    })
}

/// parse a lowercase or uppercase letter
pub fn letter() -> Parser(String, e) {
    alt(lowercase_letter(), uppercase_letter())
}

/// parse a specific character
pub fn char(c) -> Parser(String, e) {
    satisfy(when: fn(c2) {c == c2})
}

/// parse a digit
pub fn digit() -> Parser(String, e) {
    satisfy(fn(c) {
        string.contains("0123456789", c)
    })
}

/// parse the first parser, or the second if the first fails
pub fn alt(p: Parser(a, e), q: Parser(a, e)) -> Parser(a, e) {
    Parser(fn(source, i) {
        result.or(run(p, source, i), run(q, source, i))
    })
}

/// parse with the first parser in the list that doesn't fail
pub fn choice(ps: List(Parser(a, e))) -> Parser(a, e) {
    Parser(fn(source, i) {
        case ps {
            [] -> Error(Unexpected("choiceless choice"))
            [p] -> run(p, source, i)
            [p, ..t] -> 
                case run(p, source, i) {
                    Ok(#(x, r, i2)) -> Ok(#(x, r, i2))
                    Error(_) -> run(choice(t), source, i)
                }
        }
    })
}

/// parse an alphanumeric character
pub fn alphanum() -> Parser(String, e) {
    alt(digit(), letter())
}

/// keep trying the parser until it fails, and return the array of parsed results.
/// This cannot fail because it parses zero or more times!
pub fn many(p: Parser(a, e)) -> Parser(List(a), e) {
    Parser(fn(source, i) {
        case run(p, source, i) {
            Error(_) -> Ok(#([], source, i))
            Ok(#(x, r, i2)) -> result.map(run(many(p), r, i2), fn(res) {#([x, ..res.0], res.1, res.2)})
        }
    })
}

/// keep trying the parser until it fails, and return the array of parsed results.
/// This can fail, because it must parse successfully at least once!
pub fn many1(p: Parser(a, e)) -> Parser(List(a), e) {
    Parser(fn(source, i) {
        case run(p, source, i) {
            Error(e) -> Error(e)
            Ok(#(x, r, i2)) -> result.map(run(many(p), r, i2), fn(res) {#([x, ..res.0], res.1, res.2)})
        }
    })
}

/// do the first parser, ignore its result, then do the second parser
pub fn seq(p: Parser(a, e), q: Parser(b, e)) -> Parser(b, e) {
    use _ <- do(p)
    q
}

/// do `p`, then apply `f` to the result if it succeeded
pub fn map(p: Parser(a, e), f: fn(a)->b) -> Parser(b, e) {
    Parser(fn(source, i) {
        case run(p, source, i) {
            Ok(#(x, r, i2)) -> Ok(#(f(x), r, i2))
            Error(e) -> Error(e)
        }
    })
}

/// do `p`, the apply `f` to the result if it succeeded.
/// `f` itself can fail with the user-defined error type, 
/// and if it does the result is a `UserError` with the error.
pub fn try(p: Parser(a, e), f: fn(a)->Result(b, e)) -> Parser(b, e) {
    Parser(fn (source, i) {
        case run(p, source, i) {
            Ok(#(x, r, i2)) -> 
                case f(x) {
                    Ok(a) -> Ok(#(a, r, i2))
                    Error(e) -> Error(UserError(e))
                }
            Error(e) -> Error(e)
        }
    })
}

/// transform the user-defined error type 
/// with a user-provided conversion function
pub fn error_map(p: Parser(a, e), f: fn(e)->f) -> Parser(a, f) {
    Parser(fn(source, i) {
        case run(p, source, i) {
            Ok(res) -> Ok(res)
            Error(e) -> 
                case e {
                    UserError(e) -> Error(UserError(f(e)))
                    Unexpected(s) -> Error(Unexpected(s))
                }
        }
    })
}

/// try running a parser, but still succeed (with `Error(Nil)`) if it failed
pub fn perhaps(p: Parser(a, e)) -> Parser(Result(a, Nil), e) {
    Parser(fn(source, i) {
        case run(p, source, i) {
            Ok(#(x, r, i2)) -> Ok(#(Ok(x), r, i2))
            Error(_) -> Ok(#(Error(Nil), source, i))
        }
    })
}

/// run a parser as normal, but the parser itself isn't evaluated until it is used.
/// This is needed for recursive grammars, such as `E := n | E + E` where `n` is a number.
/// example: `lazy(digit)` instead of `digit()`
pub fn lazy(p: fn() -> Parser(a, e)) -> Parser(a, e) {
    Parser(fn(source, i) {
        run(p(), source, i)
    })
}

/// a monadic bind for pleasant interplay with gleam's `use` syntax.
/// example: 
/// ```
/// fn identifier() -> Parser(String, e) {
///     use first <- do(lowercase_letter())
///     use rest <- do(many(alt(alphanum(), char("_"))))
///     return(first <> string.concat(rest))
/// }
/// ```
pub fn do(p: Parser(a, e), f: fn(a)->Parser(b, e)) -> Parser(b, e) {
    Parser(fn(source, i) {
        case run(p, source, i) {
            Ok(#(x, r, i2)) -> run(f(x), r, i2)
            Error(e) -> Error(e)
        }
    })
}

/// a monadic return for pleasant interplay with gleam's `use` syntax.
/// see `do` for more details and an example.
/// This is redundant if the last `do` is a `map` instead.
/// But I prefer using it, stylistically.
pub fn return(x) {
    Parser(fn(s, i) {Ok(#(x, s, i))})
}

