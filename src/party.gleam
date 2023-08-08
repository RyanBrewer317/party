import gleam/string
import gleam/result

//// Party: A simple parser combinator library.
//// Party is pre-alpha and breaking changes should be expected in the near future.

/// The custom error type for the parser, 
/// which can itself be parameterized by a user-defined error type.
/// The user-defined error type is useful for, for example, 
/// adding a `int.parse` call into your parser pipeline.
/// See `try` for using this feature.
pub type ParseError(e) {
    Unexpected(error: String)
    UserError(error: e)
}

/// The type for positions within a string.
pub type Position {
    Position(row: Int, col: Int)
}

/// The parser type, parameterized by the type it parses and 
/// the user-defined error type it can return.
pub opaque type Parser(a, e) {
    Parser(parse: fn(String, Position) -> Result(#(a, String, Position), ParseError(e)))
}

/// Apply a parser to a string (holding on to extra result info that is hidden from the library user).
fn run(p: Parser(a, e), src: String, pos: Position) -> Result(#(a, String, Position), ParseError(e)) {
    case p {
        Parser(f) -> f(src, pos)
    }
}

/// Apply a parser to a string.
pub fn go(p: Parser(a, e), src: String) -> Result(a, ParseError(e)) {
    case run(p, src, Position(1, 1)) {
        Ok(#(x, _, _)) -> Ok(x)
        Error(e) -> Error(e)
    }
}

/// Get the current parser position.
pub fn pos() -> Parser(Position, e) {
    Parser(fn(source, p) {
        Ok(#(p, source, p))
    })
}

/// Parse a character if it matches the predicate.
pub fn satisfy(when pred: fn(String)->Bool) -> Parser(String, e) {
    Parser(fn(source, pos) {
        let assert Position(row, col) = pos
        case string.pop_grapheme(source) {
            Ok(#(h, t)) -> 
                case pred(h) {
                    True -> 
                        case h {
                            "\n" -> Ok(#(h, t, Position(row+1, col)))
                            _ -> Ok(#(h, t, Position(row, col+1)))
                        }
                    False -> Error(Unexpected(h))
                }
            Error(Nil) -> Error(Unexpected("EOF"))
        }
    })
}

/// Parse a lowercase letter.
pub fn lowercase_letter() -> Parser(String, e) {
    satisfy(when: fn(c) {
        string.contains("abcdefghijklmnopqrstuvwxyz", c)
    })
}

/// Parse an uppercase letter.
pub fn uppercase_letter() -> Parser(String, e) {
    satisfy(when: fn(c) {
        string.contains("ABCDEFGHIJKLMNOPQRSTUVWXYZ", c)
    })
}

/// Parse a lowercase or uppercase letter.
pub fn letter() -> Parser(String, e) {
    alt(lowercase_letter(), uppercase_letter())
}

/// Parse a specific character.
pub fn char(c) -> Parser(String, e) {
    satisfy(when: fn(c2) {c == c2})
}

/// Parse a digit.
pub fn digit() -> Parser(String, e) {
    satisfy(fn(c) {
        string.contains("0123456789", c)
    })
}

/// Parse the first parser, or the second if the first fails.
pub fn alt(p: Parser(a, e), q: Parser(a, e)) -> Parser(a, e) {
    Parser(fn(source, pos) {
        result.or(run(p, source, pos), run(q, source, pos))
    })
}

/// Parse with the first parser in the list that doesn't fail.
pub fn choice(ps: List(Parser(a, e))) -> Parser(a, e) {
    Parser(fn(source, pos) {
        case ps {
            [] -> Error(Unexpected("choiceless choice"))
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
    alt(digit(), letter())
}

/// Keep trying the parser until it fails, and return the array of parsed results.
/// This cannot fail because it parses zero or more times!
pub fn many(p: Parser(a, e)) -> Parser(List(a), e) {
    Parser(fn(source, pos) {
        case run(p, source, pos) {
            Error(_) -> Ok(#([], source, pos))
            Ok(#(x, r, pos2)) -> result.map(run(many(p), r, pos2), fn(res) {#([x, ..res.0], res.1, res.2)})
        }
    })
}

/// Keep trying the parser until it fails, and return the array of parsed results.
/// This can fail, because it must parse successfully at least once!
pub fn many1(p: Parser(a, e)) -> Parser(List(a), e) {
    Parser(fn(source, pos) {
        case run(p, source, pos) {
            Error(e) -> Error(e)
            Ok(#(x, r, pos2)) -> result.map(run(many(p), r, pos2), fn(res) {#([x, ..res.0], res.1, res.2)})
        }
    })
}

/// Do the first parser, ignore its result, then do the second parser.
pub fn seq(p: Parser(a, e), q: Parser(b, e)) -> Parser(b, e) {
    use _ <- do(p)
    q
}

/// Do `p`, then apply `f` to the result if it succeeded.
pub fn map(p: Parser(a, e), f: fn(a)->b) -> Parser(b, e) {
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
pub fn try(p: Parser(a, e), f: fn(a)->Result(b, e)) -> Parser(b, e) {
    Parser(fn (source, pos) {
        case run(p, source, pos) {
            Ok(#(x, r, pos2)) -> 
                case f(x) {
                    Ok(a) -> Ok(#(a, r, pos2))
                    Error(e) -> Error(UserError(e))
                }
            Error(e) -> Error(e)
        }
    })
}

/// Transform the user-defined error type 
/// with a user-provided conversion function.
pub fn error_map(p: Parser(a, e), f: fn(e)->f) -> Parser(a, f) {
    Parser(fn(source, pos) {
        case run(p, source, pos) {
            Ok(res) -> Ok(res)
            Error(e) -> 
                case e {
                    UserError(e) -> Error(UserError(f(e)))
                    Unexpected(s) -> Error(Unexpected(s))
                }
        }
    })
}

/// Try running a parser, but still succeed (with `Error(Nil)`) if it failed.
pub fn perhaps(p: Parser(a, e)) -> Parser(Result(a, Nil), e) {
    Parser(fn(source, pos) {
        case run(p, source, pos) {
            Ok(#(x, r, pos2)) -> Ok(#(Ok(x), r, pos2))
            Error(_) -> Ok(#(Error(Nil), source, pos))
        }
    })
}

/// Do each parser in the list, returning the result of the last parser.
pub fn all(ps: List(Parser(a, e))) -> Parser(a, e) {
    case ps {
        [p] -> p
        [h, ..t] -> {
            use _ <- do(h)
            all(t)
        }
    }
}

/// Parse an exact string of characters.
pub fn string(s: String) -> Parser(String, e) {
    case string.pop_grapheme(s) {
        Ok(#(h, t)) -> {
            use c <- do(char(h))
            use rest <- do(string(t))
            return(c<>rest)
        }
        Error(_) -> return("")
    }
}

/// Negate a parser: if it succeeds, this fails, and vice versa.
/// Example: `seq(string("if"), not(alt(alphanum(), char("_"))))`
pub fn not(p: Parser(a, e)) -> Parser(Nil, e) {
    Parser(fn(source, pos) {
        case run(p, source, pos) {
            Ok(_) -> Error(Unexpected("")) // todo: better error message here (add a label system)
            Error(_) -> Ok(#(Nil, source, pos))
        }
    })
}

/// Parses successfully only when at the end of the input string.
pub fn end() -> Parser(Nil, e) {
    Parser(fn(source, pos) {
        case source {
            "" -> Ok(#(Nil, source, pos))
            _ -> Error(Unexpected(source))
        }
    })
}

/// Run a parser as normal, but the parser itself isn't evaluated until it is used.
/// This is needed for recursive grammars, such as `E := n | E + E` where `n` is a number.
/// Example: `lazy(digit)` instead of `digit()`.
pub fn lazy(p: fn() -> Parser(a, e)) -> Parser(a, e) {
    Parser(fn(source, pos) {
        run(p(), source, pos)
    })
}

/// A monadic bind for pleasant interplay with gleam's `use` syntax.
/// example: 
/// ```
/// fn identifier() -> Parser(String, e) {
///     use pos <- do(pos())
///     use first <- do(lowercase_letter())
///     use rest <- do(many(alt(alphanum(), char("_"))))
///     return(Ident(pos, first <> string.concat(rest)))
/// }
/// ```
pub fn do(p: Parser(a, e), f: fn(a)->Parser(b, e)) -> Parser(b, e) {
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
pub fn return(x) {
    Parser(fn(source, pos) {Ok(#(x, source, pos))})
}

