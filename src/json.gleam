// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//// A simple JSON parser written in Gleam, 
//// using the Party parser combinator library 
//// and fully usable as a combinator itself.
//// It's not the fastest option.

import gleam/string
import gleam/int
import gleam/float
import gleam/dict.{type Dict}
import party.{
  type Parser, char, choice, digit, digits, do, either, fail, lazy, many,
  many_concat, perhaps, return, satisfy, sep, string,
}

/// The datatype of a JSON value, and what the parser returns.
pub type JSONValue {
  JSONObject(Dict(String, JSONValue))
  JSONArray(List(JSONValue))
  JSONString(String)
  JSONNumber(Float)
  JSONBool(Bool)
  JSONNull
}

fn json_ws() -> Parser(Nil, e) {
  use _ <- do(many(choice([char(" "), char("\t"), char("\n"), char("\r")])))
  return(Nil)
}

fn unicode() -> Parser(String, e) {
  use _ <- do(char("u"))
  let hex = satisfy(string.contains(does: "abcdefABCDEF0123456789", contain: _))
  use c1 <- do(hex)
  use c2 <- do(hex)
  use c3 <- do(hex)
  use c4 <- do(hex)
  let assert Ok(n) = int.base_parse(c1 <> c2 <> c3 <> c4, 16)
  let assert Ok(codepoint) = string.utf_codepoint(n)
  return(string.from_utf_codepoints([codepoint]))
}

fn character() -> Parser(String, e) {
  use mb_escape <- do(perhaps(char("\\")))
  case mb_escape {
    Ok(_) -> {
      let utf = fn(n: Int) -> String {
        let assert Ok(cp) = string.utf_codepoint(n)
        string.from_utf_codepoints([cp])
      }
      either(
        satisfy(string.contains(does: "\"\\/bfnrt", contain: _))
          |> party.map(fn(c) {
            case c {
              "b" -> utf(8)
              "f" -> utf(12)
              "n" -> utf(10)
              "r" -> utf(13)
              "t" -> utf(9)
              _ -> c
            }
          }),
        unicode(),
      )
    }
    Error(_) -> satisfy(fn(c) { c != "\"" })
  }
}

fn str() -> Parser(String, e) {
  use _ <- do(char("\""))
  use s <- do(many_concat(character()))
  use _ <- do(char("\""))
  return(s)
}

fn json_string() -> Parser(JSONValue, e) {
  str()
  |> party.map(JSONString)
}

fn json_number() -> Parser(JSONValue, e) {
  use mb_neg <- do(perhaps(char("-")))
  use first <- do(digit())
  use whole <- do(case first {
    "0" -> return(0)
    _ -> {
      use rest <- do(digits())
      let assert Ok(n) = int.parse(first <> rest)
      case mb_neg {
        Ok(_) -> return(-n)
        Error(_) -> return(n)
      }
    }
  })
  use mb_frac <- do(perhaps(char(".")))
  use x: Float <- do(case mb_frac {
    Ok(_) -> {
      use frac <- do(digits())
      let assert Ok(f) = int.parse(frac)
      let assert Ok(denominator) =
        int.power(10, int.to_float(string.length(frac)))
      return(int.to_float(whole) +. int.to_float(f) /. denominator)
    }
    Error(_) -> return(int.to_float(whole))
  })
  use mb_exp <- do(perhaps(choice([char("e"), char("E")])))
  case mb_exp {
    Ok(_) -> {
      use mb_sign <- do(perhaps(choice([char("-"), char("+")])))
      use exp_str <- do(digits())
      let assert Ok(absolute_exp) = int.parse(exp_str)
      let exp = case mb_sign {
        Ok("-") -> -absolute_exp
        _ -> absolute_exp
      }
      // reject imaginary numbers-- TODO: is this the right behavior?
      case float.power(x, int.to_float(exp)) {
        Ok(out) -> return(JSONNumber(out))
        Error(_) -> fail()
      }
    }
    Error(_) -> return(JSONNumber(x))
  }
}

fn json_true() -> Parser(JSONValue, e) {
  use _ <- do(string("true"))
  return(JSONBool(True))
}

fn json_false() -> Parser(JSONValue, e) {
  use _ <- do(string("false"))
  return(JSONBool(False))
}

fn json_null() -> Parser(JSONValue, e) {
  use _ <- do(string("null"))
  return(JSONNull)
}

fn json_member() -> Parser(#(String, JSONValue), e) {
  use _ <- do(json_ws())
  use key <- do(str())
  use _ <- do(json_ws())
  use _ <- do(char(":"))
  use val <- do(lazy(json_value))
  return(#(key, val))
}

fn json_object() -> Parser(JSONValue, e) {
  use _ <- do(char("{"))
  use members <- do(sep(json_member(), by: char(",")))
  use _ <- do(char("}"))
  return(JSONObject(dict.from_list(members)))
}

fn json_array() -> Parser(JSONValue, e) {
  use _ <- do(char("["))
  use values <- do(sep(lazy(json_value), by: char(",")))
  use _ <- do(char("]"))
  return(JSONArray(values))
}

/// Parses a JSON document
pub fn json_value() -> Parser(JSONValue, e) {
  use _ <- do(json_ws())
  use v <- do(
    choice([
      json_object(),
      json_array(),
      json_string(),
      json_number(),
      json_true(),
      json_false(),
      json_null(),
    ]),
  )
  use _ <- do(json_ws())
  return(v)
}
