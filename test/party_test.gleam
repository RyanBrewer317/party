// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

import gleeunit
import gleeunit/should
import party
import gleam/int
import gleam/list
import gleam/string

pub fn main() {
  gleeunit.main()
}

pub fn pos_test() {
  party.go(
    {
      use _ <- party.do(party.many(party.digit()))
      party.pos()
    },
    "117e",
  )
  |> should.equal(Ok(party.Position(1, 4)))
}

pub fn demorgan_test() {
  let characters =
    string.to_graphemes(
      "abcdefghijklmnopqrstuvwxyzABCDEGHIJKLMNOPQRSTUVWXYZ0123456789,.!@#$%^&*()\\|[]{};':\"<>/?`~",
    )
  list.each(characters, fn(s) {
    party.go(party.seq(party.not(party.digit()), party.not(party.letter())), s)
    |> should.equal(party.go(
      party.not(party.either(party.digit(), party.letter())),
      s,
    ))
  })
}

pub fn satisfy_test() {
  party.go(party.satisfy(fn(c) { c == "a" }), "a")
  |> should.equal(Ok("a"))
  party.go(party.satisfy(fn(c) { c == "b" }), "a")
  |> should.equal(Error(party.Unexpected(party.Position(1, 1), "a")))
}

pub fn lowercase_letter_test() {
  party.go(party.lowercase_letter(), "a")
  |> should.equal(Ok("a"))
  party.go(party.lowercase_letter(), "A")
  |> should.equal(Error(party.Unexpected(party.Position(1, 1), "A")))
}

pub fn uppercase_letter_test() {
  party.go(party.uppercase_letter(), "A")
  |> should.equal(Ok("A"))
  party.go(party.uppercase_letter(), "a")
  |> should.equal(Error(party.Unexpected(party.Position(1, 1), "a")))
}

pub fn letter_test() {
  party.go(party.letter(), "a")
  |> should.equal(Ok("a"))
  party.go(party.letter(), "A")
  |> should.equal(Ok("A"))
  party.go(party.letter(), "1")
  |> should.equal(Error(party.Unexpected(party.Position(1, 1), "1")))
}

pub fn char_test() {
  let characters =
    string.to_graphemes(
      "abcdefghijklmnopqrstuvwxyzABCDEGHIJKLMNOPQRSTUVWXYZ0123456789,.!@#$%^&*()\\|[]{};':\"<>/?`~",
    )
  list.each(characters, fn(s) {
    party.go(party.char(s), s)
    |> should.equal(Ok(s))
  })
  party.go(party.char("x"), "y")
  |> should.equal(Error(party.Unexpected(party.Position(1, 1), "y")))
}

pub fn digit_test() {
  party.go(party.try(party.digit(), int.parse), "1")
  |> should.equal(Ok(1))
  party.go(party.digit(), "2")
  |> should.equal(Ok("2"))
  party.go(party.digit(), "a")
  |> should.equal(Error(party.Unexpected(party.Position(1, 1), "a")))
}

pub fn digits_test() {
  party.go(party.digits(), "123")
  |> should.equal(Ok("123"))
  party.go(party.digits(), "a123")
  |> should.equal(Error(party.Unexpected(party.Position(1, 1), "a")))
}

pub fn either_test() {
  party.go(party.either(party.char("a"), party.char("b")), "a")
  |> should.equal(Ok("a"))
  party.go(party.either(party.char("a"), party.char("b")), "b")
  |> should.equal(Ok("b"))
  party.go(party.either(party.char("x"), party.char("y")), "z")
  |> should.equal(Error(party.Unexpected(party.Position(1, 1), "z")))
}

pub fn choice_test() {
  party.go(party.choice([party.char("a"), party.char("b")]), "a")
  |> should.equal(Ok("a"))
  party.go(party.choice([party.char("a"), party.char("b")]), "b")
  |> should.equal(Ok("b"))
  party.go(party.choice([party.char("x"), party.char("y")]), "z")
  |> should.equal(Error(party.Unexpected(party.Position(1, 1), "z")))
}

pub fn alphanum_test() {
  party.go(party.alphanum(), "a")
  |> should.equal(Ok("a"))
  party.go(party.alphanum(), "1")
  |> should.equal(Ok("1"))
  party.go(party.alphanum(), "#")
  |> should.equal(Error(party.Unexpected(party.Position(1, 1), "#")))
}

pub fn whitespace_test() {
  party.go(party.whitespace(), " \t\n")
  |> should.equal(Ok(" \t\n"))
  party.go(party.whitespace(), "a")
  |> should.equal(Ok(""))
}

pub fn whitespace1_test() {
  party.go(party.whitespace1(), " \t\n")
  |> should.equal(Ok(" \t\n"))
  party.go(party.whitespace1(), "a")
  |> should.equal(Error(party.Unexpected(party.Position(1, 1), "a")))
}

pub fn many_test() {
  party.go(party.many(party.char("a")), "aaab")
  |> should.equal(Ok(["a", "a", "a"]))
  party.go(party.many(party.char("b")), "aaab")
  |> should.equal(Ok([]))
}

pub fn many_concat_test() {
  party.go(party.many_concat(party.char("a")), "aaab")
  |> should.equal(Ok("aaa"))
  party.go(party.many_concat(party.char("b")), "aaab")
  |> should.equal(Ok(""))
}

pub fn many1_test() {
  party.go(party.many1(party.char("a")), "aaab")
  |> should.equal(Ok(["a", "a", "a"]))
  party.go(party.many1(party.char("b")), "aaab")
  |> should.equal(Error(party.Unexpected(party.Position(1, 1), "a")))
}

pub fn many1_concat_test() {
  party.go(party.many1_concat(party.char("a")), "aaab")
  |> should.equal(Ok("aaa"))
  party.go(party.many1(party.char("b")), "aaab")
  |> should.equal(Error(party.Unexpected(party.Position(1, 1), "a")))
}

pub fn seq_test() {
  party.go(party.seq(party.char("a"), party.char("b")), "ab")
  |> should.equal(Ok("b"))
  party.go(party.seq(party.char("a"), party.char("b")), "aa")
  |> should.equal(Error(party.Unexpected(party.Position(1, 2), "a")))
  party.go(party.seq(party.char("a"), party.char("b")), "b")
  |> should.equal(Error(party.Unexpected(party.Position(1, 1), "b")))
}

pub fn sep_test() {
  party.go(party.sep(party.char("a"), by: party.char(",")), "a,a")
  |> should.equal(Ok(["a", "a"]))
  party.go(party.sep(party.char("a"), by: party.char(",")), "aa")
  |> should.equal(Ok(["a"]))
  party.go(party.sep(party.char("a"), by: party.char(",")), "")
  |> should.equal(Ok([]))
}

pub fn sep1_test() {
  party.go(party.sep1(party.char("a"), by: party.char(",")), "a,a")
  |> should.equal(Ok(["a", "a"]))
  party.go(party.sep1(party.char("a"), by: party.char(",")), "aa")
  |> should.equal(Ok(["a"]))
  party.go(party.sep1(party.char("a"), by: party.char(",")), "")
  |> should.equal(Error(party.Unexpected(party.Position(1, 1), "EOF")))
}

pub fn map_test() {
  party.go(party.map(party.char("a"), fn(c) { Ok(c) }), "a")
  |> should.equal(Ok(Ok("a")))
  party.go(party.map(party.char("a"), fn(c) { Ok(c) }), "b")
  |> should.equal(Error(party.Unexpected(party.Position(1, 1), "b")))
}

pub fn try_test() {
  party.go(party.try(party.digit(), int.parse), "1")
  |> should.equal(Ok(1))
  party.go(party.try(party.digit(), int.parse), "a")
  |> should.equal(Error(party.Unexpected(party.Position(1, 1), "a")))
  party.go(party.try(party.digit(), fn(_s) { Error("hi") }), "1")
  |> should.equal(Error(party.UserError(party.Position(1, 1), "hi")))
}

pub fn error_map_test() {
  party.go(party.error_map(party.digit(), fn(_e) { "hi" }), "1")
  |> should.equal(Ok("1"))
  party.go(party.error_map(party.digit(), fn(_e) { "hi" }), "a")
  |> should.equal(Error(party.Unexpected(party.Position(1, 1), "a")))
  party.go(
    party.error_map(party.try(party.digit(), fn(_s) { Error("hi") }), fn(_e) {
      "hello"
    }),
    "1",
  )
  |> should.equal(Error(party.UserError(party.Position(1, 1), "hello")))
}

pub fn perhaps_test() {
  party.go(party.perhaps(party.digit()), "1")
  |> should.equal(Ok(Ok("1")))
  party.go(party.perhaps(party.digit()), "a")
  |> should.equal(Ok(Error(Nil)))
}

pub fn all_test() {
  party.go(party.all([party.digit(), party.char("a"), party.alphanum()]), "1a2")
  |> should.equal(Ok("2"))
  party.go(party.all([party.digit(), party.char("a"), party.alphanum()]), "1x2")
  |> should.equal(Error(party.Unexpected(party.Position(1, 2), "x")))
}

pub fn string_test() {
  party.go(party.string("hello"), "hello")
  |> should.equal(Ok("hello"))
  party.go(party.string("hello"), "world")
  |> should.equal(Error(party.Unexpected(party.Position(1, 1), "w")))
  party.go(party.string("hello"), "hi")
  |> should.equal(Error(party.Unexpected(party.Position(1, 2), "i")))
}

pub fn not_test() {
  party.go(party.not(party.digit()), "a")
  |> should.equal(Ok(Nil))
  party.go(party.not(party.digit()), "1")
  |> should.equal(Error(party.Unexpected(party.Position(1, 1), "1")))
}

pub fn end_test() {
  party.go(party.end(), "")
  |> should.equal(Ok(Nil))
  party.go(party.end(), " ")
  |> should.equal(Error(party.Unexpected(party.Position(1, 1), " ")))
}

pub fn lazy_test() {
  party.go(party.lazy(fn() { party.char("a") }), "a")
  |> should.equal(Ok("a"))
}

pub fn do_return_test() {
  {
    use a <- party.do(party.char("a"))
    party.return(a <> a)
  }
  |> party.go("a")
  |> should.equal(Ok("aa"))
}

pub fn fail_test() {
  party.go(party.fail(), "a")
  |> should.equal(Error(party.Unexpected(party.Position(1, 1), "a")))
}

pub fn until_test() {
  party.go(party.until(do: party.char("a"), until: party.char("b")), "aab")
  |> should.equal(Ok(["a", "a"]))
  party.go(party.until(do: party.char("a"), until: party.char("b")), "aaa")
  |> should.equal(Error(party.Unexpected(party.Position(1, 4), "EOF")))
  party.go(party.until(do: party.char("a"), until: party.char("b")), "ac")
  |> should.equal(Error(party.Unexpected(party.Position(1, 2), "c")))
}