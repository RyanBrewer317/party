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
  let characters = string.to_graphemes("abcdefghijklmnopqrstuvwxyzABCDEGHIJKLMNOPQRSTUVWXYZ0123456789,.!@#$%^&*()\\|[]{};':\"<>/?`~")
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
  |> should.equal(Error(party.Unexpected(party.Position(1,1), "a")))
}

pub fn lowercase_letter_test() {
  party.go(party.lowercase_letter(), "a")
  |> should.equal(Ok("a"))
  party.go(party.lowercase_letter(), "A")
  |> should.equal(Error(party.Unexpected(party.Position(1,1), "A")))
}

pub fn uppercase_letter_test() {
  party.go(party.uppercase_letter(), "A")
  |> should.equal(Ok("A"))
  party.go(party.uppercase_letter(), "a")
  |> should.equal(Error(party.Unexpected(party.Position(1,1), "a")))
}

pub fn letter_test() {
  party.go(party.letter(), "a")
  |> should.equal(Ok("a"))
  party.go(party.letter(), "A")
  |> should.equal(Ok("A"))
  party.go(party.letter(), "1")
  |> should.equal(Error(party.Unexpected(party.Position(1,1), "1")))
}

pub fn char_test() {
  let characters = string.to_graphemes("abcdefghijklmnopqrstuvwxyzABCDEGHIJKLMNOPQRSTUVWXYZ0123456789,.!@#$%^&*()\\|[]{};':\"<>/?`~")
  list.each(characters, fn(s) {
    party.go(party.char(s), s)
    |> should.equal(Ok(s))
  })
  party.go(party.char("x"), "y")
  |> should.equal(Error(party.Unexpected(party.Position(1,1), "y")))
}

pub fn digit_test() {
  party.go(party.try(party.digit(), int.parse), "1")
  |> should.equal(Ok(1))
  party.go(party.digit(), "2")
  |> should.equal(Ok("2"))
  party.go(party.digit(), "a")
  |> should.equal(Error(party.Unexpected(party.Position(1,1), "a")))
}

pub fn digits_test() {
  party.go(party.digits(), "123")
  |> should.equal(Ok("123"))
  party.go(party.digits(), "a123")
  |> should.equal(Error(party.Unexpected(party.Position(1,1), "a")))
}

pub fn either_test() {
  party.go(party.either(party.char("a"), party.char("b")), "a")
  |> should.equal(Ok("a"))
  party.go(party.either(party.char("a"), party.char("b")), "b")
  |> should.equal(Ok("b"))
  party.go(party.either(party.char("x"), party.char("y")), "z")
  |> should.equal(Error(party.Unexpected(party.Position(1,1), "z")))
}

pub fn choice_test() {
  party.go(party.choice([party.char("a"), party.char("b")]), "a")
  |> should.equal(Ok("a"))
  party.go(party.choice([party.char("a"), party.char("b")]), "b")
  |> should.equal(Ok("b"))
  party.go(party.choice([party.char("x"), party.char("y")]), "z")
  |> should.equal(Error(party.Unexpected(party.Position(1,1), "z")))
}

pub fn alphanum_test() {
  party.go(party.alphanum(), "a")
  |> should.equal(Ok("a"))
  party.go(party.alphanum(), "1")
  |> should.equal(Ok("1"))
  party.go(party.alphanum(), "#")
  |> should.equal(Error(party.Unexpected(party.Position(1,1), "#")))
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
  |> should.equal(Error(party.Unexpected(party.Position(1,1), "a")))
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
  |> should.equal(Error(party.Unexpected(party.Position(1,1), "b")))
}

pub fn try_test() {
  party.go(party.try(party.digit(), int.parse), "1")
  |> should.equal(Ok(1))
  party.go(party.try(party.digit(), int.parse), "a")
  |> should.equal(Error(party.Unexpected(party.Position(1,1), "a")))
  party.go(party.try(party.digit(), fn(_s) { Error("hi") }), "1")
  |> should.equal(Error(party.UserError(party.Position(1, 1), "hi")))
}