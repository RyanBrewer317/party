import gleeunit
import gleeunit/should
import party
import gleam/int

pub fn main() {
    gleeunit.main()
}

// TODO: add more tests

pub fn digit_test() {
    party.go(party.try(party.digit(), int.parse), "1")
    |> should.equal(Ok(1))
}

pub fn pos_test() {
    party.go({
        use _ <- party.do(party.many(party.digit()))
        party.pos()
    }, "117e")
    |> should.equal(Ok(3))
}
