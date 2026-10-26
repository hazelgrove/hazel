open Alcotest;
open Util;

let big = Bigint.of_string;
let s = Bigint.to_string;

/* The cases below are the 27 arithmetic oracle cases (fleet candidate
   arithmetic-oracle.json, expected values from Python's Fraction), copied in
   as data. Pair(n, d) is an arbitrary integer pair that goes through
   construction; Value(n, d) is an already-canonical value. */
type arg =
  | Pair(string, string)
  | Value(string, string);

type op =
  | Normalize
  | Add
  | Sub
  | Mul
  | Div
  | Inv
  | Equal
  | Compare;

type expected =
  | Is(string, string)
  | DivideByZero
  | Bool(bool)
  | Lt
  | Eq
  | Gt;

let oracle = [
  ("norm-reduce", Normalize, [Pair("6", "8")], Is("3", "4")),
  ("norm-neg-den", Normalize, [Pair("1", "-3")], Is("-1", "3")),
  ("norm-both-neg", Normalize, [Pair("-4", "-6")], Is("2", "3")),
  ("norm-zero-pos", Normalize, [Pair("0", "5")], Is("0", "1")),
  ("norm-zero-neg", Normalize, [Pair("0", "-7")], Is("0", "1")),
  ("norm-den-zero", Normalize, [Pair("1", "0")], DivideByZero),
  ("norm-zero-over-zero", Normalize, [Pair("0", "0")], DivideByZero),
  (
    "norm-big-exact",
    Normalize,
    [Pair("9007199254740993", "3")],
    Is("3002399751580331", "1"),
  ),
  (
    "add-third-sixth",
    Add,
    [Value("1", "3"), Value("1", "6")],
    Is("1", "2"),
  ),
  ("add-to-zero", Add, [Value("-5", "7"), Value("5", "7")], Is("0", "1")),
  (
    "sub-negative",
    Sub,
    [Value("1", "4"), Value("3", "4")],
    Is("-1", "2"),
  ),
  ("mul-cancel", Mul, [Value("-2", "3"), Value("9", "4")], Is("-3", "2")),
  ("mul-by-zero", Mul, [Value("0", "1"), Value("-7", "3")], Is("0", "1")),
  ("div-basic", Div, [Value("1", "1"), Value("1", "3")], Is("3", "1")),
  (
    "div-neg-divisor",
    Div,
    [Value("3", "5"), Value("-6", "7")],
    Is("-7", "10"),
  ),
  (
    "div-zero-dividend",
    Div,
    [Value("0", "1"), Value("4", "9")],
    Is("0", "1"),
  ),
  ("div-by-zero", Div, [Value("1", "3"), Value("0", "1")], DivideByZero),
  ("inv-negative", Inv, [Value("-2", "7")], Is("-7", "2")),
  ("inv-zero", Inv, [Value("0", "1")], DivideByZero),
  (
    "eq-equivalent-pairs",
    Equal,
    [Pair("2", "4"), Pair("-1", "-2")],
    Bool(true),
  ),
  (
    "eq-different",
    Equal,
    [Value("1", "3"), Value("333333333333333333", "1000000000000000000")],
    Bool(false),
  ),
  ("cmp-negatives", Compare, [Value("-1", "2"), Value("-1", "3")], Lt),
  ("cmp-equivalent", Compare, [Pair("10", "-4"), Value("-5", "2")], Eq),
  (
    "big-cmp-adjacent",
    Compare,
    [Value("9007199254740993", "1"), Value("9007199254740992", "1")],
    Gt,
  ),
  (
    "big-add-reciprocals",
    Add,
    [Value("1", "9007199254740993"), Value("1", "9007199254740991")],
    Is("18014398509481984", "81129638414606681695789005144063"),
  ),
  (
    "big-mul-cancel",
    Mul,
    [
      Value("1000000000000000000000000000000", "7"),
      Value("7", "1000000000000000000000000000001"),
    ],
    Is("1000000000000000000000000000000", "1000000000000000000000000000001"),
  ),
  (
    "big-div-to-one",
    Div,
    [
      Value("-1000000000000000000000000000000", "3"),
      Pair("1000000000000000000000000000000", "-3"),
    ],
    Is("1", "1"),
  ),
];

/* The invariant, checked with Zarith's gcd rather than anything in Rational. */
let check_canonical = (msg, r) => {
  let n = Rational.num(r)
  and d = Rational.den(r);
  check(bool, msg ++ ": den > 0", true, Bigint.(d > zero));
  check(
    string,
    msg ++ ": gcd(num, den) = 1",
    "1",
    Z.to_string(
      Z.gcd(Bigint.to_zarith_bigint(n), Bigint.to_zarith_bigint(d)),
    ),
  );
};

let parts = r => (s(Rational.num(r)), s(Rational.den(r)));

/* None models the contract-A DivideByZero outcome. */
let build = (id, a): option(Rational.t) =>
  switch (a) {
  | Pair(n, d) => Rational.make(big(n), big(d))
  | Value(n, d) =>
    switch (Rational.make(big(n), big(d))) {
    | Some(r) =>
      check(
        pair(string, string),
        id ++ ": oracle value is already canonical",
        (n, d),
        parts(r),
      );
      Some(r);
    | None => fail(id ++ ": oracle value has a zero denominator")
    }
  };

type outcome =
  | R(option(Rational.t))
  | B(bool)
  | C(int);

let run = (id, op, args) => {
  let args = List.map(build(id), args);
  switch (op, args) {
  | (_, [None, ..._]) => R(None)
  | (_, [_, None]) => R(None)
  | (Normalize, [a]) => R(a)
  | (Inv, [Some(a)]) => R(Rational.inv(a))
  | (Add, [Some(a), Some(b)]) => R(Some(Rational.add(a, b)))
  | (Sub, [Some(a), Some(b)]) => R(Some(Rational.sub(a, b)))
  | (Mul, [Some(a), Some(b)]) => R(Some(Rational.mul(a, b)))
  | (Div, [Some(a), Some(b)]) => R(Rational.div(a, b))
  | (Equal, [Some(a), Some(b)]) => B(Rational.equal(a, b))
  | (Compare, [Some(a), Some(b)]) => C(Rational.compare(a, b))
  | _ => fail(id ++ ": malformed oracle case")
  };
};

let sign = n => n < 0 ? (-1) : n > 0 ? 1 : 0;

let oracle_case = ((id, op, args, expected)) =>
  test_case(
    id,
    `Quick,
    () => {
      let got = run(id, op, args);
      switch (expected, got) {
      | (Is(n, d), R(Some(r))) =>
        check(pair(string, string), id, (n, d), parts(r));
        check_canonical(id, r);
      | (DivideByZero, R(None)) => ()
      | (Bool(b), B(got)) => check(bool, id, b, got)
      | (Lt, C(c)) => check(int, id, -1, sign(c))
      | (Eq, C(c)) => check(int, id, 0, sign(c))
      | (Gt, C(c)) => check(int, id, 1, sign(c))
      | (DivideByZero, R(Some(r))) =>
        fail(id ++ ": expected DivideByZero, got " ++ Rational.show(r))
      | (Is(_), R(None)) => fail(id ++ ": unexpected DivideByZero")
      | _ => fail(id ++ ": wrong kind of result")
      };
    },
  );

let r = (n, d) =>
  switch (Rational.make(big(n), big(d))) {
  | Some(r) => r
  | None => failwith("zero denominator in test value")
  };

/* Values for the law and round-trip tests: signs, zero, integers, and parts
   well beyond 2^53. */
let pool = [
  r("0", "1"),
  r("1", "1"),
  r("-1", "3"),
  r("22", "7"),
  r("-9007199254740993", "2"),
  r("1", "9007199254740991"),
  r("1000000000000000000000000000001", "-1000000000000000000000000000000"),
  r("123456789012345678901234567890", "1"),
];

let rational = testable(Rational.pp, Rational.equal);

let laws =
  test_case("arithmetic laws over the pool", `Quick, () =>
    List.iter(
      a =>
        List.iter(
          b => {
            let ab = Rational.show(a) ++ ", " ++ Rational.show(b);
            check(
              rational,
              "(a - b) + b = a: " ++ ab,
              a,
              Rational.add(Rational.sub(a, b), b),
            );
            check(
              rational,
              "a + b = b + a: " ++ ab,
              Rational.add(a, b),
              Rational.add(b, a),
            );
            check(
              bool,
              "equal iff compare = 0: " ++ ab,
              Rational.equal(a, b),
              Rational.compare(a, b) == 0,
            );
            check(
              int,
              "compare is antisymmetric: " ++ ab,
              sign(Rational.compare(a, b)),
              - sign(Rational.compare(b, a)),
            );
            check_canonical("a * b: " ++ ab, Rational.mul(a, b));
            switch (Rational.div(Rational.mul(a, b), b)) {
            | Some(q) => check(rational, "(a * b) / b = a: " ++ ab, a, q)
            | None =>
              check(
                bool,
                "only zero divisors fail: " ++ ab,
                true,
                Rational.equal(b, Rational.zero),
              )
            };
          },
          pool,
        ),
      pool,
    )
  );

let neg_and_inv =
  test_case("neg and inv", `Quick, () =>
    List.iter(
      a => {
        check(
          rational,
          "a + (-a) = 0",
          Rational.zero,
          Rational.add(a, Rational.neg(a)),
        );
        check_canonical("neg", Rational.neg(a));
        switch (Rational.inv(a)) {
        | Some(i) =>
          check(rational, "a * (1/a) = 1", Rational.one, Rational.mul(a, i));
          check(option(rational), "1/(1/a) = a", Some(a), Rational.inv(i));
        | None =>
          check(rational, "only zero has no inverse", Rational.zero, a)
        };
      },
      pool,
    )
  );

let to_float =
  test_case(
    "to_float",
    `Quick,
    () => {
      check(float(0.), "-7/2", -3.5, Rational.to_float(r("-7", "2")));
      check(float(0.), "0", 0., Rational.to_float(Rational.zero));
      check(float(1e-15), "1/3", 1. /. 3., Rational.to_float(r("1", "3")));
      let huge = "1" ++ String.make(400, '0');
      check(
        float(1e-15),
        "(10^400 + 1) / 10^400, whose parts overflow a float",
        1.,
        Rational.to_float(
          r(Bigint.to_string(Bigint.(big(huge) + one)), huge),
        ),
      );
      check(
        float(1e-15),
        "1 / 10^400 underflows to 0",
        0.,
        Rational.to_float(r("1", huge)),
      );
    },
  );

let roundtrip =
  test_case(
    "sexp and json round trips",
    `Quick,
    () => {
      List.iter(
        a => {
          check(
            rational,
            "json",
            a,
            Rational.(t_of_yojson(yojson_of_t(a))),
          );
          check(rational, "sexp", a, Rational.(t_of_sexp(sexp_of_t(a))));
        },
        pool,
      );
      /* Pins the provisional encoding. */
      check(
        string,
        "json form",
        {|["-1","3"]|},
        Yojson.Safe.to_string(Rational.yojson_of_t(r("1", "-3"))),
      );
      check(
        string,
        "sexp form",
        "(-1 3)",
        Sexplib.Sexp.to_string(Rational.sexp_of_t(r("1", "-3"))),
      );
      let n = "100000000000000000000000000000000000000001";
      let decoded =
        Rational.t_of_yojson(`List([`String(n), `String("3")]));
      check(
        pair(string, string),
        "large json decode is exact",
        (n, "3"),
        parts(decoded),
      );
    },
  );

let rejects_json = json =>
  switch (Rational.t_of_yojson(json)) {
  | r =>
    fail(
      "decoded " ++ Yojson.Safe.to_string(json) ++ " to " ++ Rational.show(r),
    )
  | exception (Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error(_)) => ()
  };

let rejects_sexp = sexp =>
  switch (Rational.t_of_sexp(sexp)) {
  | r =>
    fail(
      "decoded "
      ++ Sexplib.Sexp.to_string(sexp)
      ++ " to "
      ++ Rational.show(r),
    )
  | exception (Sexplib0.Sexp_conv.Of_sexp_error(_)) => ()
  };

let str_pair = (n, d) => `List([`String(n), `String(d)]);
let sexp_pair = (n, d) => Sexplib.Sexp.(List([Atom(n), Atom(d)]));

/* Decoding must never produce a value that construction could not. */
let invalid_text = [
  ("1", "0"),
  ("0", "0"),
  ("2", "4"),
  ("1", "-3"),
  ("-1", "-3"),
  ("0", "5"),
  ("+1", "3"),
  ("007", "3"),
  ("-0", "1"),
  ("1_000", "3"),
  (" 1", "3"),
  ("1.5", "3"),
  ("a", "b"),
  ("", "1"),
];

let decode_rejection =
  test_case(
    "decoders reject invalid input",
    `Quick,
    () => {
      List.iter(
        ((n, d)) => {
          rejects_json(str_pair(n, d));
          rejects_sexp(sexp_pair(n, d));
        },
        invalid_text,
      );
      List.iter(
        rejects_json,
        [
          `List([`String("1")]),
          `List([`String("1"), `String("3"), `String("1")]),
          `List([`Int(1), `Int(3)]),
          `String("1/3"),
          `Null,
          `Assoc([("num", `String("1")), ("den", `String("3"))]),
        ],
      );
      List.iter(
        rejects_sexp,
        Sexplib.Sexp.[
          Atom("1/3"),
          List([Atom("1")]),
          List([Atom("1"), Atom("3"), Atom("1")]),
          List([List([Atom("1")]), Atom("3")]),
        ],
      );
    },
  );

let tests = (
  "Rational",
  List.map(oracle_case, oracle)
  @ [laws, neg_and_inv, to_float, roundtrip, decode_rejection],
);
