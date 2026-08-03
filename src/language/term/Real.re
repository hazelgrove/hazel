open Util;

/* Exact real atoms. Rational values are always normalized: the denominator is
 * positive and numerator/denominator are coprime. [spelling] is presentation
 * metadata for an unchanged source decimal and is deliberately ignored by
 * mathematical equality. */
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Rational({
      numerator: Bigint.t,
      denominator: Bigint.t,
      spelling: option(string),
    })
  | Pi;

let normalize = (numerator, denominator, spelling) => {
  if (Bigint.equal(denominator, Bigint.zero)) {
    invalid_arg("Real.normalize: zero denominator");
  };
  let (numerator, denominator) =
    denominator < Bigint.zero
      ? (Bigint.neg(numerator), Bigint.neg(denominator))
      : (numerator, denominator);
  let rec gcd = (a, b) =>
    Bigint.equal(b, Bigint.zero) ? a : gcd(b, Bigint.rem(a, b));
  let divisor = gcd(Bigint.abs(numerator), denominator);
  Rational({
    numerator: Bigint.(/)(numerator, divisor),
    denominator: Bigint.(/)(denominator, divisor),
    spelling,
  });
};

let of_bigint = numerator => normalize(numerator, Bigint.one, None);

let of_decimal = spelling => {
  let negative = String.length(spelling) > 0 && spelling.[0] == '-';
  let unsigned =
    negative
      ? String.sub(spelling, 1, String.length(spelling) - 1) : spelling;
  switch (String.split_on_char('.', unsigned)) {
  | [whole, fraction] =>
    let digits = whole ++ fraction;
    let numerator = Bigint.of_string(digits == "" ? "0" : digits);
    let numerator = negative ? Bigint.neg(numerator) : numerator;
    let denominator =
      Bigint.pow(
        Bigint.of_int(10),
        Bigint.of_int(String.length(fraction)),
      );
    normalize(numerator, denominator, Some(spelling));
  | [whole] =>
    let numerator = Bigint.of_string(whole == "" ? "0" : whole);
    normalize(
      negative ? Bigint.neg(numerator) : numerator,
      Bigint.one,
      Some(spelling),
    );
  | _ => invalid_arg("Real.of_decimal")
  };
};

let equal = (a, b) =>
  switch (a, b) {
  | (Pi, Pi) => true
  | (Rational(a), Rational(b)) =>
    Bigint.equal(a.numerator, b.numerator)
    && Bigint.equal(a.denominator, b.denominator)
  | _ => false
  };

/* Pi is intentionally symbolic, so comparisons involving it are not currently
 * decidable by this representation. Keep comparison partial rather than
 * inventing an ordering that disagrees with the mathematical reals. */
let compare_rationals = (a, b) =>
  switch (a, b) {
  | (Rational(a), Rational(b)) =>
    Some(
      Bigint.compare(
        Bigint.(a.numerator * b.denominator),
        Bigint.(b.numerator * a.denominator),
      ),
    )
  | (Pi, _)
  | (_, Pi) => None
  };

let add = (a, b) =>
  switch (a, b) {
  | (Rational(a), Rational(b)) =>
    Some(
      normalize(
        Bigint.(a.numerator * b.denominator + b.numerator * a.denominator),
        Bigint.(a.denominator * b.denominator),
        None,
      ),
    )
  | _ => None
  };
let sub = (a, b) =>
  switch (a, b) {
  | (Rational(a), Rational(b)) =>
    Some(
      normalize(
        Bigint.(a.numerator * b.denominator - b.numerator * a.denominator),
        Bigint.(a.denominator * b.denominator),
        None,
      ),
    )
  | _ => None
  };
let mul = (a, b) =>
  switch (a, b) {
  | (Rational(a), Rational(b)) =>
    Some(
      normalize(
        Bigint.(a.numerator * b.numerator),
        Bigint.(a.denominator * b.denominator),
        None,
      ),
    )
  | _ => None
  };
let pow = (base, exponent) =>
  switch (base, exponent) {
  | (Rational(base), Rational({numerator: exponent, denominator, _}))
      when Bigint.equal(denominator, Bigint.one) =>
    if (exponent < Bigint.zero) {
      Bigint.equal(base.numerator, Bigint.zero)
        ? Some(Error(InvalidOperationError.DivideByZero))
        : Some(
            Ok(
              normalize(
                Bigint.pow(base.denominator, Bigint.neg(exponent)),
                Bigint.pow(base.numerator, Bigint.neg(exponent)),
                None,
              ),
            ),
          );
    } else {
      Some(
        Ok(
          normalize(
            Bigint.pow(base.numerator, exponent),
            Bigint.pow(base.denominator, exponent),
            None,
          ),
        ),
      );
    }
  | _ => None
  };
let div = (a, b) =>
  switch (a, b) {
  | (_, Rational({numerator, _})) when Bigint.equal(numerator, Bigint.zero) =>
    Some(Error(InvalidOperationError.DivideByZero))
  | (Rational(a), Rational(b)) =>
    Some(
      Ok(
        normalize(
          Bigint.(a.numerator * b.denominator),
          Bigint.(a.denominator * b.numerator),
          None,
        ),
      ),
    )
  | _ => None
  };

let neg =
  fun
  | Rational(r) =>
    Some(
      Rational({
        ...r,
        numerator: Bigint.neg(r.numerator),
        spelling: None,
      }),
    )
  | Pi => None;

let rec factor_out = (n, factor) =>
  Bigint.equal(Bigint.rem(n, factor), Bigint.zero)
    ? factor_out(Bigint.(/)(n, factor), factor) : n;

let to_literal =
  fun
  | Pi => "pi_real"
  | Rational({spelling: Some(spelling), _}) => spelling
  | Rational({numerator, denominator, spelling: None}) =>
    if (Bigint.equal(denominator, Bigint.one)) {
      Bigint.to_string(numerator);
    } else {
      let rest =
        factor_out(
          factor_out(denominator, Bigint.of_int(2)),
          Bigint.of_int(5),
        );
      if (!Bigint.equal(rest, Bigint.one)) {
        Bigint.to_string(numerator) ++ "/" ++ Bigint.to_string(denominator);
      } else {
        /* Exact terminating decimal, generated without conversion through float. */
        let rec scale = (power10, places) =>
          Bigint.equal(Bigint.rem(power10, denominator), Bigint.zero)
            ? (Bigint.(numerator * (power10 / denominator)), places)
            : scale(Bigint.(power10 * of_int(10)), places + 1);
        let (scaled, places) = scale(Bigint.one, 0);
        let digits = Bigint.to_string(Bigint.abs(scaled));
        let padded =
          String.make(max(0, places + 1 - String.length(digits)), '0')
          ++ digits;
        let cut = String.length(padded) - places;
        (scaled < Bigint.zero ? "-" : "")
        ++ String.sub(padded, 0, cut)
        ++ "."
        ++ String.sub(padded, cut, places);
      };
    };
