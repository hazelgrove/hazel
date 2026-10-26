open Language;

/* Nat and SInt nodes inside a writer-created `use Rational` region. The
   writer wraps Rational nodes in `(use Rational in ...)` and Int nodes
   inside such a region in `(use Int in ...)`; a Nat or SInt operator or
   literal written there plainly would be re-kinded by `use Rational`.
   Each fixture is closed and well typed: the Nat/SInt part reaches the
   Rational expression through an Int exponent converted by an existing
   builtin. Reuses Test_RationalReadback's write/reread/faithfulness check. */

module G = IdTagged.FreshGrammar.Exp;

let roundtrip = Test_RationalReadback.roundtrip;
let rat = Test_RationalReadback.rat_value;
let nat = n => G.nat(Bigint.of_int(n));
let call = (f, arg) => G.ap(Forward, G.builtin_fun(f), arg);
let pow = (base, exponent) => G.bin_op(Rat(Power), base, exponent);

let tests = (
  "RationalIntegralModeReadback",
  [
    roundtrip(
      "Nat operator inside a Rational region",
      pow(rat(2, 3), call("int_of_nat", G.bin_op(Nat(Plus), nat(1), nat(1)))),
    ),
    roundtrip(
      "SInt operator inside a Rational region",
      pow(
        rat(2, 3),
        call("int_of_sint", G.bin_op(SInt(Plus), G.sint(1), G.sint(1))),
      ),
    ),
    roundtrip(
      "Nat literal in a synthesized position inside a Rational region",
      pow(
        rat(1, 2),
        call(
          "int_of_nat",
          G.let_(IdTagged.FreshGrammar.Pat.var("k"), nat(2), G.var("k")),
        ),
      ),
    ),
    roundtrip(
      "SInt literal in a synthesized position inside a Rational region",
      pow(
        rat(1, 2),
        call(
          "int_of_sint",
          G.let_(IdTagged.FreshGrammar.Pat.var("k"), G.sint(2), G.var("k")),
        ),
      ),
    ),
    roundtrip(
      "nested Nat operators inside a Rational region",
      pow(
        rat(1, 2),
        call(
          "int_of_nat",
          G.bin_op(Nat(Times), G.bin_op(Nat(Plus), nat(1), nat(1)), nat(2)),
        ),
      ),
    ),
    roundtrip(
      "Int operator inside a Nat operator inside a Rational region",
      pow(
        rat(1, 2),
        call(
          "int_of_nat",
          G.bin_op(
            Nat(Plus),
            call("nat_of_int", G.bin_op(Int(Plus), G.int(1), G.int(1))),
            nat(1),
          ),
        ),
      ),
    ),
    /* Controls: Nat literals in analysed positions, already typed by the
       builtin's parameter. */
    roundtrip(
      "control: Nat literal as a builtin argument",
      pow(rat(1, 2), call("int_of_nat", nat(3))),
    ),
    roundtrip(
      "control: Nat literals in a tuple argument",
      pow(
        rat(2, 3),
        call("int_of_nat", call("nat_mod", G.tuple([nat(7), nat(4)]))),
      ),
    ),
  ],
);
