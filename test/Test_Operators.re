open Alcotest;
open Language;

let tests = (
  "Operators",
  [
    test_case(
      "numeric_bin_op: numeric classes lift op_bin_num",
      `Quick,
      () => {
        check(
          bool,
          "Int(Plus)",
          true,
          Operators.numeric_bin_op(Atom.Int, Plus)
          == Some(Operators.Int(Plus)),
        );
        check(
          bool,
          "SInt(Times)",
          true,
          Operators.numeric_bin_op(Atom.SInt, Times)
          == Some(Operators.SInt(Times)),
        );
        check(
          bool,
          "Nat(Minus)",
          true,
          Operators.numeric_bin_op(Atom.Nat, Minus)
          == Some(Operators.Nat(Minus)),
        );
        check(
          bool,
          "Float(Plus) wraps via op_bin_float_of_num",
          true,
          Operators.numeric_bin_op(Atom.Float, Plus)
          == Some(Operators.Float(Plus)),
        );
      },
    ),
    test_case(
      "numeric_bin_op: non-numeric classes return None",
      `Quick,
      () => {
        check(
          bool,
          "Bool",
          true,
          Option.is_none(Operators.numeric_bin_op(Atom.Bool, Plus)),
        );
        check(
          bool,
          "String",
          true,
          Option.is_none(Operators.numeric_bin_op(Atom.String, Plus)),
        );
      },
    ),
  ],
);
