open Haz3lcore;
open Js_of_ocaml;

/* Dynamically load Algebrite from a URL */
let loadAlgebrite = () => {
  let script = Dom_html.createScript(Dom_html.document);
  script##.src :=
    Js.string(
      "http://algebrite.org/dist/1.4.0/algebrite.bundle-for-browser.js",
    );

  Dom.appendChild(Dom_html.document##.body, script);
};

let rec print_exp_for_algebrite = (exp: Exp.t): string =>
  switch (exp.term) {
  | Int(value) => string_of_int(value)
  | Float(value) => string_of_float(value)
  | Bool(value) => string_of_bool(value)
  | String(value) => value
  | BinOp(op, exp_left, exp_right) =>
    "("
    ++ print_exp_for_algebrite(exp_left)
    ++ " "
    ++ Operators.bin_op_to_string(op)
    ++ " "
    ++ print_exp_for_algebrite(exp_right)
    ++ ")"
  | Parens(exp) => "(" ++ print_exp_for_algebrite(exp) ++ ")"
  | Var(value) => value
  | _ => "Unknown"
  };

let checkEquality = (expr1, expr2) => {
  let algebrite = Js.Unsafe.global##.Algebrite;
  print_endline("Comparing: " ++ expr1 ++ " and " ++ expr2);
  let diffExpr = Printf.sprintf("simplify((%s)-(%s))", expr1, expr2);
  print_endline(diffExpr);
  print_endline(algebrite##run(Js.string(diffExpr)));
  switch (Js.to_string(algebrite##run(Js.string(diffExpr)))) {
  | "0" => true
  | _ => false
  };
};

// underscores indicate unused arguments
let check_rewrite = (_from: Exp.t, _to: Exp.t): bool => {
  let left_str = print_exp_for_algebrite(_from);
  let right_str = print_exp_for_algebrite(_to);
  checkEquality(left_str, right_str);
  // switch (left_term) {
  // // Binary operations
  // | BinOp(Int(Plus), exp_left, exp_right) =>
  //   switch (right_term) {
  //   | Int(value) =>
  //     print_endline("Expanding binary operation into single term");
  //     let (op_left, op_right) = (
  //       Exp.term_of(exp_left),
  //       Exp.term_of(exp_right),
  //     );
  //     switch (op_left, op_right) {
  //     | (Int(int_left), Int(int_right)) =>
  //       let result = int_left + int_right;
  //       if (value == result) {
  //         print_endline("Successful rewrite");
  //         true;
  //       } else {
  //         print_endline("Failed rewrite");
  //         false;
  //       };
  //     | _ =>
  //       let random = Random.float(1.0);
  //       random >= 0.5;
  //     };
  //   | _ =>
  //     let random = Random.float(1.0);
  //     random >= 0.5;
  //   }
  // // Single terms expanded into larger ones
  // | Int(value) =>
  //   print_endline("Expanding single term into larger one");
  //   switch (right_term) {
  //   | BinOp(Int(op), exp_left, exp_right) =>
  //     let (op_left, op_right) = (
  //       Exp.term_of(exp_left),
  //       Exp.term_of(exp_right),
  //     );
  //     switch (op_left, op_right) {
  //     | (Int(int_left), Int(int_right)) =>
  //       let result =
  //         switch (op) {
  //         | Plus => int_left + int_right
  //         | Minus => int_left - int_right
  //         | Times => int_left * int_right
  //         | Divide => int_left / int_right
  //         | _ => (-1)
  //         };
  //       if (value == result) {
  //         print_endline("Successful rewrite");
  //         true;
  //       } else {
  //         print_endline("Failed rewrite");
  //         false;
  //       };
  //     | _ =>
  //       let random = Random.float(1.0);
  //       random >= 0.5;
  //     };
  //   | _ =>
  //     let random = Random.float(1.0);
  //     random >= 0.5;
  //   };
  // | _ =>
  //   let random = Random.float(1.0);
  //   random >= 0.5;
  // };
};
