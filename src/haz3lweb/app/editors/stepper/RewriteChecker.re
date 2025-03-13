open Haz3lcore;

// underscores indicate unused arguments
let check_rewrite = (_from: Exp.t, _to: Exp.t): bool => {
  let left_term = Exp.term_of(_from);
  let right_term = Exp.term_of(_to);

  switch (left_term) {
  // Binary operations
  | BinOp(Int(Plus), exp_left, exp_right) =>
    switch (right_term) {
    | Int(value) =>
      print_endline("Expanding binary operation into single term");
      let (op_left, op_right) = (
        Exp.term_of(exp_left),
        Exp.term_of(exp_right),
      );
      switch (op_left, op_right) {
      | (Int(int_left), Int(int_right)) =>
        let result = int_left + int_right;
        if (value == result) {
          print_endline("Successful rewrite");
          true;
        } else {
          print_endline("Failed rewrite");
          false;
        };
      | _ =>
        let random = Random.float(1.0);
        random >= 0.5;
      };
    | _ =>
      let random = Random.float(1.0);
      random >= 0.5;
    }
  // Single terms expanded into larger ones
  | Int(value) =>
    print_endline("Expanding single term into larger one");
    switch (right_term) {
    | BinOp(Int(op), exp_left, exp_right) =>
      let (op_left, op_right) = (
        Exp.term_of(exp_left),
        Exp.term_of(exp_right),
      );
      switch (op_left, op_right) {
      | (Int(int_left), Int(int_right)) =>
        let result =
          switch (op) {
          | Plus => int_left + int_right
          | Minus => int_left - int_right
          | Times => int_left * int_right
          | Divide => int_left / int_right
          | _ => (-1)
          };
        if (value == result) {
          print_endline("Successful rewrite");
          true;
        } else {
          print_endline("Failed rewrite");
          false;
        };
      | _ =>
        let random = Random.float(1.0);
        random >= 0.5;
      };
    | _ =>
      let random = Random.float(1.0);
      random >= 0.5;
    };
  | _ =>
    let random = Random.float(1.0);
    random >= 0.5;
  };
};
