let ctx: VarCtx.t = [
  ("length", HTyp.Arrow(String, Int)),
  ("string_of_int", Arrow(Int, String)),
  ("int_of_string", Arrow(String, Int)),
  ("string_of_bool", Arrow(Bool, String)),
  ("bool_of_string", Arrow(String, Bool)),
  ("string_of_float", Arrow(Float, String)),
  ("float_of_string", Arrow(String, Float)),
  ("int_of_float", Arrow(Float, Int)),
  ("float_of_int", Arrow(Int, Float)),
  ("equal", Arrow(String, Arrow(String, Bool))),
  ("compare", Arrow(String, Arrow(String, Int))),
  ("trim", Arrow(String, String)),
  ("escaped", Arrow(String, String)),
  ("assert", Arrow(Bool, Prod([]))),
];

let lookup = x => VarMap.lookup(ctx, x);

let rec is_int_of_string_mix = (s, hex) =>
  if (s == "") {
    false;
  } else if (String.length(s) == 1) {
    if (s.[0] >= '0' && s.[0] <= hex) {
      true;
    } else {
      false;
    };
  } else if (s.[0] >= '0' && s.[0] <= hex) {
    is_int_of_string_mix(String.sub(s, 1, String.length(s) - 1), hex);
  } else {
    false;
  };

let rec is_int_of_string_hex = s =>
  if (s == "") {
    false;
  } else if (String.length(s) == 1) {
    let ch1 = Char.lowercase_ascii(s.[0]);
    if (ch1 >= '0' && ch1 <= '9' || ch1 >= 'a' && ch1 <= 'f') {
      true;
    } else {
      false;
    };
  } else {
    let ch1 = Char.lowercase_ascii(s.[0]);
    if (ch1 >= '0' && ch1 <= '9' || ch1 >= 'a' && ch1 <= 'f') {
      is_int_of_string_hex(String.sub(s, 1, String.length(s) - 1));
    } else {
      false;
    };
  };

let is_int_of_string = s =>
  if (s == "") {
    false;
  } else if (String.length(s) == 1) {
    if (s.[0] >= '0' && s.[0] <= '9') {
      true;
    } else {
      false;
    };
  } else {
    let hex = String.sub(s, 0, 2);
    switch (hex) {
    | "0b" =>
      is_int_of_string_mix(String.sub(s, 2, String.length(s) - 2), '1')
    | "0o" =>
      is_int_of_string_mix(String.sub(s, 2, String.length(s) - 2), '7')
    | "0x" => is_int_of_string_hex(String.sub(s, 2, String.length(s) - 2))
    | _ => is_int_of_string_mix(s, '9')
    };
  };

let is_bool_of_string = s =>
  if (s == "true" || s == "false") {
    true;
  } else {
    false;
  };

let rec is_float_of_string_dec = (s, count) =>
  if (s == "" || count > 1) {
    false;
  } else if (String.length(s) == 1) {
    if (s.[0] >= '0' && s.[0] <= '9' || s.[0] == '.') {
      true;
    } else {
      false;
    };
  } else if (s.[0] >= '0' && s.[0] <= '9') {
    is_float_of_string_dec(String.sub(s, 1, String.length(s) - 1), count);
  } else if (s.[0] == '.') {
    is_float_of_string_dec(
      String.sub(s, 1, String.length(s) - 1),
      count + 1,
    );
  } else {
    false;
  };

let is_float_of_string = s =>
  if (s == "" || s == ".") {
    false;
  } else if (is_int_of_string(s)) {
    true;
  } else {
    is_float_of_string_dec(s, 0);
  };

let builtinfunctions_evaluate =
    (x: string, l: list(DHExp.t)): Eval_Result.result =>
  switch (x) {
  | "length" =>
    switch (l) {
    | [] => Indet(ApBuiltin(x, l))
    | [a, ..._] =>
      switch (a) {
      | StringLit(s) => BoxedValue(IntLit(String.length(s)))
      | _ => Indet(Ap(ApBuiltin(x, l), a))
      }
    }
  | "int_of_string" =>
    switch (l) {
    | [] => Indet(ApBuiltin(x, l))
    | [a, ..._] =>
      switch (a) {
      | StringLit(s) =>
        if (is_int_of_string(s) && float_of_string(s) <= 2147483647.) {
          BoxedValue(IntLit(int_of_string(s)));
        } else {
          Indet(InvalidOperation(Ap(ApBuiltin(x, l), a), StrNotConvToInt));
        }
      | _ => Indet(Ap(ApBuiltin(x, l), a))
      }
    }
  | "bool_of_string" =>
    switch (l) {
    | [] => Indet(ApBuiltin(x, l))
    | [a, ..._] =>
      switch (a) {
      | StringLit(s) =>
        if (is_bool_of_string(s)) {
          BoxedValue(BoolLit(bool_of_string(s)));
        } else {
          Indet(
            InvalidOperation(Ap(ApBuiltin(x, l), a), StrNotConvToBool),
          );
        }
      | _ => Indet(Ap(ApBuiltin(x, l), a))
      }
    }
  | "float_of_string" =>
    switch (l) {
    | [] => Indet(ApBuiltin(x, l))
    | [a, ..._] =>
      switch (a) {
      | StringLit(s) =>
        if (is_float_of_string(s)) {
          BoxedValue(FloatLit(float_of_string(s)));
        } else {
          Indet(
            InvalidOperation(Ap(ApBuiltin(x, l), a), StrNotConvToFloat),
          );
        }
      | _ => Indet(Ap(ApBuiltin(x, l), a))
      }
    }
  | "trim" =>
    switch (l) {
    | [] => Indet(ApBuiltin(x, l))
    | [a, ..._] =>
      switch (a) {
      | StringLit(s) =>
        let (s', _) = StringUtil.find_and_replace("", s, "OK");
        BoxedValue(StringLit(String.trim(s')));
      | _ => Indet(Ap(ApBuiltin(x, l), a))
      }
    }
  | "escaped" =>
    switch (l) {
    | [] => Indet(ApBuiltin(x, l))
    | [a, ..._] =>
      switch (a) {
      | StringLit(s) => BoxedValue(StringLit(String.escaped(s)))
      | _ => Indet(Ap(ApBuiltin(x, l), a))
      }
    }
  | "string_of_int" =>
    switch (l) {
    | [] => Indet(ApBuiltin(x, l))
    | [a, ..._] =>
      switch (a) {
      | IntLit(i) => BoxedValue(StringLit(string_of_int(i)))
      /* int overflow */
      | Cast(NonEmptyHole(_, _, _, _, FloatLit(n)), _, Int)
          when Float.is_integer(n) =>
        Indet(InvalidOperation(Ap(ApBuiltin(x, l), a), IntOutBound))
      | _ => Indet(Ap(ApBuiltin(x, l), a))
      }
    }
  | "float_of_int" =>
    switch (l) {
    | [] => Indet(ApBuiltin(x, l))
    | [a, ..._] =>
      switch (a) {
      | IntLit(i) => BoxedValue(FloatLit(float_of_int(i)))
      | Cast(NonEmptyHole(_, _, _, _, FloatLit(n)), _, Int)
          when Float.is_integer(n) =>
        Indet(InvalidOperation(Ap(ApBuiltin(x, l), a), IntOutBound))
      | _ => Indet(Ap(ApBuiltin(x, l), a))
      }
    }
  | "string_of_bool" =>
    switch (l) {
    | [] => Indet(ApBuiltin(x, l))
    | [a, ..._] =>
      switch (a) {
      | BoolLit(b) => BoxedValue(StringLit(string_of_bool(b)))
      | _ => Indet(Ap(ApBuiltin(x, l), a))
      }
    }
  /*| "assert" =>
    switch (l) {
    | [] => Indet(ApBuiltin(x, l))
    | [a, ..._] =>
      switch (a) {
      | BoolLit(b) => b ? BoxedValue(Triv) : Indet(FailedAssert(a))
      | _ => Indet(Ap(ApBuiltin(x, l), a))
      }
    }*/
  | "string_of_float" =>
    switch (l) {
    | [] => Indet(ApBuiltin(x, l))
    | [a, ..._] =>
      switch (a) {
      | FloatLit(f) => BoxedValue(StringLit(string_of_float(f)))
      | _ => Indet(Ap(ApBuiltin(x, l), a))
      }
    }
  | "int_of_float" =>
    switch (l) {
    | [] => Indet(ApBuiltin(x, l))
    | [a, ..._] =>
      switch (a) {
      | FloatLit(f) => BoxedValue(IntLit(int_of_float(f)))
      | _ => Indet(Ap(ApBuiltin(x, l), a))
      }
    }
  /* multiple arguments */
  // TODO: simplest way is to simple add all of them case by case but I think we can make it work recursively
  | "equal" =>
    switch (l) {
    | [] => Indet(ApBuiltin(x, l))
    | [a, b] =>
      switch (a, b) {
      | (StringLit(s1), StringLit(s2)) =>
        BoxedValue(BoolLit(String.equal(s1, s2)))
      | _ => Indet(Ap(Ap(ApBuiltin(x, l), a), b))
      }
    | _ => BoxedValue(Triv)
    }
  | "compare" =>
    switch (l) {
    | [] => Indet(ApBuiltin(x, l))
    | [a, b] =>
      switch (a, b) {
      | (StringLit(s1), StringLit(s2)) =>
        BoxedValue(IntLit(String.compare(s1, s2)))
      | _ => Indet(Ap(Ap(ApBuiltin(x, l), a), b))
      }
    | _ => BoxedValue(Triv)
    }
  | _ => failwith("impossible")
  };
