open Language;

/* Sound utility functions for interpreting Sound expressions.
 * Used by both EvalResult.re (for the results panel audio view)
 * and PlayerProj.re (for the Player refractor). */

/* Unwrap closures and ascriptions to get the inner value */
let rec unwrap_value = (exp: Exp.t): Exp.t =>
  switch (exp.term) {
  | Closure(_, inner) => unwrap_value(inner)
  | Asc(inner, _) => unwrap_value(inner)
  | Parens(inner) => unwrap_value(inner)
  | _ => exp
  };

/* Get constructor name from an expression */
let get_constructor = (exp: Exp.t): option(string) => {
  let unwrapped = unwrap_value(exp);
  switch (unwrapped.term) {
  | Ap(_, fn, _) =>
    let fn_unwrapped = unwrap_value(fn);
    switch (fn_unwrapped.term) {
    | Constructor(name, _) => Some(name)
    | _ => None
    };
  | Constructor(name, _) => Some(name)
  | _ => None
  };
};

/* Get constructor argument from an expression */
let get_constructor_arg = (exp: Exp.t): option(Exp.t) => {
  let unwrapped = unwrap_value(exp);
  switch (unwrapped.term) {
  | Ap(_, _, arg) => Some(unwrap_value(arg))
  | _ => None
  };
};

/* Extract string from expression */
let get_string = (exp: Exp.t): option(string) => {
  let unwrapped = unwrap_value(exp);
  switch (unwrapped.term) {
  | Atom(String(s)) => Some(s)
  | _ => None
  };
};

/* Extract float from expression */
let get_float = (exp: Exp.t): option(float) => {
  let unwrapped = unwrap_value(exp);
  switch (unwrapped.term) {
  | Atom(Float(f)) => Some(f)
  | Atom(Int(i)) => Some(Util.Bigint.to_float(i))
  | _ => None
  };
};

/* Extract tuple elements from expression */
let get_tuple = (exp: Exp.t): option(list(Exp.t)) => {
  let unwrapped = unwrap_value(exp);
  switch (unwrapped.term) {
  | Tuple(elems) => Some(List.map(unwrap_value, elems))
  | _ => None
  };
};

/* Extract list elements from expression */
let get_list = (exp: Exp.t): option(list(Exp.t)) => {
  let rec extract_list = (exp: Exp.t): option(list(Exp.t)) => {
    let unwrapped = unwrap_value(exp);
    switch (unwrapped.term) {
    | ListLit(elems) => Some(List.map(unwrap_value, elems))
    | Cons(hd, tl) =>
      switch (extract_list(tl)) {
      | Some(rest) => Some([unwrap_value(hd), ...rest])
      | None => None
      }
    | _ => None
    };
  };
  extract_list(exp);
};

/* Check if expression is a Sound value */
let is_sound = (exp: Exp.t): bool =>
  switch (get_constructor(exp)) {
  | Some(
      "Note" | "Sample" | "Rev" | "Fast" | "Slow" | "Seq" | "Stack" | "JuxRev",
    ) =>
    true
  | _ => false
  };

/* Recursively interpret a Sound expression and build a Strudel pattern */
let rec interpret_sound = (exp: Exp.t): option(Util.Strudel.pattern) =>
  switch (get_constructor(exp)) {
  | Some("Note") =>
    switch (get_constructor_arg(exp)) {
    | Some(arg) =>
      switch (get_string(arg)) {
      | Some(s) => Util.Strudel.note(s)
      | None => None
      }
    | None => None
    }
  | Some("Sample") =>
    switch (get_constructor_arg(exp)) {
    | Some(arg) =>
      switch (get_string(arg)) {
      | Some(s) => Util.Strudel.sound(s)
      | None => None
      }
    | None => None
    }
  | Some("Rev") =>
    switch (get_constructor_arg(exp)) {
    | Some(inner) =>
      switch (interpret_sound(inner)) {
      | Some(p) => Some(Util.Strudel.rev(p))
      | None => None
      }
    | None => None
    }
  | Some("Fast") =>
    switch (get_constructor_arg(exp)) {
    | Some(arg) =>
      switch (get_tuple(arg)) {
      | Some([factor, inner]) =>
        switch (get_float(factor), interpret_sound(inner)) {
        | (Some(f), Some(p)) => Some(Util.Strudel.fast(f, p))
        | _ => None
        }
      | _ => None
      }
    | None => None
    }
  | Some("Slow") =>
    switch (get_constructor_arg(exp)) {
    | Some(arg) =>
      switch (get_tuple(arg)) {
      | Some([factor, inner]) =>
        switch (get_float(factor), interpret_sound(inner)) {
        | (Some(f), Some(p)) => Some(Util.Strudel.slow(f, p))
        | _ => None
        }
      | _ => None
      }
    | None => None
    }
  | Some("Seq") =>
    switch (get_constructor_arg(exp)) {
    | Some(arg) =>
      switch (get_list(arg)) {
      | Some(elems) =>
        let patterns = List.filter_map(interpret_sound, elems);
        List.length(patterns) > 0 ? Some(Util.Strudel.seq(patterns)) : None;
      | None => None
      }
    | None => None
    }
  | Some("Stack") =>
    switch (get_constructor_arg(exp)) {
    | Some(arg) =>
      switch (get_list(arg)) {
      | Some(elems) =>
        let patterns = List.filter_map(interpret_sound, elems);
        List.length(patterns) > 0
          ? Some(Util.Strudel.stack(patterns)) : None;
      | None => None
      }
    | None => None
    }
  | Some("JuxRev") =>
    switch (get_constructor_arg(exp)) {
    | Some(inner) =>
      switch (interpret_sound(inner)) {
      | Some(p) => Some(Util.Strudel.juxRev(p))
      | None => None
      }
    | None => None
    }
  | _ => None
  };

/* Generate a short description of a Sound expression */
let rec sound_description = (exp: Exp.t): string =>
  switch (get_constructor(exp)) {
  | Some("Note") =>
    switch (get_constructor_arg(exp)) {
    | Some(arg) =>
      switch (get_string(arg)) {
      | Some(s) => s
      | None => "Note(?)"
      }
    | None => "Note(?)"
    }
  | Some("Sample") =>
    switch (get_constructor_arg(exp)) {
    | Some(arg) =>
      switch (get_string(arg)) {
      | Some(s) => {js|♪ |js} ++ s
      | None => "Sample(?)"
      }
    | None => "Sample(?)"
    }
  | Some("Rev") =>
    switch (get_constructor_arg(exp)) {
    | Some(inner) => "rev(" ++ sound_description(inner) ++ ")"
    | None => "Rev(?)"
    }
  | Some("Fast") =>
    switch (get_constructor_arg(exp)) {
    | Some(arg) =>
      switch (get_tuple(arg)) {
      | Some([factor, inner]) =>
        switch (get_float(factor)) {
        | Some(f) =>
          "fast("
          ++ Printf.sprintf("%.1f", f)
          ++ ", "
          ++ sound_description(inner)
          ++ ")"
        | None => "Fast(?)"
        }
      | _ => "Fast(?)"
      }
    | None => "Fast(?)"
    }
  | Some("Slow") =>
    switch (get_constructor_arg(exp)) {
    | Some(arg) =>
      switch (get_tuple(arg)) {
      | Some([factor, inner]) =>
        switch (get_float(factor)) {
        | Some(f) =>
          "slow("
          ++ Printf.sprintf("%.1f", f)
          ++ ", "
          ++ sound_description(inner)
          ++ ")"
        | None => "Slow(?)"
        }
      | _ => "Slow(?)"
      }
    | None => "Slow(?)"
    }
  | Some("Seq") =>
    switch (get_constructor_arg(exp)) {
    | Some(arg) =>
      switch (get_list(arg)) {
      | Some(elems) =>
        "seq["
        ++ String.concat(", ", List.map(sound_description, elems))
        ++ "]"
      | None => "Seq(?)"
      }
    | None => "Seq(?)"
    }
  | Some("Stack") =>
    switch (get_constructor_arg(exp)) {
    | Some(arg) =>
      switch (get_list(arg)) {
      | Some(elems) =>
        "stack["
        ++ String.concat(", ", List.map(sound_description, elems))
        ++ "]"
      | None => "Stack(?)"
      }
    | None => "Stack(?)"
    }
  | Some("JuxRev") =>
    switch (get_constructor_arg(exp)) {
    | Some(inner) => "juxRev(" ++ sound_description(inner) ++ ")"
    | None => "JuxRev(?)"
    }
  | _ => "?"
  };
