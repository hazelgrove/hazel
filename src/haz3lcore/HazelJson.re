open Util_web;
open Language;

/* Codec for converting between Yojson and Hazel expressions of the
   built-in JSON ADT type defined in BuiltinsADT
   (Assoc | Bool | Float | Int | List | String | Null) */
module JsonADT = {
  module Fresh = Language.IdTagged.FreshGrammar;

  let ctr = (name: string): Language.Exp.t =>
    Fresh.Exp.constructor(name, None);

  let ap_ctr = (name: string, arg: Language.Exp.t): Language.Exp.t =>
    Fresh.Exp.ap(Forward, ctr(name), arg);

  /* Convert Yojson to a Hazel expression of the JSON ADT type */
  let rec yojson_to_exp =
          (json: Yojson.Safe.t): result(Language.Exp.t, string) =>
    switch (json) {
    | `Null => Ok(ctr("Null"))
    | `Bool(b) => Ok(ap_ctr("Bool", Fresh.Exp.bool(b)))
    | `Int(i) => Ok(ap_ctr("Int", Fresh.Exp.big_int(Bigint.of_int(i))))
    | `Float(f) => Ok(ap_ctr("Float", Fresh.Exp.float(f)))
    | `String(s) => Ok(ap_ctr("String", Fresh.Exp.string(s)))
    | `List(elements) =>
      elements
      |> List.map(yojson_to_exp)
      |> Result.all
      |> Result.map(~f=exps => ap_ctr("List", Fresh.Exp.list_lit(exps)))
    | `Assoc(pairs) =>
      pairs
      |> List.map(((key, value)) =>
           yojson_to_exp(value)
           |> Result.map(~f=v_exp =>
                Fresh.Exp.tuple([Fresh.Exp.string(key), v_exp])
              )
         )
      |> Result.all
      |> Result.map(~f=pair_exps =>
           ap_ctr("Assoc", Fresh.Exp.list_lit(pair_exps))
         )
    | `Intlit(_) => Error("Intlit not supported in JsonADT")
    | `Tuple(_) => Error("Tuple not supported in JsonADT")
    | `Variant(_) => Error("Variant not supported in JsonADT")
    };

  /* Convert a Hazel expression of the JSON ADT type back to Yojson */
  /* Recursively strip evaluation wrappers (Parens, Closure, etc.)
     to get to the underlying value term. */
  let rec strip_wrappers = (exp: Language.Exp.t): Language.Exp.t =>
    switch (Exp.term_of(exp)) {
    | Parens(inner)
    | Closure(_, inner)
    | Filter(_, inner)
    | Asc(inner, _)
    | Projector(_, inner) => strip_wrappers(inner)
    | _ => exp
    };

  let cls_name = (exp: Language.Exp.t): string =>
    Exp.show_cls(Exp.cls_of_term(Exp.term_of(exp)));

  let rec exp_to_yojson =
          (exp: Language.Exp.t): result(Yojson.Safe.t, string) => {
    let exp = strip_wrappers(exp);
    switch (Exp.term_of(exp)) {
    | Constructor("Null", _) => Ok(`Null)
    | Ap(_, fn_exp, arg) =>
      let fn_exp = strip_wrappers(fn_exp);
      let arg = strip_wrappers(arg);
      switch (Exp.term_of(fn_exp)) {
      | Constructor("Bool", _) =>
        switch (Exp.term_of(arg)) {
        | Atom(Bool(b)) => Ok(`Bool(b))
        | _ => Error("JsonADT: Bool expects a boolean literal")
        }
      | Constructor("Int", _) =>
        switch (Exp.term_of(arg)) {
        | Atom(Int(i)) =>
          switch (Bigint.to_int(i)) {
          | Some(n) => Ok(`Int(n))
          | None => Error("JsonADT: integer too large")
          }
        | UnOp(
            Int(Minus) | Float(Minus) | SInt(Minus) | Nat(Minus),
            neg_arg,
          ) =>
          let neg_arg = strip_wrappers(neg_arg);
          switch (Exp.term_of(neg_arg)) {
          | Atom(Int(i)) =>
            switch (Bigint.to_int(i)) {
            | Some(n) => Ok(`Int(- n))
            | None => Error("JsonADT: integer too large")
            }
          | _ => Error("JsonADT: Int expects an integer literal")
          };
        | _ => Error("JsonADT: Int expects an integer literal")
        }
      | Constructor("Float", _) =>
        switch (Exp.term_of(arg)) {
        | Atom(Float(f)) => Ok(`Float(f))
        | _ =>
          Error(
            "JsonADT: Float expects float literal, got: " ++ cls_name(arg),
          )
        }
      | Constructor("String", _) =>
        switch (Exp.term_of(arg)) {
        | Atom(String(s)) => Ok(`String(StringUtil.unescape_linebreaks(s)))
        | _ => Error("JsonADT: String expects a string literal")
        }
      | Constructor("List", _) =>
        switch (Exp.term_of(arg)) {
        | ListLit(elements) =>
          elements
          |> List.map(exp_to_yojson)
          |> Result.all
          |> Result.map(~f=json_elems => `List(json_elems))
        | _ => Error("JsonADT: List expects a list literal")
        }
      | Constructor("Assoc", _) =>
        switch (Exp.term_of(arg)) {
        | ListLit(elements) =>
          elements
          |> List.map(convert_assoc_pair)
          |> Result.all
          |> Result.map(~f=pairs => `Assoc(pairs))
        | _ => Error("JsonADT: Assoc expects a list literal")
        }
      | _ =>
        Error("JsonADT: unrecognized JSON constructor: " ++ cls_name(fn_exp))
      };
    | _ => Error("JsonADT: unrecognized JSON expression: " ++ cls_name(exp))
    };
  }
  and convert_assoc_pair =
      (pair_exp: Language.Exp.t): result((string, Yojson.Safe.t), string) => {
    let pair_exp = strip_wrappers(pair_exp);
    switch (Exp.term_of(pair_exp)) {
    | Tuple([key_exp, val_exp]) =>
      let key_exp = strip_wrappers(key_exp);
      switch (Exp.term_of(key_exp)) {
      | Atom(String(k)) =>
        switch (exp_to_yojson(val_exp)) {
        | Ok(v) => Ok((k, v))
        | Error(_) as e => e
        }
      | _ => Error("JsonADT: Assoc key must be a string literal")
      };
    | _ => Error("JsonADT: Assoc expects (String, JSON) pairs")
    };
  };
};
