/* Hazel values, rendered as Fumola source.
 *
 * The mirror of FumolaValue, which brings Fumola results into Hazel. This is
 * the way in: a Hazel value becomes the text of a Fumola expression, so a
 * program can be run *on* something Hazel holds rather than only reporting
 * back to it.
 *
 * Source text rather than a marshalling format, for the same reason the thunk
 * name is source text: Fumola's own parser decides what its syntax means, and
 * Hazel needs to know none of it. The cost is that only values with a written
 * form can cross, which is every first-order value and nothing else.
 *
 * The rendering is deliberately partial. A function, a reference, a hole --
 * anything whose meaning is not carried by its text -- is refused with a
 * message naming what could not be written, rather than guessed at. */

open Util;

/* Hazel's Symbol constructors, which mirror Fumola's symbol syntax:
 *
 *   Name("x")            `x
 *   Num(7)               7
 *   Call((`a, `b))       `a(`b)
 *   Dot((`a, `b))        `a.`b
 *
 * Recognised by name. A user's own constructor called Name would be read as a
 * symbol here, which is the price of not carrying types through this pass;
 * the constructor's type is available when statics has annotated it, and this
 * should consult it once there is a case that needs the distinction. */
let symbol_constructors = ["Name", "Num", "Call", "Dot"];

let rec of_exp = (e: TermBase.Exp.t): result(string, string) => {
  let unsupported = (what: string) =>
    Error("no Fumola source for " ++ what);
  let all = (parts: list(result(string, string))) =>
    List.fold_right(
      (part, acc) =>
        switch (part, acc) {
        | (Error(e), _) => Error(e)
        | (_, Error(e)) => Error(e)
        | (Ok(x), Ok(xs)) => Ok([x, ...xs])
        },
      parts,
      Ok([]),
    );
  switch (e.term) {
  | Parens(inner) => of_exp(inner)
  | Asc(inner, _) => of_exp(inner)
  | Atom(Int(n)) => Ok(Bigint.to_string(n))
  | Atom(Bool(b)) => Ok(b ? "true" : "false")
  | Atom(Float(f)) => Ok(Printf.sprintf("%g", f))
  /* Hazel string literals cannot contain a quote, so neither can this. */
  | Atom(String(s)) => Ok("\"" ++ s ++ "\"")
  | Tuple([]) => Ok("()")
  /* A tuple of labelled elements is a Fumola record; one without labels is a
     Fumola tuple. A mix of the two has no Fumola form. */
  | Tuple(es) =>
    let labelled =
      List.filter_map(
        (el: TermBase.Exp.t) =>
          switch (el.term) {
          | TupLabel({term: Label(l), _}, v) => Some((l, v))
          | _ => None
          },
        es,
      );
    if (List.length(labelled) == List.length(es) && es != []) {
      switch (all(List.map(((_, v)) => of_exp(v), labelled))) {
      | Error(e) => Error(e)
      | Ok(values) =>
        let fields =
          List.map2(((l, _), v) => l ++ " = " ++ v, labelled, values);
        Ok("{" ++ String.concat("; ", fields) ++ "}");
      };
    } else if (labelled == []) {
      switch (all(List.map(of_exp, es))) {
      | Error(e) => Error(e)
      | Ok(values) => Ok("(" ++ String.concat(", ", values) ++ ")")
      };
    } else {
      unsupported("a tuple that is only partly labelled");
    };
  | ListLit(es) =>
    switch (all(List.map(of_exp, es))) {
    | Error(e) => Error(e)
    | Ok(values) => Ok("[" ++ String.concat(", ", values) ++ "]")
    }
  /* Fumola's option: None is null, Some(x) is ?(x). */
  | Constructor("None", _) => Ok("null")
  | Ap(Forward, {term: Constructor("Some", _), _}, payload) =>
    switch (of_exp(payload)) {
    | Error(e) => Error(e)
    | Ok(payload) => Ok("?(" ++ payload ++ ")")
    }
  | Ap(Forward, {term: Constructor(name, _), _}, payload)
      when List.mem(name, symbol_constructors) =>
    symbol_source(name, payload)
  /* Any other constructor is a Fumola variant tag, written as Hazel spells
     it. Fumola accepts a capitalised tag, so the capitalisation that
     translation adds on the way in survives the way out. */
  | Constructor(name, _) => Ok("#" ++ name)
  | Ap(Forward, {term: Constructor(name, _), _}, payload) =>
    switch (of_exp(payload)) {
    | Error(e) => Error(e)
    | Ok(payload) => Ok("#" ++ name ++ "(" ++ payload ++ ")")
    }
  | EmptyHole => unsupported("a hole")
  | Invalid(_) => unsupported("an invalid expression")
  | Fun(_)
  | TypFun(_) => unsupported("a function")
  | FumolaPeek(_) => unsupported("a reference into a Fumola runtime")
  | _ => unsupported("this expression")
  };
}

/* A Hazel Symbol value, as Fumola writes one. */
and symbol_source =
    (name: string, payload: TermBase.Exp.t): result(string, string) => {
  let pair = (join: string) =>
    switch (payload.term) {
    | Tuple([l, r]) =>
      switch (of_symbol(l), of_symbol(r)) {
      | (Error(e), _)
      | (_, Error(e)) => Error(e)
      | (Ok(l), Ok(r)) => Ok(l ++ join ++ r)
      }
    | _ => Error("no Fumola source for a malformed symbol")
    };
  switch (name) {
  | "Name" =>
    switch (payload.term) {
    | Atom(String(s)) => Ok("`" ++ s)
    | _ => Error("no Fumola source for a symbol name that is not text")
    }
  | "Num" =>
    switch (payload.term) {
    | Atom(Int(n)) => Ok(Bigint.to_string(n))
    | _ => Error("no Fumola source for a symbol number that is not an int")
    }
  /* `a(`b) applies one symbol to another; `a.`b joins them. */
  | "Call" =>
    switch (payload.term) {
    | Tuple([f, a]) =>
      switch (of_symbol(f), of_symbol(a)) {
      | (Error(e), _)
      | (_, Error(e)) => Error(e)
      | (Ok(f), Ok(a)) => Ok(f ++ "(" ++ a ++ ")")
      }
    | _ => Error("no Fumola source for a malformed symbol")
    }
  | "Dot" => pair(".")
  | _ => Error("no Fumola source for " ++ name)
  };
}

and of_symbol = (e: TermBase.Exp.t): result(string, string) =>
  switch (e.term) {
  | Parens(inner) => of_symbol(inner)
  | Ap(Forward, {term: Constructor(name, _), _}, payload) =>
    symbol_source(name, payload)
  | _ => of_exp(e)
  };
