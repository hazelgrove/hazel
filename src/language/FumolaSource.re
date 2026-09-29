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
  let unsupported = (what: string) => Error("no Fumola source for " ++ what);
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
  /* What the evaluator wraps a value in. Since the run moved to evaluation
     these are what a `hazel … end` actually holds: a value that came from a
     variable arrives inside the closure that captured its environment, and
     stepper filters wrap whatever they are watching. Neither changes the
     value, so both are seen through. */
  | Closure(_, inner) => of_exp(inner)
  | Filter(_, inner) => of_exp(inner)
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
  /* Named(xs) (docs/remote-refs.md): a list sent as (symbol, element) pairs,
     each symbol built from the element's AST id -- the shape List.fromIter
     and LevelTree.fromArray take, which name each cell by the caller's
     symbol. The ids are the source literals' own until evaluation finishes,
     which is when an escape is read, so a cell's name is the literal it came
     from; a computed element has an id, just not one the source shows. */
  | Ap(Forward, {term: Constructor("Named", _), _}, payload) =>
    switch (list_items(payload)) {
    | None => unsupported("Named of something other than a list")
    | Some(es) =>
      switch (all(List.map(of_exp, es))) {
      | Error(e) => Error(e)
      | Ok(values) =>
        let pairs =
          List.map2(
            (el: TermBase.Exp.t, v) =>
              "(" ++ id_symbol(IdTagged.rep_id(el)) ++ ", " ++ v ++ ")",
            es,
            values,
          );
        Ok("[" ++ String.concat(", ", pairs) ++ "]");
      }
    }
  /* Any other constructor is a Fumola variant tag, written as Hazel spells
     it. Fumola accepts a capitalised tag, so the capitalisation that
     translation adds on the way in survives the way out. */
  /* Recased on the way out, as FumolaValue recases on the way in: without
     this a value read from Fumola as `#leaf` went back as `#Leaf`, a
     different tag. See FumolaCase. */
  | Constructor(name, _) => Ok("#" ++ FumolaCase.to_fumola(name))
  | Ap(Forward, {term: Constructor(name, _), _}, payload) =>
    switch (of_exp(payload)) {
    | Error(e) => Error(e)
    | Ok(payload) =>
      Ok("#" ++ FumolaCase.to_fumola(name) ++ "(" ++ payload ++ ")")
    }
  | EmptyHole => unsupported("a hole")
  | Invalid(_) => unsupported("an invalid expression")
  | Fun(_)
  | TypFun(_) => unsupported("a function")
  /* A reference goes back out as the pointer it is, not as the value it
     holds. A bare `x is a *symbol*, and `@` on a symbol is a value of the
     wrong kind, so what crosses is the symbol turned back into a pointer.

     `pointer` rather than `prim "adaptonPointer"`, which is the same
     operation: it is one of the four names fumola_wasm binds unqualified at
     the top of every instance from fumola/system/prelude.fumola, and it is
     how a Fumola program is meant to be read. The cost is a dependency on
     that binding having loaded; the host reports it loudly if it did not.

     Parenthesized, like everything else this renders, and here it is load
     bearing rather than defensive: `@ pointer(`x)` is a *syntax* error --
     `@` takes an atom, and it reaches `pointer` before the argument --
     while `@ (pointer(`x))` is the read. Checked against the runtime in
     all three directions: reading with `@`, writing with `:=`, and reading
     through it inside a thunk, which records the dependency exactly as a
     read written in Fumola would.

     An opaque value names no cell and has no source, so it is still refused:
     what came back said what it was, and there is nothing to send. */
  | FumolaPeek({source, _}) when source != "" =>
    Ok("(pointer(" ++ source ++ "))")
  | FumolaPeek(_) => unsupported("a reference into a Fumola runtime")
  | _ => unsupported("this expression")
  };
}

/* A list's elements, through the wrappers the evaluator leaves on a value. */
and list_items = (e: TermBase.Exp.t): option(list(TermBase.Exp.t)) =>
  switch (e.term) {
  | Parens(inner)
  | Asc(inner, _)
  | Closure(_, inner)
  | Filter(_, inner) => list_items(inner)
  | ListLit(es) => Some(es)
  | _ => None
  }

/* The Fumola symbol for a Hazel AST id: `hazel(`id_<uuid>), dashes as
   underscores. An identifier rather than the string symbol `"<uuid>", which
   Fumola accepts too, because a symbol comes back into Hazel as its text and
   a Hazel string literal cannot hold the quotes a string symbol prints
   with. One-to-one with the id either way. */
and id_symbol = (id: Id.t): string =>
  "`hazel(`id_"
  ++ String.map(c => c == '-' ? '_' : c, Id.to_string(id))
  ++ ")"

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
