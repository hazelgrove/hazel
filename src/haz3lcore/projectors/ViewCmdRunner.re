open Language;
open IdTagged.FreshGrammar;
open MvuShape;

/* Walks a ViewCmd tree down to the Html it finally yields.

   Figure 3's view is a COMMAND, not a function returning Html, so
   something has to run it. The evaluator cannot: reading a splice or
   building a splice editor is the editor's business, and evaluation has
   no access to the editor. So the evaluator's job ends at building the
   tree and this is where it gets performed -- the same division
   CmdRunner already makes for MVU's Cmd, except that a ViewCmd RETURNS
   a value and threads it through a continuation, which Cmd never does.

   editor and eval_splice run. The splice editor names is the
   projector's own, found by id when HazelDOM draws the Html. The value
   eval_splice reads rides in the ref itself (see splice_ref_typ). So
   neither needs a store. result_view is not implemented yet; reporting
   that plainly beats rendering something misleading. */

/* eval_splice's answer (Sec. 3.2.3): Some(Val(v)) when the code reduced
   to a value, Some(Indet) when it did not -- a hole, or a variable with
   no value in this run. None is for no run at all, which a ref carrying
   its value never is. */
let result_of = (v: DHExp.t): DHExp.t => {
  let ctor = (name, arg) =>
    Exp.ap(Forward, Exp.constructor(name, None), arg);
  ValueChecker.is_value(v)
    ? ctor("Some", ctor("Val", v))
    : ctor("Some", Exp.constructor("Indet", None));
};

/* Div([Style([... width n ch ...])], [Splice(r)]). Scrolls rather than
   grows, so a long splice cannot push the widget's layout around. */
let editor_html = (r: DHExp.t, n: int): DHExp.t => {
  let ctor = (name, arg) =>
    Exp.ap(Forward, Exp.constructor(name, None), arg);
  let prop = (k, v) => Exp.tuple([Exp.string(k), Exp.string(v)]);
  ctor(
    "Div",
    Exp.tuple([
      Exp.list_lit([
        ctor(
          "Style",
          Exp.list_lit([
            prop("display", "inline-block"),
            prop("width", string_of_int(n) ++ "ch"),
            prop("overflow-x", "auto"),
          ]),
        ),
      ]),
      Exp.list_lit([ctor("Splice", r)]),
    ]),
  );
};

/* A command's answer, handed to its continuation: the continuation
   returns the next command, which runs in turn. */
let rec resume = (k: DHExp.t, x: DHExp.t): result(DHExp.t, string) =>
  switch (safe_evaluate(Exp.ap(Forward, k, x))) {
  | Error(e) => Error("continuation: " ++ e)
  | Ok(next) => run(next)
  }

and run = (d: DHExp.t): result(DHExp.t, string) =>
  switch (of_constructor_raw(d)) {
  | None => Error("view did not return a ViewCmd")

  /* The answer. */
  | Some(("Pure", h)) => Ok(h)

  /* `do p <- c in body` became bind(c, fun p -> body), which built this
     node. Run the command, hand its answer to the continuation, and keep
     going -- the continuation returns another command, not a value. */
  | Some(("Bind", body)) =>
    switch (of_tuple(body)) {
    | Some([c, k]) =>
      switch (run(c)) {
      | Error(_) as e => e
      | Ok(x) => resume(k, x)
      }
    | _ => Error("malformed bind: expected a command and a continuation")
    }

  /* editor(r, FixedWidth(n)): the client's code for r, editable in
     place, n characters wide (Sec. 3.2.3 gives Dim in character units).
     The answer is Html with the splice at its centre, the same node
     Html.splice(r) makes, so it is drawn by the one path that already
     resolves a SpliceRef to this projector's splice -- a ref to someone
     else's splice renders as an error there, not as their code. */
  | Some(("Editor", body)) =>
    switch (of_tuple(body)) {
    | Some([args, k]) =>
      switch (of_tuple(args)) {
      | Some([r, dim]) =>
        switch (of_constructor_raw(dim)) {
        | Some(("FixedWidth", n)) =>
          switch (of_int(n)) {
          | Some(n) => resume(k, editor_html(r, n))
          | None => Error("editor: FixedWidth needs an Int")
          }
        | _ => Error("editor: expected a Dim, FixedWidth(n)")
        }
      | _ => Error("malformed editor: expected (SpliceRef, Dim)")
      }
    | _ => Error("malformed editor: expected arguments and a continuation")
    }

  | Some(("EvalSplice", body)) =>
    switch (of_tuple(body)) {
    | Some([r, k]) =>
      switch (SpliceStore.splice_ref(r)) {
      | Some((_, v)) => resume(k, result_of(v))
      | None => Error("eval_splice: expected a SpliceRef")
      }
    | _ => Error("malformed eval_splice: expected a ref and a continuation")
    }
  | Some(("ResultView", _)) =>
    Error("result_view is not implemented: there is no splice store yet")

  | Some((name, _)) => Error("not a ViewCmd command: " ++ name)
  };
