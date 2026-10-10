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

   editor, eval_splice and result_view run. The splice editor names is
   the projector's own, found by id when HazelDOM draws the Html. The
   value eval_splice reads, and result_view draws, rides in the ref
   itself (see splice_ref_typ). So none of them needs a store. */

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

/* Div([Style([... min-width n ch ...])], [Splice(r)]). At least n
   characters wide, and it grows with the client's code rather than
   scrolling: a scroll box clips everything inside it, the splice's own
   context menu included, and the projector already widens the widget for
   what its splices hold (LivelitProj.widen_for_splices). */
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
            prop("min-width", string_of_int(n) ++ "ch"),
          ]),
        ),
      ]),
      Exp.list_lit([ctor("Splice", r)]),
    ]),
  );
};

/* result_view's Html: Div([Style([... min-width n ch ...])],
   [SpliceResult(r)]), sized as editor_html is, so a view can swap one for
   the other. HazelDOM draws the value r carries. */
let result_html = (r: DHExp.t, n: int): DHExp.t => {
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
            prop("min-width", string_of_int(n) ++ "ch"),
          ]),
        ),
      ]),
      Exp.list_lit([ctor("SpliceResult", r)]),
    ]),
  );
};

/* A command's answer, handed to its continuation: the continuation
   returns the next command, which runs in turn. */
/* Commands are evaluated and taken apart OPEN (safe_evaluate_open,
   of_constructor_open, of_tuple_open): a continuation closes over the
   whole view, and finishing each step copied all of it in -- most of a
   Kids' Choice redraw in Firefox. */
let rec resume = (k: DHExp.t, x: DHExp.t): result(DHExp.t, string) =>
  switch (safe_evaluate_open(Exp.ap(Forward, k, x))) {
  | Error(e) => Error("continuation: " ++ e)
  | Ok(next) => run(next)
  }

and run = (d: DHExp.t): result(DHExp.t, string) =>
  switch (of_constructor_open(d)) {
  | None => Error("view did not return a ViewCmd")

  /* The answer. */
  | Some(("Pure", h)) => Ok(h)

  /* `do p <- c in body` became bind(c, fun p -> body), which built this
     node. Run the command, hand its answer to the continuation, and keep
     going -- the continuation returns another command, not a value. */
  | Some(("Bind", body)) =>
    switch (of_tuple_open(body)) {
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
    switch (of_tuple_open(body)) {
    | Some([args, k]) =>
      switch (of_tuple_open(args)) {
      | Some([r, dim]) =>
        switch (of_constructor_open(dim)) {
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
    switch (of_tuple_open(body)) {
    | Some([r, k]) =>
      switch (SpliceStore.splice_ref(r)) {
      | Some((_, v)) => resume(k, result_of(v))
      | None => Error("eval_splice: expected a SpliceRef")
      }
    | _ => Error("malformed eval_splice: expected a ref and a continuation")
    }
  /* result_view(r, FixedWidth(n)) (Sec. 3.2.3, "Result Rendering"): the
     splice's evaluation result, drawn by Hazel, for the view to place --
     the paper's $dataframe shows each cell this way, and only its formula
     bar is an editor. Some(html) when the code reduced to a value; None
     exactly when eval_splice would answer Indet, since a variable with no
     value in this run would otherwise be drawn as its own unevaluated
     code, which reads as a result and is not one. The view decides what
     None looks like. */
  | Some(("ResultView", body)) =>
    switch (of_tuple_open(body)) {
    | Some([args, k]) =>
      switch (of_tuple_open(args)) {
      | Some([r, dim]) =>
        switch (of_constructor_open(dim), SpliceStore.splice_ref(r)) {
        | (Some(("FixedWidth", n)), Some((_, v))) =>
          switch (of_int(n)) {
          | Some(n) =>
            resume(
              k,
              ValueChecker.is_value(v)
                ? Exp.ap(
                    Forward,
                    Exp.constructor("Some", None),
                    result_html(r, n),
                  )
                : Exp.constructor("None", None),
            )
          | None => Error("result_view: FixedWidth needs an Int")
          }
        | (Some(("FixedWidth", _)), None) =>
          Error("result_view: expected a SpliceRef")
        | _ => Error("result_view: expected a Dim, FixedWidth(n)")
        }
      | _ => Error("malformed result_view: expected (SpliceRef, Dim)")
      }
    | _ =>
      Error("malformed result_view: expected arguments and a continuation")
    }

  | Some((name, _)) => Error("not a ViewCmd command: " ++ name)
  };
