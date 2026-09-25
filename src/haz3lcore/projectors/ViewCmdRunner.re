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

   The three splice commands are not implemented. They need a place to
   read splices from, which does not exist yet, and reporting that
   plainly beats rendering something misleading. `Pure` and `Bind` are
   enough for every livelit that does not touch splices, which today is
   all of them. */

let rec run = (d: DHExp.t): result(DHExp.t, string) =>
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
      | Ok(x) =>
        switch (safe_evaluate(Exp.ap(Forward, k, x))) {
        | Error(e) => Error("bind continuation: " ++ e)
        | Ok(next) => run(next)
        }
      }
    | _ => Error("malformed bind: expected a command and a continuation")
    }

  | Some(("EvalSplice", _)) =>
    Error("eval_splice is not implemented: there is no splice store yet")
  | Some(("Editor", _)) =>
    Error("editor is not implemented: there is no splice store yet")
  | Some(("ResultView", _)) =>
    Error("result_view is not implemented: there is no splice store yet")

  | Some((name, _)) => Error("not a ViewCmd command: " ++ name)
  };
