open Language;
open IdTagged.FreshGrammar;
open MvuShape;

/* Walks an UpdateCmd tree down to the model it finally yields.

   The twin of ViewCmdRunner, and it runs HERE rather than in the
   evaluator for the reason Sec. 3.2 gives: new_splice and set_splice
   change the editor, and evaluation cannot change the editor. The
   projector can, which is why performing an update is the projector's
   job now and not a redex left in the program text.

   NewSplice and SetSplice are not implemented. They are the two that
   would actually reach editor state, and there is nowhere yet to put a
   splice; saying so beats committing a model built from a command that
   silently did nothing. */

let rec run = (d: DHExp.t): result(DHExp.t, string) =>
  switch (of_constructor_raw(d)) {
  | None => Error("update did not return an UpdateCmd")

  | Some(("Pure", m)) => Ok(m)

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

  | Some(("NewSplice", _)) =>
    Error("new_splice is not implemented: there is no splice store yet")
  | Some(("SetSplice", _)) =>
    Error("set_splice is not implemented: there is no splice store yet")

  | Some((name, _)) => Error("not an UpdateCmd command: " ++ name)
  };
