open Language;
open IdTagged.FreshGrammar;
open MvuShape;

/* Walks an UpdateCmd tree down to the model it finally yields, and the
   splice effects performed on the way.

   The twin of ViewCmdRunner, and it runs HERE rather than in the
   evaluator for the reason Sec. 3.2 gives: new_splice and set_splice
   change the editor, and evaluation cannot change the editor. The
   projector can, which is why performing an update is the projector's
   job and not a redex left in the program text.

   Neither command touches the editor itself. Each leaves a
   SpliceStore.effect, and the commit that writes the model into the text
   applies them (SpliceStore.write_model), so an update that fails
   halfway changes nothing. */

let rec run =
        (d: DHExp.t): result((DHExp.t, list(SpliceStore.effect)), string) =>
  switch (of_constructor_raw(d)) {
  | None => Error("update did not return an UpdateCmd")

  | Some(("Pure", m)) => Ok((m, []))

  | Some(("Bind", body)) =>
    switch (of_tuple(body)) {
    | Some([c, k]) =>
      switch (run(c)) {
      | Error(_) as e => e
      | Ok((x, done_)) => resume(~done_, k, x)
      }
    | _ => Error("malformed bind: expected a command and a continuation")
    }

  /* new_splice(typ, init) (Sec. 3.2.1): a fresh splice, holding init's
     code, or a hole. The ref it answers carries that code as its value. */
  | Some(("NewSplice", body)) =>
    switch (of_tuple(body)) {
    | Some([args, k]) =>
      switch (of_tuple(args)) {
      | Some([typ, init]) =>
        switch (make_splice(typ, init)) {
        | Error(e) => Error("new_splice: " ++ e)
        | Ok((r, done_)) => resume(~done_, k, r)
        }
      | _ => Error("malformed new_splice: expected (Typ, Maybe(Exp))")
      }
    | _ =>
      Error("malformed new_splice: expected arguments and a continuation")
    }

  /* new_remote(typ, instance, cell, init) (docs/remote-refs.md): new_splice's
     splice, answered as a RemoteRef, the record that binds it to `cell` in
     the Fumola instance `instance`. The binding is only data here: the
     livelit's view pulls the cell after each run. */
  | Some(("NewRemote", body)) =>
    switch (of_tuple(body)) {
    | Some([args, k]) =>
      switch (of_tuple(args)) {
      | Some([typ, instance, cell, init]) =>
        switch (of_string(instance), of_string(cell)) {
        | (Some(instance), Some(cell)) =>
          switch (make_splice(typ, init)) {
          | Error(e) => Error("new_remote: " ++ e)
          | Ok((r, done_)) =>
            resume(
              ~done_,
              k,
              Exp.tuple([
                Exp.tup_label(Exp.label("instance"), Exp.string(instance)),
                Exp.tup_label(Exp.label("cell"), Exp.string(cell)),
                Exp.tup_label(Exp.label("code"), r),
              ]),
            )
          }
        | _ => Error("new_remote: instance and cell must be strings")
        }
      | _ =>
        Error(
          "malformed new_remote: expected (Typ, String, String, Maybe(Exp))",
        )
      }
    | _ =>
      Error("malformed new_remote: expected arguments and a continuation")
    }

  /* set_splice(r, e) (Sec. 3.2.4): the splice r names will hold e. */
  | Some(("SetSplice", body)) =>
    switch (of_tuple(body)) {
    | Some([args, k]) =>
      switch (of_tuple(args)) {
      | Some([r, e]) =>
        switch (SpliceStore.splice_ref(r), SpliceStore.code_of_exp(e)) {
        | (None, _) => Error("set_splice: expected a SpliceRef")
        | (_, Error(e)) => Error("set_splice: " ++ e)
        | (Some((id, _)), Ok(code)) =>
          resume(~done_=[SpliceStore.Set(id, code)], k, Exp.tuple([]))
        }
      | _ => Error("malformed set_splice: expected (SpliceRef, Exp)")
      }
    | _ =>
      Error("malformed set_splice: expected arguments and a continuation")
    }

  | Some((name, _)) => Error("not an UpdateCmd command: " ++ name)
  }

/* The splice new_splice makes, shared with new_remote: a fresh id holding
   init's code (or a hole), and the ref naming it. The Typ has nowhere to
   live yet -- a splice is typed by where it sits -- so beyond the declared
   type written into the text as an ascription (Sec. 3.2.1), it is read and
   not kept. */
and make_splice =
    (typ: DHExp.t, init: DHExp.t)
    : result((DHExp.t, list(SpliceStore.effect)), string) =>
  switch (SpliceStore.code_of_init(init)) {
  | Error(_) as e => e
  | Ok(code) =>
    let id = Id.to_string(Id.mk());
    let declare =
      switch (SpliceStore.typ_of_typ_value(typ)) {
      | Some(t) => [SpliceStore.Declare(id, t)]
      | None => []
      };
    Ok((
      SpliceStore.mk_ref(id, code),
      [SpliceStore.New(id, code), ...declare],
    ));
  }

/* A command's answer, handed to its continuation, which returns the next
   command. `done_` is what the command itself performed. */
and resume = (~done_, k: DHExp.t, x: DHExp.t) =>
  switch (safe_evaluate(Exp.ap(Forward, k, x))) {
  | Error(e) => Error("continuation: " ++ e)
  | Ok(next) =>
    switch (run(next)) {
    | Error(_) as e => e
    | Ok((m, rest)) => Ok((m, done_ @ rest))
    }
  };

/* A new use's model (Sec. 3.2.1). init is an UpdateCmd(Model), performed
   when the use is created; the model it answers is written out with the
   splices it made, and that is the new use's text. The definition must
   be closed, as for update: it is evaluated in the builtin environment. */
let init_model = (def_elab: DHExp.t): result(TermBase.Exp.t, string) =>
  switch (safe_evaluate(def_elab)) {
  | Error(e) => Error("definition error: " ++ e)
  | Ok(record) =>
    switch (record_field(record, "init")) {
    | None => Error("definition is missing init")
    | Some(init) =>
      switch (run(init)) {
      | Error(e) => Error("init: " ++ e)
      | Ok((m, effects)) =>
        Ok(SpliceStore.write_model(~effects, ~existing=[], m))
      }
    }
  };
