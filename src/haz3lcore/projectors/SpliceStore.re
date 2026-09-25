open Language;
open IdTagged.FreshGrammar;
open MvuShape;

/* Where a livelit's splices live: in the program text.

   A splice is the client's own code, so it has to be typed and
   evaluated in the client's scope, and edited where the client's code
   is edited. Both only happen for code inside the program. So the store
   is the text itself: each SpliceRef in a use's model is written into
   the model argument as the splice it names, in parens, at the ref's
   own position, and UserLivelit.expose_splice_refs decodes it back into
   a ref on every pass. Nothing is kept beside the text, so nothing can
   disagree with it, and undo, save and reload need no help.

   new_splice is the only thing that makes a splice (Sec. 3.2.1), and
   set_splice the only thing that rewrites one (Sec. 3.2.4). Performing
   either leaves an EFFECT, and the commit that writes the model applies
   them. Deletion is implicit: a splice no ref in the committed model
   reaches is simply not written back. */

type effect =
  /* new_splice: a fresh splice's id, and the code it starts with */
  | New(string, TermBase.Exp.t)
  /* set_splice: the ref's id, and the code that replaces what it holds */
  | Set(string, TermBase.Exp.t);

/* SpliceRef((id, value)) ~> (id, value) */
let splice_ref = (r: DHExp.t): option((string, DHExp.t)) =>
  switch (of_constructor(r)) {
  | Some(("SpliceRef", body)) =>
    switch (of_tuple(body)) {
    | Some([id, v]) => Option.map(id => (id, v), of_string(id))
    | _ => None
    }
  | _ => None
  };

/* A ref to a splice holding `code`, as the runner hands it back: the
   value it carries is the code itself, which for a literal or a hole is
   already its value. */
let mk_ref = (id: string, code: TermBase.Exp.t): DHExp.t =>
  Exp.ap(
    Forward,
    Exp.constructor("SpliceRef", None),
    Exp.tuple([Exp.string(id), code]),
  );

/* An Exp value as the code it denotes. IntLit(n) is Figure 3's own (l.49-52)
   and needs no quotation. The rest of Exp arrives with quotation. */
let code_of_exp = (e: DHExp.t): result(TermBase.Exp.t, string) =>
  switch (of_constructor(e)) {
  | Some(("IntLit", n)) =>
    switch (of_int(n)) {
    | Some(n) => Ok(Exp.int(n))
    | None => Error("IntLit needs an Int")
    }
  | Some((name, _)) => Error("no code for the Exp " ++ name ++ " yet")
  | None => Error("expected an Exp")
  };

/* new_splice's Maybe(Exp): None starts the splice empty, a hole. */
let code_of_init = (init: DHExp.t): result(TermBase.Exp.t, string) =>
  switch (of_constructor(init)) {
  | Some(("None", _)) => Ok(Exp.empty_hole())
  | Some(("Some", e)) => code_of_exp(e)
  | _ => Error("new_splice: expected None or Some(an Exp)")
  };

let mk_splice = (id: Id.t, code: TermBase.Exp.t): TermBase.Exp.t =>
  Exp.parens(IdTagged.mk_internal([id], Splice(code): TermBase.Exp.term));

/* The committed model VALUE, back into program text.

   Each SpliceRef becomes the splice it names, in parens:
   - one set_splice rewrote: a FRESH splice holding the new code. A fresh
     id, because the commit reattaches existing splices by id
     (ExpToSegment.reuse_splices), which would put the old code back.
     The ref is decoded from its position next pass, so the id changing
     does no harm;
   - one new_splice made: that splice, with its starting code;
   - one already in the text (`existing`): that id, and the commit
     reattaches the live splice, so whatever the client typed survives;
   - any other: left as the value it is. It names no splice this use has,
     so it is stale or forged, and writing a splice for it would invent
     client code.

   The stopgap pair (ref=SpliceRef(...), value=v) collapses to its splice
   the same way: the value is the splice's own, and is rebuilt from it. */
let write_model =
    (~effects: list(effect), ~existing: list(string), model: TermBase.Exp.t)
    : TermBase.Exp.t => {
  let set_code = id =>
    List.find_map(
      fun
      | Set(i, code) when i == id => Some(code)
      | _ => None,
      List.rev(effects),
    );
  let new_code = id =>
    List.find_map(
      fun
      | New(i, code) when i == id => Some(code)
      | _ => None,
      effects,
    );
  let splice_for = (id: string): option(TermBase.Exp.t) =>
    switch (set_code(id), new_code(id)) {
    | (Some(code), _) => Some(mk_splice(Id.mk(), code))
    | (None, Some(code)) =>
      Option.map(id => mk_splice(id, code), Id.of_string(id))
    | (None, None) when List.mem(id, existing) =>
      Option.map(id => mk_splice(id, Exp.empty_hole()), Id.of_string(id))
    | (None, None) =>
      print_endline("SpliceStore: a ref to no splice of this use: " ++ id);
      None;
    };
  let pair_ref = (fields: list(TermBase.Exp.t)): option(DHExp.t) => {
    let named = n =>
      List.find_map(
        f =>
          switch (Language.Exp.match_tup_label(f)) {
          | Some((l, v)) when l == n => Some(v)
          | _ => None
          },
        fields,
      );
    switch (List.length(fields), named("ref"), named("value")) {
    | (2, Some(r), Some(_)) => Some(r)
    | _ => None
    };
  };
  let rec go = (e: TermBase.Exp.t): TermBase.Exp.t => {
    let as_splice = r =>
      Option.bind(splice_ref(r), ((id, _)) => splice_for(id));
    switch (e.term) {
    | Ap(_, {term: Constructor("SpliceRef", _), _}, _) =>
      Option.value(as_splice(e), ~default=e)
    | Tuple(fs) =>
      switch (Option.bind(pair_ref(fs), as_splice)) {
      | Some(s) => s
      | None => {
          ...e,
          term: Tuple(List.map(go, fs)),
        }
      }
    | TupLabel(l, v) => {
        ...e,
        term: TupLabel(l, go(v)),
      }
    | ListLit(xs) => {
        ...e,
        term: ListLit(List.map(go, xs)),
      }
    | Parens(inner) => {
        ...e,
        term: Parens(go(inner)),
      }
    | Ap(dir, {term: Constructor(_), _} as c, arg) => {
        ...e,
        term: Ap(dir, c, go(arg)),
      }
    | _ => e
    };
  };
  go(model);
};

/* The ids of the splices a model argument holds, in the text. */
let rec splice_ids = (e: TermBase.Exp.t): list(string) =>
  switch (e.term) {
  | Splice(_) => [Id.to_string(IdTagged.rep_id(e))]
  | Parens(x)
  | TupLabel(_, x) => splice_ids(x)
  | Tuple(xs)
  | ListLit(xs) => List.concat_map(splice_ids, xs)
  | Ap(_, _, arg) => splice_ids(arg)
  | _ => []
  };
