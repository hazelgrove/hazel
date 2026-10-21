/* A program made of named modules: definitions every module can use (the
   preamble, ending in `in`), then each module's body (its members, with no
   `in`), then what the program gives (the tail). It is ONE term,

     <preamble>
     module Common = { <Common's body> } in
     module TrapRoom = { <TrapRoom's body> } in
     <tail>

   so each module sees the preamble and the modules before it, and the
   tail sees them all. A body is read as an editor rooted at Mod reads it
   (MakeTerm at Sort.Mod), so a body from text and a body from a tile
   editor give the same term, and the program's errors and holes are told
   per module, by the ids in each module's term.

   First use: Messed Up Dress Up's Scripting panel (an editor per module,
   📚 Common and one per room), through hazel-lib.js's analyzeProgram and
   runProgram. */
open Util;
open Language;

/* Text read the way a saved editor is loaded (FastParse, then the
   recovering parser). None only when both fail. */
let zipper_of_text = (~root: Sort.t, text: string): option(Zipper.t) =>
  PersistentZipper.parse_text(~source="module program", ~root, text);

let term_of_zipper = (~root: Sort.t, z: Zipper.t): Exp.t =>
  MakeTerm.from_zip_for_sem(z, ~root).term;

/* A module's body, from an editor rooted at Mod, as the expression
   Module(items). A body with nothing written in it (whitespace, comments,
   and the empty editor's own hole) is the empty module, not a hole. */
let body_of_zipper = (z: Zipper.t): Exp.t =>
  List.for_all(
    (p: Piece.t) => Piece.is_secondary(p) || Piece.is_grout(p),
    MakeTerm.semantic_source(z),
  )
    ? Exp.fresh(Module([])) : term_of_zipper(~root=Sort.Mod, z);

let body_of_text = (text: string): option(Exp.t) =>
  String.trim(text) == ""
    ? Some(Exp.fresh(Module([])))
    : zipper_of_text(~root=Sort.Mod, text) |> Option.map(body_of_zipper);

let exp_of_text = (text: string): option(Exp.t) =>
  zipper_of_text(~root=Sort.Exp, text)
  |> Option.map(term_of_zipper(~root=Sort.Exp));

/* The preamble's definitions, with a hole after them for fill_tail */
let preamble_of_text = (text: string): option(Exp.t) =>
  exp_of_text(text ++ " ?");

/* e with rest in place of the hole that ends its chain of definitions
   (let, type, module and the like), or, when that chain ends in
   something else, e then rest. A module definition's body is followed
   too, so a module scopes over what comes after it. */
let rec fill_tail = (e: Exp.t, rest: Exp.t): Exp.t => {
  let with_term = (term: Exp.term): Exp.t => {
    ...e,
    term,
  };
  switch (e.term) {
  | EmptyHole => rest
  | Let(p, def, body) => with_term(Let(p, def, fill_tail(body, rest)))
  | TyAlias(tp, def, body) =>
    with_term(TyAlias(tp, def, fill_tail(body, rest)))
  | ModuleExp(mp, def, body) =>
    with_term(ModuleExp(mp, def, fill_tail(body, rest)))
  | Theorem(p, def, body) =>
    with_term(Theorem(p, def, fill_tail(body, rest)))
  | Use(t, body) => with_term(Use(t, fill_tail(body, rest)))
  | Filter(kind, body) => with_term(Filter(kind, fill_tail(body, rest)))
  | Seq(e1, e2) => with_term(Seq(e1, fill_tail(e2, rest)))
  | _ => Exp.fresh(Seq(e, rest))
  };
};

/* Every id a term carries, through all its sorts */
let ids_of = (e: Exp.t): list(Id.t) => {
  let acc = ref([]);
  let record = (continue, x) => {
    acc := IdTagged.ids(x) @ acc^;
    continue(x);
  };
  let _: Exp.t =
    Exp.map_term(
      ~f_exp=record,
      ~f_pat=record,
      ~f_typ=record,
      ~f_tpat=record,
      ~f_rul=record,
      ~f_mod=record,
      ~f_sig=record,
      ~f_mpat=record,
      e,
    );
  acc^;
};

/* The ids of a term's holes: empty holes of every sort, typed `?` or
   left by the editor */
let hole_ids = (e: Exp.t): list(Id.t) => {
  let acc = ref([]);
  let note = (is_hole, continue, x) => {
    if (is_hole) {
      acc := [IdTagged.rep_id(x), ...acc^];
    };
    continue(x);
  };
  let _: Exp.t =
    Exp.map_term(
      ~f_exp=
        (continue, x: Exp.t) =>
          note(
            switch (x.term) {
            | EmptyHole => true
            | _ => false
            },
            continue,
            x,
          ),
      ~f_pat=
        (continue, x: Pat.t) =>
          note(
            switch (x.term) {
            | EmptyHole => true
            | _ => false
            },
            continue,
            x,
          ),
      ~f_typ=
        (continue, x: Typ.t) =>
          note(
            switch (x.term) {
            | Unknown(Hole(EmptyHole)) => true
            | _ => false
            },
            continue,
            x,
          ),
      ~f_tpat=
        (continue, x: TPat.t) =>
          note(
            switch (x.term) {
            | EmptyHole => true
            | _ => false
            },
            continue,
            x,
          ),
      ~f_mod=
        (continue, x: Mod.t) =>
          note(
            switch (x.term) {
            | EmptyHole => true
            | _ => false
            },
            continue,
            x,
          ),
      ~f_sig=
        (continue, x: Sig.t) =>
          note(
            switch (x.term) {
            | EmptyHole => true
            | _ => false
            },
            continue,
            x,
          ),
      ~f_mpat=
        (continue, x: MPat.t) =>
          note(
            switch (x.term) {
            | EmptyHole => true
            | _ => false
            },
            continue,
            x,
          ),
      e,
    );
  acc^;
};

/* An error, and the module it is in: None for the preamble, the tail and
   the joins between them */
type error = {
  module_: option(string),
  id: Id.t,
  message: string,
};

type t = {
  term: Exp.t,
  /* the modules' names, in order */
  names: list(string),
  /* every id in a module's body or name, to the module's name */
  owners: Id.Map.t(string),
  /* what the term alone shows: a text that does not parse, a name given
     twice */
  problems: list(error),
};

let is_module_name = (name: string): bool =>
  Token.is_var(name) || Token.is_ctr(name);

let module_pat = (name: string): MPat.t =>
  MPat.fresh(is_module_name(name) ? Var(name) : Invalid(name));

/* The program from its parts: the preamble's trailing hole filled by
   `module Name = body in` for each module in order, then the tail */
let stitch =
    (~preamble: Exp.t, ~modules: list((string, Exp.t)), ~tail: Exp.t): t => {
  let modules =
    List.map(((name, body)) => (name, module_pat(name), body), modules);
  let chain =
    List.fold_right(
      ((_, pat, body), rest) => Exp.fresh(ModuleExp(pat, body, rest)),
      modules,
      tail,
    );
  let owners =
    List.fold_left(
      (owners, (name, pat: MPat.t, body)) =>
        List.fold_left(
          (owners, id) => Id.Map.add(id, name, owners),
          owners,
          IdTagged.ids(pat) @ ids_of(body),
        ),
      Id.Map.empty,
      modules,
    );
  /* statics reads a name that is not one as a module pattern with no
     error, and a second module of one name hides the first */
  let (_, problems) =
    List.fold_left(
      ((seen, problems), (name, pat: MPat.t, _)) => {
        let problem = message => [
          {
            module_: Some(name),
            id: IdTagged.rep_id(pat),
            message,
          },
        ];
        (
          [name, ...seen],
          problems
          @ (
            is_module_name(name)
              ? [] : problem("\"" ++ name ++ "\" is not a module's name")
          )
          @ (
            List.mem(name, seen)
              ? problem("another module is also named " ++ name) : []
          ),
        );
      },
      ([], []),
      modules,
    );
  {
    term: fill_tail(preamble, chain),
    names: List.map(((name, _, _)) => name, modules),
    owners,
    problems,
  };
};

/* The program from texts: the preamble (definitions ending in `in`, or
   nothing), each module's body, and the tail. A text that does not parse
   (rare: the recovering parser reads nearly anything) stands as a hole,
   or as the empty module, and is a problem of its own. */
let of_texts =
    (~preamble: string, ~modules: list((string, string)), ~tail: string): t => {
  let unparsed = ref([]);
  let problem = (module_, what) =>
    unparsed :=
      unparsed^
      @ [
        {
          module_,
          id: Id.invalid,
          message: what ++ " does not parse",
        },
      ];
  let preamble =
    switch (preamble_of_text(preamble)) {
    | Some(e) => e
    | None =>
      problem(None, "the preamble");
      Exp.fresh(EmptyHole);
    };
  let modules =
    List.map(
      ((name, text)) =>
        switch (body_of_text(text)) {
        | Some(body) => (name, body)
        | None =>
          problem(Some(name), "the module " ++ name);
          (name, Exp.fresh(Module([])));
        },
      modules,
    );
  let tail =
    switch (exp_of_text(tail)) {
    | Some(e) => e
    | None =>
      problem(None, "the tail");
      Exp.fresh(EmptyHole);
    };
  let p = stitch(~preamble, ~modules, ~tail);
  {
    ...p,
    problems: unparsed^ @ p.problems,
  };
};

let module_of = (p: t, id: Id.t): option(string) =>
  Id.Map.find_opt(id, p.owners);

/* The program's errors, each with its module: the problems, then what
   statics found (info_map from Statics.mk on p.term) */
let errors = (p: t, info_map: Statics.Map.t): list(error) =>
  p.problems
  @ List.map(
      ((id, message)) =>
        {
          module_: module_of(p, id),
          id,
          message,
        },
      ErrorPrint.all_with_ids(info_map),
    );

/* Each module's holes, by name in order, and the holes outside every
   module (in the preamble and the tail) */
let holes = (p: t): (list((string, int)), int) => {
  let counts = Hashtbl.create(8);
  let outside = ref(0);
  List.iter(
    id =>
      switch (module_of(p, id)) {
      | Some(name) =>
        Hashtbl.replace(
          counts,
          name,
          1 + Option.value(Hashtbl.find_opt(counts, name), ~default=0),
        )
      | None => incr(outside)
      },
    hole_ids(p.term),
  );
  (
    List.map(
      name =>
        (name, Option.value(Hashtbl.find_opt(counts, name), ~default=0)),
      p.names,
    ),
    outside^,
  );
};
