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
   and the empty editor's own hole) is the empty module, not a hole:
   MakeTerm reads it so at the Mod root, so an editor's statics agree. */
let body_of_zipper = (z: Zipper.t): Exp.t =>
  term_of_zipper(~root=Sort.Mod, z);

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

/* The ids of the tiles a term was completed around: a closer or keyword
   the text does not have (a `(` with no `)`, a `case` with no `end`), or
   an opener for a closer that has none (a stray `}`), each once. The
   editor shows the shards it supplies as ghosts. */
let incomplete_ids = (e: Exp.t): list(Id.t) => {
  let seen = Hashtbl.create(8);
  let acc = ref([]);
  let note = (continue, x: IdTagged.t(_)) => {
    List.iter(
      ((id, _)) =>
        if (!Hashtbl.mem(seen, id)) {
          Hashtbl.add(seen, id, ());
          acc := [id, ...acc^];
        },
      x.annotation.incomplete,
    );
    continue(x);
  };
  let _: Exp.t =
    Exp.map_term(
      ~f_exp=note,
      ~f_pat=note,
      ~f_typ=note,
      ~f_tpat=note,
      ~f_rul=note,
      ~f_mod=note,
      ~f_sig=note,
      ~f_mpat=note,
      e,
    );
  List.rev(acc^);
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

/* What of_texts last read, part by part (the preamble, each module by its
   place and name, the tail), each with the text it was read from, so a
   program read again re-reads only the parts whose text changed. One
   entry per part, so it is as big as the last program read. The place is
   in the key so that two modules never share a term, and so its ids. */
type part =
  | Preamble
  | Body(int, string)
  | Tail;

type cache = Hashtbl.t(part, (string, option(Exp.t)));

let cache = (): cache => Hashtbl.create(8);

let read_part =
    (cache: option(cache), part: part, read: string => option(Exp.t), text)
    : option(Exp.t) =>
  switch (cache) {
  | None => read(text)
  | Some(cache) =>
    switch (Hashtbl.find_opt(cache, part)) {
    | Some((read_from, e)) when read_from == text => e
    | _ =>
      let e = read(text);
      Hashtbl.replace(cache, part, (text, e));
      e;
    }
  };

/* The program from texts: the preamble (definitions ending in `in`, or
   nothing), each module's body, and the tail. A text that does not parse
   (rare: the recovering parser reads nearly anything) stands as a hole,
   or as the empty module, and is a problem of its own. With a cache, a
   part whose text is the one it last read is not read again. */
let read_texts =
    (
      ~cache: option(cache),
      ~preamble: string,
      ~modules: list((string, string)),
      ~tail: string,
    )
    : t => {
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
    switch (read_part(cache, Preamble, preamble_of_text, preamble)) {
    | Some(e) => e
    | None =>
      problem(None, "the preamble");
      Exp.fresh(EmptyHole);
    };
  let modules =
    List.mapi(
      (i, (name, text)) =>
        switch (read_part(cache, Body(i, name), body_of_text, text)) {
        | Some(body) => (name, body)
        | None =>
          problem(Some(name), "the module " ++ name);
          (name, Exp.fresh(Module([])));
        },
      modules,
    );
  let tail =
    switch (read_part(cache, Tail, exp_of_text, tail)) {
    | Some(e) => e
    | None =>
      problem(None, "the tail");
      Exp.fresh(EmptyHole);
    };
  /* the modules no longer in the program */
  Option.iter(
    cache =>
      Hashtbl.filter_map_inplace(
        (part, entry) =>
          switch (part) {
          | Body(i, name) =>
            switch (List.nth_opt(modules, i)) {
            | Some((still, _)) when still == name => Some(entry)
            | _ => None
            }
          | Preamble
          | Tail => Some(entry)
          },
        cache,
      ),
    cache,
  );
  let p = stitch(~preamble, ~modules, ~tail);
  {
    ...p,
    problems: unparsed^ @ p.problems,
  };
};

let of_texts = read_texts(~cache=None);

/* of_texts, reading again only the parts whose text changed since the
   cache last read them */
let of_texts_cached = (cache: cache) => read_texts(~cache=Some(cache));

/* Whether a body's text has nothing written in it but whitespace and
   closed comments (`# ... #`). A comment ends on its own line, as Hazel
   reads one (Token.comment_regexp), so a `#` with no `#` after it on its
   line is written text. */
let nothing_written = (body: string): bool => {
  let rec go = (i, in_comment) =>
    i >= String.length(body)
      ? !in_comment
      : (
        switch (body.[i]) {
        | '#' => go(i + 1, !in_comment)
        | '\n'
        | '\r' => !in_comment && go(i + 1, false)
        | ' '
        | '\t' => go(i + 1, in_comment)
        | _ => in_comment && go(i + 1, in_comment)
        }
      );
  go(0, false);
};

/* The program as one text, laid out as stitch lays out its term: the
   preamble, then `module Name = { body } in` for each module in order,
   then the tail. A body with nothing written in it is `{}`, the empty
   module, its comments above it, since braces around nothing read as a
   hole. Read back as an expression, it is the program of_texts builds,
   except where a body leaves a bracket open, which then reaches into what
   follows (zipper_of_parts reads each body inside its own braces). */
let text_of_parts =
    (~preamble: string, ~modules: list((string, string)), ~tail: string)
    : string =>
  String.concat(
    "\n",
    (String.trim(preamble) == "" ? [] : [preamble])
    @ List.concat_map(
        ((name, body)) =>
          nothing_written(body)
            ? (String.trim(body) == "" ? [] : [body])
              @ ["module " ++ name ++ " = {} in"]
            : ["module " ++ name ++ " = {\n" ++ body ++ "\n} in"],
        modules,
      )
    @ [tail],
  );

/* The token that stands for the i-th module's body in the layout that
   zipper_of_parts reads, before the body is put in its place */
let body_mark = (i: int): string =>
  "module_program_body_" ++ string_of_int(i);

/* The program as one zipper rooted at Exp, laid out as text_of_parts lays
   out its text. Each body is read on its own, as an editor rooted at Mod
   (by read_body: by default zipper_of_text, so as body_of_text reads
   it), and put inside its module's braces, so a bracket a body leaves
   open is closed inside them, as of_texts reads it, and only that body
   takes the slower recovering parser. The rest is read with a mark in
   place of each body, then each mark is replaced by its body. None when a
   part does not parse. */
let zipper_of_parts =
    (
      ~read_body: option((int, string, string) => option(Zipper.t))=?,
      ~preamble: string,
      ~modules: list((string, string)),
      ~tail: string,
      (),
    )
    : option(Zipper.t) => {
  let read_body =
    Option.value(read_body, ~default=(_, _, text) =>
      zipper_of_text(~root=Sort.Mod, text)
    );
  let written =
    List.mapi((i, (name, text)) => (i, name, text), modules)
    |> List.filter(((_, _, text)) => !nothing_written(text));
  let bodies =
    List.map(
      ((i, name, text)) => (body_mark(i), read_body(i, name, text)),
      written,
    );
  let marked =
    List.mapi(
      (i, (name, text)) =>
        (name, nothing_written(text) ? text : body_mark(i)),
      modules,
    );
  switch (
    zipper_of_text(
      ~root=Sort.Exp,
      text_of_parts(~preamble, ~modules=marked, ~tail),
    )
  ) {
  | None => None
  | Some(_) when List.exists(((_, body)) => Option.is_none(body), bodies) =>
    None
  | Some(layout) =>
    let bodies =
      List.filter_map(
        ((mark, body)) => Option.map(z => (mark, z), body),
        bodies,
      );
    let rec fill = (seg: Segment.t): Segment.t =>
      List.concat_map(
        (p: Piece.t) =>
          switch (p) {
          | Tile(t) =>
            switch (Tile.label(t)) {
            | [token] when List.mem_assoc(token, bodies) =>
              Zipper.unselect_and_zip(
                ~erase_buffer=true,
                List.assoc(token, bodies),
              )
            | _ => [
                Tile({
                  ...t,
                  children: List.map(fill, t.children),
                }),
              ]
            }
          | p => [p]
          },
        seg,
      );
    let filled = fill(Zipper.unselect_and_zip(~erase_buffer=true, layout));
    /* the probes each part was read with */
    let manuals =
      List.concat_map(
        (z: Zipper.t) => z.refractors.manuals,
        [layout, ...List.map(snd, bodies)],
      );
    Some(
      Zipper.unzip(~direction=Left, filled)
      |> Zipper.remold_regrout(Left, ~root=Sort.Exp)
      |> ZipperBase.update_refractors(_, r =>
           {
             ...r,
             manuals,
           }
         ),
    );
  };
};

let rec name_of_module_pat = (mp: MPat.t): option(string) =>
  switch (mp.term) {
  | Var(name) => Some(name)
  | Asc(mp, _) => name_of_module_pat(mp)
  | _ => None
  };

/* A program written out as one term (text_of_parts read back, or any
   text): each `module Name = def in` along the chain of definitions that
   fill_tail follows owns the ids in def and in its name, and errors and
   holes are told per module by them, as for a stitched program. names:
   the modules whose holes to count, in order; by default, the modules
   found. */
let of_term = (~names: option(list(string))=?, term: Exp.t): t => {
  let rec chain = (e: Exp.t, found) =>
    switch (e.term) {
    | Let(_, _, rest)
    | TyAlias(_, _, rest)
    | Theorem(_, _, rest)
    | Use(_, rest)
    | Filter(_, rest)
    | Seq(_, rest) => chain(rest, found)
    | ModuleExp(mp, def, rest) =>
      switch (name_of_module_pat(mp)) {
      | Some(name) => chain(rest, [(name, mp, def), ...found])
      | None => chain(rest, found)
      }
    | _ => List.rev(found)
    };
  let modules = chain(term, []);
  let owners =
    List.fold_left(
      (owners, (name, mp: MPat.t, def)) =>
        List.fold_left(
          (owners, id) => Id.Map.add(id, name, owners),
          owners,
          IdTagged.ids(mp) @ ids_of(def),
        ),
      Id.Map.empty,
      modules,
    );
  let found =
    List.fold_left(
      (names, (name, _, _)) =>
        List.mem(name, names) ? names : names @ [name],
      [],
      modules,
    );
  {
    term,
    names: Option.value(names, ~default=found),
    owners,
    problems: [],
  };
};

let module_of = (p: t, id: Id.t): option(string) =>
  Id.Map.find_opt(id, p.owners);

/* Outside every module (in the tail, say), the ids of each use of a
   module's member, `TrapRoom.on_event`, to that module's name: an error
   there (the member missing, or not of the type the tail asks of it) is
   one the module has to fix. */
let member_uses = (p: t): Id.Map.t(string) => {
  let rec head = (e: Exp.t): option(string) =>
    switch (e.term) {
    | Dot(e, _)
    | Parens(e) => head(e)
    | Var(name)
    | Constructor(name, _) when List.mem(name, p.names) => Some(name)
    | _ => None
    };
  let uses = ref(Id.Map.empty);
  let _: Exp.t =
    Exp.map_term(
      ~f_exp=
        (continue, e: Exp.t) => {
          switch (e.term) {
          | Dot(_) =>
            switch (head(e)) {
            | Some(name) =>
              List.iter(
                id =>
                  if (module_of(p, id) == None) {
                    uses := Id.Map.add(id, name, uses^);
                  },
                ids_of(e),
              )
            | None => ()
            }
          | _ => ()
          };
          continue(e);
        },
      p.term,
    );
  uses^;
};

/* The program's errors, each with its module: the problems, then what
   statics found (info_map from Statics.mk on p.term). An error outside
   every module, on a use of a module's member, is that module's
   (member_uses). */
let errors = (p: t, info_map: Statics.Map.t): list(error) => {
  let uses = lazy(member_uses(p));
  p.problems
  @ List.map(
      ((id, message)) =>
        {
          module_:
            switch (module_of(p, id)) {
            | Some(name) => Some(name)
            | None => Id.Map.find_opt(id, Lazy.force(uses))
            },
          id,
          message,
        },
      ErrorPrint.all_with_ids(info_map),
    );
};

/* How many of ids each module has, by name in order, and how many are
   outside every module (in the preamble and the tail) */
let per_module = (p: t, ids: list(Id.t)): (list((string, int)), int) => {
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
    ids,
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

/* Each module's holes, by name in order, and the holes outside every
   module (in the preamble and the tail) */
let holes = (p: t): (list((string, int)), int) =>
  per_module(p, hole_ids(p.term));

/* Each module's incomplete tiles (incomplete_ids), as holes counts its
   holes: a text that leaves a bracket open, or closes one it never
   opened, is not finished, though it has no hole and no error */
let incomplete = (p: t): (list((string, int)), int) =>
  per_module(p, incomplete_ids(p.term));
