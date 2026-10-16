/* Loading ReasonML text in bulk.

   The typing parser (Parser.to_segment) replays text one character at
   a time through the editor, regrouting and remolding the whole current
   level per character: quadratic, and Reason programs keep long flat
   levels (a module body is one level of chained `let ... ;` forms), so
   a 5KB file takes seconds. This loader builds the same tile structure
   in one pass instead:

   1. lex: the token boundaries typing would find (maximal runs that
      stay potential tokens), with strings and comments whole;
   2. match: open each multi-delimiter form at its leading token and
      close it as its later delimiters arrive, innermost first (an
      ambiguous delimiter like `;` or `=>` only ever completes the
      innermost form, as when typing); unmatched forms stay incomplete;
   3. mold every level once, top-down, and grout the tree once.

   Delimiter expansion is sort-independent in the Reason surface except
   for `|`, read here from the enclosing form: a sum-type bar inside a
   type definition, a rule everywhere else. */

open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type unit_ =
  | Space
  | Linebreak
  | Comment(string)
  | Tok(string);

/* 1. Lexing */

let lex = (str: string): list(unit_) => {
  let chars = Array.of_list(Token.to_list(str));
  let n = Array.length(chars);
  let at = (i, s) => i < n && chars[i] == s;
  let out = ref([]);
  let emit = u => out := [u, ...out^];
  let join = (acc: list(string)) => String.concat("", List.rev(acc));
  let rec until_newline = (i, acc) =>
    i >= n || chars[i] == "\n"
      ? (join(acc), i) : until_newline(i + 1, [chars[i], ...acc]);
  /* a block comment body from i (after its opening), one comment per
     line: tokens never span lines */
  let rec block = (i, acc) =>
    if (i >= n) {
      emit(Comment("/*" ++ join(acc) ++ "*/"));
      i;
    } else if (at(i, "*") && at(i + 1, "/")) {
      emit(Comment("/*" ++ join(acc) ++ "*/"));
      i + 2;
    } else if (chars[i] == "\n") {
      emit(Comment("/*" ++ join(acc) ++ " */"));
      emit(Linebreak);
      let j = ref(i + 1);
      while (j^ < n && chars[j^] == " ") {
        emit(Space);
        incr(j);
      };
      block(j^, [" "]);
    } else {
      block(i + 1, [chars[i], ...acc]);
    };
  let rec string_lit = (i, acc) =>
    if (i >= n || chars[i] == "\n") {
      (join(acc), i);
    } else if (chars[i] == "\\" && i + 1 < n) {
      string_lit(i + 2, [chars[i + 1], chars[i], ...acc]);
    } else if (chars[i] == "\"") {
      (join([chars[i], ...acc]), i + 1);
    } else {
      string_lit(i + 1, [chars[i], ...acc]);
    };
  /* the longest run from i that stays a potential token */
  let rec munch = (i, acc, tok) =>
    if (i < n) {
      let tok' = tok ++ chars[i];
      Token.is_potential_token(tok')
        ? munch(i + 1, [chars[i], ...acc], tok') : (tok, i);
    } else {
      (tok, i);
    };
  let rec go = i =>
    if (i >= n) {
      ();
    } else {
      switch (chars[i]) {
      | " " =>
        emit(Space);
        go(i + 1);
      | "\t" =>
        emit(Space);
        emit(Space);
        go(i + 1);
      | "\r" => go(i + 1)
      | "\n" =>
        emit(Linebreak);
        go(i + 1);
      | "\"" =>
        let (s, j) = string_lit(i + 1, ["\""]);
        emit(Tok(s));
        go(j);
      | "/" when at(i + 1, "/") =>
        let (c, j) = until_newline(i, []);
        emit(Comment(c));
        go(j);
      | "/" when at(i + 1, "*") => go(block(i + 2, []))
      | "[" when at(i + 1, "@") =>
        /* an attribute, to its closing bracket on this line */
        let rec attr = (j, depth, acc) =>
          if (j >= n || chars[j] == "\n") {
            (join(acc), j);
          } else if (chars[j] == "[") {
            attr(j + 1, depth + 1, [chars[j], ...acc]);
          } else if (chars[j] == "]" && depth == 1) {
            (join([chars[j], ...acc]), j + 1);
          } else if (chars[j] == "]") {
            attr(j + 1, depth - 1, [chars[j], ...acc]);
          } else {
            attr(j + 1, depth, [chars[j], ...acc]);
          };
        let (a, j) = attr(i, 0, []);
        emit(Token.is_comment(a) ? Comment(a) : Tok(a));
        go(j);
      | c =>
        let (tok, j) = munch(i + 1, [c], c);
        emit(Tok(tok));
        go(j);
      };
    };
  go(0);
  List.rev(out^);
};

/* 2. Matching */

type frame = {
  id: Id.t,
  label: Label.t,
  /* shards present so far, latest first */
  shards: list(int),
  /* children between consecutive present shards, latest first */
  children: list(Segment.t),
  /* pieces since the latest shard, latest first */
  cur: list(Piece.t),
};

/* classified at a sort where the form has a row (remolding reads the
   current mold's child sorts), refined when its level is molded */
let tile = (id: Id.t, label: Label.t, shards, children): Piece.t => {
  let (form, sort) =
    switch (Form.base_candidates(label)) {
    | [(form, m), ..._] => (form, m.out)
    | [] => Form.classify_label(Exp, label)
    };
  Tile({
    id,
    form,
    sort,
    shards,
    children,
  });
};

let next_token = (f: frame): option(Token.t) =>
  switch (f.shards) {
  | [k, ..._] => List.nth_opt(f.label, k + 1)
  | [] => None
  };

/* an open form's pieces, as an (incomplete) tile followed by what came
   after its latest shard */
let close_incomplete = (f: frame): list(Piece.t) => [
  tile(f.id, f.label, List.rev(f.shards), List.rev(f.children)),
  ...List.rev(f.cur),
];

/* the label a leading token opens, and whether it is the leading or
   the trailing delimiter */
let expansion = (in_typ_def: bool, in_rule_pat: bool, t: Token.t) =>
  switch (t) {
  | "|" when in_typ_def || in_rule_pat => ([t], Direction.Right)
  | "|" => (Form.label_of_family(Rule), Left)
  | _ =>
    List.fold_left(
      (found, sort: Sort.t) =>
        switch (found) {
        | Some(_) => found
        | None => Form.Expansion.try_get(sort, t)
        },
      None,
      [Exp, Rul, Pat, Typ, TPat],
    )
    |> Option.value(~default=([t], Direction.Right))
  };

let build = (units: list(unit_)): Segment.t => {
  /* open forms, innermost first, over the top level's pieces */
  let stack: ref(list(frame)) = ref([]);
  let top: ref(list(Piece.t)) = ref([]);
  let push_piece = (p: Piece.t) =>
    switch (stack^) {
    | [f, ...fs] =>
      stack :=
        [
          {
            ...f,
            cur: [p, ...f.cur],
          },
          ...fs,
        ]
    | [] => top := [p, ...top^]
    };
  let push_pieces = List.iter(push_piece);
  /* advance frame f (which is now innermost) past its next shard */
  let advance = (f: frame) => {
    let k =
      switch (f.shards) {
      | [k, ..._] => k + 1
      | [] => 0
      };
    let f = {
      ...f,
      shards: [k, ...f.shards],
      children: [List.rev(f.cur), ...f.children],
      cur: [],
    };
    stack := List.tl(stack^);
    if (k == List.length(f.label) - 1) {
      push_piece(
        tile(f.id, f.label, List.rev(f.shards), List.rev(f.children)),
      );
    } else {
      stack := [f, ...stack^];
    };
  };
  let in_typ_def = () =>
    switch (stack^) {
    | [{label: ["type", "=", ";"], shards: [1, ..._], _}, ..._] => true
    | _ => false
    };
  let in_rule_pat = () =>
    switch (stack^) {
    | [{label: ["|", "=>"], shards: [0], _}, ..._] => true
    | _ => false
    };
  let on_token = (t: Token.t) =>
    switch (stack^) {
    | [f, ..._] when next_token(f) == Some(t) => advance(f)
    | frames =>
      /* an unambiguous delimiter may complete an outer form, leaving
         the forms opened inside it incomplete */
      let outer =
        Form.is_ambiguous_polymorph(t)
          ? None
          : ListUtil.findi_opt(
              (f: frame) => next_token(f) == Some(t),
              frames,
            );
      switch (outer) {
      | Some((i, _)) =>
        for (_ in 1 to i) {
          let inner = List.hd(stack^);
          stack := List.tl(stack^);
          push_pieces(close_incomplete(inner));
        };
        advance(List.hd(stack^));
      | None =>
        switch (expansion(in_typ_def(), in_rule_pat(), t)) {
        | (label, Left) when List.length(label) > 1 =>
          stack :=
            [
              {
                id: Id.mk(),
                label,
                shards: [0],
                children: [],
                cur: [],
              },
              ...stack^,
            ]
        | (label, Right) when List.length(label) > 1 =>
          /* an orphaned closer */
          push_piece(tile(Id.mk(), label, [List.length(label) - 1], []))
        | _ => push_piece(tile(Id.mk(), [t], [0], []))
        }
      };
    };
  List.iter(
    fun
    | Space => push_piece(Secondary(Secondary.mk_space(Id.mk())))
    | Linebreak => push_piece(Secondary(Secondary.mk_newline(Id.mk())))
    | Comment(c) => push_piece(Secondary(Secondary.mk(Id.mk(), c)))
    | Tok(t) => on_token(t),
    units,
  );
  while (stack^ != []) {
    let f = List.hd(stack^);
    stack := List.tl(stack^);
    push_pieces(close_incomplete(f));
  };
  List.rev(top^);
};

/* 3. Molding and grouting */

let rec deep_remold = (seg: Segment.t, sort: Sort.t): Segment.t =>
  Segment.remold(seg, sort)
  |> List.map((p: Piece.t) =>
       switch (p) {
       | Tile(t) =>
         let mold = Tile.mold(t);
         let shards = Array.of_list(t.shards);
         Piece.Tile({
           ...t,
           children:
             List.mapi(
               (i, child) => {
                 let (l, r) = (shards[i], shards[i + 1]);
                 let sort =
                   l + 1 == r
                     ? Option.value(
                         List.nth_opt(mold.in_, l),
                         ~default=Sort.Any,
                       )
                     : Sort.Any;
                 deep_remold(child, sort);
               },
               t.children,
             ),
         });
       | _ => p
       }
     );

let to_segment = (~root: Sort.t, str: string): Segment.t =>
  str
  |> lex
  |> build
  |> deep_remold(_, root)
  |> Segment.regrout(Nib.Shape.(concave(), concave()));
