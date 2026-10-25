/* The editing kernel (plans/editing-in-the-large.html): definition-level
   edits addressed by id, run as all-or-nothing transactions that return
   what each op did. A program is nested blocks of entries (ItemSpans'
   levels): module members, let chains, a definition's body. Four ops,
   each splicing one level (EntryLayout), so every untouched piece stays
   physically the same. Formatting is local: new code takes its place's
   indentation, and the gaps an op makes or closes are house style
   (EntryLayout.settle), not its neighbours' layout. Semantics
   (which tokens a rename touches, whether an edit adds errors) lives in
   planners and checks above */
open Util;

module L = EntryLayout;
module Nav = SpineNav;

/* a block of entries, by the head of the definition holding it */
[@deriving (show({with_path: false}), eq)]
type block =
  | Top
  /* module [m]'s members */
  | Members(Id.t)
  /* value [d]'s local definitions, above its result */
  | Body(Id.t);

/* an entry is named by any id in it, the innermost entry holding it:
   typically its anchor (ItemSpans'), which may be no piece */
[@deriving (show({with_path: false}), eq)]
type place =
  | Before(Id.t)
  | After(Id.t)
  | FirstIn(block)
  | LastIn(block);

/* a blank definition, named (None: a hole for its name), or a test */
[@deriving (show({with_path: false}), eq)]
type template =
  | Value(option(string))
  | Type(option(string))
  | Module(option(string))
  | Test;

[@deriving (show({with_path: false}), eq)]
type source =
  /* an entry's text, its `;` or `in` optional (the place's form decides),
     or a node's; fresh ids */
  | Text(string)
  | Template(template)
  /* another entry or node, fresh ids */
  | Copy(Id.t)
  /* one token's new text, its id kept. A definition's keyword (`let`,
     `type`, `module`) re-reads its head as that kind; a blank definition
     becomes the kind's blank */
  | Token(string);

/* parts of a definition, as paths name them: its head, its pattern's
   node, its definition's, and its body's: the definition past its `fun`
   heads */
[@deriving (show({with_path: false}), eq)]
type part = EntryPath.part = | Pattern | Definition | Body | Head;

/* Replace takes any node: an entry by its anchor (or first tile), else
   the term rooted at the piece, a case rule from its `|` through its
   result; a token with Token. ReplacePart takes a definition's part by
   its head: a definition starting with a local entry is rooted at that
   entry's head, which as Replace's id names the entry */
[@deriving (show({with_path: false}), eq)]
type op =
  | Insert(place, source)
  | Replace(Id.t, source)
  | ReplacePart(Id.t, part, source)
  | Delete(Id.t)
  /* ids kept, converting `;` ⇄ `in` between member and let-chain forms */
  | Move(Id.t, place);

/* one op's effect, by id, or one entry's of an op landing several;
   [program] is the program after it, so a replay can show the steps one at
   a time */
type step = {
  op,
  /* the entry it made: its row */
  added: list(Id.t),
  removed: list(Id.t),
  /* an entry moved: where it was, where it went */
  moved: list((Id.t, place, place)),
  /* a node and what replaced it: its root, or its row (the same id for a
     token) */
  replaced: list((Id.t, Id.t)),
  focus: option(Id.t),
  program: Segment.t,
};

type change = {
  program: Segment.t,
  steps: list(step),
  /* the last step's */
  focus: option(Id.t),
};

[@deriving (show({with_path: false}), eq)]
type reason =
  | Missing
  | Projected
  /* a block's result: it stays last */
  | Result
  /* an unfinished definition takes the entries after it */
  | Unfinished
  /* no entry that way */
  | Edge
  /* a comment set apart lies between */
  | Apart
  | NotABlock
  | NoMemberForm
  /* a statement's `;` would end the member it went into */
  | NoStatement
  /* a local definition in front of a function in parentheses would land
     outside its lambda */
  | OutsideFunction
  | IntoItself
  | Unreadable
  | NotAToken
  | NoPlace;

/* short: the outline is narrow */
let describe = (r: reason): string =>
  switch (r) {
  | Missing => "it isn't in the program"
  | Projected => "it's inside a projector"
  | Result => "a block's result stays last"
  | Unfinished => "it's unfinished: it takes in what follows"
  | Edge => "there's nothing that way"
  | Apart => "a comment set apart is in the way"
  | NotABlock => "only a module or a value holds definitions"
  | NoMemberForm => "it can't be a module member"
  | NoStatement => "a statement can't go into a member's body"
  | OutsideFunction => "it would land outside the function"
  | IntoItself => "it can't go into itself"
  | Unreadable => "the text doesn't read here"
  | NotAToken => "only a token takes new text"
  | NoPlace => "it can't go there"
  };

[@deriving (show({with_path: false}), eq)]
type refusal = {
  reason,
  /* what it concerns */
  id: option(Id.t),
  /* the op's index in its transaction */
  op: int,
};

exception Refused(reason, option(Id.t));
let refuse = (~id=?, reason) => raise(Refused(reason, id));
let need = (~id=?, reason, o: option('a)): 'a =>
  switch (o) {
  | Some(x) => x
  | None => refuse(~id?, reason)
  };

/* ---- entries ---- */

type entry = {
  b: L.block,
  j: int,
  rebuild: Segment.t => Segment.t,
};

let entry = (~mod_root: bool, id: Id.t, seg: Segment.t): entry =>
  switch (L.locate(~mod_root, id, seg)) {
  | Some((b, j, rebuild)) => {
      b,
      j,
      rebuild,
    }
  | None =>
    switch (Nav.find_path(~root=L.root_of(mod_root), id, seg)) {
    | Some(path) when Nav.projected(path) => refuse(~id, Projected)
    | _ => refuse(~id, Missing)
    }
  };

let anchor = (b: L.block, k: int): Id.t => b.lv.spans[k].anchor;

/* an op may take it whole */
let whole = (~id: Id.t, b: L.block, k: int) => {
  if (!L.movable(b, k)) {
    refuse(~id, Result);
  };
  if (L.runs_on(b, k)) {
    refuse(~id, Unfinished);
  };
};

let is_stmt = (b: L.block, k: int): bool => b.lv.spans[k].kind == Stmt;

/* the head of module [m], and of the entry holding the brace [brace] */
let module_of_brace = (~mod_root, brace: Id.t, seg): option(Id.t) =>
  Option.map(
    ((b, k, _)) => anchor(b, k),
    L.locate(~mod_root, brace, seg),
  );

/* where entry [k] of [b] is, by its neighbours */
let place_of = (~mod_root, b: L.block, k: int, seg: Segment.t): place => {
  let n = Array.length(b.sl);
  if (k > 0) {
    After(anchor(b, k - 1));
  } else if (k + 1 < n) {
    Before(anchor(b, k + 1));
  } else {
    switch (b.lv.owner) {
    | Members(brace) =>
      switch (module_of_brace(~mod_root, brace, seg)) {
      | Some(m) => FirstIn(Members(m))
      | None => FirstIn(Top)
      }
    | Block(d) => FirstIn(Body(d))
    /* a let chain's entries all sit above its result */
    | Chain(_)
    | Top => FirstIn(Top)
    };
  };
};

/* ---- text in a block's form ---- */

/* an entry's kind as its first word reads: a definition or directive
   takes `in` in a let chain, anything else is a statement and takes `;` */
let takes_in = (txt: string): bool =>
  switch (String.split_on_char(' ', String.trim(txt))) {
  | [
      "let" | "type" | "module" | "theorem" | "use" | "hide" | "eval" | "pause" |
      "debug",
      ..._,
    ] =>
    true
  | _ => false
  };

/* [txt] without a last `;`, or an `in` ending its last line */
let unterminated = (txt: string): string => {
  let t = String.trim(txt);
  let n = String.length(t);
  if (n > 0 && t.[n - 1] == ';') {
    String.trim(String.sub(t, 0, n - 1));
  } else if (n > 2
             && String.sub(t, n - 2, 2) == "in"
             && String.contains(" \n\t)", t.[n - 3])) {
    String.trim(String.sub(t, 0, n - 2));
  } else {
    t;
  };
};

/* an entry's pieces from text, in a block's form, and whether it is a
   statement */
let entry_text = (~member: bool, txt: string): option((Segment.t, bool)) => {
  let t = unterminated(txt);
  let def = takes_in(t);
  /* several entries take the last one's terminator, which the first word
     doesn't tell */
  let read = (term, stmt) =>
    Option.map(
      ps => (L.lay_in(~indent=0, ps), stmt),
      L.letin_core(t ++ term),
    );
  let as_def = () => read(" in", false);
  let as_stmt = () => read(";", true);
  let either = (a, b) =>
    switch (a()) {
    | None => b()
    | found => found
    };
  member
    ? Option.map(ps => (ps, !def), L.member_core(t))
    : def ? either(as_def, as_stmt) : either(as_stmt, as_def);
};

let template_text = (t: template): string =>
  switch (t) {
  | Value(name) =>
    "let " ++ Option.value(name, ~default="x") ++ {js| = ¿|js}
  | Type(name) =>
    "type " ++ Option.value(name, ~default="T") ++ {js| = ¿|js}
  | Module(name) => "module " ++ Option.value(name, ~default="M") ++ " = {}"
  | Test => {js|test ¿ end|js}
  };

let unnamed = (t: template): bool =>
  switch (t) {
  | Value(None)
  | Type(None)
  | Module(None) => true
  | Value(Some(_))
  | Type(Some(_))
  | Module(Some(_))
  | Test => false
  };

/* the item's head with holes for its pattern: not every pattern sort
   reads one (`module ¿`), so the template is read named */
let holed = (item: Segment.t): Segment.t => {
  let hole = (p: Piece.t) =>
    Piece.is_secondary(p)
      ? p
      : Grout({
          id: Id.mk(),
          shape: Convex,
        });
  let rec go = (ps: Segment.t) =>
    switch (ps) {
    | [Tile({children: [pat, ...kids], _} as t), ...rest] => [
        Piece.Tile({
          ...t,
          children: [List.map(hole, pat), ...kids],
        }),
        ...rest,
      ]
    | [p, ...rest] => [p, ...go(rest)]
    | [] => []
    };
  go(item);
};

let semi = (sort: Sort.t): Piece.t =>
  Piece.mk_tile(Form.classify_label(sort, [";"]), []);

let rec trim_end_ws = (ps: Segment.t): Segment.t =>
  switch (ListUtil.split_last_opt(ps)) {
  | Some((init, p)) when Nav.is_edge_ws(p) => trim_end_ws(init)
  | _ => ps
  };

/* a definition head's let-chain and member forms */
let head_forms: list((Form.t, Form.t)) = [
  (Compound(Let), Compound(ModLet)),
  (Compound(TypeAlias), Compound(ModType)),
  (Compound(ModuleExp), Compound(ModuleMod)),
];

let reform = (t: Tile.t, form: Form.t, sort: Sort.t, children): Piece.t =>
  Piece.Tile({
    ...t,
    form,
    sort,
    shards: List.mapi((i, _) => i, Form.label_of(form)),
    children,
  });

/* an entry's bare pieces (EntryLayout.bare) in the other block form, its
   ids kept: a definition's head between `let … =` (its definition then
   after it, the place's `;` after that) and `let … = … in`; a statement
   with its let-chain `;` or without. None with no such form */
let reformed =
    (~member: bool, ~kind: ItemSpans.kind, ps: Segment.t): option(Segment.t) => {
  let rec split = (lead, ps: Segment.t) =>
    switch (ps) {
    | [p, ...rest] when Piece.is_secondary(p) => split([p, ...lead], rest)
    | _ => (List.rev(lead), ps)
    };
  let (lead, rest) = split([], ps);
  switch (kind, rest) {
  | (Def, [Tile(h), ...after]) when Tile.is_complete(h) =>
    let pair =
      List.find_opt(((exp, m)) => h.form == (member ? exp : m), head_forms);
    switch (pair, h.children) {
    | (Some((_, m)), [pat, def]) when member && after == [] =>
      Some(lead @ [reform(h, m, Mod, [pat])] @ trim_end_ws(def))
    | (Some((exp, _)), [pat]) when !member =>
      Some(
        lead
        @ [reform(h, exp, Exp, [pat, trim_end_ws(after) @ [L.space()]])],
      )
    | _ => None
    };
  | (Stmt, [_, ..._]) =>
    if (member) {
      switch (ListUtil.split_last_opt(trim_end_ws(ps))) {
      | Some((init, last)) when Piece.is_semi(last) =>
        Some(trim_end_ws(init))
      | _ => None
      };
    } else {
      Some(trim_end_ws(ps) @ [semi(Exp)]);
    }
  | _ => None
  };
};

/* [ps] (entry [k] of [b]'s bare pieces) in a block's form: as it is, or
   converted, its `in` laid out by house style. A form [reformed] doesn't
   know goes through text, fresh ids, as it did before the kernel */
let in_form = (~member: bool, b: L.block, k: int, ps: Segment.t): Segment.t =>
  if (member == b.in_module) {
    ps;
  } else {
    let kind = b.lv.spans[k].kind;
    let ps =
      switch (reformed(~member, ~kind, ps)) {
      | Some(ps) => ps
      | None =>
        let via_text =
          member
            ? L.member_core(L.drop_suffix("in", L.text_of(ps)))
            : {
              let def =
                switch (List.find_map(Piece.is_tile, ps)) {
                | Some(t) => Nav.is_def_head(t)
                | None => false
                };
              L.letin_core(L.text_of(ps) ++ (def ? " in" : ";"));
            };
        need(~id=anchor(b, k), member ? NoMemberForm : NoPlace, via_text);
      };
    member ? ps : L.lay_in(~indent=L.item_indent(b, k), ps);
  };

/* ---- nodes ---- */

/* text read as a node of [sort], in a context that reads it so: the fast
   parser reads only programs */
let parse_at = (sort: Sort.t, txt: string): option(Segment.t) => {
  let t = String.trim(txt);
  let kid = (src, c) =>
    switch (L.parse(src)) {
    | Some([Tile(h), ..._]) when List.length(h.children) > c =>
      Some(Nav.core_ws(List.nth(h.children, c)))
    | _ => None
    };
  let found =
    switch (sort) {
    | Exp
    | Mod => Option.map(Nav.core_ws, L.parse(~root=sort, t))
    | Pat => kid("let " ++ t ++ {js| = ¿ in ¿|js}, 0)
    | TPat => kid("type " ++ t ++ {js| = ¿ in ¿|js}, 0)
    | MPat => kid("module " ++ t ++ {js| = {} in ¿|js}, 0)
    | Typ => kid("type T = " ++ t ++ {js| in ¿|js}, 1)
    | Rul =>
      /* a case's rules, past its scrutinee */
      Option.map(
        kid => {
          let rec past = (ps: Segment.t) =>
            switch (ps) {
            | [p, ...rest] when !Piece.is_case_rule(p) => past(rest)
            | _ => ps
            };
          past(kid);
        },
        kid({js|case ¿ |js} ++ t ++ " end", 0),
      )
    | _ => None
    };
  switch (found) {
  | Some([]) => None
  | found => found
  };
};

/* the sort a step's level holds */
let level_sort = (~root: Sort.t, up: list(Nav.step)): Sort.t =>
  switch (ListUtil.last_opt(up)) {
  | Some({from: Kid(t, c), _}) =>
    Option.value(List.nth_opt(Tile.mold(t).in_, c), ~default=Sort.Exp)
  | Some({from: Syntax, _}) => Exp
  | Some({from: Top | Sub, _})
  | None => root
  };

/* the node rooted at [level]'s piece [i]: its pieces' range and the sort
   its context reads it at */
let extent =
    (~sort: Sort.t, level: Segment.t, i: int): option((int, int, Sort.t)) => {
  let arr = Array.of_list(level);
  let nib = (j, left) =>
    switch (arr[j]) {
    | Tile(t) =>
      let (l, r) = Tile.mold(t).nibs;
      Some((left ? l : r).sort);
    | _ => None
    };
  let or_ = (s, o) => Option.value(o, ~default=s);
  let within = (sk, i) => {
    let (a, b) = Skel.range(sk);
    a <= i && i <= b;
  };
  switch (arr[i]) {
  | Tile(t) when Tile.is_case_rule(t) =>
    /* through its result, to the next rule */
    let n = Array.length(arr);
    let rec next = j =>
      j >= n || Piece.is_case_rule(arr[j]) ? j : next(j + 1);
    let rec back = j =>
      j > i + 1 && Piece.is_secondary(arr[j - 1]) ? back(j - 1) : j;
    Some((i, back(next(i + 1)) - 1, Rul));
  | Secondary(_) => None
  | _ =>
    switch (Segment.skel(~sort, level)) {
    | exception _ => None
    | sk =>
      let rec go = (sk: Skel.t, ctx) => {
        let root = Skel.root(sk);
        let ops = Aba.get_as(root);
        if (List.mem(i, ops)) {
          let (a, b) = Skel.range(sk);
          Some((a, b, ctx));
        } else {
          switch (List.find_opt(b => within(b, i), Aba.get_bs(root)), sk) {
          | (Some(inner), _) =>
            go(inner, or_(ctx, nib(List.hd(ops), false)))
          | (None, Pre(_, s)) =>
            go(s, or_(ctx, nib(ListUtil.last(ops), false)))
          | (None, Post(s, _)) =>
            go(s, or_(ctx, nib(List.hd(ops), true)))
          | (None, Bin(l, _, r)) =>
            within(l, i)
              ? go(l, or_(ctx, nib(List.hd(ops), true)))
              : go(r, or_(ctx, nib(ListUtil.last(ops), false)))
          | (None, Op(_)) => None
          };
        };
      };
      go(sk, sort);
    }
  };
};

type node = {
  up: list(Nav.step),
  level: Segment.t,
  lo: int,
  hi: int /* inclusive */,
  sort: Sort.t,
};

let node = (~root: Sort.t, id: Id.t, seg: Segment.t): node => {
  let path = need(~id, Missing, Nav.find_path(~root, id, seg));
  let (up, level, i) = Nav.home(~seg, path);
  let (lo, hi, sort) =
    need(~id, NoPlace, extent(~sort=level_sort(~root, up), level, i));
  {
    up,
    level,
    lo,
    hi,
    sort,
  };
};

/* the head whose whole definition node [n] is, comments aside, and
   the gaps around it in [n.level]: from [a] (past the head's `=`) to
   [b] (its `in`, `;` or the level's end) */
type def_gaps = {
  head: Id.t,
  a: int,
  b: int,
  in_: bool,
};

let def_gaps = (n: node): option(def_gaps) => {
  let arr = Array.of_list(n.level);
  let len = Array.length(arr);
  let rec back = i =>
    i > 0 && Piece.is_secondary(arr[i - 1]) ? back(i - 1) : i;
  let rec on = i => i < len && Piece.is_secondary(arr[i]) ? on(i + 1) : i;
  let (a, b) = (back(n.lo), on(n.hi + 1));
  switch (ListUtil.last_opt(n.up)) {
  | Some({from: Kid(t, c), _})
      when
        Nav.is_def_head(t)
        && Tile.ends_with_in(t)
        && c == List.length(t.children)
        - 1 =>
    a == 0 && b == len
      ? Some({
          head: t.id,
          a,
          b,
          in_: true,
        })
      : None
  | _ =>
    switch (a > 0 ? Some(arr[a - 1]) : None) {
    | Some(Tile(t))
        when
          Nav.is_def_head(t)
          && !Tile.ends_with_in(t)
          && (b == len || Piece.is_semi(arr[b])) =>
      Some({
        head: t.id,
        a,
        b,
        in_: false,
      })
    | _ => None
    }
  };
};

let node_pieces = (n: node): Segment.t =>
  ListUtil.slice(n.lo, n.hi + 1 - n.lo, n.level);

/* a node's id: its root's, else its first piece's */
let root_id = (ps: Segment.t): option(Id.t) =>
  switch (List.filter(p => !Piece.is_secondary(p), ps)) {
  | [] => None
  | [p] => Some(Piece.id(p))
  | [p, ..._] =>
    switch (Segment.root_id(Segment.skel(ps), ps)) {
    | id => Some(id)
    | exception _ => Some(Piece.id(p))
    }
  };

let rec past_fun_heads = (ps: Segment.t): Segment.t =>
  switch (ps) {
  | [p, ...rest] when Piece.is_secondary(p) || ItemSpans.is_fun_head(p) =>
    past_fun_heads(rest)
  | _ => ps
  };

let part =
    (~root: Sort.t, head: Id.t, part: part, seg: Segment.t): option(Id.t) =>
  switch (Nav.tile_ctx(~root, head, seg)) {
  | Some((_, _, t) as at) when Nav.is_def_head(t) =>
    switch (part) {
    | Head => Some(t.id)
    | Pattern => root_id(Nav.pat_of(t))
    | Definition => Option.bind(Nav.def_at(at), root_id)
    | Body =>
      Option.bind(Nav.def_at(at), def => root_id(past_fun_heads(def)))
    }
  | _ => None
  };

/* ---- ops ---- */

type ctx = {
  mod_root: bool,
  root: Sort.t,
};

let done_ =
    (
      ~added=[],
      ~removed=[],
      ~moved=[],
      ~replaced=[],
      ~focus=None,
      op: op,
      program: Segment.t,
    )
    : step => {
  op,
  added,
  removed,
  moved,
  replaced,
  focus,
  program,
};

/* [s], the op's step landing the entries of [item] (in its block's form),
   as a step per entry: each carries the program with the entries after it
   not yet in, so a replay brings them in one at a time. The first keeps
   what [s] removed, the last [s]'s program and focus */
let entrywise = (c: ctx, item: Segment.t, s: step): list(step) => {
  let mine = List.map(Piece.id, item);
  let at =
    Option.bind(ListUtil.hd_opt(s.added), row =>
      L.locate(~mod_root=c.mod_root, row, s.program)
    );
  let anchors =
    switch (at) {
    | Some((b, _, _)) =>
      List.init(Array.length(b.sl), k => (anchor(b, k), k))
      |> List.filter(((a, _)) => List.mem(a, mine))
    | None => []
    };
  switch (at, anchors) {
  | (None, _)
  | (_, [] | [_]) => [s]
  | (Some((b, _, _)), _) =>
    let row = k => L.row_id(L.bare(b, k) @ L.term(b, k));
    /* the program before each later entry, latest first */
    let rec back = (prog, later) =>
      switch (later) {
      | [] => Some([])
      | [a, ...later] =>
        /* a replay's record only: what it can't take apart, it keeps
           whole */
        switch (L.locate(~mod_root=c.mod_root, a, prog)) {
        | Some((b, j, rebuild)) =>
          switch (rebuild(L.remove(b, j))) {
          | prog => Option.map(ps => [prog, ...ps], back(prog, later))
          | exception _ => None
          }
        | None => None
        }
      };
    switch (back(s.program, List.rev_map(fst, List.tl(anchors)))) {
    | None => [s]
    | Some(before) =>
      let n = List.length(anchors);
      List.mapi(
        (i, ((_, k), program)) => {
          let made = row(k);
          {
            ...s,
            added: Option.to_list(made),
            removed: i == 0 ? s.removed : [],
            replaced:
              List.map(
                ((x, _)) => (x, Option.value(made, ~default=x)),
                s.replaced,
              ),
            focus: i == n - 1 ? s.focus : made,
            program,
          };
        },
        List.combine(anchors, List.rev(before) @ [s.program]),
      );
    };
  };
};

/* the program's top level as a block, and the program around it */
let top_block = (c: ctx, seg: Segment.t): (L.block, Segment.t => Segment.t) => {
  let lv = ItemSpans.top(~root=c.root, seg);
  (L.block(lv), L.rebuild(~seg, [Nav.step(~from=Top, lv, 0)]));
};

/* a head holding members, checked: a module's, or a signature's */
let module_head = (c: ctx, m: Id.t, seg: Segment.t) =>
  switch (Option.bind(Nav.tile_ctx(~root=c.root, m, seg), Nav.def_at)) {
  | Some(def) when Nav.brace_at(def) != None || L.module_def(def) => ()
  | Some(_) => refuse(~id=m, NotABlock)
  | None => refuse(~id=m, Missing)
  };

/* a value's head, checked for an entry ([stmt]: a statement) to go in */
let value_head = (c: ctx, ~stmt: bool, d: Id.t, seg: Segment.t) =>
  switch (Nav.tile_ctx(~root=c.root, d, seg)) {
  | Some((_, _, t) as at) when L.value_def(at) =>
    switch (Nav.def_at(at)) {
    | Some(def) when L.wrapped_fun(def) => refuse(~id=d, OutsideFunction)
    | _ when stmt && !Tile.ends_with_in(t) => refuse(~id=d, NoStatement)
    | _ => ()
    }
  | Some(_) => refuse(~id=d, NotABlock)
  | None => refuse(~id=d, Missing)
  };

/* a source as an entry in a block's form: its pieces, whether it is a
   statement, and the indentation its lines were written at */
let entry_of =
    (c: ctx, ~member: bool, src: source, seg: Segment.t)
    : (Segment.t, bool, option(int)) =>
  switch (src) {
  | Text(txt) =>
    let (ps, stmt) = need(Unreadable, entry_text(~member, txt));
    (ps, stmt, Some(0));
  | Template(t) =>
    let (ps, stmt) =
      need(Unreadable, entry_text(~member, template_text(t)));
    (unnamed(t) ? holed(ps) : ps, stmt, Some(0));
  | Copy(y) =>
    let e = entry(~mod_root=c.mod_root, y, seg);
    whole(~id=y, e.b, e.j);
    let bare = L.bare(e.b, e.j);
    /* read again from its text, as the outline's duplicate always was:
       fresh ids, and what doesn't read refuses */
    let txt = L.text_of(in_form(~member, e.b, e.j, bare));
    (
      need(
        ~id=y,
        Unreadable,
        member ? L.member_core(txt) : L.letin_core(txt),
      ),
      is_stmt(e.b, e.j),
      L.start_indent(~mod_root=c.mod_root, bare, seg),
    );
  | Token(_) => refuse(NotAToken)
  };

let is_after = (place: place): bool =>
  switch (place) {
  | After(_) => true
  | Before(_)
  | FirstIn(_)
  | LastIn(_) => false
  };
let is_first = (place: place): bool =>
  switch (place) {
  | FirstIn(_) => true
  | Before(_)
  | After(_)
  | LastIn(_) => false
  };

/* [item] (in [blk]'s form) first or last in [blk], coming as [arrival] */
let into =
    (
      c: ctx,
      blk: block,
      ~first: bool,
      ~arrival: L.arrival,
      ~stmt: bool,
      item: Segment.t,
      seg: Segment.t,
    )
    : option(Segment.t) => {
  let mod_root = c.mod_root;
  switch (blk, arrival) {
  | (Members(m), Fresh(src_indent) | Moved(_, src_indent)) =>
    switch (L.into_members(~mod_root, ~first, ~arrival, m, item, seg)) {
    | Some(_) as r => r
    | None => L.into_empty(~mod_root, ~src_indent?, m, item, seg)
    }
  | (Body(d), _) =>
    L.body_last(~mod_root, ~first, ~arrival, ~stmt, d, item, seg)
  | (Top, _) =>
    let (b, rebuild) = top_block(c, seg);
    switch (arrival) {
    | Fresh(_) when Array.length(b.sl) == 0 => Some(item @ seg)
    | _ => Option.map(rebuild, L.at_edge(b, ~first, ~arrival, item))
    };
  };
};

/* entries of [blk] are in member form */
let member_form = (c: ctx, blk: block): bool =>
  switch (blk) {
  | Members(_) => true
  | Body(_) => false
  | Top => c.mod_root
  };

/* [program] with the gaps beside the entries holding [item]'s pieces
   (the entries an op landed) laid out by house style, or with [inner]
   only those between them. A blank line the code put between them, or [lead] above them, sets a
   test below it apart */
let settled =
    (c: ctx, ~inner=false, ~lead=false, item: Segment.t, program: Segment.t)
    : Segment.t => {
  let mine =
    List.fold_left(
      (m, p) => Piece.is_secondary(p) ? m : Id.Map.add(Piece.id(p), (), m),
      Id.Map.empty,
      item,
    );
  switch (Id.Map.choose_opt(mine)) {
  | None => program
  | Some((x, ())) =>
    switch (L.locate(~mod_root=c.mod_root, x, program)) {
    | None => program
    | Some((b, _, rebuild)) =>
      let holds = k => {
        let (c0, c1) = b.lv.spans[k].core;
        let rec go = i =>
          i < c1 && (Id.Map.mem(Piece.id(b.arr[i]), mine) || go(i + 1));
        go(c0);
      };
      switch (List.filter(holds, List.init(Array.length(b.sl), k => k))) {
      | [] => program
      | [k0, ..._] as ks =>
        let k1 = ListUtil.last(ks);
        let js =
          inner
            ? List.init(k1 - k0, j => k0 + j)
            : List.init(k1 - k0 + 2, j => k0 - 1 + j);
        let blank = j => L.breaks(b, b.sl[j].eol, b.sl[j + 1].lo) > 1;
        let apart =
          List.filter(
            j =>
              L.is_test(b.lv, j + 1)
              && (j == k0 - 1 ? lead && !inner : j >= k0 && blank(j)),
            List.init(k1 - k0 + 1, j => k0 - 1 + j),
          );
        let seg = L.settle(~apart, b, js);
        seg === b.seg ? program : rebuild(seg);
      };
    }
  };
};

/* [program] with the gap where an entry was taken out, below entry
   [above] (if any), laid out by house style */
let closed = (c: ctx, ~above: option(Id.t), program: Segment.t): Segment.t =>
  switch (Option.bind(above, a => L.locate(~mod_root=c.mod_root, a, program))) {
  | Some((b, j, rebuild)) =>
    let seg = L.settle(b, [j]);
    seg === b.seg ? program : rebuild(seg);
  | None => program
  };

let insert =
    (c: ctx, op, place: place, src: source, seg: Segment.t): list(step) => {
  let mod_root = c.mod_root;
  /* code starting with a blank line */
  let lead =
    switch (src) {
    | Text(txt) =>
      let rec go = i =>
        i < String.length(txt)
        && (
          txt.[i] == '\n' || (txt.[i] == ' ' || txt.[i] == '\t') && go(i + 1)
        );
      go(0);
    | Template(_)
    | Copy(_)
    | Token(_) => false
    };
  let lands = (~member, f) => {
    let (item, stmt, src_indent) = entry_of(c, ~member, src, seg);
    let program =
      settled(c, ~lead, item, need(NoPlace, f(item, stmt, src_indent)));
    let row = L.row_id(item);
    entrywise(
      c,
      item,
      done_(~added=Option.to_list(row), ~focus=row, op, program),
    );
  };
  switch (place) {
  | Before(y)
  | After(y) =>
    let e = entry(~mod_root, y, seg);
    let after = is_after(place);
    if (after && !e.b.in_module && !L.movable(e.b, e.j)) {
      refuse(~id=y, Result);
    };
    lands(
      ~member=e.b.in_module,
      (item, stmt, src_indent) => {
        if (stmt && !e.b.stmts) {
          refuse(~id=y, NoStatement);
        };
        L.insert(
          e.b,
          ~pos=after ? After(e.j) : Before(e.j),
          ~member=e.b.in_module,
          ~src_indent?,
          item,
        )
        |> Option.map(e.rebuild);
      },
    );
  | FirstIn(blk)
  | LastIn(blk) =>
    switch (blk) {
    | Members(m) => module_head(c, m, seg)
    | Body(_)
    | Top => ()
    };
    lands(
      ~member=member_form(c, blk),
      (item, stmt, src_indent) => {
        switch (blk) {
        | Body(d) => value_head(c, ~stmt, d, seg)
        | Members(_)
        | Top => ()
        };
        into(
          c,
          blk,
          ~first=is_first(place),
          ~arrival=Fresh(src_indent),
          ~stmt,
          item,
          seg,
        );
      },
    );
  };
};

let delete = (c: ctx, op, x: Id.t, seg: Segment.t): step => {
  let e = entry(~mod_root=c.mod_root, x, seg);
  whole(~id=x, e.b, e.j);
  done_(
    ~removed=[x],
    op,
    closed(
      c,
      ~above=L.above_anchor(e.b, e.j),
      e.rebuild(L.remove(e.b, e.j)),
    ),
  );
};

let move = (c: ctx, op, x: Id.t, place: place, seg: Segment.t): step => {
  let mod_root = c.mod_root;
  let e = entry(~mod_root, x, seg);
  whole(~id=x, e.b, e.j);
  let bare = L.bare(e.b, e.j);
  let stmt = is_stmt(e.b, e.j);
  let from = place_of(~mod_root, e.b, e.j, seg);
  let moved = (~focus, program) =>
    done_(~moved=[(x, from, place)], ~focus, op, program);
  let inside = id => Nav.seg_contains_id(id, bare);
  /* out of its block, into another place: the item in its new form */
  let carry = (~member, f) => {
    let item = in_form(~member, e.b, e.j, bare);
    let tk = need(~id=x, NoPlace, L.take_out(~mod_root, x, seg));
    let tk = {
      ...tk,
      rest: closed(c, ~above=tk.above, tk.rest),
    };
    let src_indent = L.start_indent(~mod_root, bare, seg);
    moved(
      ~focus=L.row_id(item),
      settled(c, item, need(NoPlace, f(tk, item, src_indent))),
    );
  };
  switch (place) {
  | Before(y)
  | After(y) when inside(y) =>
    anchor(e.b, e.j) == y || x == y
      ? moved(~focus=Some(x), seg) : refuse(~id=y, IntoItself)
  | Before(y)
  | After(y) =>
    let after = is_after(place);
    let t = entry(~mod_root, y, seg);
    let same = t.b.lv.owner == e.b.lv.owner;
    if (after && !t.b.in_module && !L.movable(t.b, t.j)) {
      refuse(~id=y, Result);
    };
    let slot = after ? t.j + 1 : t.j;
    if (same && (slot == e.j || slot == e.j + 1)) {
      moved(~focus=Some(x), seg);
    } else if (same && (after && t.j == e.j + 1 || !after && t.j == e.j - 1)) {
      /* past its neighbour: they trade places */
      whole(~id=y, t.b, t.j);
      moved(
        ~focus=Some(x),
        settled(
          c,
          bare @ L.bare(t.b, t.j),
          e.rebuild(L.swap(e.b, min(e.j, t.j))),
        ),
      );
    } else {
      carry(~member=t.b.in_module, (tk, item, src_indent) =>
        Option.bind(
          L.locate(~mod_root, y, tk.rest),
          ((b, j, rebuild)) => {
            if (stmt && !b.stmts) {
              refuse(~id=y, NoStatement);
            };
            let at: L.landing = after ? Below(j) : Above(j);
            Some(
              rebuild(
                L.put(
                  b,
                  ~at,
                  ~lines=L.lines_at(b, at),
                  ~offset=tk.origin.offset,
                  ~src_indent,
                  ~member=b.in_module,
                  item,
                ),
              ),
            );
          },
        )
      );
    };
  | FirstIn(blk)
  | LastIn(blk) =>
    switch (blk) {
    | Members(h)
    | Body(h) when h == x || inside(h) => refuse(~id=h, IntoItself)
    | Members(m) => module_head(c, m, seg)
    | Body(d) => value_head(c, ~stmt, d, seg)
    | Top => ()
    };
    carry(~member=member_form(c, blk), (tk, item, src_indent) =>
      into(
        c,
        blk,
        ~first=is_first(place),
        ~arrival=Moved(tk.origin, src_indent),
        ~stmt,
        item,
        tk.rest,
      )
    );
  };
};

/* [pat]'s edge whitespace around [core]'s pieces */
let respace = (old: Segment.t, core: Segment.t): Segment.t => {
  let (pre, _, suf) = Nav.trim_ws(old);
  pre @ Nav.core_ws(core) @ suf;
};

/* [pat] with its first piece given [id] */
let rec with_first_id = (id: Id.t, pat: Segment.t): Segment.t =>
  switch (pat) {
  | [Tile(t), ...rest] => [
      Piece.Tile({
        ...t,
        id,
      }),
      ...rest,
    ]
  | [p, ...rest] when Piece.is_secondary(p) => [
      p,
      ...with_first_id(id, rest),
    ]
  | _ => pat
  };

/* definition head [t], piece [i] of [level], re-read as [kw]'s: its tile
   and its pattern's name keep their ids, its pattern's edge whitespace
   stays. Its definition stays, or if blank (only holes) is remade as the
   kind's blank, its pattern then its name alone */
let rekind =
    (kw: string, level: Segment.t, i: int, t: Tile.t): option(Segment.t) => {
  let member = !Tile.ends_with_in(t);
  let pat = Nav.pat_of(t);
  let def = Option.value(Nav.def_at((level, i, t)), ~default=[]);
  let blank = L.blank_def(def);
  let name = List.find_opt(p => !Piece.is_secondary(p), pat);
  let pat_text =
    switch (name) {
    | Some(n) when blank => L.piece_text(n)
    | _ => L.text(pat)
    };
  let head =
    kw
    ++ " "
    ++ String.trim(pat_text)
    ++ " = "
    ++ (kw == "module" || !blank ? "{}" : {js|¿|js});
  switch (member ? L.member_core(head) : L.letin_core(head ++ " in")) {
  | Some([Tile(h), ...rest]) =>
    let pat' =
      switch (name) {
      | Some(n) => respace(pat, with_first_id(Piece.id(n), Nav.pat_of(h)))
      | None => respace(pat, Nav.pat_of(h))
      };
    let made = children =>
      Piece.Tile({
        ...h,
        id: t.id,
        children,
      });
    if (member) {
      let (def, after) =
        Nav.split_at_semi(ListUtil.remove_first_n(i + 1, level));
      Some(
        ListUtil.truncate(i, level)
        @ [made([pat'])]
        @ (blank ? respace(def, rest) : def)
        @ after,
      );
    } else {
      let def' = blank ? respace(def, ListUtil.last(h.children)) : def;
      Some(ListUtil.put_nth(i, made([pat', def']), level));
    };
  | _ => None
  };
};

let token = (c: ctx, op, x: Id.t, txt: string, seg: Segment.t): step => {
  let path = need(~id=x, Missing, Nav.find_path(~root=c.root, x, seg));
  let (up, level, i) = Nav.home(~seg, path);
  let level' =
    switch (List.nth(level, i)) {
    /* a plain token's text is a token run's (transaction) */
    | Tile(t)
        when Nav.is_def_head(t) && List.mem(txt, ["let", "type", "module"]) =>
      need(~id=x, Unreadable, rekind(txt, level, i, t))
    | _ => refuse(~id=x, NotAToken)
    };
  done_(
    ~replaced=[(x, x)],
    ~focus=Some(x),
    op,
    Nav.rebuild(~seg, up, level'),
  );
};

/* where the comment lines directly above [level]'s piece [i] start (no
   blank line between): their first comment; [i] with none */
let doc_start = (level: Segment.t, i: int): int => {
  let arr = Array.of_list(level);
  /* back from [j] over whitespace and comments, [lbs] linebreaks since
     the last comment */
  let rec go = (j, lbs, found) =>
    if (j == 0) {
      found;
    } else {
      let p = arr[j - 1];
      if (ItemSpans.is_comment(p)) {
        go(j - 1, 0, j - 1);
      } else if (Piece.is_linebreak(p)) {
        lbs >= 1 ? found : go(j - 1, lbs + 1, found);
      } else if (Piece.is_space(p)) {
        go(j - 1, lbs, found);
      } else {
        found;
      };
    };
  go(i, 0, i);
};

/* an entry named by its anchor or first tile, rather than a node in it */
let names_entry = (c: ctx, x: Id.t, seg: Segment.t): option(entry) =>
  switch (L.locate(~mod_root=c.mod_root, x, seg)) {
  | Some((b, j, rebuild))
      when b.lv.spans[j].anchor == x || b.lv.spans[j].first == Some(x) =>
    Some({
      b,
      j,
      rebuild,
    })
  | _ => None
  };

/* the term rooted at piece [x] */
let replace_node = (c: ctx, op, x: Id.t, src: source, seg: Segment.t): step => {
  let n = node(~root=c.root, x, seg);
  let txt =
    switch (src) {
    | Text(txt) => txt
    | Copy(y) => L.text_of(node_pieces(node(~root=c.root, y, seg)))
    | Template(_)
    | Token(_) => refuse(~id=x, NoPlace)
    };
  let ps =
    need(~id=x, Unreadable, parse_at(n.sort, txt))
    |> (n.sort == Exp ? L.lay_in(~indent=0) : Fun.id);
  let indent =
    Option.value(Nav.line_indent(~root=c.root, x, seg), ~default=0);
  /* new code that starts with a comment replaces the comment lines
     directly above the node */
  let lo =
    switch (ps) {
    | [p, ..._] when ItemSpans.is_comment(p) => doc_start(n.level, n.lo)
    | _ => n.lo
    };
  let slice = (i, j) => ListUtil.slice(i, j - i, n.level);
  let level =
    switch (def_gaps(n)) {
    /* a definition's code, laid out as one */
    | Some(g) when !L.module_def(ps) =>
      let head =
        Option.value(Nav.line_indent(~root=c.root, g.head, seg), ~default=0);
      slice(0, g.a)
      @ L.lay_def(
          ~indent=head,
          ~at=indent,
          ~in_=g.in_,
          slice(g.a, lo),
          ps,
          slice(n.hi + 1, g.b),
        )
      @ ListUtil.remove_first_n(g.b, n.level);
    | _ =>
      let (pre, post) = (
        ListUtil.truncate(lo, n.level),
        ListUtil.remove_first_n(n.hi + 1, n.level),
      );
      pre @ L.unfused(pre, L.shift_by(indent, ps), post) @ post;
    };
  let made = Option.value(root_id(ps), ~default=x);
  done_(
    ~replaced=[(x, made)],
    ~focus=Some(made),
    op,
    Nav.rebuild(~seg, n.up, level),
  );
};

let replace = (c: ctx, op, x: Id.t, src: source, seg: Segment.t): list(step) =>
  switch (src, names_entry(c, x, seg)) {
  | (Token(txt), _) => [token(c, op, x, txt, seg)]
  | (_, Some(e)) =>
    let tail = !e.b.in_module && !L.movable(e.b, e.j);
    let (item, src_indent) =
      if (tail) {
        let txt =
          switch (src) {
          | Text(txt) => txt
          | Copy(y) => L.text_of(node_pieces(node(~root=c.root, y, seg)))
          | Template(_)
          | Token(_) => refuse(~id=x, Result)
          };
        (
          L.lay_in(~indent=0, need(Unreadable, parse_at(Exp, txt))),
          Some(0),
        );
      } else {
        let (item, stmt, src_indent) =
          entry_of(c, ~member=e.b.in_module, src, seg);
        if (stmt && !e.b.stmts) {
          refuse(~id=x, NoStatement);
        };
        (item, src_indent);
      };
    let at = L.indent_end(e.b, e.j);
    let shift =
      Option.fold(
        ~none=0,
        ~some=src => L.item_indent(e.b, e.j) - src,
        src_indent,
      );
    let (pre, post) = (
      ListUtil.truncate(at, e.b.seg),
      ListUtil.remove_first_n(e.b.sl[e.j].core, e.b.seg),
    );
    let level = pre @ L.unfused(pre, L.shift_by(shift, item), post) @ post;
    let row = L.row_id(item);
    let step =
      done_(
        ~replaced=[(x, Option.value(row, ~default=x))],
        ~added=Option.to_list(row),
        ~removed=[x],
        ~focus=row,
        op,
        settled(c, ~inner=true, item, e.rebuild(level)),
      );
    tail ? [step] : entrywise(c, item, step);
  | (_, None) => [replace_node(c, op, x, src, seg)]
  };

let replace_part =
    (c: ctx, op, head: Id.t, pt: part, src: source, seg: Segment.t)
    : list(step) => {
  let x = need(~id=head, Missing, part(~root=c.root, head, pt, seg));
  switch (pt, src) {
  | (Head, _)
  | (_, Token(_)) => replace(c, op, x, src, seg)
  | (Pattern | Definition | Body, _) => [replace_node(c, op, x, src, seg)]
  };
};

/* an op's steps: one per entry it lands */
let run = (c: ctx, seg: Segment.t, op: op): list(step) =>
  switch (op) {
  | Insert(place, src) => insert(c, op, place, src, seg)
  | Replace(x, src) => replace(c, op, x, src, seg)
  | ReplacePart(h, pt, src) => replace_part(c, op, h, pt, src, seg)
  | Delete(x) => [delete(c, op, x, seg)]
  | Move(x, place) => [move(c, op, x, place, seg)]
  };

/* the plain tokens among [ops]' leading Token replacements, by id, and the
   ops after them: a rename's run, relabeled in one walk */
let token_run =
    (c: ctx, seg: Segment.t, ops: list(op))
    : (list((Id.t, string, list(Nav.step))), list(op)) => {
  let rec go = (acc, ops) =>
    switch (ops) {
    | [Replace(x, Token(txt)), ...rest] =>
      switch (Nav.find_path(~root=c.root, x, seg)) {
      | Some(path) =>
        let st = ListUtil.last(path);
        switch (st.lv.pieces[st.i]) {
        | Tile({form: Tok(_), id, _}) when id == x =>
          go([(x, txt, path), ...acc], rest)
        | _ => (List.rev(acc), ops)
        };
      | None => (List.rev(acc), ops)
      }
    | _ => (List.rev(acc), ops)
    };
  go([], ops);
};

/* [seg] with each token of [run] given its text, ids kept: one walk down
   the paths to them, so only the pieces on those paths are re-made */
let relabel = (run: list((Id.t, string, list(Nav.step))), seg: Segment.t) => {
  let texts =
    List.fold_left(
      (m, (x, txt, _)) => Id.Map.add(x, txt, m),
      Id.Map.empty,
      run,
    );
  let on_path =
    List.fold_left(
      (s, (_, _, path)) =>
        List.fold_left(
          (s, st: Nav.step) => Id.Set.add(Piece.id(st.lv.pieces[st.i]), s),
          s,
          path,
        ),
      Id.Set.empty,
      run,
    );
  Rename.relabel_by(
    ~within=id => Id.Set.mem(id, on_path),
    id => Id.Map.find_opt(id, texts),
    seg,
  );
};

/* [ops] in order on the program [seg] (at [root]), each on the one before
   it left: all of them, or the first refusal and nothing. A run of plain
   token replacements is one walk, its steps all carrying the program
   after the run */
let transaction =
    (~root: Sort.t, seg: Segment.t, ops: list(op)): result(change, refusal) => {
  let c = {
    mod_root: root == Sort.Mod,
    root: root == Sort.Mod ? Sort.Mod : Sort.Exp,
  };
  let rec go = (k, seg, steps, ops) =>
    switch (ops) {
    | [] =>
      Ok({
        program: seg,
        steps: List.rev(steps),
        focus:
          switch (steps) {
          | [last, ..._] => last.focus
          | [] => None
          },
      })
    | [op, ...rest] =>
      switch (token_run(c, seg, ops)) {
      | ([_, ..._] as run, rest) =>
        let program = relabel(run, seg);
        let made =
          List.map(
            ((x, txt, _)) =>
              done_(
                ~replaced=[(x, x)],
                ~focus=Some(x),
                Replace(x, Token(txt)),
                program,
              ),
            run,
          );
        go(
          k + List.length(run),
          program,
          List.rev_append(made, steps),
          rest,
        );
      | _ =>
        switch (run(c, seg, op)) {
        | made =>
          go(
            k + 1,
            ListUtil.last(made).program,
            List.rev_append(made, steps),
            rest,
          )
        | exception (Refused(reason, id)) =>
          Error({
            reason,
            id,
            op: k,
          })
        }
      }
    };
  go(0, seg, [], ops);
};

/* a token's id names one in [seg]: a rename's planner keeps to those */
let is_token = (~root: Sort.t, id: Id.t, seg: Segment.t): bool =>
  switch (Nav.find_path(~root, id, seg)) {
  | Some(path) =>
    let (_, level, i) = Nav.home(~seg, path);
    switch (List.nth_opt(level, i)) {
    | Some(Tile({form: Tok(_), id: found, _})) => found == id
    | _ => false
    };
  | None => false
  };
