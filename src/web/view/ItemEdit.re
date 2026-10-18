module Focus = ScratchFocus;

open Haz3lcore;
open Util;

module S = ItemSpans;

let parse = (~root=Sort.Exp, txt: string): option(Segment.t) =>
  FastParse.of_text(
    ~materialize=Triggers.invoked_projector,
    ~collect_refractors=true,
    ~root,
    txt,
  );

/* a module body left with only whitespace: the parser's form for `{}` is
   one nullary tile, not a brace with an empty child */
let empty_body = (): Piece.t =>
  Piece.mk_tile(Form.classify_label(Exp, [Token.empty_module]), []);

/* a module body brace holding [members] */
let brace_of = (members: Segment.t): Piece.t =>
  Piece.mk_tile((Form.Compound(ModBody), Exp), [members]);

/* the parser's `{}` for a module, not for a signature */
let is_empty_body = (t: Tile.t): bool =>
  Tile.label(t) == [Token.empty_module] && Tile.mold(t).out == Exp;

/* a module body: braces around a member list, or `{}` */
let is_module_body = (t: Tile.t): bool =>
  Focus.holds_members(Tile.mold(t)) || is_empty_body(t);

/* a definition that is a module body */
let module_def = (def: Segment.t): bool =>
  switch (List.filter(p => !Piece.is_secondary(p), def)) {
  | [Tile(t)] => is_module_body(t)
  | _ => false
  };

/* a definition of only holes, or an empty module's `{}` */
let blank_def = (def: Segment.t): bool =>
  List.for_all(
    (p: Piece.t) =>
      switch (p) {
      | Secondary(w) => !Secondary.is_comment(w)
      | Grout(_) => true
      | Tile(t) =>
        is_empty_body(t)
        || (
          switch (Tile.label(t)) {
          | [tok] => Token.is_explicit_hole(tok)
          | _ => false
          }
        )
      | Projector(_) => false
      },
    def,
  );

/* the tile of a `let` of a value, not of a module: its body takes local
   definitions */
let value_def = ((_, _, t) as at: (Segment.t, int, Tile.t)): bool =>
  Tile.token(t, 0) == "let"
  && (
    switch (Focus.def_at(at)) {
    | Some(def) => !module_def(def)
    | None => false
    }
  );

let root_of = (mod_root: bool): Sort.t => mod_root ? Sort.Mod : Sort.Exp;

/* item [d] is a `let` of a value */
let is_value = (~mod_root: bool, d: Id.t, seg: Segment.t): bool =>
  switch (Focus.tile_ctx(~root=root_of(mod_root), d, seg)) {
  | Some(at) => value_def(at)
  | None => false
  };

/* a member's `;` */
let fresh_semi = (): Piece.t =>
  Piece.mk_tile((Form.Compound(CellJoin), Mod), []);

let first_tile_id = (seg: Segment.t): option(Id.t) =>
  Option.map((t: Tile.t) => t.id, List.find_map(Piece.is_tile, seg));

/* the outline row of a new item: a `…;` statement's is its `;` (the
   Seq's), any other item's its first tile */
let row_id = (item: Segment.t): option(Id.t) =>
  switch (ListUtil.last_opt(item)) {
  | Some(Tile(t)) when Tile.is_semi(t) => Some(t.id)
  | _ => first_tile_id(item)
  };

let text_of = (ps: Segment.t): string =>
  String.trim(MarkerParse.to_text(Zipper.unzip(ps)));

let drop_suffix = (suffix: string, s: string): string => {
  let (n, k) = (String.length(s), String.length(suffix));
  n >= k && String.sub(s, n - k, k) == suffix
    ? String.trim(String.sub(s, 0, n - k)) : s;
};

/* fresh-id pieces for one item, without edge whitespace: in member form
   (parsed as a module's, without a last `;`), or in let-in form (with its
   `in` or `;`), which is parsed with a dummy tail and then dropped */
let member_core = (txt: string): option(Segment.t) => {
  let txt = String.trim(txt);
  let txt =
    String.ends_with(~suffix=";", txt)
      ? String.sub(txt, 0, String.length(txt) - 1) : txt;
  Option.map(Focus.core_ws, parse(~root=Mod, txt));
};
let letin_core = (txt: string): option(Segment.t) =>
  Option.map(
    (sk: Segment.t) =>
      Focus.core_ws(
        switch (List.rev(sk)) {
        | [Piece.Tile(_), ...rest] => List.rev(rest)
        | _ => sk
        },
      ),
    parse(String.trim(txt) ++ "\n0"),
  );

/* a kind's keyword, and its blank definition */
let kind_kw = (kind: OutlineRename.kind): string =>
  switch (kind) {
  | KType => "type"
  | KModule => "module"
  | KValue => "let"
  };
let blank_text = (kind: OutlineRename.kind): string =>
  kind == KModule ? "{}" : {js|¿|js};

/* a new definition's text */
let template = (kind: OutlineRename.kind, name: string): string =>
  kind_kw(kind) ++ " " ++ name ++ " = " ++ blank_text(kind);
let test_template = {js|test ¿ end|js};

let space = (): Piece.t => Piece.Secondary(Secondary.mk_space(Id.mk()));
let linebreak = (): Piece.t =>
  Piece.Secondary(Secondary.mk_newline(Id.mk()));

/* the spacing after an item, for a new one beside it: as many
   linebreaks, or one space between items on a line */
let spacing = (trail: Segment.t): Segment.t =>
  switch (List.filter(Piece.is_linebreak, trail)) {
  | [] => trail == [] ? [] : [space()]
  | lbs => List.map(_ => linebreak(), lbs)
  };

/* an item's place in its block, by line. It owns its first line's
   indentation, the comment lines directly above it (no blank line
   between), and what follows it through its last linebreak. In a member
   block the terminator (`;`, a last `;` with the hole the parser leaves
   after it, or nothing after the last member) belongs to the place, not
   the item */
type slot = {
  lo: int,
  core: int, /* the item's end */
  term: int, /* its terminator's end */
  hi: int,
};

type block = {
  /* an ItemSpans level: its pieces are the block's */
  lv: S.level,
  seg: Segment.t,
  arr: array(Piece.t),
  /* a member block: its items are 2-shard members (one that is a let
     chain too), and new ones are, not `… in` forms */
  in_module: bool,
  /* a module body's: its last item, a statement without its `;`, is a
     member like the rest, not a result */
  body: bool,
  /* the segment starts a line */
  bol: bool,
  sl: array(slot),
};

let is_hole = (p: Piece.t): bool =>
  switch (p) {
  | Grout({shape: Convex, _}) => true
  | _ => false
  };

/* [lv]'s items as places, each slot by the line rule of ItemSpans'
   [lines] (which keeps whole lines only) */
let block = (~in_module: bool, lv: S.level): block => {
  let arr = lv.pieces;
  let len = Array.length(arr);
  let n = Array.length(lv.spans);
  let is_sec = i => Piece.is_secondary(arr[i]);
  let comment = i =>
    switch (arr[i]) {
    | Piece.Secondary(w) => Secondary.is_comment(w)
    | _ => false
    };
  let space = i => Piece.is_space(arr[i]);
  let lb = i => Piece.is_linebreak(arr[i]);
  let line_start = i => i == 0 ? lv.bol : lb(i - 1);
  let floor = ref(0);
  let sl =
    Array.init(
      n,
      k => {
        let sp = lv.spans[k];
        let stop = k == n - 1 ? len : sp.hi;
        /* its first piece of content, and its content's end */
        let rec first = i => i < stop && is_sec(i) ? first(i + 1) : i;
        let f = first(fst(sp.core));
        let rec back = i => i > f && is_sec(i - 1) ? back(i - 1) : i;
        let e = back(stop);
        /* a module's last member: the hole the parser leaves after its `;`
           ends its terminator */
        let (ce, hole) =
          if (in_module && k == n - 1 && e > f && is_hole(arr[e - 1])) {
            let e' = back(e - 1);
            e' > f && Piece.is_semi(arr[e' - 1])
              ? (e', Some(e - 1)) : (e, None);
          } else {
            (e, None);
          };
        let rec back_spaces = i =>
          i > floor^ && space(i - 1) ? back_spaces(i - 1) : i;
        let i = back_spaces(f);
        /* [lo] starts a line: take the line above if it holds only
           comments */
        let rec attach = lo =>
          if (lo <= floor^) {
            lo;
          } else {
            let rec back = (j, seen) =>
              j > floor^ && (space(j - 1) || comment(j - 1))
                ? back(j - 1, seen || comment(j - 1)) : (j, seen);
            let (j, seen) = back(lo - 1, false);
            seen && line_start(j) ? attach(j) : lo;
          };
        let lo = line_start(i) ? attach(i) : f;
        let (core, term) =
          if (in_module && ce > f && Piece.is_semi(arr[ce - 1])) {
            (
              ce - 1,
              switch (hole) {
              | Some(g) => g + 1
              | None => ce
              },
            );
          } else {
            (ce, ce);
          };
        /* same-line spaces and comments, then whitespace through its
           last linebreak (the next line's indentation is the next
           item's) */
        let rec same_line = i =>
          i < len && (space(i) || comment(i)) ? same_line(i + 1) : i;
        let s = same_line(term);
        let hi =
          if (s < len && lb(s)) {
            let rec last = (i, at) =>
              i < len && Focus.is_edge_ws(arr[i])
                ? last(i + 1, lb(i) ? i + 1 : at) : at;
            last(s, s + 1);
          } else {
            s;
          };
        floor := hi;
        {
          lo,
          core,
          term,
          hi,
        };
      },
    );
  {
    lv,
    seg: lv.seg,
    arr,
    in_module,
    body: Focus.is_members(lv),
    bol: lv.bol,
    sl,
  };
};

/* items that ops may move or remove: not the trailing expression, which
   a module body hasn't */
let movable = (b: block, k: int): bool =>
  k >= 0 && k < Array.length(b.sl) && (b.body || !Focus.is_tail(b.lv, k));

/* item [k]'s head, when it is a definition (or a directive): its index
   and tile */
let head = (b: block, k: int): option((int, Tile.t)) => {
  let sp = b.lv.spans[k];
  let (c0, c1) = sp.core;
  let rec go = i =>
    i >= c1
      ? None
      : (
        switch (b.arr[i]) {
        | Tile(t) when t.id == sp.anchor => Some((i, t))
        | _ => go(i + 1)
        }
      );
  switch (sp.kind) {
  | Def
  | Use => go(c0)
  | Stmt
  | Tail => None
  };
};

/* item [k]'s head, when it defines a module: its definition is a module
   body alone, whatever binds it */
let module_at = (b: block, k: int): option(Id.t) =>
  switch (head(b, k)) {
  | Some((h, t)) =>
    switch (
      Option.map(
        List.filter(p => !Piece.is_secondary(p)),
        Focus.def_at((b.seg, h, t)),
      )
    ) {
    | Some([Tile(m)]) when is_module_body(m) => Some(t.id)
    | _ => None
    }
  | None => None
  };

let indent_end = (b: block, k: int): int => {
  let rec go = i =>
    i < b.sl[k].core && Piece.is_space(b.arr[i]) ? go(i + 1) : i;
  go(b.sl[k].lo);
};
let indent = (b: block, k: int) =>
  ListUtil.slice(b.sl[k].lo, indent_end(b, k) - b.sl[k].lo, b.seg);
/* the item itself: no indentation or terminator */
let bare = (b: block, k: int) => {
  let i = indent_end(b, k);
  ListUtil.slice(i, b.sl[k].core - i, b.seg);
};
let term = (b: block, k: int) =>
  ListUtil.slice(b.sl[k].core, b.sl[k].term - b.sl[k].core, b.seg);
let trail = (b: block, k: int) =>
  ListUtil.slice(b.sl[k].term, b.sl[k].hi - b.sl[k].term, b.seg);
/* between items [k] and [k + 1]: comments neither owns */
let gap = (b: block, k: int) =>
  ListUtil.slice(b.sl[k].hi, b.sl[k + 1].lo - b.sl[k].hi, b.seg);
let starts_line = (b: block, k: int): bool =>
  b.sl[k].lo == 0 ? b.bol : Piece.is_linebreak(b.arr[b.sl[k].lo - 1]);

/* items [k] and [k + 1] trade places; indentation, terminators and the
   spacing between stay put */
let swap = (b: block, k: int): Segment.t =>
  ListUtil.truncate(b.sl[k].lo, b.seg)
  @ indent(b, k)
  @ bare(b, k + 1)
  @ term(b, k)
  @ trail(b, k)
  @ gap(b, k)
  @ indent(b, k + 1)
  @ bare(b, k)
  @ term(b, k + 1)
  @ trail(b, k + 1)
  @ ListUtil.remove_first_n(b.sl[k + 1].hi, b.seg);

/* item [k] gone; a module's last member hands its terminator to the one
   before it */
let remove = (b: block, k: int): Segment.t =>
  if (k > 0 && k == Array.length(b.sl) - 1 && b.in_module) {
    ListUtil.truncate(b.sl[k - 1].core, b.seg)
    @ term(b, k)
    @ (
      gap(b, k - 1) == [] ? trail(b, k) : trail(b, k - 1) @ gap(b, k - 1)
    )
    @ ListUtil.remove_first_n(b.sl[k].hi, b.seg);
  } else {
    ListUtil.truncate(b.sl[k].lo, b.seg)
    @ ListUtil.remove_first_n(b.sl[k].hi, b.seg);
  };

type pos =
  | Before(int)
  | After(int);

/* [item] (bare, in the block's form) as a new item at [pos], laid out
   like its neighbours: the indentation where it lands, with its other
   lines shifted to match from [src_indent], then [trail] (else the
   spacing that follows its neighbour). In a member block it takes `;`,
   or the last member's terminator when it lands last */
let insert =
    (
      b: block,
      ~pos: pos,
      ~member: bool,
      ~src_indent: option(int)=?,
      ~trail as given: option(Segment.t)=?,
      item: Segment.t,
    )
    : option(Segment.t) => {
  let n = Array.length(b.sl);
  let shifted = like => {
    let ind = List.length(indent(b, like));
    switch (src_indent) {
    | Some(src) when starts_line(b, like) && ind != src =>
      LocalReformat.shift(ind - src, item)
    | _ => item
    };
  };
  let spaces = like => List.map(_ => space(), indent(b, like));
  let trail_or = default =>
    switch (given) {
    | Some(t) when t != [] => t
    | _ => default
    };
  /* at [at], laid out like item [like], then [default] spacing */
  let splice = (at, like, default) =>
    ListUtil.truncate(at, b.seg)
    @ spaces(like)
    @ shifted(like)
    @ (member ? [fresh_semi()] : [])
    @ trail_or(default)
    @ ListUtil.remove_first_n(at, b.seg);
  switch (pos) {
  | After(k) when k == n - 1 =>
    /* the new last member: it takes the old last's terminator, which
       takes `;` */
    member && b.in_module
      ? {
        let sep = trail(b, k) == [] ? [space()] : trail(b, k);
        Some(
          ListUtil.truncate(b.sl[k].core, b.seg)
          @ [fresh_semi()]
          @ sep
          @ spaces(k)
          @ shifted(k)
          @ term(b, k)
          @ trail_or(spacing(trail(b, k)))
          @ ListUtil.remove_first_n(b.sl[k].hi, b.seg),
        );
      }
      : None
  | After(k) => Some(splice(b.sl[k].hi, k + 1, spacing(trail(b, k))))
  | Before(p) =>
    Some(
      splice(
        b.sl[p].lo,
        p,
        p > 0
          ? spacing(trail(b, p - 1))
          : [starts_line(b, p) ? linebreak() : space()],
      ),
    )
  };
};

/* [op] on item [j] of its own block: an op invalid there (a move at the
   block's edge) no-ops rather than act on the enclosing item */
let apply_at =
    (~name: option(string)=?, op: OutlineEdit.def_op, b: block, j: int)
    : option((Segment.t, option(Id.t))) => {
  let named = placeholder => Option.value(name, ~default=placeholder);
  let n = Array.length(b.sl);
  let movable = movable(b);
  /* below the trailing expression would strand it above the new item:
     the new item goes above it. A member block has no trailing
     expression: its last member, `;` or not, takes new items after it */
  let new_item = (~above=false, ~member, item) => {
    let trailing = !movable(j) && !b.in_module;
    insert(b, ~pos=above || trailing ? Before(j) : After(j), ~member, item)
    |> Option.map(seg => (seg, row_id(item)));
  };
  if (j >= n) {
    None;
  } else {
    switch (op) {
    | Delete when movable(j) => Some((remove(b, j), None))
    | Delete => None
    | MoveUp when movable(j - 1) && movable(j) =>
      Some((swap(b, j - 1), None))
    | MoveDown when movable(j) && movable(j + 1) =>
      Some((swap(b, j), None))
    | MoveUp
    | MoveDown
    /* across levels: [indent] and [outdent] */
    | Indent
    | Outdent => None
    | NewBelow
    | NewTypeBelow
    | NewModuleBelow =>
      let member = b.in_module;
      let txt =
        switch (op) {
        | NewTypeBelow => template(KType, named("NewType"))
        | NewModuleBelow => template(KModule, named("NewModule"))
        | _ => template(KValue, named("new_def"))
        };
      Option.bind(
        member ? member_core(txt) : letin_core(txt ++ " in"),
        new_item(~member),
      );
    | NewTestBelow
    | NewTestAbove =>
      let member = b.in_module;
      Option.bind(
        member
          ? member_core(test_template) : letin_core(test_template ++ ";"),
        new_item(~above=op == NewTestAbove, ~member),
      );
    | Duplicate when movable(j) =>
      let member = b.in_module;
      let txt = text_of(bare(b, j));
      Option.bind(
        member ? member_core(txt) : letin_core(txt),
        new_item(~member),
      );
    | Duplicate => None
    };
  };
};

/* the program [seg] with [path]'s last level made [level]; a module body
   left with only whitespace is the parser's `{}` */
let rebuild =
    (~seg: Segment.t, path: list(Focus.step), level: Segment.t): Segment.t =>
  switch (List.rev(path)) {
  | [{from: Kid(t, 0), _}, ...[above, ..._] as rest]
      when
        Tile.label(t) == ["{", "}"]
        && List.length(t.children) == 1
        && List.for_all(Focus.is_edge_ws, level) =>
    Focus.rebuild(
      ~seg,
      List.rev(rest),
      ListUtil.put_nth(above.i, empty_body(), Focus.pieces_of(~seg, above)),
    )
  | _ => Focus.rebuild(~seg, path, level)
  };

/* the item for row [fid]: the block holding it, the item's index there,
   and the program with that block replaced. The block is the innermost
   level whose item holds [fid] (ItemSpans' levels: module members, a
   member's let chain or body run, a definition's body). A level is a
   member block when it is a module body's member list, or the top of a
   module-rooted program */
let locate =
    (~mod_root: bool, fid: Id.t, seg: Segment.t)
    : option((block, int, Segment.t => Segment.t)) =>
  switch (Focus.find_path(~root=root_of(mod_root), fid, seg)) {
  | Some(path) when !Focus.projected(path) =>
    /* innermost first */
    let rec up = (rev: list(Focus.step)) =>
      switch (rev) {
      | [] => None
      | [st, ...outer] =>
        switch (Focus.item_at(st.lv, st.i)) {
        | Some(j) =>
          let in_module =
            switch (st.from) {
            | Top => mod_root
            | Kid(_)
            | Sub
            | Syntax => Focus.is_members(st.lv)
            };
          Some((
            block(~in_module, st.lv),
            j,
            rebuild(~seg, List.rev(rev)),
          ));
        | None => up(outer)
        }
      };
    up(List.rev(path));
  | _ => None
  };

let apply =
    (
      ~name: option(string)=?,
      ~mod_root=false,
      op: OutlineEdit.def_op,
      fid: Id.t,
      seg: Segment.t,
    )
    : option((Segment.t, option(Id.t))) =>
  Option.bind(locate(~mod_root, fid, seg), ((b, j, rebuild)) =>
    apply_at(~name?, op, b, j)
    |> Option.map(((level, target)) => (rebuild(level), target))
  );

/* module [m]'s definition and its members' level: its braces' child */
let members_of =
    (~mod_root: bool, m: Id.t, seg: Segment.t): option((Segment.t, S.level)) =>
  switch (Focus.tile_at(~root=root_of(mod_root), m, seg)) {
  | Some((up, level, i, t)) =>
    let lv = ListUtil.last(up).lv;
    switch (
      Option.bind(Focus.def_at((level, i, t)), def =>
        Option.map(b => (def, b), Focus.brace_at(def))
      )
    ) {
    | Some((def, (j, brace))) =>
      let literal =
        switch (Focus.slice_at(lv, i)) {
        | Some(k) when lv.spans[k].body == Some(brace.id) => S.members(lv, k)
        | _ => None
        };
      let members =
        switch (literal) {
        | Some(members) => members
        | None when Tile.ends_with_in(t) =>
          let c = List.length(t.children) - 1;
          Focus.kid_level(Focus.kid_level(lv, i, t, c), j, brace, 0);
        | None => Focus.kid_level(lv, i + 1 + j, brace, 0)
        };
      Some((def, members));
    | None => None
    };
  | None => None
  };

/* [item] (member form) into module [m]'s members: after the last, or
   ([first]) before the first */
let into_members =
    (
      ~mod_root: bool,
      ~first=false,
      ~src_indent: option(int)=?,
      ~trail: option(Segment.t)=?,
      m: Id.t,
      item: Segment.t,
      seg: Segment.t,
    )
    : option(Segment.t) =>
  switch (members_of(~mod_root, m, seg)) {
  | None => None
  | Some((def_seg, members)) =>
    let b = block(~in_module=true, members);
    let n = Array.length(b.sl);
    n == 0
      ? None
      : insert(
          b,
          ~pos=first ? Before(0) : After(n - 1),
          ~member=true,
          ~src_indent?,
          ~trail?,
          item,
        )
        |> Option.map(members =>
             Focus.splice_def(
               ~root=root_of(mod_root),
               m,
               Focus.with_brace_child(def_seg, members),
               seg,
             )
           );
  };

/* [item] (member form) as the only member of module [m], whose body is
   the parser's empty `{}`: on its own line, indented past the module */
let into_empty =
    (
      ~mod_root: bool,
      ~src_indent: option(int)=?,
      m: Id.t,
      item: Segment.t,
      seg: Segment.t,
    )
    : option(Segment.t) => {
  let root = root_of(mod_root);
  switch (Focus.find_def(~root, m, seg)) {
  | None => None
  | Some(def_seg) =>
    let empty = (p: Piece.t) =>
      switch (p) {
      | Tile(t) => is_empty_body(t)
      | _ => false
      };
    let outer = Option.value(Focus.line_indent(~root, m, seg), ~default=0);
    let inner = outer + 2;
    let item =
      switch (src_indent) {
      | Some(src) when src != inner => LocalReformat.shift(inner - src, item)
      | _ => item
      };
    let brace =
      brace_of(
        [linebreak()]
        @ Indentation.make_indent_spaces(inner)
        @ item
        @ [linebreak()]
        @ Indentation.make_indent_spaces(outer),
      );
    List.exists(empty, def_seg)
      ? Some(
          Focus.splice_def(
            ~root,
            m,
            List.map(p => empty(p) ? brace : p, def_seg),
            seg,
          ),
        )
      : None;
  };
};

/* a function's head: `fun … ->` or `typfun … ->` */
let is_fun_head = (p: Piece.t): bool =>
  switch (p) {
  | Tile(t) =>
    switch (Tile.label(t)) {
    | ["fun", "->"]
    | ["typfun", "->"] => true
    | _ => false
    }
  | _ => false
  };

/* a function only inside parentheses or a projector: a local definition
   in front of it would land outside its lambda */
let wrapped_fun = (def: Segment.t): bool => {
  let solid = seg => List.filter(p => !Piece.is_secondary(p), seg);
  let rec within = (seg: Segment.t) =>
    switch (solid(seg)) {
    | [p, ..._] when is_fun_head(p) => true
    | [Tile({children: [kid], _} as t)] when Tile.label(t) == ["(", ")"] =>
      within(kid)
    | [Projector(pr)] => within([pr.syntax])
    | _ => false
    };
  switch (solid(def)) {
  | [p, ..._] when is_fun_head(p) => false
  | _ => within(def)
  };
};

/* [item] (in let-in form) as value [d]'s last local definition: just
   before its body's result, inside a function's lambda, spaced as the
   body is (on its line when it's one line) */
let body_last =
    (
      ~mod_root: bool,
      ~src_indent: option(int)=?,
      d: Id.t,
      item: Segment.t,
      seg: Segment.t,
    )
    : option(Segment.t) => {
  let root = root_of(mod_root);
  switch (Focus.tile_at(~root, d, seg)) {
  | Some((up, level, i, t)) when value_def((level, i, t)) =>
    switch (Focus.def_at((level, i, t))) {
    | Some(body) when wrapped_fun(body) => None
    | None => None
    | Some(body) =>
      let lv = ListUtil.last(up).lv;
      let outer = S.outer(lv);
      let kid = Tile.ends_with_in(t);
      /* its body's items, a lone result too */
      let body_lv =
        S.build(
          ~sort=Exp,
          ~owner=Block(t.id),
          ~nested=false,
          ~bol=false,
          ~eol=false,
          ~indent=
            Some(
              kid
                ? S.kid_indent(
                    ~outer,
                    lv.pieces,
                    i,
                    t,
                    List.length(t.children) - 1,
                  )
                : S.indent_at(~outer, lv.pieces, i + 1),
            ),
          ~offset=kid ? 0 : i + 1,
          body,
        );
      let b = block(~in_module=false, body_lv);
      let n = Array.length(b.sl);
      n > 0 && Focus.is_tail(body_lv, n - 1)
        ? insert(b, ~pos=Before(n - 1), ~member=false, ~src_indent?, item)
          |> Option.map(body => Focus.splice_def(~root, d, body, seg))
        : None;
    }
  | _ => None
  };
};

/* [member] into module [fid]'s body, at any depth: last, or [first]; or
   into value [fid]'s, as its last local definition */
let new_inside =
    (
      ~mod_root: bool,
      ~first=false,
      ~member: string,
      fid: Id.t,
      seg: Segment.t,
    )
    : option((Segment.t, option(Id.t))) =>
  is_value(~mod_root, fid, seg)
    ? first
        ? None
        : Option.bind(letin_core(member ++ " in"), item =>
            body_last(~mod_root, fid, item, seg)
            |> Option.map(seg => (seg, row_id(item)))
          )
    : Option.bind(member_core(member), item =>
        (
          switch (into_members(~mod_root, ~first, fid, item, seg)) {
          | Some(_) as r => r
          | None => into_empty(~mod_root, fid, item, seg)
          }
        )
        |> Option.map(seg => (seg, first_tile_id(item)))
      );

/* an item's bare pieces in a block's form: members end in their
   place's `;`, other blocks use `… in`. Converting goes through text
   (fresh ids); a `use` has no member form, so it reads as none */
let in_form =
    (~member: bool, ~was_member: bool, ps: Segment.t): option(Segment.t) =>
  switch (member, was_member) {
  | (true, true)
  | (false, false) => Some(ps)
  | (true, false) => member_core(drop_suffix("in", text_of(ps)))
  | (false, true) =>
    /* a definition takes `in`; a statement (a test) keeps its `;` */
    let def =
      switch (List.find_map(Piece.is_tile, ps)) {
      | Some(t) => Focus.is_def_head(t)
      | None => false
      };
    letin_core(text_of(ps) ++ (def ? " in" : ";"));
  };

/* item [k]'s head, when it's a `let` of a value */
let value_at = (b: block, k: int): option(Id.t) =>
  switch (head(b, k)) {
  | Some((h, t)) when value_def((b.seg, h, t)) => Some(t.id)
  | _ => None
  };

type spot = {
  /* the item is a 2-shard member */
  s_member: bool,
  s_item: Segment.t,
  s_trail: Segment.t,
  /* the neighbour in the move's direction, if it defines a module, or a
     value */
  s_module: option(Id.t),
  s_value: option(Id.t),
  s_edge: bool,
};

/* where the item holding [fid] sits, seen from its own block */
let spot = (~mod_root, ~up: bool, fid: Id.t, seg: Segment.t): option(spot) =>
  Option.bind(
    locate(~mod_root, fid, seg),
    ((b, j, _)) => {
      let k = up ? j - 1 : j + 1;
      j < Array.length(b.sl)
        ? Some({
            s_member: b.in_module,
            s_item: bare(b, j),
            s_trail: trail(b, j),
            s_module: movable(b, k) ? module_at(b, k) : None,
            s_value: movable(b, k) ? value_at(b, k) : None,
            s_edge: !movable(b, k),
          })
        : None;
    },
  );

/* [item] placed just above ([up]) or below the item holding [target],
   in that block's form */
let insert_near =
    (
      ~mod_root,
      ~up: bool,
      ~src_indent: option(int)=?,
      ~trail: Segment.t,
      target: Id.t,
      item: Segment.t,
      seg: Segment.t,
    )
    : option(Segment.t) =>
  Option.bind(locate(~mod_root, target, seg), ((b, j, rebuild)) =>
    j < Array.length(b.sl)
      ? insert(
          b,
          ~pos=up ? Before(j) : After(j),
          ~member=b.in_module,
          ~src_indent?,
          ~trail,
          item,
        )
        |> Option.map(rebuild)
      : None
  );

/* The item [s] holding [fid] moved across a level's edge: each way below
   takes it out of [seg], puts it in where it lands, reindented, and
   gives back the row it lands on */

/* the indentation of the line item [s] starts on */
let start_indent = (~mod_root: bool, s: spot, seg: Segment.t): option(int) =>
  switch (s.s_item) {
  | [p, ..._] =>
    Focus.line_indent(~root=root_of(mod_root), Piece.id(p), seg)
  | [] => None
  };

/* the spacing item [s] takes along: its lines, not a space it had on a
   line it shared (the spacing where it lands then follows it) */
let own_lines = (s: spot): Segment.t =>
  List.exists(Piece.is_linebreak, s.s_trail) ? s.s_trail : [];

/* into module [m]'s members: last, or [first]; an empty body takes it as
   its only member */
let into_module =
    (~mod_root, ~first: bool, s: spot, fid: Id.t, m: Id.t, seg: Segment.t)
    : option((Segment.t, option(Id.t))) =>
  switch (
    apply(~mod_root, Delete, fid, seg),
    in_form(~member=true, ~was_member=s.s_member, s.s_item),
  ) {
  | (Some((seg1, _)), Some(item)) =>
    let src_indent = start_indent(~mod_root, s, seg);
    (
      switch (
        into_members(
          ~mod_root,
          ~first,
          ~src_indent?,
          ~trail=own_lines(s),
          m,
          item,
          seg1,
        )
      ) {
      | Some(_) as r => r
      | None => into_empty(~mod_root, ~src_indent?, m, item, seg1)
      }
    )
    |> Option.map(seg2 => (seg2, row_id(item)));
  | _ => None
  };

/* into value [d]'s body, as its last local definition */
let into_body =
    (~mod_root, s: spot, fid: Id.t, d: Id.t, seg: Segment.t)
    : option((Segment.t, option(Id.t))) =>
  switch (
    apply(~mod_root, Delete, fid, seg),
    in_form(~member=false, ~was_member=s.s_member, s.s_item),
  ) {
  | (Some((seg1, _)), Some(item)) =>
    body_last(
      ~mod_root,
      ~src_indent=?start_indent(~mod_root, s, seg),
      d,
      item,
      seg1,
    )
    |> Option.map(seg2 => (seg2, row_id(item)))
  | _ => None
  };

/* just above or below the item holding [target], in that block's form */
let beside =
    (~mod_root, ~up: bool, s: spot, fid: Id.t, target: Id.t, seg: Segment.t)
    : option((Segment.t, option(Id.t))) =>
  switch (
    apply(~mod_root, Delete, fid, seg),
    spot(~mod_root, ~up, target, seg),
  ) {
  | (Some((seg1, _)), Some(ts)) =>
    switch (in_form(~member=ts.s_member, ~was_member=s.s_member, s.s_item)) {
    | Some(item) =>
      insert_near(
        ~mod_root,
        ~up,
        ~src_indent=?start_indent(~mod_root, s, seg),
        ~trail=own_lines(s),
        target,
        item,
        seg1,
      )
      |> Option.map(seg2 => (seg2, row_id(item)))
    | None => None
    }
  | _ => None
  };

/* Alt↑↓: into an expanded module beside the item, bound by `module` or
   `let` (its end going up, its start going down); from a module's first
   or last member, out to just above or below it. Modules not [is_open]
   are stepped over, as is any module by an item with no member form (a
   `use`), and function bodies keep their items. [owner]: the item's
   module */
let move =
    (
      ~mod_root: bool,
      ~is_open: Id.t => bool,
      ~owner: option(Id.t),
      ~up: bool,
      fid: Id.t,
      seg: Segment.t,
    )
    : option((Segment.t, option(Id.t))) =>
  switch (spot(~mod_root, ~up, fid, seg)) {
  | None => None
  | Some(s) =>
    let step = () =>
      s.s_edge
        ? None
        : apply(~mod_root, up ? MoveUp : MoveDown, fid, seg)
          |> Option.map(((seg, _)) => (seg, Some(fid)));
    switch (s.s_module) {
    | Some(k) when is_open(k) =>
      switch (into_module(~mod_root, ~first=!up, s, fid, k, seg)) {
      | Some(_) as r => r
      | None => step()
      }
    | _ when !s.s_edge => step()
    | _ => Option.bind(owner, m => beside(~mod_root, ~up, s, fid, m, seg))
    };
  };

/* Tab: the item holding [fid] into the item just above it, in place, a
   level deeper: a module's last member, or a value's last local
   definition. Only into a row the outline [shown]s */
let indent =
    (~mod_root: bool, ~shown: Id.t => bool, fid: Id.t, seg: Segment.t)
    : option((Segment.t, option(Id.t))) =>
  switch (spot(~mod_root, ~up=true, fid, seg)) {
  | Some({s_module: Some(m), _} as s) when shown(m) =>
    into_module(~mod_root, ~first=false, s, fid, m, seg)
  | Some({s_value: Some(d), _} as s) when shown(d) =>
    into_body(~mod_root, s, fid, d, seg)
  | _ => None
  };

/* ⇧Tab: the item holding [fid], the last of [owner]'s members or local
   definitions, out to just after [owner], in place, a level out */
let outdent =
    (~mod_root: bool, ~owner: Id.t, fid: Id.t, seg: Segment.t)
    : option((Segment.t, option(Id.t))) =>
  switch (
    spot(~mod_root, ~up=false, fid, seg),
    Focus.find_def(~root=root_of(mod_root), owner, seg),
  ) {
  | (Some({s_edge: true, _} as s), Some(def))
      when Focus.seg_contains_id(fid, def) =>
    beside(~mod_root, ~up=false, s, fid, owner, seg)
  | _ => None
  };

/* how the outline edits a program's items, addressed by id */
type t =
  | Op(OutlineEdit.def_op, Id.t)
  /* as typed: a kind keyword first changes the item's kind */
  | Rename(Id.t, string)
  /* a new definition below the row: `name`, `type Name` or
     `module Name` */
  | Insert(Id.t, string)
  /* the same, last inside a module */
  | InsertInside(Id.t, string)
  /* or first */
  | InsertFirst(Id.t, string)
  /* a new definition with a hole for its name, below the row or first or
     last inside it, its name to be typed in the editor */
  | Blank(OutlineEdit.place, Id.t, OutlineTree.kind);

/* the op that makes a new definition of [kind] below an item */
let new_op = (kind: OutlineRename.kind): OutlineEdit.def_op =>
  switch (kind) {
  | KType => NewTypeBelow
  | KModule => NewModuleBelow
  | KValue => NewBelow
  };

let rename_kind = (kind: OutlineTree.kind): OutlineRename.kind =>
  switch (kind) {
  | KType => KType
  | KModule => KModule
  | _ => KValue
  };

/* where item [id]'s name is typed: its pattern's first piece */
let name_slot = (~mod_root: bool, id: Id.t, seg: Segment.t): option(Id.t) =>
  Option.bind(Focus.find_pat(~root=root_of(mod_root), id, seg), pat =>
    List.find_opt(p => !Piece.is_secondary(p), pat) |> Option.map(Piece.id)
  );

/* a name's text, holes blank */
let rec text = (seg: Segment.t): string =>
  String.concat("", List.map(piece_text, seg))
and piece_text = (p: Piece.t): string =>
  switch (p) {
  | Tile(t) =>
    Aba.mk(t.shards, t.children)
    |> Aba.join(List.nth(Tile.label(t)), text)
    |> String.concat("")
  | Grout(_) => ""
  | Secondary(w) => Secondary.get_string(w.content)
  | Projector(pr) => piece_text(pr.syntax)
  };

/* a name as the editor holds it: the text of item [def]'s pattern (or,
   with no [def], of the zipper's whole segment, a cell's header), the
   caret's offset in it and the selection's other end (the caret's offset
   when nothing is selected), in JS string units: when the caret and the
   selection are in the name */
let name_at = (~def: option(Id.t), z: Zipper.t): option((string, int, int)) => {
  let (pre, suf) = z.relatives.siblings;
  /* the caret's text before and after it, from the innermost level out
     to the name's level: None when the caret isn't in the name */
  let rec out = (before, after, ancestors: Ancestors.t) =>
    switch (ancestors, def) {
    | ([], None) => Some((before, after))
    | ([], Some(_)) => None
    | ([(a, (gpre, gsuf)), ...rest], _) =>
      switch (def) {
      | Some(d) when a.id == d =>
        fst(a.children) == [] ? Some((before, after)) : None
      | _ =>
        let (l, r) = Ancestor.disassemble(a);
        out(
          text(gpre) ++ text(l) ++ before,
          after ++ text(r) ++ text(gsuf),
          rest,
        );
      }
    };
  let selecting = z.selection.content != [];
  let selected = text(z.selection.content);
  /* an inner caret splits the token to its right; a selection's is in the
     selection's own token, which sits between the siblings */
  let (before, after) =
    switch (z.caret, suf) {
    | (Inner(n), [p, ...rest]) when !selecting =>
      let t = piece_text(p);
      (
        text(pre) ++ OutlineEdit.slice(t, 0, n + 1),
        OutlineEdit.slice(t, n + 1, OutlineEdit.units(t)) ++ text(rest),
      );
    | _ => (text(pre), text(suf))
    };
  /* what's selected of it: a selection's end can be inside a token */
  let (skipped, picked) =
    !selecting
      ? (0, "")
      : (
        OutlineEdit.units(
          fst(
            Unicode.split_nth(
              selected,
              min(
                fst(Zipper.selection_trim_offsets(z)),
                Unicode.length(selected),
              ),
            ),
          ),
        ),
        Zipper.trim_selected_text(z, selected),
      );
  Option.bind(
    out(before, after, z.relatives.ancestors),
    ((before, after)) => {
      let all = before ++ selected ++ after;
      let rec lead = i =>
        i < String.length(all) && String.contains(" \t\n\r", all.[i])
          ? lead(i + 1) : i;
      let name = String.trim(all);
      let from =
        OutlineEdit.units(before)
        + skipped
        - OutlineEdit.units(String.sub(all, 0, lead(0)));
      let clamp = i => max(0, min(OutlineEdit.units(name), i));
      let (lo, hi) = (
        clamp(from),
        clamp(from + OutlineEdit.units(picked)),
      );
      /* a selection of the space around the name isn't in it */
      OutlineEdit.slice(name, lo, hi) != picked
        ? None
        : z.selection.focus == Left
            ? Some((name, lo, hi)) : Some((name, hi, lo));
    },
  );
};

/* a typed name and the kind its keyword picks */
let typed_name = (text: string): (OutlineTree.kind, string) => {
  let (kind, prefix) = OutlineEdit.new_kind(text);
  (
    kind,
    String.trim(
      String.sub(
        text,
        String.length(prefix),
        String.length(text) - String.length(prefix),
      ),
    ),
  );
};

/* whether item [it] (row [row]) can be a [kind]'s, keeping its definition
   (else it's remade blank): a definition of only holes can be any kind's,
   a `{…}` a value's or a module's. Only a plain name changes kind, or one
   under an annotation, which stays with its definition */
let refit =
    (
      ~mod_root: bool,
      it: OutlineRename.item,
      kind: OutlineRename.kind,
      row: Id.t,
      seg,
    )
    : result(bool, string) =>
  switch (Focus.tile_ctx(~root=root_of(mod_root), row, seg)) {
  | None => Error("this row has no name to change")
  | Some((_, _, t) as at) =>
    let def = Option.value(Focus.def_at(at), ~default=[]);
    let named =
      switch (List.filter(p => !Piece.is_secondary(p), Focus.pat_of(t))) {
      | [Tile(b)] => b.id == it.binder
      | [Tile(b), Tile(colon), ..._] =>
        b.id == it.binder && Tile.label(colon) == [":"]
      | _ => false
      };
    switch (it.kind, kind) {
    | _ when !named => Error("only a plain name changes kind")
    | _ when blank_def(def) => Ok(false)
    | (KValue, KModule)
    | (KModule, KValue) when module_def(def) => Ok(true)
    | (_, KModule) => Error({js|only a {…} body can be a module|js})
    | (_, KType) => Error("a type needs a type body")
    | (KType, KValue) => Error("a value needs a value body")
    | (_, KValue) => Error({js|only a {…} module can be a value|js})
    };
  };

/* [pat] with the first token reading [name] given id [id] */
let with_binder = (name: string, id: Id.t, pat: Segment.t): Segment.t => {
  let rec go = (found, seg: Segment.t) =>
    List.fold_left_map(
      (found, p: Piece.t) =>
        switch (p) {
        | Tile(t) when !found && Tile.label(t) == [name] => (
            true,
            Piece.Tile({
              ...t,
              id,
            }),
          )
        | Tile(t) =>
          let (found, children) = List.fold_left_map(go, found, t.children);
          (
            found,
            Piece.Tile({
              ...t,
              children,
            }),
          );
        | p => (found, p)
        },
      found,
      seg,
    );
  snd(go(false, pat));
};

/* item [row] remade as a [kind]'s, named [name], keeping its id and its
   binding token's ([binder]). With [keep], its definition stays, and its
   pattern's annotation with it; else it's blank, as a new one is. The
   head is parsed from text, in the item's own form */
let rekind =
    (
      ~mod_root: bool,
      ~binder: Id.t,
      ~keep: bool,
      row: Id.t,
      kind: OutlineRename.kind,
      name: string,
      seg: Segment.t,
    )
    : option(Segment.t) =>
  switch (Focus.tile_ctx(~root=root_of(mod_root), row, seg)) {
  | None => None
  | Some((_, _, t)) =>
    let member = !Tile.ends_with_in(t);
    let pat = Focus.pat_of(t);
    let pat_text =
      keep
        ? String.trim(text(OutlineRename.relabel([binder], name, pat)))
        : name;
    let head =
      kind_kw(kind)
      ++ " "
      ++ pat_text
      ++ " = "
      ++ (keep ? "{}" : blank_text(kind));
    /* [core] in [old]'s spacing */
    let respace = (old: Segment.t, core: Segment.t) => {
      let (pre, _, suf) = Focus.trim_ws(old);
      pre @ Focus.core_ws(core) @ suf;
    };
    switch (member ? member_core(head) : letin_core(head ++ " in")) {
    | Some([Tile(h), ...rest]) =>
      let pat = respace(pat, with_binder(name, binder, Focus.pat_of(h)));
      let made = (children: list(Segment.t)) =>
        Piece.Tile({
          ...h,
          id: t.id,
          children,
        });
      Some(
        Focus.splice_at(
          ~root=root_of(mod_root),
          row,
          (level, i, t) =>
            if (member) {
              let (def, after) =
                Focus.split_at_semi(ListUtil.remove_first_n(i + 1, level));
              ListUtil.truncate(i, level)
              @ [made([pat])]
              @ (keep ? def : respace(def, rest))
              @ after;
            } else {
              let def = ListUtil.last(t.children);
              let def' = ListUtil.last(h.children);
              ListUtil.put_nth(
                i,
                made([pat, keep ? def : respace(def, def')]),
                level,
              );
            },
          seg,
        ),
      );
    | _ => None
    };
  };

/* a rename typed on an item's line, checked. A kind keyword typed first
   changes its kind (its own kind's is dropped): the new kind, and whether
   its definition stays */
type renaming = {
  rn_item: OutlineRename.item,
  rn_name: string,
  rn_kind: option((OutlineRename.kind, bool)),
  rn_refs: list(Id.t),
};

/* renaming row [row] as [typed]: nothing to do, or why not */
let plan_rename =
    (
      ~mod_root: bool,
      ~info_map: Lazy.t(Language.Statics.Map.t),
      ~term: Language.Exp.t,
      ~seg: Lazy.t(Segment.t),
      row: Id.t,
      typed: string,
    )
    : result(option(renaming), string) => {
  let (kw, rest) = OutlineEdit.split_typed(typed);
  let name = String.trim(rest);
  switch (OutlineRename.find_item(row, term)) {
  | None => Error("this row has no name to change")
  | Some(it) =>
    let kind =
      kw == "" ? it.kind : rename_kind(fst(OutlineEdit.new_kind(kw)));
    let change =
      kind == it.kind
        ? Ok(None)
        : Result.map(
            ~f=keep => Some((kind, keep)),
            refit(~mod_root, it, kind, row, Lazy.force(seg)),
          );
    Result.bind(change, ~f=change =>
      OutlineRename.prepare(~info_map, ~kind, it, name)
      |> Result.map(~f=refs =>
           Option.map(
             rn_refs =>
               {
                 rn_item: it,
                 rn_name: name,
                 rn_kind: change,
                 rn_refs,
               },
             refs,
           )
         )
    );
  };
};

/* the program renamed as planned, at row [row] */
let apply_rename =
    (~mod_root: bool, rn: renaming, row: Id.t, seg: Segment.t)
    : option(Segment.t) =>
  switch (rn.rn_kind) {
  | None =>
    Some(
      OutlineRename.relabel(
        [rn.rn_item.binder, ...rn.rn_refs],
        rn.rn_name,
        seg,
      ),
    )
  | Some((kind, keep)) =>
    rekind(
      ~mod_root,
      ~binder=rn.rn_item.binder,
      ~keep,
      row,
      kind,
      rn.rn_name,
      OutlineRename.relabel(rn.rn_refs, rn.rn_name, seg),
    )
  };

type ctx = {
  mod_root: bool,
  term: Language.Exp.t,
  info_map: Lazy.t(Language.Statics.Map.t),
  /* whether a module row is expanded: moves step over collapsed ones,
     and over modules the outline doesn't show (`let _ = {…}`) */
  is_open: Id.t => bool,
};

/* row [fid]'s parent row in [term]'s outline, and its kind */
let parent_row =
    (term: Language.Exp.t, fid: Id.t): option((Id.t, OutlineTree.kind)) =>
  switch (Option.map(List.rev, OutlineTree.trail_of(fid, term))) {
  | Some([_, parent, ..._]) =>
    Option.map(k => (parent, k), OutlineTree.kind_of(parent, term))
  | _ => None
  };

/* the new program and the row the edit lands on, or why not */
let edit =
    (ctx: ctx, e: t, seg: Segment.t)
    : result((Segment.t, option(Id.t)), string) =>
  switch (e) {
  | Op((MoveUp | MoveDown) as op, fid) =>
    let owner =
      switch (parent_row(ctx.term, fid)) {
      | Some((parent, KModule)) => Some(parent)
      | _ => None
      };
    move(
      ~mod_root=ctx.mod_root,
      ~is_open=ctx.is_open,
      ~owner,
      ~up=op == MoveUp,
      fid,
      seg,
    )
    |> Option.to_result(~none="it can't move there");
  | Op(Indent, fid) =>
    indent(
      ~mod_root=ctx.mod_root,
      ~shown=id => OutlineTree.node_of(id, ctx.term) != None,
      fid,
      seg,
    )
    |> Option.to_result(~none="it can't go in there")
  | Op(Outdent, fid) =>
    (
      switch (parent_row(ctx.term, fid)) {
      | Some((owner, KModule | KFn | KConst)) =>
        outdent(~mod_root=ctx.mod_root, ~owner, fid, seg)
      | _ => None
      }
    )
    |> Option.to_result(~none="it can't go out there")
  | Op(op, fid) =>
    apply(~mod_root=ctx.mod_root, op, fid, seg)
    |> Option.to_result(~none="it can't go here")
  | Rename(row, typed) =>
    switch (
      plan_rename(
        ~mod_root=ctx.mod_root,
        ~info_map=ctx.info_map,
        ~term=ctx.term,
        ~seg=Lazy.from_val(seg),
        row,
        typed,
      )
    ) {
    | Error(why) => Error(why)
    | Ok(None) => Ok((seg, Some(row)))
    | Ok(Some(rn)) =>
      apply_rename(~mod_root=ctx.mod_root, rn, row, seg)
      |> Option.to_result(~none="it can't be renamed")
      |> Result.map(~f=seg => (seg, Some(row)))
    }
  | Insert(at, text)
  | InsertInside(at, text)
  | InsertFirst(at, text) =>
    let (kind, name) = typed_name(text);
    let rkind = rename_kind(kind);
    switch (OutlineRename.check_name(rkind, name)) {
    | Some(why) => Error(why)
    | None =>
      (
        switch (e) {
        | InsertInside(_) =>
          new_inside(
            ~mod_root=ctx.mod_root,
            ~member=template(rkind, name),
            at,
            seg,
          )
        | InsertFirst(_) =>
          new_inside(
            ~mod_root=ctx.mod_root,
            ~first=true,
            ~member=template(rkind, name),
            at,
            seg,
          )
        | _ => apply(~name, ~mod_root=ctx.mod_root, new_op(rkind), at, seg)
        }
      )
      |> Option.to_result(~none="a definition can't go here")
    };
  | Blank(place, at, kind) =>
    /* made with a placeholder name, which a hole replaces: not every
       pattern sort parses one (`module ¿`) */
    let rkind = rename_kind(kind);
    let name =
      switch (rkind) {
      | KType => "T"
      | KModule => "M"
      | KValue => "x"
      };
    (
      switch (place) {
      | Below => apply(~name, ~mod_root=ctx.mod_root, new_op(rkind), at, seg)
      | First =>
        new_inside(
          ~mod_root=ctx.mod_root,
          ~first=true,
          ~member=template(rkind, name),
          at,
          seg,
        )
      | Last =>
        new_inside(
          ~mod_root=ctx.mod_root,
          ~member=template(rkind, name),
          at,
          seg,
        )
      }
    )
    |> Option.to_result(~none="a definition can't go here")
    |> Result.map(~f=((seg, d)) =>
         switch (
           Option.bind(d, d =>
             Focus.find_pat(~root=root_of(ctx.mod_root), d, seg)
           ),
           d,
         ) {
         | (Some(pat), Some(d)) =>
           let hole = (p: Piece.t) =>
             Piece.is_secondary(p)
               ? p
               : Grout({
                   id: Id.mk(),
                   shape: Convex,
                 });
           (
             Focus.splice_pat(
               ~root=root_of(ctx.mod_root),
               d,
               List.map(hole, pat),
               seg,
             ),
             Some(d),
           );
         | _ => (seg, d)
         }
       );
  };
