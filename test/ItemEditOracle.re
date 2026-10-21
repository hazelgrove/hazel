/* ItemEdit as of 72411578a6, before it read ItemSpans: frozen, over
   FocusOracle, the oracle Test_CellsParity and Test_ItemSpans compare
   against */
open Web;
module Focus = FocusOracle;

open Haz3lcore;
open Util;

/* unchanged helpers are the live module's */
let parse = Web.ItemEdit.parse;
let empty_body = Web.ItemEdit.empty_body;
let brace_of = Web.ItemEdit.brace_of;
let is_empty_body = Web.ItemEdit.is_empty_body;
let is_module_body = Web.ItemEdit.is_module_body;
let module_def = Web.ItemEdit.module_def;
let blank_def = Web.ItemEdit.blank_def;

/* item [d] is a `let` of a value, not of a module: its body takes local
   definitions */
let is_value = (d: Id.t, seg: Segment.t): bool =>
  switch (Focus.tile_ctx(d, seg), Focus.find_def(d, seg)) {
  | (Some((_, _, t)), Some(def)) =>
    Tile.token(t, 0) == "let" && !module_def(def)
  | _ => false
  };

let fresh_semi = Web.ItemEdit.fresh_semi;
let first_tile_id = Web.ItemEdit.first_tile_id;
let row_id = Web.ItemEdit.row_id;
let text_of = Web.ItemEdit.text_of;
let drop_suffix = Web.ItemEdit.drop_suffix;
let member_core = Web.ItemEdit.member_core;
let letin_core = Web.ItemEdit.letin_core;
let kind_kw = Web.ItemEdit.kind_kw;
let blank_text = Web.ItemEdit.blank_text;
let template = Web.ItemEdit.template;
let test_template = Web.ItemEdit.test_template;
let space = Web.ItemEdit.space;
let linebreak = Web.ItemEdit.linebreak;
let spacing = Web.ItemEdit.spacing;

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
  seg: Segment.t,
  arr: array(Piece.t),
  spans: array(Focus.item_span),
  /* a member block: new items are 2-shard members, not `… in` forms */
  in_module: bool,
  /* a module body's: its last item, a statement without its `;`, is a
     member like the rest, not a result */
  body: bool,
  /* the segment starts a line */
  bol: bool,
  /* a 2-shard member, not a `… in` item */
  member: int => bool,
  sl: array(slot),
};

let span_in_tile = (arr: array(Piece.t), sp: Focus.item_span): bool =>
  switch (Focus.first_tile(arr, sp.sp_start, sp.sp_stop)) {
  | Some(t) => Tile.ends_with_in(t)
  | None => false
  };

let block =
    (
      ~in_module: bool,
      ~body: bool,
      ~bol: bool,
      spans: array(Focus.item_span),
      seg: Segment.t,
    )
    : block => {
  let arr = Array.of_list(seg);
  let len = Array.length(arr);
  let member = k => in_module && !span_in_tile(arr, spans[k]);
  let is_sec = i => Piece.is_secondary(arr[i]);
  let comment = i =>
    switch (arr[i]) {
    | Piece.Secondary(w) => Secondary.is_comment(w)
    | _ => false
    };
  let space = i => Piece.is_space(arr[i]);
  let lb = i => Piece.is_linebreak(arr[i]);
  let line_start = i => i == 0 ? bol : lb(i - 1);
  let first = (sp: Focus.item_span) => {
    let rec go = i => i < sp.sp_stop && is_sec(i) ? go(i + 1) : i;
    go(sp.sp_start);
  };
  let content_end = (sp: Focus.item_span) => {
    let f = first(sp);
    let rec back = i => i > f && is_sec(i - 1) ? back(i - 1) : i;
    back(sp.sp_stop);
  };
  let n = Array.length(spans);
  /* a last span that is only the hole after a member's `;` */
  let hole = (sp: Focus.item_span) => {
    let rec go = (i, found) =>
      i >= sp.sp_stop
        ? found
        : (
          switch (arr[i]) {
          | Piece.Grout({shape: Convex, _}) when found == None =>
            go(i + 1, Some(i))
          | Piece.Secondary(_) => go(i + 1, found)
          | _ => None
          }
        );
    sp.sp_kind == ITail ? go(sp.sp_start, None) : None;
  };
  let semi_ended = (sp: Focus.item_span) => {
    let ce = content_end(sp);
    ce > sp.sp_start && Piece.is_semi(arr[ce - 1]);
  };
  /* trailing spans without an item: comments only, the hole, or the
     hole of a member block with no members */
  let (count, merged) =
    if (n == 0) {
      (0, None);
    } else if (first(spans[n - 1]) >= spans[n - 1].sp_stop) {
      (n - 1, None);
    } else {
      switch (hole(spans[n - 1])) {
      | Some(g) when n >= 2 && member(n - 2) && semi_ended(spans[n - 2]) => (
          n - 1,
          Some(g),
        )
      | Some(_) when n == 1 && in_module => (0, None)
      | _ => (n, None)
      };
    };
  let floor = ref(0);
  let sl =
    Array.init(
      count,
      k => {
        let sp = spans[k];
        let f = first(sp);
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
        let ce = content_end(sp);
        let (core, term) =
          if (member(k) && ce > f && Piece.is_semi(arr[ce - 1])) {
            (
              ce - 1,
              switch (merged) {
              | Some(g) when k == count - 1 => g + 1
              | _ => ce
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
    seg,
    arr,
    spans,
    in_module,
    body,
    bol,
    member,
    sl,
  };
};

/* items that ops may move or remove: not the trailing expression, which
   a module body hasn't */
let movable = (b: block, k: int): bool =>
  k >= 0 && k < Array.length(b.sl) && (b.body || b.spans[k].sp_kind != ITail);

/* item [k]'s head, when it defines a module: its definition is a module
   body alone, whatever binds it */
let module_at = (b: block, k: int): option(Id.t) =>
  switch (b.spans[k]) {
  | {sp_kind: IDef, sp_id: Some(head), _} =>
    switch (
      Option.map(
        List.filter(p => !Piece.is_secondary(p)),
        Focus.find_def(head, b.seg),
      )
    ) {
    | Some([Tile(t)]) when is_module_body(t) => Some(head)
    | _ => None
    }
  | _ => None
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
  if (k > 0 && k == Array.length(b.sl) - 1 && b.member(k) && b.member(k - 1)) {
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
    member && b.member(k)
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
  /* a member block can still hold a let-in item (an expression's
     chain): it takes let-in forms, and moves never mix the two families
     (that would cross block levels); outside modules, moves may mix defs
     and statements */
  let same_family = (j, k) =>
    !b.in_module
    || span_in_tile(b.arr, b.spans[j]) == span_in_tile(b.arr, b.spans[k]);
  /* below the trailing expression would strand it above the new item:
     the new item goes above it. A member block has no trailing
     expression: its last member, `;` or not, takes new items after it */
  let new_item = (~above=false, ~member, item) => {
    let trailing = !movable(j) && !b.member(j);
    insert(b, ~pos=above || trailing ? Before(j) : After(j), ~member, item)
    |> Option.map(seg => (seg, row_id(item)));
  };
  if (j >= n) {
    None;
  } else {
    switch (op) {
    | Delete when movable(j) => Some((remove(b, j), None))
    | Delete => None
    | MoveUp when movable(j - 1) && movable(j) && same_family(j, j - 1) =>
      Some((swap(b, j - 1), None))
    | MoveDown when movable(j) && movable(j + 1) && same_family(j, j + 1) =>
      Some((swap(b, j), None))
    | MoveUp
    | MoveDown
    /* across levels: [indent] and [outdent] */
    | Indent
    | Outdent => None
    | NewBelow
    | NewTypeBelow
    | NewModuleBelow =>
      let member = b.member(j);
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
      let member = b.member(j);
      Option.bind(
        member
          ? member_core(test_template) : letin_core(test_template ++ ";"),
        new_item(~above=op == NewTestAbove, ~member),
      );
    | Duplicate when movable(j) =>
      let member = b.member(j);
      let txt = text_of(bare(b, j));
      Option.bind(
        member ? member_core(txt) : letin_core(txt),
        new_item(~member),
      );
    | Duplicate => None
    };
  };
};

/* the item for row [fid]: the block holding it, the item's index there,
   and the program with that block replaced. The block is [fid]'s own
   level when the item is one of its spans by id, a member's flat body
   when [fid] lies in one, and otherwise the nearest level up whose span
   holds [fid]. A level is a member block when it is a module body's
   member list (Focus.holds_members), or the top of a module-rooted
   program */
let locate =
    (~mod_root: bool, fid: Id.t, seg: Segment.t)
    : option((block, int, Segment.t => Segment.t)) => {
  /* the level holding piece [id]: its pieces, the piece's index there,
     and the generations around it */
  let level_of = id =>
    switch (Move.relatives_beside_id(Left, id, seg)) {
    | Some({siblings: (pre, suf), ancestors}) =>
      Some((pre @ suf, List.length(pre), ancestors))
    | None => None
    };
  /* the program with [level] for the level under [ancestors]; a module
     body left with only whitespace is the parser's `{}` */
  let rebuild = (ancestors: Ancestors.t, level: Segment.t): Segment.t =>
    switch (ancestors) {
    | [
        (Ancestor.Tile({children: ([], []), _} as a), (pre, suf)),
        ...rest,
      ]
        when
          Ancestor.label(a) == ["{", "}"]
          && List.for_all(Focus.is_edge_ws, level) =>
      Ancestors.zip(pre @ [empty_body(), ...suf], rest)
    | _ => Ancestors.zip(level, ancestors)
    };
  let rec at = (~own: bool, (level, i, ancestors)) => {
    let top = ancestors == [];
    /* a module body's members, a lone statement among them too */
    let body =
      switch (ancestors) {
      | [] => false
      | [(Ancestor.Tile(a), _), ..._] =>
        Focus.holds_members(Ancestor.mold(a))
      | [(Ancestor.Projector(_) | Ancestor.Splice(_), _), ..._] => false
      };
    let spans =
      Array.of_list(
        Focus.item_spans(~divided_only_tail=!top && !body, level),
      );
    let here = j =>
      Some((
        block(
          ~in_module=top ? mod_root : body,
          ~body,
          ~bol=top,
          spans,
          level,
        ),
        j,
        rebuild(ancestors),
      ));
    let found =
      own
        ? Array.find_index(sp => sp.Focus.sp_id == Some(fid), spans) : None;
    let flat =
      own && found == None
        ? Focus.flat_body_of(
            fid,
            Array.of_list(level),
            Array.to_list(spans),
          )
        : None;
    switch (found, flat) {
    | (Some(j), _) => here(j)
    | (None, Some((b0, b1))) =>
      /* a let or the tail of a member's flat body: acts in that body,
         from its first line's indentation */
      let arr = Array.of_list(level);
      let rec back = i =>
        i > 0 && Piece.is_space(arr[i - 1]) ? back(i - 1) : i;
      let line = back(b0);
      let bol = line > 0 && Piece.is_linebreak(arr[line - 1]);
      let b0 = bol ? line : b0;
      let body = ListUtil.slice(b0, b1 - b0, level);
      let spans = Array.of_list(Focus.item_spans(body));
      let barr = Array.of_list(body);
      Array.find_index(
        (sp: Focus.item_span) =>
          sp.sp_id == Some(fid)
          || Focus.piece_at(fid, sp.sp_start, sp.sp_stop, barr),
        spans,
      )
      |> Option.map(k =>
           (
             block(~in_module=false, ~body=false, ~bol, spans, body),
             k,
             (
               body =>
                 rebuild(
                   ancestors,
                   ListUtil.truncate(b0, level)
                   @ body
                   @ ListUtil.remove_first_n(b1, level),
                 )
             ),
           )
         );
    | (None, None) =>
      /* contained in one of this level's statement or tail spans (e.g.
         a ModExp test's row id is the inner test term): that span is
         the item */
      switch (
        Array.find_index(
          (sp: Focus.item_span) => sp.sp_start <= i && i < sp.sp_stop,
          spans,
        ),
        ancestors,
      ) {
      | (Some(j), _) => here(j)
      | (None, [(a, _), ..._]) =>
        Option.bind(level_of(Ancestor.id(a)), at(~own=false))
      | (None, []) => None
      }
    };
  };
  Option.bind(level_of(fid), at(~own=true));
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

/* [item] (member form) into module [m]'s members: after the last, or
   ([first]) before the first */
let into_members =
    (
      ~first=false,
      ~src_indent: option(int)=?,
      ~trail: option(Segment.t)=?,
      m: Id.t,
      item: Segment.t,
      seg: Segment.t,
    )
    : option(Segment.t) =>
  switch (Focus.find_def(m, seg)) {
  | None => None
  | Some(def_seg) =>
    switch (Focus.brace_child(def_seg)) {
    | None => None
    | Some(members) =>
      let b =
        block(
          ~in_module=true,
          ~body=true,
          ~bol=false,
          Array.of_list(Focus.item_spans(members)),
          members,
        );
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
                 m,
                 Focus.with_brace_child(def_seg, members),
                 seg,
               )
             );
    }
  };

/* [item] (member form) as the only member of module [m], whose body is
   the parser's empty `{}`: on its own line, indented past the module */
let into_empty =
    (~src_indent: option(int)=?, m: Id.t, item: Segment.t, seg: Segment.t)
    : option(Segment.t) =>
  switch (Focus.find_def(m, seg)) {
  | None => None
  | Some(def_seg) =>
    let empty = (p: Piece.t) =>
      switch (p) {
      | Tile(t) => is_empty_body(t)
      | _ => false
      };
    let outer = Option.value(Focus.line_indent(m, seg), ~default=0);
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
            m,
            List.map(p => empty(p) ? brace : p, def_seg),
            seg,
          ),
        )
      : None;
  };

let is_fun_head = Web.ItemEdit.is_fun_head;
let wrapped_fun = Web.ItemEdit.wrapped_fun;

/* [item] (in let-in form) as value [d]'s last local definition: just
   before its body's result, inside a function's lambda, spaced as the
   body is (on its line when it's one line) */
let body_last =
    (~src_indent: option(int)=?, d: Id.t, item: Segment.t, seg: Segment.t)
    : option(Segment.t) =>
  switch (is_value(d, seg) ? Focus.find_def(d, seg) : None) {
  | Some(body) when wrapped_fun(body) => None
  | None => None
  | Some(body) =>
    let b =
      block(
        ~in_module=false,
        ~body=false,
        ~bol=false,
        Array.of_list(Focus.item_spans(body)),
        body,
      );
    let n = Array.length(b.sl);
    n > 0 && b.spans[n - 1].sp_kind == ITail
      ? insert(b, ~pos=Before(n - 1), ~member=false, ~src_indent?, item)
        |> Option.map(body => Focus.splice_def(d, body, seg))
      : None;
  };

/* [member] into module [fid]'s body, at any depth: last, or [first]; or
   into value [fid]'s, as its last local definition */
let new_inside =
    (~first=false, ~member: string, fid: Id.t, seg: Segment.t)
    : option((Segment.t, option(Id.t))) =>
  is_value(fid, seg)
    ? first
        ? None
        : Option.bind(letin_core(member ++ " in"), item =>
            body_last(fid, item, seg)
            |> Option.map(seg => (seg, row_id(item)))
          )
    : Option.bind(member_core(member), item =>
        (
          switch (into_members(~first, fid, item, seg)) {
          | Some(_) as r => r
          | None => into_empty(fid, item, seg)
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
  switch (b.spans[k]) {
  | {sp_kind: IDef, sp_id: Some(head), _} when is_value(head, b.seg) =>
    Some(head)
  | _ => None
  };

type spot =
  Web.ItemEdit.spot = {
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
            s_member: b.member(j),
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
          ~member=b.member(j),
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
let start_indent = (s: spot, seg: Segment.t): option(int) =>
  switch (s.s_item) {
  | [p, ..._] => Focus.line_indent(Piece.id(p), seg)
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
    let src_indent = start_indent(s, seg);
    (
      switch (
        into_members(
          ~first,
          ~src_indent?,
          ~trail=own_lines(s),
          m,
          item,
          seg1,
        )
      ) {
      | Some(_) as r => r
      | None => into_empty(~src_indent?, m, item, seg1)
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
    body_last(~src_indent=?start_indent(s, seg), d, item, seg1)
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
        ~src_indent=?start_indent(s, seg),
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
  switch (spot(~mod_root, ~up=false, fid, seg), Focus.find_def(owner, seg)) {
  | (Some({s_edge: true, _} as s), Some(def))
      when Focus.seg_contains_id(fid, def) =>
    beside(~mod_root, ~up=false, s, fid, owner, seg)
  | _ => None
  };

/* how the outline edits a program's items, addressed by id */
type t =
  Web.ItemEdit.t =
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

let new_op = Web.ItemEdit.new_op;
let rename_kind = Web.ItemEdit.rename_kind;

/* where item [id]'s name is typed: its pattern's first piece */
let name_slot = (id: Id.t, seg: Segment.t): option(Id.t) =>
  Option.bind(Focus.find_pat(id, seg), pat =>
    List.find_opt(p => !Piece.is_secondary(p), pat) |> Option.map(Piece.id)
  );

let text = Web.ItemEdit.text;
let piece_text = Web.ItemEdit.piece_text;
let name_at = Web.ItemEdit.name_at;
let typed_name = Web.ItemEdit.typed_name;

/* whether item [it] (row [row]) can be a [kind]'s, keeping its definition
   (else it's remade blank): a definition of only holes can be any kind's,
   a `{…}` a value's or a module's. Only a plain name changes kind, or one
   under an annotation, which stays with its definition */
let refit =
    (it: OutlineRename.item, kind: OutlineRename.kind, row: Id.t, seg)
    : result(bool, string) =>
  switch (Focus.tile_ctx(row, seg)) {
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

let with_binder = Web.ItemEdit.with_binder;

/* item [row] remade as a [kind]'s, named [name], keeping its id and its
   binding token's ([binder]). With [keep], its definition stays, and its
   pattern's annotation with it; else it's blank, as a new one is. The
   head is parsed from text, in the item's own form */
let rekind =
    (
      ~binder: Id.t,
      ~keep: bool,
      row: Id.t,
      kind: OutlineRename.kind,
      name: string,
      seg: Segment.t,
    )
    : option(Segment.t) =>
  switch (Focus.tile_ctx(row, seg)) {
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
            refit(it, kind, row, Lazy.force(seg)),
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
    (rn: renaming, row: Id.t, seg: Segment.t): option(Segment.t) =>
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
      ~binder=rn.rn_item.binder,
      ~keep,
      row,
      kind,
      rn.rn_name,
      OutlineRename.relabel(rn.rn_refs, rn.rn_name, seg),
    )
  };

type ctx =
  Web.ItemEdit.ctx = {
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
      apply_rename(rn, row, seg)
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
          new_inside(~member=template(rkind, name), at, seg)
        | InsertFirst(_) =>
          new_inside(~first=true, ~member=template(rkind, name), at, seg)
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
        new_inside(~first=true, ~member=template(rkind, name), at, seg)
      | Last => new_inside(~member=template(rkind, name), at, seg)
      }
    )
    |> Option.to_result(~none="a definition can't go here")
    |> Result.map(~f=((seg, d)) =>
         switch (Option.bind(d, d => Focus.find_pat(d, seg)), d) {
         | (Some(pat), Some(d)) =>
           let hole = (p: Piece.t) =>
             Piece.is_secondary(p)
               ? p
               : Grout({
                   id: Id.mk(),
                   shape: Convex,
                 });
           (Focus.splice_pat(d, List.map(hole, pat), seg), Some(d));
         | _ => (seg, d)
         }
       );
  };
