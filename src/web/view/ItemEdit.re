module Focus = ScratchFocus;

open Haz3lcore;
open Util;

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

/* a member's `;` */
let fresh_semi = (): Piece.t =>
  Piece.mk_tile((Form.Compound(CellJoin), Mod), []);

let first_tile_id = (seg: Segment.t): option(Id.t) =>
  Option.map((t: Tile.t) => t.id, List.find_map(Piece.is_tile, seg));

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
  seg: Segment.t,
  arr: array(Piece.t),
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
    bol,
    member,
    sl,
  };
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
  let semi = () => member ? [fresh_semi()] : [];
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
  | After(k) =>
    let at = b.sl[k].hi;
    Some(
      ListUtil.truncate(at, b.seg)
      @ spaces(k + 1)
      @ shifted(k + 1)
      @ semi()
      @ trail_or(spacing(trail(b, k)))
      @ ListUtil.remove_first_n(at, b.seg),
    );
  | Before(p) =>
    let at = b.sl[p].lo;
    let default =
      p > 0
        ? spacing(trail(b, p - 1))
        : [starts_line(b, p) ? linebreak() : space()];
    Some(
      ListUtil.truncate(at, b.seg)
      @ spaces(p)
      @ shifted(p)
      @ semi()
      @ trail_or(default)
      @ ListUtil.remove_first_n(at, b.seg),
    );
  };
};

/* [op] on item [j] of its own block: an op invalid there (a move at the
   block's edge) no-ops rather than act on the enclosing item. [in_module]:
   a member block, so new items are 2-shard members, not `… in` forms */
let apply_at =
    (
      ~name: option(string)=?,
      op: OutlineEdit.def_op,
      ~in_module: bool,
      ~bol: bool,
      spans: array(Focus.item_span),
      j: int,
      seg: Segment.t,
    )
    : option((Segment.t, option(Id.t))) => {
  let named = placeholder => Option.value(name, ~default=placeholder);
  let b = block(~in_module, ~bol, spans, seg);
  let n = Array.length(b.sl);
  let movable = k => k >= 0 && k < n && spans[k].Focus.sp_kind != Focus.ITail;
  /* a member block can still hold a let-in item (an expression's
     chain): it takes let-in forms, and moves never mix the two families
     (that would cross block levels); outside modules, moves may mix defs
     and statements */
  let same_family = (j, k) =>
    !in_module
    || span_in_tile(b.arr, spans[j]) == span_in_tile(b.arr, spans[k]);
  let new_item = (~member, item) =>
    /* below the trailing expression would strand it above the new def:
       the new def goes above it */
    insert(b, ~pos=movable(j) ? After(j) : Before(j), ~member, item)
    |> Option.map(seg => (seg, first_tile_id(item)));
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
    | MoveDown => None
    | NewBelow
    | NewTypeBelow
    | NewModuleBelow =>
      let member = b.member(j);
      let txt =
        switch (op) {
        | NewTypeBelow => "type " ++ named("NewType") ++ {js| = ¿|js}
        | NewModuleBelow => "module " ++ named("NewModule") ++ " = {}"
        | _ => "let " ++ named("new_def") ++ {js| = ¿|js}
        };
      Option.bind(
        member ? member_core(txt) : letin_core(txt ++ " in"),
        new_item(~member),
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

/* where a level sits: members live under a brace tile inside the
   module tile's def child, two hops from the module, so it's threaded */
type block_ctx =
  | BPlain
  | BModDef /* the module tile's def child: the brace lives here */
  | BModBody; /* the brace's child: the member list */

/* where [fid]'s item is, seen from a level: not at or below it, there
   but the op refused, or the level rebuilt around the op's result */
type found('a) =
  | Absent
  | Refused
  | Done('a);

let map_found = (f: 'a => 'b, r: found('a)): found('b) =>
  switch (r) {
  | Absent => Absent
  | Refused => Refused
  | Done(x) => Done(f(x))
  };

/* [act] at the block that owns [fid]'s item, the block rebuilt around
   its result. A module body, or the top level of a module-rooted
   program, is a member block ([in_module]). An op its block refuses
   stops there: the item around it is a different row */
let rec at_level_found =
        (
          ~act:
             (
               ~in_module: bool,
               ~bol: bool,
               array(Focus.item_span),
               int,
               Segment.t
             ) =>
             option((Segment.t, option(Id.t))),
          ~mod_root: bool,
          fid: Id.t,
          ~bctx: block_ctx,
          ~top: bool,
          seg: Segment.t,
        )
        : found((Segment.t, option(Id.t))) => {
  let spans = Array.of_list(Focus.item_spans(~divided_only_tail=!top, seg));
  let n = Array.length(spans);
  let find = pred => {
    let rec go = j => j >= n ? None : pred(spans[j]) ? Some(j) : go(j + 1);
    go(0);
  };
  let acted = r =>
    switch (r) {
    | Some(x) => Done(x)
    | None => Refused
    };
  let in_module = bctx == BModBody || top && mod_root;
  let found = find((sp: Focus.item_span) => sp.sp_id == Some(fid));
  let flat =
    found == None
      ? Focus.flat_body_of(fid, Array.of_list(seg), Array.to_list(spans))
      : None;
  switch (found, flat) {
  | (Some(j), _) => acted(act(~in_module, ~bol=top, spans, j, seg))
  | (None, Some((b0, b1))) =>
    /* a let or the tail of a member's flat body: acts in that body,
       from its first line's indentation */
    let arr = Array.of_list(seg);
    let rec back = i =>
      i > 0 && Piece.is_space(arr[i - 1]) ? back(i - 1) : i;
    let line = back(b0);
    let bol = line > 0 && Piece.is_linebreak(arr[line - 1]);
    let b0 = bol ? line : b0;
    let body = ListUtil.slice(b0, b1 - b0, seg);
    let bspans = Array.of_list(Focus.item_spans(body));
    let barr = Array.of_list(body);
    let rec holding = k =>
      k >= Array.length(bspans)
        ? None
        : bspans[k].sp_id == Some(fid)
          || Focus.piece_at(fid, bspans[k].sp_start, bspans[k].sp_stop, barr)
            ? Some(k) : holding(k + 1);
    switch (holding(0)) {
    | Some(k) =>
      act(~in_module=false, ~bol, bspans, k, body)
      |> Option.map(((body', target)) =>
           (
             ListUtil.truncate(b0, seg)
             @ body'
             @ ListUtil.remove_first_n(b1, seg),
             target,
           )
         )
      |> acted
    | None => Refused
    };
  | (None, None) =>
    /* descend into tile children first (the owning block may be a
       module or fn body) */
    let is_module_tile = (t: Base.tile) =>
      switch (Tile.label(t)) {
      | ["module", ..._] => true
      | _ => false
      };
    let is_body = (t: Base.tile) =>
      Tile.label(t) == ["{", "}"] && List.length(t.children) == 1;
    /* [after_head]: the tile follows a 2-shard `module X =` head, whose
       body is its next sibling rather than a child */
    let child_bctx =
        (~after_head: bool, t: Base.tile, is_last: bool): block_ctx =>
      if (is_module_tile(t) && is_last) {
        BModDef;
      } else if ((bctx == BModDef || after_head) && is_body(t)) {
        BModBody;
      } else {
        BPlain;
      };
    let rec try_children =
            (~after_head=false, ps: Segment.t)
            : found((Segment.t, option(Id.t))) =>
      switch (ps) {
      | [] => Absent
      | [Piece.Secondary(_) as p, ...rest] =>
        try_children(~after_head, rest)
        |> map_found(((rest', target)) => ([p, ...rest'], target))
      | [Piece.Tile(t) as p, ...rest] =>
        let n_kids = List.length(t.children);
        let rec try_kids = (before, k, kids) =>
          switch (kids) {
          | [] => Absent
          | [ch, ...more] =>
            switch (
              at_level_found(
                ~act,
                ~mod_root,
                fid,
                ~bctx=child_bctx(~after_head, t, k == n_kids - 1),
                ~top=false,
                ch,
              )
            ) {
            | Done((ch', target)) =>
              Done((List.rev(before) @ [ch', ...more], target))
            | Refused => Refused
            | Absent => try_kids([ch, ...before], k + 1, more)
            }
          };
        switch (try_kids([], 0, t.children)) {
        | Done((children, target)) =>
          let tile =
            switch (children) {
            | [kid] when is_body(t) && List.for_all(Focus.is_edge_ws, kid) =>
              empty_body()
            | _ =>
              Piece.Tile({
                ...t,
                children,
              })
            };
          Done(([tile, ...rest], target));
        | Refused => Refused
        | Absent =>
          try_children(
            ~after_head=is_module_tile(t) && List.length(t.shards) == 2,
            rest,
          )
          |> map_found(((rest', target)) => ([p, ...rest'], target))
        };
      | [p, ...rest] =>
        try_children(rest)
        |> map_found(((rest', target)) => ([p, ...rest'], target))
      };
    switch (try_children(seg)) {
    | Absent =>
      /* contained in one of this level's statement or tail spans (e.g.
         a ModExp test's row id is the inner test term): that span is
         the item */
      switch (
        find((sp: Focus.item_span) =>
          Focus.seg_contains_id(
            fid,
            ListUtil.slice(sp.sp_start, sp.sp_stop - sp.sp_start, seg),
          )
        )
      ) {
      | Some(j) => acted(act(~in_module, ~bol=top, spans, j, seg))
      | None => Absent
      }
    | r => r
    };
  };
};

let at_level =
    (~act, ~mod_root, fid, ~bctx, ~top, seg)
    : option((Segment.t, option(Id.t))) =>
  switch (at_level_found(~act, ~mod_root, fid, ~bctx, ~top, seg)) {
  | Done(r) => Some(r)
  | Absent
  | Refused => None
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
  at_level(
    ~act=
      (~in_module, ~bol, spans, j, seg) =>
        apply_at(~name?, op, ~in_module, ~bol, spans, j, seg),
    ~mod_root,
    fid,
    ~bctx=BPlain,
    ~top=true,
    seg,
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
      | Tile(t) => Tile.label(t) == ["{}"]
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

/* append [member] to module [fid]'s body, at any depth */
let new_inside =
    (~member: string, fid: Id.t, seg: Segment.t)
    : option((Segment.t, option(Id.t))) =>
  Option.bind(member_core(member), item =>
    (
      switch (into_members(fid, item, seg)) {
      | Some(_) as r => r
      | None => into_empty(fid, item, seg)
      }
    )
    |> Option.map(seg => (seg, first_tile_id(item)))
  );

/* an item's bare pieces in a block's form: members end in their
   place's `;`, other blocks use `… in`. Converting goes through text
   (fresh ids) */
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

type spot = {
  /* the item is a 2-shard member */
  s_member: bool,
  s_item: Segment.t,
  s_trail: Segment.t,
  /* the neighbour in the move's direction, if it is a module */
  s_module: option(Id.t),
  s_edge: bool,
};

/* where the item holding [fid] sits, seen from its own block */
let spot = (~mod_root, ~up: bool, fid: Id.t, seg: Segment.t): option(spot) => {
  let found = ref(None);
  let _ =
    at_level(
      ~act=
        (~in_module, ~bol, spans, j, seg) => {
          let b = block(~in_module, ~bol, spans, seg);
          let n = Array.length(b.sl);
          let movable = k =>
            k >= 0 && k < n && spans[k].Focus.sp_kind != Focus.ITail;
          let k = up ? j - 1 : j + 1;
          if (j < n) {
            let module_at =
              movable(k)
                ? switch (List.find_map(Piece.is_tile, bare(b, k))) {
                  | Some(t) when Tile.token(t, 0) == "module" => Some(t.id)
                  | _ => None
                  }
                : None;
            found :=
              Some({
                s_member: b.member(j),
                s_item: bare(b, j),
                s_trail: trail(b, j),
                s_module: module_at,
                s_edge: !movable(k),
              });
          };
          /* found: stop the search here */
          Some((seg, None));
        },
      ~mod_root,
      fid,
      ~bctx=BPlain,
      ~top=true,
      seg,
    );
  found^;
};

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
  at_level(
    ~act=
      (~in_module, ~bol, spans, j, seg) => {
        let b = block(~in_module, ~bol, spans, seg);
        j < Array.length(b.sl)
          ? insert(
              b,
              ~pos=up ? Before(j) : After(j),
              ~member=b.member(j),
              ~src_indent?,
              ~trail,
              item,
            )
            |> Option.map(seg => (seg, None))
          : None;
      },
    ~mod_root,
    target,
    ~bctx=BPlain,
    ~top=true,
    seg,
  )
  |> Option.map(fst);

/* Alt↑↓: into an expanded module beside the item (its end going up,
   its start going down); from a module's first or last member, out to
   just above or below it. Collapsed modules are stepped over and
   function bodies keep their items. [owner]: the item's module. The
   item is reindented to where it lands */
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
    let removed = () => Option.map(fst, apply(~mod_root, Delete, fid, seg));
    let src_indent =
      switch (s.s_item) {
      | [p, ..._] => Focus.line_indent(Piece.id(p), seg)
      | [] => None
      };
    let step = () =>
      s.s_edge
        ? None
        : apply(~mod_root, up ? MoveUp : MoveDown, fid, seg)
          |> Option.map(((seg, _)) => (seg, Some(fid)));
    switch (s.s_module) {
    | Some(k) when is_open(k) =>
      /* in: the neighbour module's members gain the item, or an empty
         body takes it as its only member; else the module is stepped
         over */
      let into =
        switch (
          removed(),
          in_form(~member=true, ~was_member=s.s_member, s.s_item),
        ) {
        | (Some(seg1), Some(item)) =>
          (
            switch (
              into_members(
                ~first=!up,
                ~src_indent?,
                ~trail=s.s_trail,
                k,
                item,
                seg1,
              )
            ) {
            | Some(_) as r => r
            | None => into_empty(~src_indent?, k, item, seg1)
            }
          )
          |> Option.map(seg2 => (seg2, first_tile_id(item)))
        | _ => None
        };
      into == None ? step() : into;
    | _ when !s.s_edge => step()
    | _ =>
      /* out: just above or below the owning module, in its block's form */
      switch (owner) {
      | None => None
      | Some(m) =>
        switch (removed(), spot(~mod_root, ~up, m, seg)) {
        | (Some(seg1), Some(ms)) =>
          switch (
            in_form(~member=ms.s_member, ~was_member=s.s_member, s.s_item)
          ) {
          | Some(item) =>
            insert_near(
              ~mod_root,
              ~up,
              ~src_indent?,
              ~trail=s.s_trail,
              m,
              item,
              seg1,
            )
            |> Option.map(seg2 => (seg2, first_tile_id(item)))
          | None => None
          }
        | _ => None
        }
      }
    };
  };

/* how the outline edits a program's items, addressed by id */
type t =
  | Op(OutlineEdit.def_op, Id.t)
  | Rename(Id.t, string)
  /* a new definition below the row: `name`, `type Name` or
     `module Name` */
  | Insert(Id.t, string)
  /* the same, last inside a module */
  | InsertInside(Id.t, string);

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

type ctx = {
  mod_root: bool,
  term: Language.Exp.t,
  info_map: Lazy.t(Language.Statics.Map.t),
  /* whether a module row is expanded: moves step over collapsed ones */
  is_open: Id.t => bool,
};

/* the new program and the row the edit lands on, or why not */
let edit =
    (ctx: ctx, e: t, seg: Segment.t)
    : result((Segment.t, option(Id.t)), string) =>
  switch (e) {
  | Op((MoveUp | MoveDown) as op, fid) =>
    let owner =
      switch (Option.map(List.rev, OutlineTree.trail_of(fid, ctx.term))) {
      | Some([_, parent, ..._])
          when OutlineTree.kind_of(parent, ctx.term) == Some(KModule) =>
        Some(parent)
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
  | Op(op, fid) =>
    apply(~mod_root=ctx.mod_root, op, fid, seg)
    |> Option.to_result(~none="it can't go here")
  | Rename(row, name) =>
    OutlineRename.rename(
      ~info_map=Lazy.force(ctx.info_map),
      ~term=ctx.term,
      row,
      name,
      seg,
    )
    |> Result.map(~f=seg => (seg, Some(row)))
  | Insert(anchor, text) =>
    let (kind, name) = typed_name(text);
    let (rkind, op): (OutlineRename.kind, OutlineEdit.def_op) =
      switch (kind) {
      | KType => (KType, NewTypeBelow)
      | KModule => (KModule, NewModuleBelow)
      | _ => (KValue, NewBelow)
      };
    switch (OutlineRename.check_name(rkind, name)) {
    | Some(why) => Error(why)
    | None =>
      apply(~name, ~mod_root=ctx.mod_root, op, anchor, seg)
      |> Option.to_result(~none="a definition can't go here")
    };
  | InsertInside(m, text) =>
    let (kind, name) = typed_name(text);
    let (rkind, member): (OutlineRename.kind, string) =
      switch (kind) {
      | KType => (KType, "type " ++ name ++ {js| = ¿|js})
      | KModule => (KModule, "module " ++ name ++ " = {}")
      | _ => (KValue, "let " ++ name ++ {js| = ¿|js})
      };
    switch (OutlineRename.check_name(rkind, name)) {
    | Some(why) => Error(why)
    | None =>
      new_inside(~member, m, seg)
      |> Option.to_result(~none="a definition can't go here")
    };
  };
