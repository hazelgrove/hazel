/* The layout under the editing kernel (EntryEdit): a block's entries as
   places by line, and the trivia rule for taking one out and putting one
   in. Each edit splices one ItemSpans level and re-makes only the levels
   above it, so everything untouched stays physically the same */
module Nav = SpineNav;
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
  Nav.holds_members(Tile.mold(t)) || is_empty_body(t);

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
    switch (Nav.def_at(at)) {
    | Some(def) => !module_def(def)
    | None => false
    }
  );

let root_of = (mod_root: bool): Sort.t => mod_root ? Sort.Mod : Sort.Exp;

/* item [d] is a `let` of a value */
let is_value = (~mod_root: bool, d: Id.t, seg: Segment.t): bool =>
  switch (Nav.tile_ctx(~root=root_of(mod_root), d, seg)) {
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
  Option.map(Nav.core_ws, parse(~root=Mod, txt));
};
let letin_core = (txt: string): option(Segment.t) =>
  Option.map(
    (sk: Segment.t) =>
      Nav.core_ws(
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
  eol: int, /* past what shares its last line */
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
  /* a `…;` statement can be an item: not in a member's let chain or
     body run, where its `;` would end the member */
  stmts: bool,
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
let block = (~in_module: bool, ~stmts=true, lv: S.level): block => {
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
              i < len && Nav.is_edge_ws(arr[i])
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
          eol: s,
          hi,
        };
      },
    );
  {
    lv,
    seg: lv.seg,
    arr,
    in_module,
    body: Nav.is_members(lv),
    stmts,
    bol: lv.bol,
    sl,
  };
};

/* items that ops may move or remove: not the trailing expression, which
   a module body hasn't */
let movable = (b: block, k: int): bool =>
  k >= 0 && k < Array.length(b.sl) && (b.body || !Nav.is_tail(b.lv, k));

/* item [k] holds an unfinished definition that runs on into the items
   after it: an `…in` form without its `in`, with an `…in` form or a `;`
   past it; or a member whose open form takes the members after its member
   line. Taking the item whole (deleting, duplicating, moving it) would
   take those too */
let runs_on = (b: block, k: int): bool => {
  let (c0, c1) = b.lv.spans[k].core;
  let starts_item = (p: Piece.t) =>
    switch (p) {
    | Tile(t) => Tile.ends_with_in(t) || Tile.is_semi(t)
    | _ => false
    };
  let rec past = i => i < c1 && (starts_item(b.arr[i]) || past(i + 1));
  let rec go = i =>
    i < c1
    && (
      switch (b.arr[i]) {
      | Tile(t) as p when Tile.ends_with_in(t) && !Segment.is_in_tile(p) =>
        past(i + 1)
      | _ => go(i + 1)
      }
    );
  go(c0) || S.runs_past_line(b.lv, k);
};

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
        Nav.def_at((b.seg, h, t)),
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

/* ---- taking an item out and putting it in elsewhere ----

   The trivia rule. An item's doc comment (the comment lines directly
   above it) is part of it. Spacing belongs to the gaps between items: a
   swap leaves it in place. An item taken out of its block takes one gap
   along: the one above it when it sits right below an item (or nothing
   follows it), else the one below; where it lands it puts that gap on
   the same side, keeping the gap that was there on the other. Its first
   line keeps its indentation's offset from that side's item. Comments
   set apart from the items (by a blank line) stay put and stay apart:
   an item lands on its own side of them, and goes into its neighbour
   only with none between. */

let is_comment_at = (b: block, i: int): bool =>
  switch (b.arr[i]) {
  | Secondary(w) => Secondary.is_comment(w)
  | _ => false
  };

/* the linebreaks among [b]'s pieces [i, j) */
let breaks = (b: block, i: int, j: int): int => {
  let rec go = (x, n) =>
    x >= j ? n : go(x + 1, Piece.is_linebreak(b.arr[x]) ? n + 1 : n);
  go(i, 0);
};

/* back from [i] past whitespace */
let ws_back = (b: block, i: int): int => {
  let rec go = j => j > 0 && Nav.is_edge_ws(b.arr[j - 1]) ? go(j - 1) : j;
  go(i);
};

/* on from [i] past whitespace */
let ws_on = (b: block, i: int): int => {
  let rec go = j =>
    j < Array.length(b.arr) && Nav.is_edge_ws(b.arr[j]) ? go(j + 1) : j;
  go(i);
};

/* past the last linebreak in [lo, i) */
let line_start = (b: block, lo: int, i: int): int => {
  let rec go = j =>
    j > lo && !Piece.is_linebreak(b.arr[j - 1]) ? go(j - 1) : j;
  go(i);
};

/* where the content above item [k]'s place ends, if the block has any
   there: the item above, or comments set apart from it. A function's
   head opens the block: it is no content */
let above = (b: block, k: int): option(int) => {
  let w = ws_back(b, b.sl[k].lo);
  k > 0 || w > 0 && is_comment_at(b, w - 1) ? Some(w) : None;
};

/* item [k]'s place is right below the item above, no comment between */
let under_item = (b: block, k: int): bool =>
  k > 0 && ws_back(b, b.sl[k].lo) <= b.sl[k - 1].eol;

/* the indentation of the line holding [b]'s piece [i] (or ending just
   before it) */
let line_indent = (b: block, i: int): int =>
  S.indent_at(~outer=S.outer(b.lv), b.arr, i);

let item_indent = (b: block, k: int): int =>
  line_indent(b, indent_end(b, k));

let rec spans_lines = (seg: Segment.t): bool =>
  List.exists(
    (p: Piece.t) =>
      switch (p) {
      | Secondary(w) => Secondary.is_linebreak(w)
      | Tile(t) => List.exists(spans_lines, t.children)
      | Projector(pr) => spans_lines([pr.syntax])
      | Grout(_) => false
      },
    seg,
  );

/* [seg]'s lines moved [delta] columns; blank lines stay blank */
let shift_by = (delta: int, seg: Segment.t): Segment.t =>
  delta == 0
    ? seg
    : Segment.map_deep(
        level => {
          let rec go = (ps: Segment.t) =>
            switch (ps) {
            | [] => []
            | [Piece.Secondary(w) as lb, ...rest]
                when Secondary.is_linebreak(w) =>
              let (sps, rest) = Segment.split_space_run(rest);
              let blank =
                switch (rest) {
                | [p, ..._] => Piece.is_linebreak(p)
                | [] => false
                };
              let n = List.length(sps);
              let m = blank ? n : max(0, n + delta);
              [lb]
              @ (
                m <= n
                  ? ListUtil.truncate(m, sps)
                  : sps @ List.init(m - n, _ => space())
              )
              @ go(rest);
            | [p, ...rest] => [p, ...go(rest)]
            };
          go(level);
        },
        seg,
      );

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

/* a value's body: a definition's level, not a member list or chain */
let is_body = (b: block): bool =>
  switch (b.lv.owner) {
  | Block(_) => !b.in_module
  | Top
  | Members(_)
  | Chain(_) => false
  };

/* a function's body: a function's head opens it */
let fn_body = (b: block): bool =>
  Array.length(b.sl) > 0
  && Array.exists(is_fun_head, Array.sub(b.arr, 0, b.sl[0].lo));

/* where an item was in its block: the gap it takes along, and its
   indentation's offset */
type origin = {
  /* the whitespace between it and its neighbour, through the last
     linebreak (none: they shared a line). None when it has none to take:
     it was alone, or opened a body before the result, whose spacing a
     move sets */
  gap: option(Segment.t),
  offset: int,
};

/* a gap of whitespace holds a linebreak */
let has_lb = (gap: Segment.t): bool => List.exists(Piece.is_linebreak, gap);

/* a copy of [b]'s gap above item [k]'s place, fresh pieces */
let gap_copy = (b: block, k: int): option(Segment.t) =>
  Option.map(
    w =>
      List.map(
        p => Piece.is_linebreak(p) ? linebreak() : space(),
        ListUtil.slice(w, b.sl[k].lo - w, b.seg),
      ),
    above(b, k),
  );

/* item [k] taken out of [b]: the block's pieces without it, and its
   place. A member block's last member hands its terminator to the one
   above. A body's first item, before the result: a result on its last
   line goes back onto the head's line, as does a constant's value when
   the item spanned lines (it was put on a line of its own for it) */
let take = (b: block, k: int): (Segment.t, origin) => {
  let n = Array.length(b.sl);
  let sl = b.sl[k];
  let offset = j => item_indent(b, k) - item_indent(b, j);
  /* the next content, past the gap below */
  let x = ws_on(b, sl.eol);
  switch (above(b, k)) {
  | Some(w) when under_item(b, k) || x == Array.length(b.arr) =>
    /* what shares its last line goes with it, but the spaces before
       content that follows there */
    let e = x == sl.eol ? max(sl.term, ws_back(b, sl.eol)) : sl.eol;
    let seg =
      b.in_module && k > 0 && k == n - 1
        ? ListUtil.truncate(b.sl[k - 1].core, b.seg)
          @ term(b, k)
          @ ListUtil.slice(b.sl[k - 1].term, w - b.sl[k - 1].term, b.seg)
          @ ListUtil.remove_first_n(e, b.seg)
        : ListUtil.truncate(w, b.seg) @ ListUtil.remove_first_n(e, b.seg);
    (
      seg,
      {
        gap: Some(ListUtil.slice(w, sl.lo - w, b.seg)),
        offset: k > 0 ? offset(k - 1) : 0,
      },
    );
  | content =>
    /* the gap below goes with it, the next content takes its place */
    let lbs = breaks(b, sl.eol, x);
    let next = k + 1 < n ? Some(k + 1) : None;
    let result =
      switch (next) {
      | Some(j) => !b.in_module && !movable(b, j)
      | None => false
      };
    let own = starts_line(b, k);
    let rejoin = result && own && is_body(b) && content == None;
    let head = line_indent(b, ws_back(b, sl.lo));
    let onto_head = (rest: Segment.t) =>
      ListUtil.truncate(ws_back(b, sl.lo), b.seg) @ [space()] @ rest;
    /* a constant's value it put on the line below it */
    let to_head =
      rejoin && lbs == 1 && n == 2 && !fn_body(b) && spans_lines(bare(b, k));
    let seg =
      if (rejoin && lbs == 0) {
        onto_head(ListUtil.remove_first_n(x, b.seg));
      } else if (to_head) {
        let r = b.sl[k + 1];
        let i = indent_end(b, k + 1);
        onto_head(
          shift_by(
            head - item_indent(b, k + 1),
            ListUtil.slice(i, r.eol - i, b.seg),
          )
          @ ListUtil.remove_first_n(r.eol, b.seg),
        );
      } else if (lbs > 0 && own) {
        ListUtil.truncate(sl.lo, b.seg)
        @ ListUtil.remove_first_n(line_start(b, sl.eol, x), b.seg);
      } else {
        ListUtil.truncate(sl.lo, b.seg)
        @ indent(b, k)
        @ ListUtil.remove_first_n(x, b.seg);
      };
    (
      seg,
      {
        /* before the result, only a gap on a line of its own was the
           item's: on the head's line, or where the result had to move
           onto a line of its own, the item had none */
        gap:
          next == None || result && (lbs == 0 || to_head)
            ? None
            : Some(
                ListUtil.slice(
                  sl.eol,
                  line_start(b, sl.eol, x) - sl.eol,
                  b.seg,
                ),
              ),
        /* a result on its last line: past the head's, as it was put */
        offset:
          rejoin && lbs == 0
            ? item_indent(b, k) - (head + 2)
            : Option.fold(~none=0, ~some=offset, next),
      },
    );
  };
};

/* item [k] gone, as a move takes it */
let remove = (b: block, k: int): Segment.t => fst(take(b, k));

/* where a moved item lands in a block */
type landing =
  /* just above item [k], after any comments set apart above it; k = n:
     after the last */
  | Above(int)
  /* just below item [k], before any comments set apart below it */
  | Below(int);

/* [item] (bare, in the block's form) moved into [b] [at]. [gap] (a
   space where it has no linebreak) parts it from the item above when it
   lands right below one, else from the item below; its first line is
   indented [offset] past that item's, its other lines shifted to match
   from [src_indent]. In a member block it takes `;`, or the last
   member's terminator when it lands last */
let put =
    (
      b: block,
      ~at: landing,
      ~gap: Segment.t,
      ~offset: int,
      ~src_indent: option(int),
      ~member: bool,
      item: Segment.t,
    )
    : Segment.t => {
  let n = Array.length(b.sl);
  let lines = has_lb(gap);
  let spaces = k => List.init(max(0, k), _ => space());
  let moved = line =>
    shift_by(
      Option.fold(~none=0, ~some=src => line - src, src_indent),
      item,
    );
  let semi = member ? [fresh_semi()] : [];
  /* below item [j], after the content ending at [w] */
  let below = (j, w) => {
    let ind = max(0, item_indent(b, j) + offset);
    let lead = lines ? gap @ spaces(ind) : [space()];
    let item = moved(lines ? ind : line_indent(b, w));
    let l = b.sl[j];
    j == n - 1 && member && b.in_module
      ? ListUtil.truncate(l.core, b.seg)
        @ [fresh_semi()]
        @ ListUtil.slice(l.term, w - l.term, b.seg)
        @ lead
        @ item
        @ term(b, j)
        @ ListUtil.remove_first_n(w, b.seg)
      : ListUtil.truncate(w, b.seg)
        @ lead
        @ item
        @ semi
        @ ListUtil.remove_first_n(w, b.seg);
  };
  switch (at) {
  | Below(j) => below(j, max(b.sl[j].term, ws_back(b, b.sl[j].eol)))
  | Above(g) when g == n => below(n - 1, ws_back(b, Array.length(b.arr)))
  | Above(g) when under_item(b, g) => below(g - 1, ws_back(b, b.sl[g].lo))
  | Above(g) =>
    /* where item [g] starts, the gap below it */
    let lo = b.sl[g].lo;
    let own = starts_line(b, g);
    let ind = max(0, item_indent(b, g) + offset);
    ListUtil.truncate(lo, b.seg)
    @ (own ? spaces(ind) : [])
    @ moved(own ? ind : line_indent(b, lo))
    @ semi
    @ (lines ? gap @ (own ? [] : spaces(ind)) : [space()])
    @ ListUtil.remove_first_n(lo, b.seg);
  };
};

/* the gap for an item that takes none along, landing beside item [j]
   (just above it, or [below]), as a new item is spaced: a copy of the
   gap it lands in, or, before the result or at the end, of the gap
   above [j] */
let gap_at = (b: block, ~below: bool, j: int): Segment.t => {
  let next = j + 1;
  let landing =
    below && next < Array.length(b.sl) && movable(b, next)
      ? gap_copy(b, next) : gap_copy(b, j);
  Option.value(landing, ~default=[linebreak()]);
};

type pos =
  | Before(int)
  | After(int);

/* [item] (bare, in the block's form) as a new item at [pos], laid out
   like its neighbours: the indentation where it lands, then the spacing
   that follows its neighbour. In a member block it takes `;`, or the
   last member's terminator when it lands last. Lines after its first
   move with it from [src_indent], the indentation they were written at */
let insert =
    (
      b: block,
      ~pos: pos,
      ~member: bool,
      ~src_indent: option(int)=?,
      item: Segment.t,
    )
    : option(Segment.t) => {
  let n = Array.length(b.sl);
  let spaces = like => List.map(_ => space(), indent(b, like));
  let item = like =>
    switch (src_indent) {
    | Some(src) => shift_by(List.length(indent(b, like)) - src, item)
    | None => item
    };
  /* at [at], laid out like item [like], then [after] */
  let splice = (at, like, after) =>
    ListUtil.truncate(at, b.seg)
    @ spaces(like)
    @ item(like)
    @ (member ? [fresh_semi()] : [])
    @ after
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
          @ item(k)
          @ term(b, k)
          @ spacing(trail(b, k))
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

/* the program [seg] with [path]'s last level made [level]; a module body
   left with only whitespace is the parser's `{}` */
let rebuild =
    (~seg: Segment.t, path: list(Nav.step), level: Segment.t): Segment.t =>
  switch (List.rev(path)) {
  | [{from: Kid(t, 0), _}, ...[above, ..._] as rest]
      when
        Tile.label(t) == ["{", "}"]
        && List.length(t.children) == 1
        && List.for_all(Nav.is_edge_ws, level) =>
    Nav.rebuild(
      ~seg,
      List.rev(rest),
      ListUtil.put_nth(above.i, empty_body(), Nav.pieces_of(~seg, above)),
    )
  | _ => Nav.rebuild(~seg, path, level)
  };

/* the item for row [fid]: the block holding it, the item's index there,
   and the program with that block replaced. The block is the innermost
   level whose item holds [fid] (ItemSpans' levels: module members, a
   member's let chain or body run, a definition's body). A level is a
   member block when it is a module body's member list, or the top of a
   module-rooted program. [fid] may be an anchor that is no piece */
let locate =
    (~mod_root: bool, fid: Id.t, seg: Segment.t)
    : option((block, int, Segment.t => Segment.t)) => {
  let root = root_of(mod_root);
  let path =
    switch (Nav.find_path(~root, fid, seg)) {
    | Some(_) as found => found
    | None => Nav.anchor_path(~root, fid, seg)
    };
  switch (path) {
  | Some(path) when !Nav.projected(path) =>
    /* innermost first */
    let rec up = (rev: list(Nav.step)) =>
      switch (rev) {
      | [] => None
      | [st, ...outer] =>
        switch (Nav.item_at(st.lv, st.i)) {
        | Some(j) =>
          let in_module =
            switch (st.from) {
            | Top => mod_root
            | Kid(_)
            | Sub
            | Syntax => Nav.is_members(st.lv)
            };
          let stmts =
            switch (st.from) {
            | Sub => false
            | Top
            | Kid(_)
            | Syntax => true
            };
          Some((
            block(~in_module, ~stmts, st.lv),
            j,
            rebuild(~seg, List.rev(rev)),
          ));
        | None => up(outer)
        }
      };
    up(List.rev(path));
  | _ => None
  };
};

/* module [m]'s definition and its members' level: its braces' child */
let members_of =
    (~mod_root: bool, m: Id.t, seg: Segment.t): option((Segment.t, S.level)) =>
  switch (Nav.tile_at(~root=root_of(mod_root), m, seg)) {
  | Some((up, level, i, t)) =>
    let lv = ListUtil.last(up).lv;
    switch (
      Option.bind(Nav.def_at((level, i, t)), def =>
        Option.map(b => (def, b), Nav.brace_at(def))
      )
    ) {
    | Some((def, (j, brace))) =>
      let literal =
        switch (Nav.slice_at(lv, i)) {
        | Some(k) when lv.spans[k].body == Some(brace.id) => S.members(lv, k)
        | _ => None
        };
      let members =
        switch (literal) {
        | Some(members) => members
        | None when Tile.ends_with_in(t) =>
          let c = List.length(t.children) - 1;
          Nav.kid_level(Nav.kid_level(lv, i, t, c), j, brace, 0);
        | None => Nav.kid_level(lv, i + 1 + j, brace, 0)
        };
      Some((def, members));
    | None => None
    };
  | None => None
  };

/* how an item comes into a block: new, spaced as its neighbours are, or
   moved, with its place and the indentation of the line it was on */
type arrival =
  | Fresh(option(int))
  | Moved(origin, option(int));

/* [item] (member form) into module [m]'s members: after the last, or
   ([first]) before the first */
let into_members =
    (
      ~mod_root: bool,
      ~first=false,
      ~arrival=Fresh(None),
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
    (
      n == 0
        ? None
        : (
          switch (arrival) {
          | Fresh(src_indent) =>
            insert(
              b,
              ~pos=first ? Before(0) : After(n - 1),
              ~member=true,
              ~src_indent?,
              item,
            )
          | Moved(origin, src_indent) =>
            Some(
              put(
                b,
                ~at=Above(first ? 0 : n),
                ~gap=Option.value(origin.gap, ~default=[linebreak()]),
                ~offset=origin.offset,
                ~src_indent,
                ~member=true,
                item,
              ),
            )
          }
        )
    )
    |> Option.map(members =>
         Nav.splice_def(
           ~root=root_of(mod_root),
           m,
           Nav.with_brace_child(def_seg, members),
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
  switch (Nav.find_def(~root, m, seg)) {
  | None => None
  | Some(def_seg) =>
    let empty = (p: Piece.t) =>
      switch (p) {
      | Tile(t) => is_empty_body(t)
      | _ => false
      };
    let outer = Option.value(Nav.line_indent(~root, m, seg), ~default=0);
    let inner = outer + 2;
    let item =
      shift_by(
        Option.fold(~none=0, ~some=src => inner - src, src_indent),
        item,
      );
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
          Nav.splice_def(
            ~root,
            m,
            List.map(p => empty(p) ? brace : p, def_seg),
            seg,
          ),
        )
      : None;
  };
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
   body is (on its line when it's one line). A statement can't go into a
   member's body, where its `;` would end the member. Moved before a
   lone result: above a result on its own line, the gap it takes along
   between them; on the head's line when it's one line; else on lines of
   its own past the head's, and a constant's value with it (a function's
   result stays on its last line). [first]: its first, above any other */
let body_last =
    (
      ~mod_root: bool,
      ~first=false,
      ~arrival=Fresh(None),
      ~stmt=false,
      d: Id.t,
      item: Segment.t,
      seg: Segment.t,
    )
    : option(Segment.t) => {
  let root = root_of(mod_root);
  switch (Nav.tile_at(~root, d, seg)) {
  | Some((up, level, i, t)) when value_def((level, i, t)) =>
    switch (Nav.def_at((level, i, t))) {
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
      let b = block(~in_module=false, ~stmts=kid, body_lv);
      let n = Array.length(b.sl);
      let g = n - 1;
      let placed =
        switch (arrival) {
        | _ when n == 0 || !Nav.is_tail(body_lv, g) || stmt && !b.stmts =>
          None
        | Fresh(src_indent) =>
          insert(
            b,
            ~pos=Before(first ? 0 : g),
            ~member=false,
            ~src_indent?,
            item,
          )
        | Moved(origin, src_indent) when first && g > 0 =>
          Some(
            put(
              b,
              ~at=Above(0),
              ~gap=Option.value(origin.gap, ~default=[linebreak()]),
              ~offset=origin.offset,
              ~src_indent,
              ~member=false,
              item,
            ),
          )
        | Moved(origin, src_indent) =>
          let put = (~gap) =>
            Some(
              put(
                b,
                ~at=Above(g),
                ~gap,
                ~offset=origin.offset,
                ~src_indent,
                ~member=false,
                item,
              ),
            );
          if (g > 0 || starts_line(b, g)) {
            put(~gap=Option.value(origin.gap, ~default=[linebreak()]));
          } else if (!spans_lines(item)) {
            put(~gap=[]);
          } else {
            let r = b.sl[g];
            let head = item_indent(b, g);
            let lead = ind => [
              linebreak(),
              ...List.init(max(0, ind), _ => space()),
            ];
            let ind = head + 2 + origin.offset;
            Some(
              ListUtil.truncate(ws_back(b, r.lo), b.seg)
              @ lead(ind)
              @ shift_by(
                  Option.fold(~none=0, ~some=src => ind - src, src_indent),
                  item,
                )
              @ (
                fn_body(b)
                  ? [space(), ...ListUtil.remove_first_n(r.lo, b.seg)]
                  : lead(head + 2)
                    @ shift_by(2, ListUtil.slice(r.lo, r.eol - r.lo, b.seg))
                    @ ListUtil.remove_first_n(r.eol, b.seg)
              ),
            );
          };
        };
      Option.map(body => Nav.splice_def(~root, d, body, seg), placed);
    }
  | _ => None
  };
};

/* item [k]'s head, when it's a `let` of a value */
let value_at = (b: block, k: int): option(Id.t) =>
  switch (head(b, k)) {
  | Some((h, t)) when value_def((b.seg, h, t)) => Some(t.id)
  | _ => None
  };

/* the indentation of the line [item] (a program's pieces) starts on */
let start_indent =
    (~mod_root: bool, item: Segment.t, seg: Segment.t): option(int) =>
  switch (item) {
  | [p, ..._] => Nav.line_indent(~root=root_of(mod_root), Piece.id(p), seg)
  | [] => None
  };

/* the item holding [fid] taken out of [seg] */
type taken = {
  /* the program without it */
  rest: Segment.t,
  origin,
  /* a `…;` statement (a test), or a member that would be one */
  stmt: bool,
  /* no comment set apart lies between it and the item above, or below:
     it can go into that item and come back to where it was */
  near_above: bool,
  near_below: bool,
};

/* None for an item a delete refuses */
let take_out = (~mod_root, fid: Id.t, seg: Segment.t): option(taken) =>
  Option.bind(locate(~mod_root, fid, seg), ((b, j, rebuild)) =>
    j < Array.length(b.sl) && movable(b, j) && !runs_on(b, j)
      ? {
        let (level, origin) = take(b, j);
        Some({
          rest: rebuild(level),
          origin,
          stmt: b.lv.spans[j].kind == Stmt,
          near_above: under_item(b, j),
          near_below: j + 1 < Array.length(b.sl) && under_item(b, j + 1),
        });
      }
      : None
  );
