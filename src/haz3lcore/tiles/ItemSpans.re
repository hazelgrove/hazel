open Util;

/* A program's items by piece structure alone (plans/hierarchical-spans.md):
   the one division the per-item layers and the cells share. A level is the
   program's top level, a module literal's member list or a member that is
   a let chain, and on demand a definition's body; its spans are its items,
   whose slices partition it. Split down to definitions, never inside one.
   Pure: no parse, no statics. Levels are memoized on their pieces. */

type kind =
  /* let / type / module / theorem: an `…in` tile, or a member through its
     `;` */
  | Def
  /* a directive over what follows: `use M in`, or a filter `hide … in` */
  | Use
  /* `…;`, and a module body's last member without its `;` */
  | Stmt
  /* an expression level's last expression */
  | Tail;

type span = {
  /* the join key: a definition's or directive's head tile, a statement's
     `;`, else the item's first piece of content (an id derived from it for
     a last member that is a let chain, whose first item anchors there) */
  anchor: Id.t,
  /* the item's first tile: the outline names a member test by its `test` */
  first: option(Id.t),
  kind,
  /* the slice [lo, hi). Slices partition the level; the trivia before an
     item is its slice's */
  lo: int,
  hi: int,
  /* the item without edge whitespace or its `;`: what a cell holds */
  core: (int, int),
  /* its place by line, (lo, term, hi): from its first line's start, with
     that line's indentation and the comment lines directly above it,
     through the last linebreak after its terminator and what shares that
     line (from its first piece of content when it shares its first line).
     [term]: its terminator's end (its `in` or `;`, or the hole the parser
     leaves after a module's last `;`), else its content's */
  slot: (int, int, int),
  /* the slot's [lo, hi) when it is whole lines; None when it shares a
     line */
  lines: option((int, int)),
  /* the indentation of the line its core starts on */
  base_col: int,
  /* a module literal's brace tile, whose child is the member level */
  body: option(Id.t),
  /* the index of the `;` that ends it, if one */
  sep: option(int),
};

/* where a member level's items end. Written: at its member `;`s that no
   open form before takes. Read: also where an unfinished member meets its
   member line (MemberLines, where completion ends it), so an open form
   stays in its member. The parse and the layout divide by Read; cells,
   outline edits and completion by Written. They differ only at a member
   level holding an unfinished member */
type ends =
  | Written
  | Read;

type owner =
  | Top
  /* a module literal's members: the child of this brace tile */
  | Members(Id.t)
  /* a module member that is a let chain: its core, under the member's
     anchor */
  | Chain(Id.t)
  /* a definition's body, under this head tile: an `…in` tile's last child,
     or a member's run between its head and `;` */
  | Block(Id.t);

type level = {
  sort: Sort.t,
  owner,
  ends,
  /* a Read level whose unfinished member ended at its member line */
  cut: bool,
  seg: Segment.t,
  pieces: array(Piece.t),
  spans: array(span),
  /* the indentation of the line the level starts on */
  indent: int,
  /* the level of the line pieces[0] is on, when the level starts mid-line
     (MemberLines): past a module's `{`, the module's member level */
  line: option(int),
  /* the level starts / ends a line */
  bol: bool,
  eol: bool,
  /* where pieces[0] sits in the segment holding the level: 0 but for a
     let chain or a member's body run, which lie in their member level */
  offset: int,
  /* anchors and first tiles to span indices, made on first use (not a
     lazy value, so levels stay comparable) */
  mutable index: option(Id.Map.t(int)),
};

let is_ws = (p: Piece.t): bool =>
  switch (p) {
  | Secondary({content: Whitespace(_), _}) => true
  | _ => false
  };
let is_comment = (p: Piece.t): bool =>
  switch (p) {
  | Secondary(w) => Secondary.is_comment(w)
  | _ => false
  };

/* an `…in` item at an expression level: a definition or a directive */
let in_kind = (t: Tile.t): kind =>
  switch (Tile.label(t)) {
  | ["let" | "type" | "module" | "theorem", ..._] => Def
  | _ => Use
  };

/* what heads a run that is a definition: at an expression level any
   let/type/module/theorem; at a Mod level a member's `let … =`,
   `type … =` or `module … =` (an `…in` there opens a statement) */
let def_head = (~sort: Sort.t, t: Tile.t): bool =>
  switch (sort, Tile.label(t)) {
  | (Mod, ["let" | "type" | "module", "="]) => true
  | (Mod, _) => false
  | (_, ["let" | "type" | "module" | "theorem", ..._]) => true
  | _ => false
  };

/* the precedence of the operand a piece takes on its right, if any: its
   right nib's (Tile.nibs), the loosest when its last shard is missing */
let right_operand = (p: Piece.t): Precedence.t =>
  switch (p) {
  | Tile(t) =>
    let m = Tile.mold(t);
    if (Tile.r_shard(t) == List.length(m.in_)) {
      switch (snd(m.nibs).shape) {
      | Concave(prec) => prec
      | Convex => Precedence.max
      };
    } else {
      Precedence.min;
    };
  | _ => Precedence.max
  };

/* a function body shares its segment with its `fun x ->` head */
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

/* a brace whose child is a module's member list */
let module_body = (p: Piece.t): option(Tile.t) =>
  switch (p) {
  | Tile({children: [_], _} as t)
      when Tile.is_complete(t) && List.mem(Sort.Mod, Tile.mold(t).in_) =>
    Some(t)
  | _ => None
  };

/* ---- line indentation: a line's leading spaces, where only a linebreak
   starts a line. Projectors are opaque ---- */

/* the spaces right after [j] (a linebreak, or -1 for the start), before
   [stop] */
let spaces_after = (arr: array(Piece.t), j: int, stop: int): int => {
  let rec go = (i, n) =>
    i < stop && Piece.is_space(arr[i]) ? go(i + 1, n + 1) : n;
  go(j + 1, 0);
};

/* the indentation after [seg]'s last linebreak, at any depth, if it has
   one: found from the end, so a multi-line tile costs its last line */
let rec last_line = (seg: Segment.t): option(int) => {
  let arr = Array.of_list(seg);
  let rec back = j =>
    j < 0
      ? None
      : (
        switch (arr[j]) {
        | Secondary(w) when Secondary.is_linebreak(w) =>
          Some(spaces_after(arr, j, Array.length(arr)))
        | Tile(t) =>
          switch (last_line_tile(t)) {
          | Some(_) as r => r
          | None => back(j - 1)
          }
        | _ => back(j - 1)
        }
      );
  back(Array.length(arr) - 1);
}
and last_line_tile = (t: Tile.t): option(int) =>
  List.fold_right(
    (kid, found) =>
      switch (found) {
      | Some(_) => found
      | None => last_line(kid)
      },
    t.children,
    None,
  );

/* the indentation of the line holding [arr]'s piece [i]. [outer]: where
   [arr] starts (None: a program's start, whose leading spaces count);
   [known]: an earlier piece and its line's indentation, where the search
   may stop */
let indent_at =
    (
      ~outer: option(int),
      ~known: option((int, int))=?,
      arr: array(Piece.t),
      i: int,
    )
    : int => {
  let rec back = j =>
    if (j < 0) {
      switch (outer) {
      | Some(n) => n
      | None => spaces_after(arr, -1, i)
      };
    } else {
      switch (arr[j], known) {
      | (Secondary(w), _) when Secondary.is_linebreak(w) =>
        spaces_after(arr, j, i)
      | (Tile(t), _) =>
        switch (last_line_tile(t), known) {
        | (Some(n), _) => n
        | (None, Some((k, n))) when j <= k => n
        | (None, _) => back(j - 1)
        }
      | (_, Some((k, n))) when j <= k => n
      | _ => back(j - 1)
      };
    };
  back(i - 1);
};

/* the indentation where child [c] of tile [t] (piece [h] of [arr])
   starts */
let kid_indent =
    (~outer: option(int), arr: array(Piece.t), h: int, t: Tile.t, c: int)
    : int => {
  let rec earlier = (k, kids) =>
    switch (kids) {
    | [] => indent_at(~outer, arr, h)
    | [kid, ...kids] =>
      switch (k < c ? last_line(kid) : None) {
      | Some(n) => n
      | None => earlier(k - 1, kids)
      }
    };
  earlier(List.length(t.children) - 1, List.rev(t.children));
};

/* ---- the cut rule ---- */

/* an item before trimming: its run [r_start, r_stop), with its trailing
   whitespace, and where its slice ends (after its `in` or separator) */
type raw = {
  r_start: int,
  r_stop: int,
  r_cut: int,
  r_kind: kind,
  r_head: option(int),
  /* the `;` (or a missing `;`'s grout) it ends with */
  r_semi: option(int),
};

/* An `…in` tile not in an operand's place ends a definition or directive
   (with its `in`: an unfinished `let x =` runs on); a `;` ends a
   statement unless a form before it in the item takes it into its right
   operand. At a Mod level an item ends only at a member `;` (or, with
   [grout], a concave grout: a missing `;`) or the range's end, never
   after an `in`. Total: any segment divides. [nested]: a definition's
   body, which has items only if it is divided. Over [arr]'s pieces
   [from, until) */
let raw_spans =
    (
      ~sort: Sort.t,
      ~nested: bool,
      ~grout=false,
      ~from=0,
      ~until=?,
      arr: array(Piece.t),
    )
    : list(raw) => {
  let len = Option.value(until, ~default=Array.length(arr));
  let at_mod = sort == Sort.Mod;
  let rec ws_end = i => i < len && is_ws(arr[i]) ? ws_end(i + 1) : i;
  let rec run_head = (i, stop) =>
    i >= stop
      ? None
      : (
        switch (arr[i]) {
        | Tile(t) => def_head(~sort, t) ? Some(i) : None
        | _ => run_head(i + 1, stop)
        }
      );
  /* the loosest right operand among the run's pieces before [i], which a
     `;` at [i] falls into; read only at `;`s */
  let reach = ref(Precedence.max);
  let upto = ref(0);
  let reach_before = i => {
    while (upto^ < i) {
      reach := max(reach^, right_operand(arr[upto^]));
      incr(upto);
    };
    reach^;
  };
  let rec walk = (i, start, ~operand, acc) =>
    if (i >= len) {
      let rec content = j => j < len && (!is_ws(arr[j]) || content(j + 1));
      let head = run_head(start, len);
      let tail_ok = head != None || !nested || acc != [];
      List.rev(
        content(start) && tail_ok
          ? [
            {
              r_start: start,
              r_stop: len,
              r_cut: len,
              r_kind: head != None ? Def : at_mod ? Stmt : Tail,
              r_head: head,
              r_semi: None,
            },
            ...acc,
          ]
          : acc,
      );
    } else {
      let next = (stop, r) => {
        reach := Precedence.max;
        upto := stop;
        walk(stop, stop, ~operand=false, [r, ...acc]);
      };
      /* a statement or member ending with the separator at [i] */
      let ends_at = i => {
        let head = run_head(start, i);
        next(
          ws_end(i + 1),
          {
            r_start: start,
            r_stop: ws_end(i + 1),
            r_cut: i + 1,
            r_kind: head != None ? Def : Stmt,
            r_head: head,
            r_semi: Some(i),
          },
        );
      };
      switch (arr[i]) {
      | Tile(t) as p when !at_mod && !operand && Segment.is_in_tile(p) =>
        let stop = ws_end(i + 1);
        next(
          stop,
          {
            r_start: i,
            r_stop: stop,
            r_cut: i + 1,
            r_kind: in_kind(t),
            r_head: Some(i),
            r_semi: None,
          },
        );
      | Tile(t) as p
          when
            Tile.is_semi(t)
            && (!at_mod || t.sort == Sort.Mod)
            && reach_before(i) <= right_operand(p) =>
        ends_at(i)
      /* a Mod skel reads a concave grout as a member `;` */
      | Grout({shape: Concave, _})
          when at_mod && grout && reach_before(i) <= Precedence.mod_seq =>
        ends_at(i)
      | p =>
        walk(i + 1, start, ~operand=operand || !Piece.is_secondary(p), acc)
      };
    };
  /* a function body's items start after its heads */
  let rec after_heads = i => {
    let j = ws_end(i);
    j < len && is_fun_head(arr[j]) ? after_heads(j + 1) : i;
  };
  let first =
    switch (at_mod ? from : after_heads(from)) {
    | i when i == from => from
    | i => ws_end(i)
    };
  upto := first;
  walk(first, first, ~operand=false, []);
};

/* ---- definitions ---- */

/* where a definition's body is: the last child of its `…in` head at [h],
   or the run [from, until) after its member head at [h] */
type site =
  | Kid(int, Tile.t)
  | Run(int, int, int);

/* the body of the definition headed at [h] whose core ends at [until] */
let site_at = (arr: array(Piece.t), h: int, until: int): option(site) =>
  switch (arr[h]) {
  | Tile(t) when Tile.is_complete(t) =>
    Some(Tile.ends_with_in(t) ? Kid(h, t) : Run(h, h + 1, until))
  | _ => None
  };

/* the only content piece among [arr]'s pieces [a, b), if one */
let only_content = (arr: array(Piece.t), a: int, b: int): option(int) => {
  let rec go = (i, found) =>
    i >= b
      ? found
      : Piece.is_secondary(arr[i])
          ? go(i + 1, found) : found == None ? go(i + 1, Some(i)) : None;
  go(a, None);
};
let only_content_seg = (seg: Segment.t): option((int, Piece.t)) => {
  let rec go = (i, ps: Segment.t, found) =>
    switch (ps) {
    | [] => found
    | [p, ...ps] when Piece.is_secondary(p) => go(i + 1, ps, found)
    | [p, ...ps] => found == None ? go(i + 1, ps, Some((i, p))) : None
    };
  go(0, seg, None);
};

/* a module literal's brace, and its index in the segment holding it: the
   head's last child, or the member level */
let brace_of = (arr: array(Piece.t), s: site): option((int, Tile.t)) =>
  switch (s) {
  | Kid(_, t) =>
    Option.bind(only_content_seg(ListUtil.last(t.children)), ((m, p)) =>
      Option.map(b => (m, b), module_body(p))
    )
  | Run(_, a, b) =>
    Option.bind(only_content(arr, a, b), m =>
      Option.map(br => (m, br), module_body(arr[m]))
    )
  };

/* a module member that is a let chain: its core [c0, c1) opens with an
   `…in` item, which the expression rule cuts */
let opens_chain = (arr: array(Piece.t), c0: int, c1: int): bool => {
  let rec content = i =>
    i < c1 && Piece.is_secondary(arr[i]) ? content(i + 1) : i;
  let i = content(c0);
  i < c1 && Segment.is_in_tile(arr[i]);
};

/* ---- levels ---- */

let index_of = (spans: array(span)): Id.Map.t(int) => {
  let m = ref(Id.Map.empty);
  Array.iteri(
    (k, sp) => {
      Option.iter(id => m := Id.Map.add(id, k, m^), sp.first);
      m := Id.Map.add(sp.anchor, k, m^);
    },
    spans,
  );
  m^;
};

/* a Read member level's raws: each Written item cut where completion
   ends an unfinished member (its units), each part divided as written;
   and whether a member line cut one */
let read_raws =
    (~nested: bool, ~line: option(int), arr: array(Piece.t))
    : (list(raw), bool) => {
  let len = Array.length(arr);
  let written = Array.of_list(raw_spans(~sort=Mod, ~nested, arr));
  let n = Array.length(written);
  let cut = ref(false);
  let parts =
    List.init(
      max(n, 1),
      k => {
        let lo = k == 0 ? 0 : written[k - 1].r_cut;
        let hi = k >= n - 1 ? len : written[k].r_cut;
        let slice = Array.to_list(Array.sub(arr, lo, hi - lo));
        switch (
          Segment.incomplete_tiles(slice) == []
            ? []
            : MemberLines.member_parts(
                ~line=
                  lo == 0
                    ? line : Some(MemberLines.level_at(~line, arr, lo)),
                ~sort=Mod,
                slice,
              )
        ) {
        | []
        | [_] => [(lo, hi)]
        | parts =>
          cut := true;
          List.rev(
            snd(
              List.fold_left(
                ((at, acc), (part, _)) => {
                  let next = at + List.length(part);
                  (next, [(at, next), ...acc]);
                },
                (lo, []),
                parts,
              ),
            ),
          );
        };
      },
    )
    |> List.concat;
  (
    List.concat_map(
      ((from, until)) => raw_spans(~sort=Mod, ~nested, ~from, ~until, arr),
      parts,
    ),
    cut^,
  );
};

/* the member separators of a member level's run: its `;`s and missing
   `;`s' grouts that no open form before them takes, as a Mod skel reads
   them */
let separators = (seg: Segment.t): list(Id.t) => {
  let arr = Array.of_list(seg);
  List.filter_map(
    (r: raw) => Option.map(i => Piece.id(arr[i]), r.r_semi),
    raw_spans(~sort=Mod, ~nested=false, ~grout=true, arr),
  );
};

/* [bol]/[eol]: the level starts/ends a line (a program does) */
let build =
    (
      ~ends=Written,
      ~line=None,
      ~sort: Sort.t,
      ~owner: owner,
      ~nested: bool,
      ~bol: bool,
      ~eol: bool,
      ~indent: option(int),
      ~offset: int,
      seg: Segment.t,
    )
    : level => {
  let arr = Array.of_list(seg);
  let len = Array.length(arr);
  let (raws, cut) =
    ends == Read && sort == Sort.Mod
      ? read_raws(~nested, ~line, arr)
      : (raw_spans(~sort, ~nested, arr), false);
  let raws = Array.of_list(raws);
  let first_content = (r: raw) => {
    let rec go = i =>
      i < r.r_stop && Piece.is_secondary(arr[i]) ? go(i + 1) : i;
    go(r.r_start);
  };
  /* trailing comments are trivia of the item before; so, at a Mod level,
     is the hole the parser leaves after a last `;`, which then ends that
     member's terminator */
  let hole_only = (r: raw) => {
    let rec go = (i, found) =>
      i >= r.r_stop
        ? found
        : (
          switch (arr[i]) {
          | Grout({shape: Convex, _}) when found == None =>
            go(i + 1, Some(i))
          | Secondary(_) => go(i + 1, found)
          | _ => None
          }
        );
    go(r.r_start, None);
  };
  let n = Array.length(raws);
  let (count, hole) =
    if (n == 0) {
      (0, None);
    } else if (first_content(raws[n - 1]) >= raws[n - 1].r_stop) {
      (n - 1, None);
    } else if (sort == Sort.Mod) {
      switch (hole_only(raws[n - 1])) {
      | Some(_) when n == 1 => (0, None)
      | Some(g) => (n - 1, Some(g))
      | None => (n, None)
      };
    } else {
      (n, None);
    };
  let line_start = i => i == 0 ? bol : Piece.is_linebreak(arr[i - 1]);
  let floor = ref(0);
  /* the last core start and its line's indentation */
  let known = ref(None);
  let spans =
    Array.init(
      count,
      k => {
        let r = raws[k];
        let last = k == count - 1;
        /* the core: the run without edge whitespace or `;` */
        let rec skip_ws = i =>
          i < r.r_stop && is_ws(arr[i]) ? skip_ws(i + 1) : i;
        let c0 = skip_ws(r.r_start);
        let rec back_ws = i =>
          i > c0 && is_ws(arr[i - 1]) ? back_ws(i - 1) : i;
        let c1 = {
          let e = back_ws(r.r_stop);
          back_ws(e > c0 && Piece.is_semi(arr[e - 1]) ? e - 1 : e);
        };
        let f = first_content(r);
        let rec first_tile = i =>
          i >= c1
            ? None
            : (
              switch (arr[i]) {
              | Tile(t) => Some(t.id)
              | _ => first_tile(i + 1)
              }
            );
        let anchor =
          switch (r.r_kind, r.r_head, r.r_semi) {
          | (Def | Use, Some(h), _) => Piece.id(arr[h])
          | (_, _, Some(s)) => Piece.id(arr[s])
          /* a last member that is a let chain: its first piece anchors the
             chain's first item */
          | (Stmt, _, None) when sort == Sort.Mod && opens_chain(arr, c0, c1) =>
            Id.derive(~salt="chain", Piece.id(arr[f]))
          | _ => Piece.id(arr[f])
          };
        /* the slot's start: its first line's, with the comment lines
           directly above */
        let rec back_spaces = i =>
          i > floor^ && Piece.is_space(arr[i - 1]) ? back_spaces(i - 1) : i;
        let i = back_spaces(f);
        let rec attach = lo =>
          if (lo <= floor^) {
            lo;
          } else {
            let rec back = (j, seen) =>
              j > floor^
              && (Piece.is_space(arr[j - 1]) || is_comment(arr[j - 1]))
                ? back(j - 1, seen || is_comment(arr[j - 1])) : (j, seen);
            let (j, seen) = back(lo - 1, false);
            seen && line_start(j) ? attach(j) : lo;
          };
        let lo = line_start(i) ? attach(i) : f;
        let term =
          switch (hole) {
          | Some(g) when last => g + 1
          | _ =>
            let rec back = i =>
              i > f && Piece.is_secondary(arr[i - 1]) ? back(i - 1) : i;
            back(r.r_stop);
          };
        /* what shares its last line, then whitespace through its last
           linebreak (the next line's indentation is the next item's) */
        let rec same_line = i =>
          i < len && (Piece.is_space(arr[i]) || is_comment(arr[i]))
            ? same_line(i + 1) : i;
        let s = same_line(term);
        let hi =
          if (s < len && Piece.is_linebreak(arr[s])) {
            let rec through = (i, at) =>
              i < len && is_ws(arr[i])
                ? through(i + 1, Piece.is_linebreak(arr[i]) ? i + 1 : at)
                : at;
            through(s, s + 1);
          } else {
            s;
          };
        floor := hi;
        let lines =
          line_start(lo)
          && (Piece.is_linebreak(arr[hi - 1]) || hi == len && eol)
            ? Some((lo, hi)) : None;
        let base_col = indent_at(~outer=indent, ~known=?known^, arr, c0);
        known := Some((c0, base_col));
        let body =
          switch (r.r_kind, r.r_head) {
          | (Def, Some(h)) =>
            Option.bind(site_at(arr, h, c1), s =>
              Option.map(((_, b: Tile.t)) => b.id, brace_of(arr, s))
            )
          | _ => None
          };
        {
          anchor,
          first: first_tile(c0),
          kind: r.r_kind,
          lo: k == 0 ? 0 : raws[k - 1].r_cut,
          hi: last ? len : r.r_cut,
          core: (c0, c1),
          slot: (lo, term, hi),
          lines,
          base_col,
          body,
          sep: r.r_semi,
        };
      },
    );
  {
    sort,
    owner,
    ends,
    cut,
    seg,
    pieces: arr,
    spans,
    indent:
      switch (indent) {
      | Some(n) => n
      | None => spaces_after(arr, -1, len)
      },
    line,
    bol,
    eol,
    offset,
    index: None,
  };
};

let index = (lv: level): Id.Map.t(int) =>
  switch (lv.index) {
  | Some(m) => m
  | None =>
    let m = index_of(lv.spans);
    lv.index = Some(m);
    m;
  };

/* levels by owner, valid while their pieces are physically the same and
   they start at the same indentation. The top level is keyed by its first
   piece too, so editors don't evict each other */
type key =
  | KTop(ends, Sort.t, Id.t)
  | KMembers(ends, Id.t)
  | KChain(ends, Id.t)
  | KBlock(Id.t);

let cache: Swept.t(key, level) = Swept.mk();
/* levels built, cumulative: observability for tests */
let built = ref(0);

let memo =
    (
      key: key,
      ~offset=0,
      ~indent: option(int),
      ~line: option(int),
      ~bol: bool,
      ~eol: bool,
      seg: Segment.t,
      mk: unit => level,
    )
    : level =>
  switch (Swept.find_opt(cache, key)) {
  | Some(lv)
      when
        Segment.ptr_eq(lv.seg, seg)
        && lv.offset == offset
        && Option.fold(~none=true, ~some=n => lv.indent == n, indent)
        && lv.line == line
        && lv.bol == bol
        && lv.eol == eol => lv
  | _ =>
    incr(built);
    let lv = mk();
    Swept.replace(cache, key, lv);
    lv;
  };

/* the top level of a program rooted at [root] (Mod, else Exp); its
   member levels end their items as it does */
let top = (~ends=Written, ~root: Sort.t, seg: Segment.t): level => {
  Swept.next_pass(cache);
  let sort = root == Sort.Mod ? Sort.Mod : Sort.Exp;
  let first =
    switch (seg) {
    | [p, ..._] => Piece.id(p)
    | [] => Id.invalid
    };
  memo(
    KTop(ends, sort, first),
    ~indent=None,
    ~line=None,
    ~bol=true,
    ~eol=true,
    seg,
    () =>
    build(
      ~ends,
      ~sort,
      ~owner=Top,
      ~nested=false,
      ~bol=true,
      ~eol=true,
      ~indent=None,
      ~offset=0,
      seg,
    )
  );
};

/* the indentation [build] takes for a level inside [lv] */
let outer = (lv: level): option(int) =>
  switch (lv.owner) {
  | Top => None
  | Members(_)
  | Chain(_)
  | Block(_) => Some(lv.indent)
  };

/* span [k]'s definition body, if it is a definition */
let site_of = (lv: level, k: int): option(site) => {
  let sp = lv.spans[k];
  let (c0, c1) = sp.core;
  let rec head = i =>
    i >= c1
      ? None : Piece.id(lv.pieces[i]) == sp.anchor ? Some(i) : head(i + 1);
  switch (sp.kind) {
  | Def => Option.bind(head(c0), h => site_at(lv.pieces, h, c1))
  | Use
  | Stmt
  | Tail => None
  };
};

/* the member level of a module literal's members, in a level starting at
   [line] whose site [s] holds the brace at [m]: its `{`'s line's level, a
   step in */
let members_line =
    (~line: option(int), arr: array(Piece.t), s: site, m: int): int =>
  MemberLines.member_level(
    switch (s) {
    | Kid(h, t) =>
      MemberLines.level_at(
        ~line=Some(MemberLines.level_at(~line, arr, h)),
        Array.of_list(ListUtil.last(t.children)),
        m,
      )
    | Run(_) => MemberLines.level_at(~line, arr, m)
    },
  );

/* span [k]'s member level, if it is a module literal */
let members = (lv: level, k: int): option(level) =>
  switch (lv.spans[k].body) {
  | None => None
  | Some(_) =>
    let s = site_of(lv, k);
    switch (Option.bind(s, brace_of(lv.pieces)), s) {
    | (Some((m, brace)), Some(s)) =>
      let indent =
        switch (s) {
        | Kid(h, t) =>
          let c = List.length(t.children) - 1;
          indent_at(
            ~outer=Some(kid_indent(~outer=outer(lv), lv.pieces, h, t, c)),
            Array.of_list(ListUtil.last(t.children)),
            m,
          );
        | Run(_) => indent_at(~outer=outer(lv), lv.pieces, m)
        };
      let seg = List.hd(brace.children);
      let line = Some(members_line(~line=lv.line, lv.pieces, s, m));
      Some(
        memo(
          KMembers(lv.ends, brace.id),
          ~indent=Some(indent),
          ~line,
          ~bol=false,
          ~eol=false,
          seg,
          () =>
          build(
            ~ends=lv.ends,
            ~line,
            ~sort=Sort.Mod,
            ~owner=Members(brace.id),
            ~nested=false,
            ~bol=false,
            ~eol=false,
            ~indent=Some(indent),
            ~offset=0,
            seg,
          )
        ),
      );
    | _ => None
    };
  };

/* span [k]'s let chain, if it is a module member that is one (its core
   opens with an `…in` item): an expression level over its core, cut by the
   expression rule, its items in place of the one member */
let chain = (lv: level, k: int): option(level) => {
  let sp = lv.spans[k];
  let (c0, c1) = sp.core;
  let arr = lv.pieces;
  switch (lv.sort, sp.kind) {
  | (Mod, Stmt) when opens_chain(arr, c0, c1) =>
    let bol = c0 == 0 ? lv.bol : Piece.is_linebreak(arr[c0 - 1]);
    let eol = c1 == Array.length(arr) ? lv.eol : Piece.is_linebreak(arr[c1]);
    let indent = Some(sp.base_col);
    let seg = Array.to_list(Array.sub(arr, c0, c1 - c0));
    let line = Some(MemberLines.level_at(~line=lv.line, arr, c0));
    Some(
      memo(
        KChain(lv.ends, sp.anchor),
        ~offset=c0,
        ~indent,
        ~line,
        ~bol,
        ~eol,
        seg,
        () =>
        build(
          ~ends=lv.ends,
          ~line,
          ~sort=Sort.Exp,
          ~owner=Chain(sp.anchor),
          ~nested=false,
          ~bol,
          ~eol,
          ~indent,
          ~offset=c0,
          seg,
        )
      ),
    );
  | _ => None
  };
};

/* the level under span [k]: a module literal's members, or the items of a
   member that is a let chain */
let children = (lv: level, k: int): option(level) =>
  switch (members(lv, k)) {
  | Some(_) as m => m
  | None => chain(lv, k)
  };

/* span [k]'s body as a level of its own items (a function body, a let
   chain), if it has any: for the outline, cells and paths, built on
   demand. Not for module literals (they have members) or type
   definitions */
let block = (lv: level, k: int): option(level) => {
  let sp = lv.spans[k];
  let mk = (~indent, ~line, ~offset, seg) => {
    let line = Some(line);
    let indent = Some(indent);
    let b =
      memo(
        KBlock(sp.anchor),
        ~offset,
        ~indent,
        ~line,
        ~bol=false,
        ~eol=false,
        seg,
        () =>
        build(
          ~line,
          ~sort=Sort.Exp,
          ~owner=Block(sp.anchor),
          ~nested=true,
          ~bol=false,
          ~eol=false,
          ~indent,
          ~offset,
          seg,
        )
      );
    Array.length(b.spans) > 0 ? Some(b) : None;
  };
  let is_type = (t: Tile.t) => Tile.token(t, 0) == "type";
  switch (sp.body, site_of(lv, k)) {
  | (None, Some(Kid(h, t))) when !is_type(t) =>
    let c = List.length(t.children) - 1;
    mk(
      ~indent=kid_indent(~outer=outer(lv), lv.pieces, h, t, c),
      ~line=MemberLines.level_at(~line=lv.line, lv.pieces, h),
      ~offset=0,
      ListUtil.last(t.children),
    );
  | (None, Some(Run(h, a, b))) when a < b =>
    switch (lv.pieces[h]) {
    | Tile(t) when !is_type(t) =>
      mk(
        ~indent=indent_at(~outer=outer(lv), lv.pieces, a),
        ~line=MemberLines.level_at(~line=lv.line, lv.pieces, a),
        ~offset=a,
        Array.to_list(Array.sub(lv.pieces, a, b - a)),
      )
    | _ => None
    }
  | _ => None
  };
};

/* span [k]'s pieces [lo, hi) */
let slice = (lv: level, k: int): Segment.t => {
  let sp = lv.spans[k];
  Array.to_list(Array.sub(lv.pieces, sp.lo, sp.hi - sp.lo));
};

/* span [k] of a member level is an unfinished member whose open form
   takes the members after its member line: Read cuts it there */
let runs_past_line = (lv: level, k: int): bool =>
  lv.sort == Sort.Mod
  && {
    let lo = lv.spans[k].lo;
    let item = slice(lv, k);
    Segment.incomplete_tiles(item) != []
    && List.length(
         MemberLines.member_parts(
           ~line=
             lo == 0
               ? lv.line
               : Some(MemberLines.level_at(~line=lv.line, lv.pieces, lo)),
           ~sort=Mod,
           item,
         ),
       )
    > 1;
  };

/* the level's partition, each slice keyed by its span's anchor; a level
   with no item (only trivia) is one slice, keyed by its first piece */
let keyed = (lv: level): list((Id.t, Segment.t)) =>
  switch (lv.spans, lv.seg) {
  | ([||], []) => []
  | ([||], [p, ..._] as seg) => [(Piece.id(p), seg)]
  | (spans, _) =>
    List.init(Array.length(spans), k => (spans[k].anchor, slice(lv, k)))
  };

let slices = (lv: level): list(Segment.t) => List.map(snd, keyed(lv));

/* a lone module literal item, for a layer that redoes its members: its
   member slices, the item with a new member list in their place (the
   brace and the spine above it rebuilt, its other pieces its own), and the
   level of the members' first line, the item starting at [line] (see
   [level.line]). Unmemoized: the item is a slice, not a program */
let module_item =
    (~root: Sort.t, ~line=None, item: Segment.t)
    : option((list(Segment.t), Segment.t => Segment.t, option(int))) => {
  let lv =
    build(
      ~line,
      ~sort=root == Sort.Mod ? Sort.Mod : Sort.Exp,
      ~owner=Top,
      ~nested=false,
      ~bol=true,
      ~eol=true,
      ~indent=None,
      ~offset=0,
      item,
    );
  switch (lv.spans) {
  | [|{body: Some(_), _}|] =>
    Option.bind(site_of(lv, 0), s =>
      Option.map(
        ((m, brace: Tile.t)) => {
          let members =
            build(
              ~sort=Sort.Mod,
              ~owner=Members(brace.id),
              ~nested=false,
              ~bol=false,
              ~eol=false,
              ~indent=Some(0),
              ~offset=0,
              List.hd(brace.children),
            );
          let put = (seg: Segment.t): Segment.t => {
            let brace' =
              Piece.Tile({
                ...brace,
                children: [seg],
              });
            switch (s) {
            | Kid(h, t) =>
              let (init, kid) = ListUtil.split_last(t.children);
              ListUtil.put_nth(
                h,
                Piece.Tile({
                  ...t,
                  children: init @ [ListUtil.put_nth(m, brace', kid)],
                }),
                item,
              );
            | Run(_) => ListUtil.put_nth(m, brace', item)
            };
          };
          (
            slices(members),
            put,
            Some(members_line(~line, lv.pieces, s, m)),
          );
        },
        brace_of(lv.pieces, s),
      )
    )
  | _ => None
  };
};

/* a lone module member that is a let chain, for a layer that redoes its
   items: the chain's slices, the member with new pieces for them (its
   trivia, `;` and what follows its own), and the level of the chain's
   first line, the member starting at [line]. Unmemoized */
let chain_item =
    (~line=None, item: Segment.t)
    : option((list(Segment.t), Segment.t => Segment.t, option(int))) => {
  let lv =
    build(
      ~line,
      ~sort=Sort.Mod,
      ~owner=Top,
      ~nested=false,
      ~bol=true,
      ~eol=true,
      ~indent=None,
      ~offset=0,
      item,
    );
  switch (lv.spans) {
  | [|{kind: Stmt, core: (c0, c1), anchor, _}|]
      when opens_chain(lv.pieces, c0, c1) =>
    let sub = (a, b) => Array.to_list(Array.sub(lv.pieces, a, b - a));
    let chain =
      build(
        ~sort=Sort.Exp,
        ~owner=Chain(anchor),
        ~nested=false,
        ~bol=false,
        ~eol=false,
        ~indent=Some(0),
        ~offset=c0,
        sub(c0, c1),
      );
    let (pre, post) = (sub(0, c0), sub(c1, Array.length(lv.pieces)));
    Some((
      slices(chain),
      (seg => pre @ seg @ post),
      Some(MemberLines.level_at(~line, lv.pieces, c0)),
    ));
  | _ => None
  };
};

/* depth first, child levels included: [f] sees each span's path from
   [lv], outermost first */
let fold = (f: ('a, list((level, int))) => 'a, acc: 'a, lv: level): 'a => {
  let rec go = (acc, path, lv) => {
    let acc = ref(acc);
    Array.iteri(
      (k, _) => {
        let here = [(lv, k), ...path];
        acc := f(acc^, List.rev(here));
        switch (children(lv, k)) {
        | Some(m) => acc := go(acc^, here, m)
        | None => ()
        };
      },
      lv.spans,
    );
    acc^;
  };
  go(acc, [], lv);
};

/* the path to the span with this anchor (or first tile), outermost first,
   through child levels: one probe per level searched. An anchor below a
   first tile is the closer match: a let chain's first item */
let find =
    (~root: Sort.t, id: Id.t, seg: Segment.t): option(list((level, int))) => {
  let rec go = (path, lv) =>
    switch (Id.Map.find_opt(id, index(lv))) {
    | Some(k) when lv.spans[k].anchor == id =>
      Some(List.rev([(lv, k), ...path]))
    | Some(k) =>
      switch (Option.bind(children(lv, k), go([(lv, k), ...path]))) {
      | Some(_) as below => below
      | None => Some(List.rev([(lv, k), ...path]))
      }
    | None =>
      let n = Array.length(lv.spans);
      let rec each = k =>
        k >= n
          ? None
          : (
            switch (children(lv, k)) {
            | Some(m) =>
              switch (go([(lv, k), ...path], m)) {
              | Some(_) as r => r
              | None => each(k + 1)
              }
            | None => each(k + 1)
            }
          );
      each(0);
    };
  go([], top(~root, seg));
};
