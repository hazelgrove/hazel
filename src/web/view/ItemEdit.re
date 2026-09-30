module Focus = ScratchFocus;

open Haz3lcore;

let parse = (~root=Sort.Exp, txt: string): option(Segment.t) =>
  FastParse.of_text(
    ~materialize=Triggers.invoked_projector,
    ~collect_refractors=true,
    ~root,
    txt,
  );

/* a module body left with only whitespace: the parser's form for `{}` is
   one nullary tile, not a brace with an empty child */
let empty_body = (): option(Piece.t) => {
  let rec find = (ps: Segment.t): option(Piece.t) =>
    List.find_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) when Tile.label(t) == ["{}"] => Some(p)
        | Tile(t) => List.find_map(find, t.children)
        | _ => None
        },
      ps,
    );
  Option.bind(parse("module Zz = {} in\n0"), find);
};

let first_tile_id = (seg: Segment.t): option(Id.t) =>
  List.find_map(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) => Some(t.id)
      | _ => None
      },
    seg,
  );

/* fresh-id member pieces for [txt] (one member, `;` optional): parsed
   inside a real module so it sorts as a member, before a dummy member
   so it keeps its `;`, then cut after that `;` and its whitespace */
let member_chunk = (txt: string): option(Segment.t) => {
  let txt = String.trim(txt);
  let txt =
    String.length(txt) > 0 && txt.[String.length(txt) - 1] == ';'
      ? String.sub(txt, 0, String.length(txt) - 1) : txt;
  switch (parse("module Zz = {" ++ txt ++ {js|;
let zz = ¿} in
0|js})) {
  | None => None
  | Some(seg) =>
    let body = {
      let rec find_mod = (ps: Segment.t) =>
        switch (ps) {
        | [] => None
        | [Piece.Tile(t), ...rest] =>
          switch (Tile.label(t)) {
          | ["module", ..._] =>
            switch (List.rev(t.children)) {
            | [def, ..._] =>
              List.find_map(
                (p: Piece.t) =>
                  switch (p) {
                  | Tile(bt) =>
                    switch (bt.children) {
                    | [inner] => Some(inner)
                    | _ => None
                    }
                  | _ => None
                  },
                def,
              )
            | [] => None
            }
          | _ => find_mod(rest)
          }
        | [_, ...rest] => find_mod(rest)
        };
      find_mod(seg);
    };
    switch (body) {
    | None => None
    | Some(members) =>
      let arr = Array.of_list(members);
      let n = Array.length(arr);
      let rec last_semi = (i, best) =>
        i >= n
          ? best : last_semi(i + 1, Focus.is_semi(arr[i]) ? Some(i) : best);
      switch (last_semi(0, None)) {
      | None => None
      | Some(j) =>
        let rec ws_end = i =>
          i < n && Focus.is_edge_ws(arr[i]) ? ws_end(i + 1) : i;
        Some(Focus.take(ws_end(j + 1), members));
      };
    };
  };
};

/* a member as (core, terminator, has `;`): the terminator is its `;` and
   trailing whitespace, or just the whitespace (a last member may lack `;`) */
let split_terminator = (ps: Segment.t): (Segment.t, Segment.t, bool) => {
  let arr = Array.of_list(ps);
  let rec back = i =>
    i > 0 && Focus.is_edge_ws(arr[i - 1]) ? back(i - 1) : i;
  let at = back(Array.length(arr));
  at > 0 && Focus.is_semi(arr[at - 1])
    ? (Focus.take(at - 1, ps), Focus.drop(at - 1, ps), true)
    : (Focus.take(at, ps), Focus.drop(at, ps), false);
};

/* drop a member block's bare trailing `;` (its last member removed, or
   a terminated one appended): unlike parsed text, a built segment has
   no hole after it, and Skel fails on the bare separator */
let drop_trailing_semi = (block: Segment.t): Segment.t => {
  let arr = Array.of_list(block);
  let rec back = i =>
    i > 0 && Focus.is_edge_ws(arr[i - 1]) ? back(i - 1) : i;
  let at = back(Array.length(arr));
  at > 0 && Focus.is_semi(arr[at - 1])
    ? Focus.take(at - 1, block) @ Focus.drop(at, block) : block;
};

/* [op] on span [j] of its own block: an op invalid there (a move at the
   block's edge) no-ops rather than act on the enclosing item. [in_module]:
   a member block, so new items are 2-shard members, not `… in` forms */
let apply_at =
    (
      ~name: option(string)=?,
      op: OutlineSidebar.def_op,
      ~in_module: bool,
      spans: array(Focus.item_span),
      j: int,
      seg: Segment.t,
    )
    : option((Segment.t, option(Id.t))) => {
  let named = placeholder => Option.value(name, ~default=placeholder);
  let n = Array.length(spans);
  let start_of = j => spans[j].Focus.sp_start;
  let end_of = j => spans[j].Focus.sp_stop;
  let movable = j => spans[j].Focus.sp_kind != Focus.ITail;
  /* a member block can still hold a let-in item (an expression's
     chain): it takes let-in forms, and moves never mix the two families
     (that would cross block levels) */
  let arr = Array.of_list(seg);
  let span_in_tile = j => {
    let rec first_tile = i =>
      i >= end_of(j)
        ? None
        : (
          switch (arr[i]) {
          | Piece.Tile(t) => Some(t)
          | _ => first_tile(i + 1)
          }
        );
    switch (first_tile(start_of(j))) {
    | Some(t) => Focus.ends_with_in(t)
    | None => false
    };
  };
  let member_form = j => in_module && !span_in_tile(j);
  /* as split_terminator */
  let split_term = (ps: Segment.t): (Segment.t, Segment.t, bool) => {
    let arr = Array.of_list(ps);
    let rec back = i =>
      i > 0 && Focus.is_edge_ws(arr[i - 1]) ? back(i - 1) : i;
    let at = back(Array.length(arr));
    at > 0 && Focus.is_semi(arr[at - 1])
      ? (Focus.take(at - 1, ps), Focus.drop(at - 1, ps), true)
      : (Focus.take(at, ps), Focus.drop(at, ps), false);
  };
  let unterminated = j => {
    let (_, _, semi) =
      split_term(Focus.slice(start_of(j), end_of(j), seg));
    member_form(j) && !semi;
  };
  /* spans [a, b) and [b, c) swapped; an unterminated member moving up
     takes the other's `;`, so the one now last goes without */
  let swap = (a, b, c) => {
    let (first, second) = (Focus.slice(a, b, seg), Focus.slice(b, c, seg));
    let (c1, t1, semi1) = split_term(first);
    let (c2, t2, semi2) = split_term(second);
    let middle =
      in_module && semi1 && !semi2 ? c2 @ t1 @ c1 @ t2 : second @ first;
    Focus.take(a, seg) @ middle @ Focus.drop(c, seg);
  };
  /* a [member, ;, ws] chunk after an unterminated member leads with
     its separator instead: [;, ws, member] */
  let after = (j, chunk: Segment.t): Segment.t =>
    if (unterminated(j)) {
      let (core, term, _) = split_term(chunk);
      term @ core;
    } else {
      chunk;
    };
  /* outside modules, moves may mix defs and statements */
  let same_family = (j, k) =>
    !in_module || span_in_tile(j) == span_in_tile(k);
  Focus.(
    switch (op) {
    | Delete when movable(j) =>
      let rest = take(start_of(j), seg) @ drop(end_of(j), seg);
      Some((in_module ? drop_trailing_semi(rest) : rest, None));
    | Delete => None
    | MoveUp
        when j > 0 && movable(j) && movable(j - 1) && same_family(j, j - 1) =>
      Some((swap(start_of(j - 1), start_of(j), end_of(j)), None))
    | MoveDown
        when
          j + 1 < n && movable(j) && movable(j + 1) && same_family(j, j + 1) =>
      Some((swap(start_of(j), start_of(j + 1), end_of(j + 1)), None))
    | MoveUp
    | MoveDown => None
    | NewBelow
    | NewTypeBelow
    | NewModuleBelow =>
      let sk =
        if (member_form(j)) {
          let txt =
            switch (op) {
            | NewTypeBelow => "type " ++ named("NewType") ++ {js| = ¿|js}
            | NewModuleBelow => "module " ++ named("NewModule") ++ " = {}"
            | _ => "let " ++ named("new_def") ++ {js| = ¿|js}
            };
          member_chunk(txt);
        } else {
          /* a bare `let _ = _ in` is not a complete program: parse
             with a dummy tail, then drop the trailing tail tile */
          let strip_tail = (sk: Segment.t): Segment.t =>
            switch (List.rev(sk)) {
            | [Piece.Tile(_), ...rest] => List.rev(rest)
            | _ => sk
            };
          let txt =
            switch (op) {
            | NewTypeBelow =>
              "type " ++ named("NewType") ++ {js| = ¿ in
0|js}
            | NewModuleBelow =>
              "module " ++ named("NewModule") ++ " = {} in\n0"
            | _ => "let " ++ named("new_def") ++ {js| = ¿ in
0|js}
            };
          Option.map(strip_tail, parse(txt));
        };
      switch (sk) {
      | None => None
      | Some(sk) =>
        /* inserting below the trailing expression would strand it
           above the new def: insert above the tail instead */
        let at = movable(j) ? end_of(j) : start_of(j);
        let sk = movable(j) ? after(j, sk) : sk;
        Some((take(at, seg) @ sk @ drop(at, seg), first_tile_id(sk)));
      };
    | Duplicate when movable(j) =>
      let span = slice(start_of(j), end_of(j), seg);
      let txt = MarkerParse.to_text(Zipper.unzip(span));
      switch (member_form(j) ? member_chunk(txt) : parse(txt)) {
      | None => None
      | Some(copy) =>
        let at = end_of(j);
        let copy = after(j, copy);
        Some((take(at, seg) @ copy @ drop(at, seg), first_tile_id(copy)));
      };
    | Duplicate => None
    }
  );
};

/* where a level sits: members live under a brace tile inside the
   module tile's def child, two hops from the module, so it's threaded */
type block_ctx =
  | BPlain
  | BModDef /* the module tile's def child: the brace lives here */
  | BModBody; /* the brace's child: the member list */

/* [act] at the block that owns [fid]'s item, the block rebuilt around
   its result. A module body, or the top level of a module-rooted
   program, is a member block ([in_module]) */
let rec at_level =
        (
          ~act:
             (~in_module: bool, array(Focus.item_span), int, Segment.t) =>
             option((Segment.t, option(Id.t))),
          ~mod_root: bool,
          fid: Id.t,
          ~bctx: block_ctx,
          ~top: bool,
          seg: Segment.t,
        )
        : option((Segment.t, option(Id.t))) => {
  let spans = Array.of_list(Focus.item_spans(~divided_only_tail=!top, seg));
  let n = Array.length(spans);
  let find = pred => {
    let rec go = j => j >= n ? None : pred(spans[j]) ? Some(j) : go(j + 1);
    go(0);
  };
  let in_module = bctx == BModBody || top && mod_root;
  let found = find((sp: Focus.item_span) => sp.sp_id == Some(fid));
  let flat =
    found == None
      ? Focus.flat_body_of(fid, Array.of_list(seg), Array.to_list(spans))
      : None;
  switch (found, flat) {
  | (Some(j), _) => act(~in_module, spans, j, seg)
  | (None, Some((b0, b1))) =>
    /* a let or the tail of a member's flat body: acts in that body */
    let body = Focus.slice(b0, b1, seg);
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
      act(~in_module=false, bspans, k, body)
      |> Option.map(((body', target)) =>
           (Focus.take(b0, seg) @ body' @ Focus.drop(b1, seg), target)
         )
    | None => None
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
            : option((Segment.t, option(Id.t))) =>
      switch (ps) {
      | [] => None
      | [Piece.Secondary(_) as p, ...rest] =>
        try_children(~after_head, rest)
        |> Option.map(((rest', target)) => ([p, ...rest'], target))
      | [Piece.Tile(t) as p, ...rest] =>
        let n_kids = List.length(t.children);
        let rec try_kids = (before, k, kids) =>
          switch (kids) {
          | [] => None
          | [ch, ...more] =>
            switch (
              at_level(
                ~act,
                ~mod_root,
                fid,
                ~bctx=child_bctx(~after_head, t, k == n_kids - 1),
                ~top=false,
                ch,
              )
            ) {
            | Some((ch', target)) =>
              Some((List.rev(before) @ [ch', ...more], target))
            | None => try_kids([ch, ...before], k + 1, more)
            }
          };
        switch (try_kids([], 0, t.children)) {
        | Some((children, target)) =>
          let tile =
            switch (children) {
            | [kid] when is_body(t) && List.for_all(Focus.is_edge_ws, kid) =>
              Option.value(
                empty_body(),
                ~default=
                  Piece.Tile({
                    ...t,
                    children,
                  }),
              )
            | _ =>
              Piece.Tile({
                ...t,
                children,
              })
            };
          Some(([tile, ...rest], target));
        | None =>
          try_children(
            ~after_head=is_module_tile(t) && List.length(t.shards) == 2,
            rest,
          )
          |> Option.map(((rest', target)) => ([p, ...rest'], target))
        };
      | [p, ...rest] =>
        try_children(rest)
        |> Option.map(((rest', target)) => ([p, ...rest'], target))
      };
    switch (try_children(seg)) {
    | Some(_) as r => r
    | None =>
      /* contained in one of this level's statement or tail spans (e.g.
         a ModExp test's row id is the inner test term): that span is
         the item */
      switch (
        find((sp: Focus.item_span) =>
          Focus.seg_contains_id(
            fid,
            Focus.slice(sp.sp_start, sp.sp_stop, seg),
          )
        )
      ) {
      | Some(j) => act(~in_module, spans, j, seg)
      | None => None
      }
    };
  };
};

let apply =
    (
      ~name: option(string)=?,
      ~mod_root=false,
      op: OutlineSidebar.def_op,
      fid: Id.t,
      seg: Segment.t,
    )
    : option((Segment.t, option(Id.t))) =>
  at_level(
    ~act=
      (~in_module, spans, j, seg) =>
        apply_at(~name?, op, ~in_module, spans, j, seg),
    ~mod_root,
    fid,
    ~bctx=BPlain,
    ~top=true,
    seg,
  );

/* append [member] to module [fid]'s body, at any depth */
let new_inside =
    (~member: string, fid: Id.t, seg: Segment.t)
    : option((Segment.t, option(Id.t))) => {
  switch (Focus.find_def(fid, seg)) {
  | None => None
  | Some(def_seg) =>
    let rec upd_brace = (ps: Segment.t): option((Segment.t, option(Id.t))) =>
      switch (ps) {
      | [] => None
      | [Piece.Tile(bt), ...rest] when Tile.label(bt) == ["{}"] =>
        /* an empty module body parses as a nullary fused `{}` tile
           (no child slot): swap in a populated 2-shard brace from a
           scaffold parse */
        switch (parse("module Zz = {" ++ member ++ "} in\n0")) {
        | None => None
        | Some(scaffold) =>
          let rec find_brace = (qs: Segment.t): option(Piece.t) =>
            switch (qs) {
            | [] => None
            | [Piece.Tile(t), ...more] =>
              Tile.label(t) == ["{", "}"]
                ? Some(Piece.Tile(t))
                : (
                  switch (List.find_map(find_brace, t.children)) {
                  | Some(_) as r => r
                  | None => find_brace(more)
                  }
                )
            | [_, ...more] => find_brace(more)
            };
          switch (find_brace(scaffold)) {
          | Some(Piece.Tile(brace) as p) =>
            let target =
              switch (brace.children) {
              | [inner] => first_tile_id(inner)
              | _ => None
              };
            Some(([p, ...rest], target));
          | _ => None
          };
        }
      | [Piece.Tile(bt), ...rest]
          when Tile.label(bt) == ["{", "}"] && List.length(bt.children) == 1 =>
        switch (member_chunk(member)) {
        | None => None
        | Some(chunk) =>
          let inner = List.hd(bt.children);
          let has_tile =
            List.exists(
              (p: Piece.t) =>
                switch (p) {
                | Tile(_) => true
                | _ => false
                },
              inner,
            );
          let inner' =
            if (has_tile) {
              let arr = Array.of_list(inner);
              let n = Array.length(arr);
              let rec back = i =>
                i > 0 && Focus.is_edge_ws(arr[i - 1]) ? back(i - 1) : i;
              let at = back(n);
              /* after an unterminated last member, the [member, ;, ws]
                 chunk becomes [;, ws, member] so they don't run together */
              let terminated =
                at > 0
                && (
                  switch (arr[at - 1]) {
                  | Piece.Tile(t) => Tile.label(t) == [";"]
                  | _ => false
                  }
                );
              let insertion =
                if (terminated) {
                  chunk;
                } else {
                  let carr = Array.of_list(chunk);
                  let cn = Array.length(carr);
                  let rec semi_at = i =>
                    i >= cn
                      ? None
                      : Focus.is_semi(carr[i]) ? Some(i) : semi_at(i + 1);
                  switch (semi_at(0)) {
                  | Some(k) => Focus.drop(k, chunk) @ Focus.take(k, chunk)
                  | None => chunk
                  };
                };
              Focus.take(at, inner) @ insertion @ Focus.drop(at, inner);
            } else {
              /* empty body: the chunk replaces the grout filler */
              chunk;
            };
          Some((
            [
              Piece.Tile({
                ...bt,
                children: [inner'],
              }),
              ...rest,
            ],
            first_tile_id(chunk),
          ));
        }
      | [p, ...rest] =>
        upd_brace(rest) |> Option.map(((rest', t)) => ([p, ...rest'], t))
      };
    upd_brace(def_seg)
    |> Option.map(((def_seg', target)) =>
         (Focus.splice_def(fid, def_seg', seg), target)
       );
  };
};

let fresh_semi = (): option(Piece.t) =>
  Option.bind(member_chunk({js|let zz = 0|js}), chunk =>
    List.find_opt(Focus.is_semi, chunk)
  );

let text_of = (ps: Segment.t): string =>
  String.trim(MarkerParse.to_text(Zipper.unzip(ps)));

let drop_suffix = (suffix: string, s: string): string => {
  let (n, k) = (String.length(s), String.length(suffix));
  n >= k && String.sub(s, n - k, k) == suffix
    ? String.trim(String.sub(s, 0, n - k)) : s;
};

/* an item's pieces in a block's form: members end in `;`, other blocks
   use `… in`. Converting goes through text (fresh ids) */
let in_form =
    (~member: bool, ~was_member: bool, ps: Segment.t): option(Segment.t) =>
  switch (member, was_member) {
  | (true, true)
  | (false, false) => Some(ps)
  | (true, false) => member_chunk(drop_suffix("in", text_of(ps)))
  | (false, true) =>
    let strip_tail = (sk: Segment.t): Segment.t =>
      switch (List.rev(sk)) {
      | [Piece.Tile(_), ...rest] => List.rev(rest)
      | _ => sk
      };
    Option.map(
      strip_tail,
      parse(drop_suffix(";", text_of(ps)) ++ " in\n0"),
    );
  };

/* [item] (a member) after [members], or before them; a member with
   another after it needs its `;` */
let append_member = (members: Segment.t, item: Segment.t): option(Segment.t) => {
  let (mc, mt, msemi) = split_terminator(members);
  let (ic, it, isemi) = split_terminator(item);
  if (List.for_all(Focus.is_edge_ws, members) || msemi) {
    Some(members @ item);
  } else if (isemi) {
    Some(mc @ it @ ic @ mt);
  } else {
    Option.map(semi => mc @ [semi] @ mt @ ic, fresh_semi());
  };
};
let prepend_member = (item: Segment.t, members: Segment.t): option(Segment.t) => {
  let (ic, it, isemi) = split_terminator(item);
  isemi
    ? Some(item @ members)
    : Option.map(semi => ic @ [semi] @ it @ members, fresh_semi());
};

type spot = {
  s_member: bool,
  s_pieces: Segment.t,
  /* the neighbour in the move's direction, if it is a module */
  s_module: option(Id.t),
  s_edge: bool,
};

/* where the item holding [fid] sits, seen from its own block */
let spot = (~mod_root, ~up: bool, fid: Id.t, seg: Segment.t): option(spot) => {
  let found = ref(None);
  let first_tile = ps =>
    List.find_map(
      (p: Piece.t) =>
        switch (p) {
        | Tile(t) => Some(t)
        | _ => None
        },
      ps,
    );
  let _ =
    at_level(
      ~act=
        (~in_module, spans, j, seg) => {
          let n = Array.length(spans);
          let movable = k =>
            k >= 0 && k < n && spans[k].Focus.sp_kind != Focus.ITail;
          let span_pieces = k =>
            Focus.slice(spans[k].Focus.sp_start, spans[k].Focus.sp_stop, seg);
          let k = up ? j - 1 : j + 1;
          let module_at =
            movable(k)
              ? switch (first_tile(span_pieces(k))) {
                | Some(t) =>
                  switch (Tile.label(t)) {
                  | ["module", ..._] => Some(t.id)
                  | _ => None
                  }
                | None => None
                }
              : None;
          found :=
            Some({
              s_member: in_module,
              s_pieces: span_pieces(j),
              s_module: module_at,
              s_edge: !movable(k),
            });
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

/* a member followed by another needs its `;` */
let ensure_term = (ps: Segment.t): option(Segment.t) => {
  let (c, t, semi) = split_terminator(ps);
  semi ? Some(ps) : Option.map(s => c @ [s] @ t, fresh_semi());
};

/* [ps] placed just above ([up]) or below the item holding [target];
   in a member block, separators follow what comes after */
let insert_near =
    (~mod_root, ~up: bool, target: Id.t, ps: Segment.t, seg: Segment.t)
    : option(Segment.t) =>
  at_level(
    ~act=
      (~in_module, spans, j, seg) => {
        let (a, b) = (spans[j].Focus.sp_start, spans[j].Focus.sp_stop);
        let put = (lo, hi, mid) =>
          Some((Focus.take(lo, seg) @ mid @ Focus.drop(hi, seg), None));
        switch (in_module, up) {
        | (false, true) => put(a, a, ps)
        | (false, false) => put(b, b, ps)
        | (true, true) => Option.bind(ensure_term(ps), put(a, a))
        | (true, false) =>
          let followed = j + 1 < Array.length(spans);
          Option.bind(followed ? ensure_term(ps) : Some(ps), item =>
            Option.bind(
              append_member(Focus.slice(a, b, seg), item),
              put(a, b),
            )
          );
        };
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
   function bodies keep their items. [owner]: the item's module. */
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
    switch (s.s_module) {
    | Some(k) when is_open(k) =>
      /* in: the neighbour module's members gain the item */
      switch (
        removed(),
        in_form(~member=true, ~was_member=s.s_member, s.s_pieces),
      ) {
      | (Some(seg1), Some(item)) =>
        switch (Focus.find_def(k, seg1)) {
        | Some(def_seg) =>
          switch (Focus.brace_child(def_seg)) {
          | Some(members) =>
            (
              up
                ? append_member(members, item)
                : prepend_member(item, members)
            )
            |> Option.map(drop_trailing_semi)
            |> Option.map(members =>
                 (
                   Focus.splice_def(
                     k,
                     Focus.with_brace_child(def_seg, members),
                     seg1,
                   ),
                   first_tile_id(item),
                 )
               )
          | None => None
          }
        | None => None
        }
      | _ => None
      }
    | _ when !s.s_edge =>
      apply(~mod_root, up ? MoveUp : MoveDown, fid, seg)
      |> Option.map(((seg, _)) => (seg, Some(fid)))
    | _ =>
      /* out: just above or below the owning module, in its block's form */
      switch (owner) {
      | None => None
      | Some(m) =>
        switch (removed(), spot(~mod_root, ~up, m, seg)) {
        | (Some(seg1), Some(ms)) =>
          switch (in_form(~member=ms.s_member, ~was_member=true, s.s_pieces)) {
          | Some(item) =>
            insert_near(~mod_root, ~up, m, item, seg1)
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
  | Op(OutlineSidebar.def_op, Id.t)
  | Rename(Id.t, string)
  /* a new definition below the row: `name`, `type Name` or
     `module Name` */
  | Insert(Id.t, string)
  /* the same, last inside a module */
  | InsertInside(Id.t, string);

/* a typed name and the kind its keyword picks */
let typed_name = (text: string): (OutlineTree.kind, string) => {
  let (kind, prefix) = OutlineSidebar.new_kind(text);
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
    |> Result.map(seg => (seg, Some(row)))
  | Insert(anchor, text) =>
    let (kind, name) = typed_name(text);
    let (rkind, op): (OutlineRename.kind, OutlineSidebar.def_op) =
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
