module Scratchpad = ScratchModel.Scratchpad;
module Model = ScratchModel.Model;
module Focus = ScratchFocus;

open Haz3lcore;

let parse = (~root=Sort.Exp, txt: string): option(Segment.t) =>
  FastParse.of_text(
    ~materialize=Triggers.invoked_projector,
    ~collect_refractors=true,
    ~root,
    txt,
  );

let first_tile_id = (seg: Segment.t): option(Id.t) =>
  List.find_map(
    (p: Piece.t) =>
      switch (p) {
      | Tile(t) => Some(t.id)
      | _ => None
      },
    seg,
  );

/* fresh-id MEMBER pieces for [txt] (a single member, `;` optional):
   the Mod-root fast parse rejects chunks with a trailing separator,
   so parse two members and cut after the last top-level `;` (the
   member's own terminator + trailing trivia) */
let member_chunk = (txt: string): option(Segment.t) => {
  let txt = String.trim(txt);
  let txt =
    String.length(txt) > 0 && txt.[String.length(txt) - 1] == ';'
      ? String.sub(txt, 0, String.length(txt) - 1) : txt;
  /* the Mod-root wrap parses plain braces whose child is EXP-sorted
     (members came out as let-ins): parse a real module instead and
     extract its body child, then cut after the member's own `;` */
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
          switch (t.label) {
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

/* apply [op] to the item holding [fid] AT ITS OWNING BLOCK: a span
   whose id is exactly [fid] applies at this level; an id contained
   in a DEF span recurses into that def's tiles (module bodies, fn
   bodies); an id contained in a statement/tail span means that span
   IS the item. No cross-level fallback — an op invalid at its own
   level (move at a block edge) no-ops rather than acting on the
   enclosing item. [in_module]: the block is a module body, so
   inserted/duplicated skeletons are 2-shard MEMBERS parsed at Mod
   root, not `… in` forms. */
let apply_at =
    (
      op: OutlineSidebar.def_op,
      ~in_module: bool,
      spans: array(Focus.item_span),
      j: int,
      seg: Segment.t,
    )
    : option((Segment.t, option(Id.t))) => {
  let n = Array.length(spans);
  let start_of = j => spans[j].Focus.sp_start;
  let end_of = j => spans[j].Focus.sp_stop;
  let movable = j => spans[j].Focus.sp_kind != Focus.ITail;
  /* member-fn bodies FLATTEN into the module-body level (a fun's
     body is siblings, not a child), so a module-level span can be a
     let-in belonging to a member's inner chain. The op FORM follows
     the target span's own head: an `…in`-headed span takes let-in
     forms even inside a module; moves must not mix the two families
     (swapping a nested let with its enclosing member head would
     cross block levels). */
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
  /* only module-body levels interleave two block levels (member-fn
     flattening); at top level mixing defs/tests in moves is fine */
  let same_family = (j, k) =>
    !in_module || span_in_tile(j) == span_in_tile(k);
  Focus.(
    switch (op) {
    | Delete when movable(j) =>
      Some((take(start_of(j), seg) @ drop(end_of(j), seg), None))
    | Delete => None
    | MoveUp
        when j > 0 && movable(j) && movable(j - 1) && same_family(j, j - 1) =>
      let (a, b, c) = (start_of(j - 1), start_of(j), end_of(j));
      Some((
        take(a, seg) @ slice(b, c, seg) @ slice(a, b, seg) @ drop(c, seg),
        None,
      ));
    | MoveDown
        when
          j + 1 < n && movable(j) && movable(j + 1) && same_family(j, j + 1) =>
      let (a, b, c) = (start_of(j), start_of(j + 1), end_of(j + 1));
      Some((
        take(a, seg) @ slice(b, c, seg) @ slice(a, b, seg) @ drop(c, seg),
        None,
      ));
    | MoveUp
    | MoveDown => None
    | NewBelow
    | NewTypeBelow
    | NewModuleBelow =>
      let sk =
        if (member_form(j)) {
          let txt =
            switch (op) {
            | NewTypeBelow => {js|type NewType = ¿|js}
            | NewModuleBelow => {js|module NewModule = {}|js}
            | _ => {js|let new_def = ¿|js}
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
            | NewTypeBelow => {js|type NewType = ¿ in
0|js}
            | NewModuleBelow => {js|module NewModule = {} in
0|js}
            | _ => {js|let new_def = ¿ in
0|js}
            };
          Option.map(strip_tail, parse(txt));
        };
      switch (sk) {
      | None => None
      | Some(sk) =>
        /* inserting below the trailing expression would strand it
           above the new def: insert ABOVE the tail instead */
        let at = movable(j) ? end_of(j) : start_of(j);
        Some((take(at, seg) @ sk @ drop(at, seg), first_tile_id(sk)));
      };
    | Duplicate when movable(j) =>
      let span = slice(start_of(j), end_of(j), seg);
      let txt = MarkerParse.to_text(Zipper.unzip(span));
      switch (member_form(j) ? member_chunk(txt) : parse(txt)) {
      | None => None
      | Some(copy) =>
        let at = end_of(j);
        Some((take(at, seg) @ copy @ drop(at, seg), first_tile_id(copy)));
      };
    | Duplicate => None
    | NewInside => None /* handled in [apply] via find_def */
    }
  );
};

/* where a level sits: module members live under a BRACE tile inside
   the module tile's def child, so "is my parent a module" is two
   hops away — thread it */
type block_ctx =
  | BPlain
  | BModDef /* the module tile's def child: the brace lives here */
  | BModBody; /* the brace's child: the member list */

let rec apply_deep =
        (
          op: OutlineSidebar.def_op,
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
  let in_module = bctx == BModBody;
  switch (find((sp: Focus.item_span) => sp.sp_id == Some(fid))) {
  | Some(j) => apply_at(op, ~in_module, spans, j, seg)
  | None =>
    /* descend into tile children first (the owning block may be a
       module or fn body) */
    let is_module_tile = (t: Base.tile) =>
      switch (t.label) {
      | ["module", ..._] => true
      | _ => false
      };
    let child_bctx = (t: Base.tile, is_last: bool): block_ctx =>
      if (is_module_tile(t) && is_last) {
        BModDef;
      } else if (bctx == BModDef
                 && t.label == ["{", "}"]
                 && List.length(t.children) == 1) {
        BModBody;
      } else {
        BPlain;
      };
    let rec try_children =
            (ps: Segment.t): option((Segment.t, option(Id.t))) =>
      switch (ps) {
      | [] => None
      | [Piece.Tile(t), ...rest] =>
        let n_kids = List.length(t.children);
        let rec try_kids = (before, k, kids) =>
          switch (kids) {
          | [] => None
          | [ch, ...more] =>
            switch (
              apply_deep(
                op,
                fid,
                ~bctx=child_bctx(t, k == n_kids - 1),
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
          Some((
            [
              Piece.Tile({
                ...t,
                children,
              }),
              ...rest,
            ],
            target,
          ))
        | None =>
          try_children(rest)
          |> Option.map(((rest', target)) =>
               ([Piece.Tile(t), ...rest'], target)
             )
        };
      | [p, ...rest] =>
        try_children(rest)
        |> Option.map(((rest', target)) => ([p, ...rest'], target))
      };
    switch (try_children(seg)) {
    | Some(_) as r => r
    | None =>
      /* contained in one of THIS level's statement/tail spans (e.g.
         a ModExp test's row id is the inner test term): that span
         is the item */
      switch (
        find((sp: Focus.item_span) =>
          Focus.seg_contains_id(
            fid,
            Focus.slice(sp.sp_start, sp.sp_stop, seg),
          )
        )
      ) {
      | Some(j) => apply_at(op, ~in_module, spans, j, seg)
      | None => None
      }
    };
  };
};

/* append a fresh member INSIDE a module row's body (works at any
   depth: find_def/splice_def handle both 3-shard `module … in` and
   2-shard member modules) */
let new_inside =
    (fid: Id.t, seg: Segment.t): option((Segment.t, option(Id.t))) => {
  switch (Focus.find_def(fid, seg)) {
  | None => None
  | Some(def_seg) =>
    let rec upd_brace = (ps: Segment.t): option((Segment.t, option(Id.t))) =>
      switch (ps) {
      | [] => None
      | [Piece.Tile(bt), ...rest] when bt.label == ["{}"] =>
        /* an EMPTY module body parses as a nullary fused `{}` tile
           (no child slot): swap in a populated 2-shard brace from a
           scaffold parse */
        switch (parse({js|module Zz = {let new_def = ¿} in
0|js})) {
        | None => None
        | Some(scaffold) =>
          let rec find_brace = (qs: Segment.t): option(Piece.t) =>
            switch (qs) {
            | [] => None
            | [Piece.Tile(t), ...more] =>
              t.label == ["{", "}"]
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
          when bt.label == ["{", "}"] && List.length(bt.children) == 1 =>
        switch (member_chunk({js|let new_def = ¿|js})) {
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
              /* the LAST member may be unterminated (mega style:
                 `…= fun _ -> true\n}`): appending needs a separator
                 FIRST or the members run together. The chunk is
                 [member, ;, ws] — reorder it to [;, ws, member] in
                 that case (the new member becomes the unterminated
                 last one). */
              let terminated =
                at > 0
                && (
                  switch (arr[at - 1]) {
                  | Piece.Tile(t) => t.label == [";"]
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

let apply =
    (op: OutlineSidebar.def_op, fid: Id.t, seg: Segment.t)
    : option((Segment.t, option(Id.t))) =>
  switch (op) {
  | NewInside => new_inside(fid, seg)
  | _ => apply_deep(op, fid, ~bctx=BPlain, ~top=true, seg)
  };
