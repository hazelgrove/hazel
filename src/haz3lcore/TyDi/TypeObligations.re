open Util;
open Language;

/* Type-forced structural obligations (T1): sites where the expected
   type forces tuple SHAPE the text doesn't yet have. Term-based and
   caret-free: any term analyzed against an n-Prod (n >= 2), sitting
   in a paren-shaped wrapper (explicit parens or an ap's argument
   slot), with k < n elements owes n-k commas at its inner right
   edge. Only separators and holes are presumed — content is never
   invented. The presumption is DEFEASIBLE: a single element whose
   synthesized type already satisfies the whole Prod suppresses it
   (typing a tuple-typed var resolves; it never errors). */

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  site: Id.t, /* the paren/ap tile whose child owes structure */
  present: int,
  expected: int,
  remaining_tys: list(Typ.t), /* element types still owed */
  /* which junctions realize as commas: None = all of them (k <= n,
     every juxtaposed item is its own element); Some(idxs) = the
     type-fit grouping of an OVERFULL juxtaposition (k > n: the
     comma COUNT is forced at n-1, only placement is ambiguous —
     presumed only when a unique least-contradictory grouping
     exists) */
  commas_at: option(list(int)),
};

let deficit = (ob: t): int => ob.expected - ob.present;

let prod_elements = (ctx: Ctx.t, ty: Typ.t): option(list(Typ.t)) => {
  let ty = Typ.weak_head_normalize(ctx, ty);
  switch (Typ.term_of(ty)) {
  | Prod(tys) when List.length(tys) >= 2 => Some(tys)
  | _ => None
  };
};

/* Element count of the wrapped term; None = no presumption. Holes
   give no anchor (and no element evidence); a nested Parens defers
   to its own wrapper entry; a single element consistent with the
   whole Prod DEFEATS the presumption — unknown types never
   suppress (incomplete content is not evidence of satisfaction). */
/* juxtaposed operands (an operator hole between elements) count as
   elements: the concave grout is JUNCTION evidence — a separator
   slot awaiting its comma */
let juxtaposed = (e: Exp.t): option(int) =>
  switch (Exp.term_of(e)) {
  | MultiHole(things) =>
    let exps =
      things
      |> List.filter((a: Any.t) =>
           switch (a) {
           | Exp(_) => true
           | _ => false
           }
         );
    List.length(exps) == List.length(things) && things != []
      ? Some(List.length(exps)) : None;
  | _ => None
  };

let present_of = (inner: Exp.t, info_map: Statics.Map.t): option(int) =>
  switch (Exp.term_of(inner)) {
  | Tuple(es) =>
    es
    |> List.fold_left(
         (acc, e) =>
           switch (acc) {
           | None => None
           | Some(k) =>
             switch (juxtaposed(e)) {
             | Some(j) => Some(k + j)
             | None => Some(k + 1)
             }
           },
         Some(0),
       )
  | MultiHole(_) => juxtaposed(inner)
  | EmptyHole
  | Parens(_) => None
  | _ =>
    switch (Id.Map.find_opt(Exp.rep_id(inner), info_map)) {
    | Some(Info.InfoExp({elab_syn_ty, ctx, ana, _})) =>
      let syn = Typ.weak_head_normalize(ctx, elab_syn_ty);
      switch (Typ.term_of(syn)) {
      | Unknown(_) => Some(1)
      | _ => Typ.is_consistent(ctx, syn, ana) ? None : Some(1)
      };
    | _ => Some(1)
    }
  };

let juxtaposed_anywhere = (inner: Exp.t): bool => {
  let is_juxt = e =>
    switch (juxtaposed(e)) {
    | Some(_) => true
    | None => false
    };
  switch (Exp.term_of(inner)) {
  | Tuple(es) => List.exists(is_juxt, es)
  | _ => is_juxt(inner)
  };
};

let mk = (~commas_at=None, site, ~present as k, tys): t => {
  site,
  present: k,
  expected: List.length(tys),
  remaining_tys: List.filteri((i, _) => i >= k, tys),
  commas_at,
};

let syn_of = (e: Exp.t, info_map): option((Ctx.t, Typ.t)) =>
  switch (Id.Map.find_opt(Exp.rep_id(e), info_map)) {
  | Some(Info.InfoExp({elab_syn_ty, ctx, _})) =>
    Some((ctx, Typ.weak_head_normalize(ctx, elab_syn_ty)))
  | _ => None
  };

/* Overfull juxtaposition (k items, n < k slots): the comma count is
   forced (n-1); placement is chosen by element-type fit. Enumerate
   the C(k-1, n-1) ordered groupings; a slot holding a SINGLE item
   with a known synthesized type inconsistent with the slot's type is
   a contradiction (groups and unknowns are neutral). A unique
   least-contradictory grouping is presumed; ties stay silent. */
let overfull_grouping =
    (items: list(Exp.t), tys: list(Typ.t), info_map: Statics.Map.t)
    : option(list(int)) => {
  let k = List.length(items);
  let n = List.length(tys);
  let rec choose = (from: int, needed: int): list(list(int)) =>
    if (needed == 0) {
      [[]];
    } else if (k - 1 - from < needed) {
      [];
    } else {
      (choose(from + 1, needed - 1) |> List.map(rest => [from, ...rest]))
      @ choose(from + 1, needed);
    };
  let score = (cut: list(int)): int => {
    /* groups = items split after each junction index in cut */
    let bounds = [(-1), ...cut] @ [k - 1];
    let rec groups = (bs: list(int)) =>
      switch (bs) {
      | [a, b, ...rest] => [
          List.filteri((i, _) => i > a && i <= b, items),
          ...groups([b, ...rest]),
        ]
      | _ => []
      };
    List.combine(groups(bounds), tys)
    |> List.filter(((group, ty)) =>
         switch (group) {
         | [item] =>
           switch (syn_of(item, info_map)) {
           | Some((ctx, syn)) =>
             switch (Typ.term_of(syn)) {
             | Unknown(_) => false
             | _ => !Typ.is_consistent(ctx, syn, ty)
             }
           | None => false
           }
         | _ => false /* multi-item groups and empties are neutral */
         }
       )
    |> List.length;
  };
  let scored = choose(0, n - 1) |> List.map(cut => (score(cut), cut));
  let best =
    scored |> List.fold_left((acc, (sc, _)) => min(acc, sc), max_int);
  switch (scored |> List.filter(((sc, _)) => sc == best)) {
  | [(_, cut)] => Some(cut)
  | _ => None /* ambiguous: no presumption */
  };
};

let multihole_items = (inner: Exp.t): option(list(Exp.t)) =>
  switch (Exp.term_of(inner)) {
  | MultiHole(things) =>
    let exps =
      things
      |> List.filter_map((a: Any.t) =>
           switch (a) {
           | Exp(e) => Some(e)
           | _ => None
           }
         );
    List.length(exps) == List.length(things) && exps != []
      ? Some(exps) : None;
  | _ => None
  };

let of_wrapper =
    (site: Id.t, inner: Exp.t, ~tys: list(Typ.t), info_map: Statics.Map.t)
    : option(t) =>
  switch (present_of(inner, info_map)) {
  | Some(k) when k < List.length(tys) => Some(mk(site, ~present=k, tys))
  | Some(k) when k == List.length(tys) && juxtaposed_anywhere(inner) =>
    /* arity satisfied but separators owed (junction-only site) */
    Some(mk(site, ~present=k, tys))
  | Some(k) when k > List.length(tys) && List.length(tys) >= 2 =>
    /* overfull bare juxtaposition: forced comma count, type-fit
       placement */
    switch (multihole_items(inner)) {
    | Some(items) when List.length(items) == k =>
      overfull_grouping(items, tys, info_map)
      |> Option.map(cut =>
           mk(~commas_at=Some(cut), site, ~present=List.length(tys), tys)
         )
    | _ => None
    }
  | _ => None /* satisfied, overfull-ambiguous, or no presumption */
  };

let ana_elements = (e: Exp.t, info_map): option((Ctx.t, list(Typ.t))) =>
  switch (Id.Map.find_opt(Exp.rep_id(e), info_map)) {
  | Some(Info.InfoExp({ana, ctx, _})) =>
    prod_elements(ctx, ana) |> Option.map(tys => (ctx, tys))
  | _ => None
  };

let first_item = (e: Exp.t): Exp.t =>
  switch (Exp.term_of(e)) {
  | Tuple([e0, ..._]) => e0
  | _ => e
  };

/* Explicit parens in an argument slot are ambiguous: grouping the
   WHOLE tuple, or opening its FIRST ELEMENT. Element-type fit
   disambiguates: if the parens' first item can't inhabit the whole
   Prod's first slot but can inhabit the first slot of that slot's
   own Prod, the parens open element 0 — the inner site owes the
   element Prod's tail, and the ap site owes the outer tail. */
let split_nested =
    (ap_site: Id.t, parens: Exp.t, inner: Exp.t, info_map): list(t) =>
  switch (ana_elements(parens, info_map)) {
  | None => []
  | Some((ctx, tys)) =>
    let grouping =
      of_wrapper(Exp.rep_id(parens), inner, ~tys, info_map) |> Option.to_list;
    switch (List.nth_opt(tys, 0)) {
    | Some(ty0) =>
      switch (Typ.term_of(Typ.weak_head_normalize(ctx, ty0))) {
      | Prod(inner_tys) when List.length(inner_tys) >= 2 =>
        let fits = (item: Exp.t, ty: Typ.t): option(bool) =>
          switch (syn_of(item, info_map)) {
          | Some((ctx, syn)) =>
            switch (Typ.term_of(syn)) {
            | Unknown(_) => None
            | _ => Some(Typ.is_consistent(ctx, syn, ty))
            }
          | None => None
          };
        let item = first_item(inner);
        switch (fits(item, ty0), fits(item, List.nth(inner_tys, 0))) {
        | (Some(false), Some(true) | None) =>
          /* first-element reading */
          (
            of_wrapper(Exp.rep_id(parens), inner, ~tys=inner_tys, info_map)
            |> Option.to_list
          )
          @ [mk(ap_site, ~present=1, tys)]
        | _ => grouping
        };
      | _ => grouping
      }
    | None => grouping
    };
  };

let derive = (info_map: Statics.Map.t): list(t) =>
  Id.Map.fold(
    (id, info: Info.t, acc) =>
      switch (info) {
      | InfoExp({user_term, ancestors, _})
          when Id.equal(id, Exp.rep_id(user_term)) =>
        /* an ap's argument parens are claimed by the ap entry
           (split_nested); don't double-derive from the parens */
        let is_ap_arg = () =>
          switch (ancestors) {
          | [parent, ..._] =>
            switch (Id.Map.find_opt(parent, info_map)) {
            | Some(Info.InfoExp({user_term: p, _})) =>
              switch (Exp.term_of(p)) {
              | Ap(Forward, _, arg) => Id.equal(Exp.rep_id(arg), id)
              | _ => false
              }
            | _ => false
            }
          | [] => false
          };
        switch (Exp.term_of(user_term)) {
        | Parens(_) when is_ap_arg() => acc
        | Parens(inner) =>
          switch (Exp.term_of(inner)) {
          | Parens(_) => acc /* innermost wrapper carries it */
          | _ =>
            switch (ana_elements(user_term, info_map)) {
            | Some((_, tys)) =>
              switch (of_wrapper(id, inner, ~tys, info_map)) {
              | Some(ob) => [ob, ...acc]
              | None => acc
              }
            | None => acc
            }
          }
        | Ap(Forward, _, arg) =>
          switch (Exp.term_of(arg)) {
          | Parens(inner) =>
            /* claim the parens here (nested split needs the ap
               site); the bare Parens case above must not also fire */
            split_nested(id, arg, inner, info_map) @ acc
          | _ =>
            switch (ana_elements(arg, info_map)) {
            | Some((_, tys)) =>
              switch (of_wrapper(id, arg, ~tys, info_map)) {
              | Some(ob) => [ob, ...acc]
              | None => acc
              }
            | None => acc
            }
          }
        | _ => acc
        };
      | _ => acc
      },
    info_map,
    [],
  );

/* === Display integration ===
 * T1 obligations render through the same chip stream as syntactic
 * insertions. An INCOMPLETE site already has a closer chip (its `)`
 * insertion) — the commas merge into it, reading ", ⬚ )". A complete
 * but deficient site gets a fresh chip at its inner right edge. */

let comma_delims = (n: int): list(CanonicalCompletion.delimiter_info) =>
  List.init(n, _ =>
    CanonicalCompletion.{
      text: ",",
      needs_hole: true,
      typed_len: None,
      of_shard: None,
    }
  );

let rec find_tile = (site: Id.t, ps: Segment.t): option(Tile.t) =>
  List.fold_left(
    (acc, p: Piece.t) =>
      switch (acc, p) {
      | (Some(_), _) => acc
      | (None, Tile(t)) =>
        Id.equal(t.id, site)
          ? Some(t)
          : List.fold_left(
              (acc, child) => acc == None ? find_tile(site, child) : acc,
              None,
              t.children,
            )
      | (None, _) => None
      },
    None,
    ps,
  );

/* the sibling list containing a piece, with its index there */
let rec sibling_ctx = (ps: Segment.t, id: Id.t): option((Segment.t, int)) => {
  let rec go = (i, rest) =>
    switch (rest) {
    | [] => None
    | [p, ...tl] =>
      if (Id.equal(Piece.id(p), id)) {
        Some((ps, i));
      } else {
        let deeper =
          switch ((p: Piece.t)) {
          | Tile(t) =>
            List.fold_left(
              (acc, ch) => acc == None ? sibling_ctx(ch, id) : acc,
              None,
              t.children,
            )
          | _ => None
          };
        deeper == None ? go(i + 1, tl) : deeper;
      }
    };
  go(0, ps);
};

/* an obligation's chosen junction grouts (all, or the type-fit cut) */
let chosen = (ob: t, grouts: list(Id.t)): list(Id.t) =>
  switch (ob.commas_at) {
  | None => grouts
  | Some(idxs) => grouts |> List.filteri((i, _) => List.mem(i, idxs))
  };

/* junction sites: concave grout between the site's juxtaposed
   elements — inside the tile's child when it's complete; in the
   flat pending region (site tile up to the closer chip's anchor,
   ~upto) when the closer is still owed */
let junction_grouts =
    (~upto: option(Id.t)=?, seg: Segment.t, site: Id.t): list(Id.t) => {
  let grouts = ps =>
    ps
    |> List.filter_map((p: Piece.t) =>
         switch (p) {
         | Grout({id, shape: Concave}) => Some(id)
         | _ => None
         }
       );
  switch (find_tile(site, seg)) {
  | Some({children: [_, ..._] as children, _}) =>
    switch (Util.ListUtil.split_last_opt(children)) {
    | Some((_, child)) => grouts(child)
    | None => []
    }
  | Some(_) =>
    switch (upto, sibling_ctx(seg, site)) {
    | (Some(stop), Some((sg, i))) =>
      let rec take = (ps: Segment.t, acc) =>
        switch (ps) {
        | [] => List.rev(acc)
        | [p, ..._] when Id.equal(Piece.id(p), stop) => List.rev(acc)
        | [Piece.Grout({id, shape: Concave}), ...tl] =>
          take(tl, [id, ...acc])
        | [_, ...tl] => take(tl, acc)
        };
      take(Util.ListUtil.split_n(i + 1, sg) |> snd, []);
    | _ => []
    }
  | None => []
  };
};

/* last content piece inside the (complete) site tile's child */
let anchor_of_site = (seg: Segment.t, site: Id.t): option(Id.t) => {
  switch (find_tile(site, seg)) {
  | Some(t) =>
    switch (List.rev(t.children)) {
    | [child, ..._] =>
      child
      |> List.rev
      |> List.find_opt((p: Piece.t) =>
           switch (p) {
           | Secondary(_)
           | Grout(_) => false
           | _ => true
           }
         )
      |> Option.map(Piece.id)
    | [] => None
    }
  | None => None
  };
};

/* last piece of the site's child, grout/whitespace included — the
   position truth for splicing owed trailing commas (the content
   anchor above skips grout, which would misorder past a real hole) */
let splice_of_site = (seg: Segment.t, site: Id.t): option(Id.t) => {
  switch (find_tile(site, seg)) {
  | Some(t) =>
    switch (List.rev(t.children)) {
    | [child, ..._] => Util.ListUtil.last_opt(child) |> Option.map(Piece.id)
    | [] => None
    }
  | None => None
  };
};

/* Frame-fresh element count at a site, read off the COMPLETED
   segment (inner tiles are reassembled there, so nested commas
   can't leak into the count; the site tile itself is always
   complete there). Elements = 1 + top-level separators of the
   site's last child: comma tiles and junction grout. This is the
   syntactic half of an obligation — `expected` comes from types
   (statics cadence), `present` from this frame's syntax, so typing
   a presumed comma updates the deficit instantly. */
let present_now = (completed: Segment.t, site: Id.t): option(int) =>
  switch (find_tile(site, completed)) {
  | Some({children: [_, ..._] as children, _}) =>
    switch (Util.ListUtil.split_last_opt(children)) {
    | Some((_, child)) =>
      let seps =
        child
        |> List.filter((p: Piece.t) =>
             switch (p) {
             | Tile({label: [","], _}) => true
             | Grout({shape: Concave, _}) => true
             | _ => false
             }
           )
        |> List.length;
      Some(1 + seps);
    | None => None
    }
  | _ => None
  };

let as_insertions =
    (
      ~seg: Segment.t,
      ~completed: Segment.t,
      ~existing: list(CanonicalCompletion.insertion),
      obs: list(t),
    )
    : list(CanonicalCompletion.insertion) => {
  let holds_site = (ins: CanonicalCompletion.insertion, site: Id.t) =>
    ins.delimiters
    |> List.exists((d: CanonicalCompletion.delimiter_info) =>
         switch (d.of_shard) {
         | Some((tid, _)) => Id.equal(tid, site)
         | None => false
         }
       );
  /* a junction owes its comma AT the junction (chip on the grout,
     no hole — both sides have content); only the trailing deficit
     adds elements at the end */
  let junction_chips = (ob: t) => {
    let upto =
      existing
      |> List.find_opt(ins => holds_site(ins, ob.site))
      |> Option.map((ins: CanonicalCompletion.insertion) => ins.adjacent_id);
    chosen(ob, junction_grouts(~upto?, seg, ob.site))
    |> List.map(gid =>
         CanonicalCompletion.{
           adjacent_id: gid,
           side: Direction.Left,
           splice: Some((gid, None, Direction.Left)),
           delimiters: [
             CanonicalCompletion.{
               text: ",",
               needs_hole: false,
               typed_len: None,
               of_shard: None,
             },
           ],
         }
       );
  };
  /* deficit from the FRESH syntax: `expected` (type fact) minus this
     frame's element count — a typed comma discharges its owed chip
     immediately instead of one statics-debounce later */
  let deficit_now = (ob: t): int =>
    switch (present_now(completed, ob.site)) {
    | Some(k) => ob.expected - k
    | None => deficit(ob)
    };
  let (existing, fresh) =
    List.fold_left(
      ((existing, fresh), ob) => {
        let fresh = junction_chips(ob) @ fresh;
        let trailing = deficit_now(ob);
        if (trailing <= 0) {
          (existing, fresh);
        } else if (List.exists(ins => holds_site(ins, ob.site), existing)) {
          (
            existing
            |> List.map((ins: CanonicalCompletion.insertion) =>
                 holds_site(ins, ob.site)
                   ? {
                     ...ins,
                     delimiters: comma_delims(trailing) @ ins.delimiters,
                   }
                   : ins
               ),
            fresh,
          );
        } else {
          switch (anchor_of_site(seg, ob.site)) {
          | Some(anchor) => (
              existing,
              [
                CanonicalCompletion.{
                  adjacent_id: anchor,
                  side: Direction.Right,
                  splice:
                    splice_of_site(seg, ob.site)
                    |> Option.map(id => (id, None, Direction.Right)),
                  delimiters: comma_delims(trailing),
                },
                ...fresh,
              ],
            )
          | None => (existing, fresh)
          };
        };
      },
      (existing, []),
      obs,
    );
  existing @ fresh;
};

/* THE assist stream (A1 single source), assembled FRAME-FRESH:
   anchors, splice points, and element counts from this frame's
   syntax; type facts (expected arities, grouping choices) from the
   last statics pass, which may be debounce-stale. Exact along the
   promised trajectory — typing presumed material is statics-stable
   by construction (statics ran on the reified program that already
   contains it) — and stale at most one debounce when the TYPES
   changed, the same lag as any type feedback. Chips, the inline
   ghost, and Tab all consume this one list. */
let assist_stream =
    (z: Zipper.t, obs: list(t)): list(CanonicalCompletion.insertion) => {
  let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  let r = CanonicalCompletion.for_editor(seg);
  as_insertions(
    ~seg,
    ~completed=r.completed_seg,
    ~existing=r.insertions,
    obs,
  );
};

/* === Reification ===
 * Splice each site's owed commas+holes into the COMPLETED segment so
 * semantics sees the presumed tuple shape (statics: per-element ana,
 * no arity error; dynamics: holes evaluate). Ids derive
 * deterministically from the site tile (Id.next chain) so reparses
 * are stable across recomputation. Paren-shaped tiles have no
 * interior slots, so their chain is unused by syntactic completion. */
let reify = (obs: list(t), seg: Segment.t): Segment.t => {
  let sites =
    obs |> List.map(ob => (ob.site, ob)) |> List.to_seq |> Hashtbl.of_seq;
  let owed_pieces = (site: Id.t, n: int): list(Piece.t) => {
    let seed = ref(site);
    let mint = () => {
      seed := Id.next(seed^);
      seed^;
    };
    List.concat(
      List.init(
        n,
        _ => {
          let comma_id = mint();
          let hole_id = mint();
          [
            Piece.Tile({
              id: comma_id,
              label: Form.get(CommaExp).label,
              mold: Form.get(CommaExp).mold,
              shards: [0],
              children: [],
            }),
            Piece.Grout({
              id: hole_id,
              shape: Convex,
            }),
          ];
        },
      ),
    );
  };
  let comma_for = (gid: Id.t): Piece.t =>
    Piece.Tile({
      id: Id.next(gid),
      label: Form.get(CommaExp).label,
      mold: Form.get(CommaExp).mold,
      shards: [0],
      children: [],
    });
  let rec go = (ps: Segment.t): Segment.t =>
    ps
    |> List.map((p: Piece.t) =>
         switch (p) {
         | Tile(t) =>
           let t = {
             ...t,
             children: List.map(go, t.children),
           };
           switch (Hashtbl.find_opt(sites, t.id)) {
           | Some(ob) =>
             switch (Util.ListUtil.split_last_opt(t.children)) {
             | Some((init, last)) =>
               /* chosen junction grout realizes as its comma in
                  place; unchosen junctions stay holes (their
                  errors localize inside the presumed slot) */
               let jdx = ref(0);
               let last =
                 last
                 |> List.map((q: Piece.t) =>
                      switch (q) {
                      | Grout({id, shape: Concave}) =>
                        let i = jdx^;
                        incr(jdx);
                        switch (ob.commas_at) {
                        | None => comma_for(id)
                        | Some(idxs) =>
                          List.mem(i, idxs) ? comma_for(id) : q
                        };
                      | q => q
                      }
                    );
               let n = deficit(ob);
               Piece.Tile({
                 ...t,
                 children:
                   init @ [n > 0 ? last @ owed_pieces(t.id, n) : last],
               });
             | None => Piece.Tile(t)
             }
           | None => Piece.Tile(t)
           };
         | p => p
         }
       );
  go(seg);
};

/* === Tab dispatch ===
 * The T1 obligation whose chip zone contains the caret. Zone
 * resolution mirrors CanonicalCompletion.obligation_at_caret: the
 * caret's whitespace/grout run plus bounding content pieces. A
 * merged chip (incomplete site) anchors at its closer insertion's
 * anchor; a complete site anchors at its inner right edge. Commas
 * order before closers in the chip, so Tab consumes the T1 chunk
 * first; once the deficit is filled the same position dispatches
 * the closer (T0). */
/* Tab payload: a junction owes a bare comma (content both sides);
   a trailing slot owes comma + space, caret ready for the element */
let at_caret = (z: Zipper.t, obs: list(t)): option(string) =>
  switch (z.caret, obs) {
  | (_, [])
  | (Inner(_), _) => None
  | (Outer, _) =>
    let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
    let existing = CanonicalCompletion.for_editor(seg).insertions;
    let anchor_of = (ob: t): option(Id.t) => {
      let merged =
        existing
        |> List.find_opt((ins: CanonicalCompletion.insertion) =>
             ins.delimiters
             |> List.exists((d: CanonicalCompletion.delimiter_info) =>
                  switch (d.of_shard) {
                  | Some((tid, _)) => Id.equal(tid, ob.site)
                  | None => false
                  }
                )
           );
      switch (merged) {
      | Some(ins) => Some(ins.adjacent_id)
      | None => anchor_of_site(seg, ob.site)
      };
    };
    let anchored =
      (
        obs
        |> List.filter_map(ob =>
             deficit(ob) > 0
               ? anchor_of(ob) |> Option.map(a => (a, ", ")) : None
           )
      )
      @ (
        obs
        |> List.concat_map(ob => {
             let upto =
               existing
               |> List.find_map((ins: CanonicalCompletion.insertion) =>
                    ins.delimiters
                    |> List.exists((d: CanonicalCompletion.delimiter_info) =>
                         switch (d.of_shard) {
                         | Some((tid, _)) => Id.equal(tid, ob.site)
                         | None => false
                         }
                       )
                      ? Some(ins.adjacent_id) : None
                  );
             chosen(ob, junction_grouts(~upto?, seg, ob.site))
             |> List.map(gid => (gid, ","));
           })
      );
    let is_content = (p: Piece.t): bool =>
      switch (p) {
      | Secondary(_)
      | Grout(_) => false
      | _ => true
      };
    let find = (id: Id.t): option(string) =>
      anchored
      |> List.find_opt(((a, _)) => Id.equal(a, id))
      |> Option.map(snd);
    let rec probe = (ps: list(Piece.t)) =>
      switch (ps) {
      | [] => None
      | [p, ...rest] =>
        switch (find(Piece.id(p))) {
        | Some(_) as hit => hit
        | None => is_content(p) ? None : probe(rest)
        }
      };
    let (l, r) = z.relatives.siblings;
    switch (probe(List.rev(l))) {
    | Some(_) as hit => hit
    | None => probe(r)
    };
  };

/* Display fork: the ghost as STRUCTURAL pieces — real comma
   tiles, real grout, the real tile's own closer shards — so the
   display shows actual holes with actual shapes and can't drift
   from the material. Spacing per the F1 join rule. */
let ghost_pieces =
    (z: Zipper.t, ins: CanonicalCompletion.insertion): option(Segment.t) => {
  let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  let space = (): Piece.t =>
    Secondary({
      id: Id.mk(),
      content: Whitespace(" "),
    });
  let hole = (): Piece.t =>
    Grout({
      id: Id.mk(),
      shape: Convex,
    });
  let hugs_left = t =>
    String.length(t) > 0
    && (
      switch (t.[0]) {
      | ','
      | ')'
      | ']'
      | '}' => true
      | _ => false
      }
    );
  let piece_of = (d: CanonicalCompletion.delimiter_info): option(Piece.t) =>
    switch (d.of_shard) {
    | Some((tid, i)) =>
      switch (find_tile(tid, seg)) {
      | Some(t) => Some(Piece.Tile(Tile.shard_of(t, i)))
      | None => None
      }
    | None =>
      /* T1 comma */
      Some(
        Piece.Tile({
          id: Id.mk(),
          label: Form.get(CommaExp).label,
          mold: Form.get(CommaExp).mold,
          shards: [0],
          children: [],
        }),
      )
    };
  let rec build = (ds: list(CanonicalCompletion.delimiter_info), first) =>
    switch (ds) {
    | [] => Some([])
    | [d, ...rest] =>
      switch (piece_of(d)) {
      | None => None
      | Some(p) =>
        switch (build(rest, false)) {
        | None => None
        | Some(tail) =>
          let sep = first || hugs_left(d.text) ? [] : [space()];
          let hole_part = d.needs_hole ? [space(), hole()] : [];
          Some(sep @ [p] @ hole_part @ tail);
        }
      }
    };
  switch (ins.delimiters) {
  | [] => None
  | [d0, ..._] when d0.typed_len != None => None /* witness: TyDi's */
  | ds => build(ds, !hugs_left(List.hd(ds).text) ? false : true)
  };
};
