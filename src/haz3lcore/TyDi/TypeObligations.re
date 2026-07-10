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
  remaining_tys: list(Typ.t) /* element types still owed */
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
let present_of = (inner: Exp.t, info_map: Statics.Map.t): option(int) =>
  switch (Exp.term_of(inner)) {
  | Tuple(es) => Some(List.length(es))
  | EmptyHole
  | MultiHole(_)
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

let mk = (site, ~present as k, tys): t => {
  site,
  present: k,
  expected: List.length(tys),
  remaining_tys: List.filteri((i, _) => i >= k, tys),
};

let of_wrapper =
    (site: Id.t, inner: Exp.t, ~tys: list(Typ.t), info_map: Statics.Map.t)
    : option(t) =>
  switch (present_of(inner, info_map)) {
  | Some(k) when k < List.length(tys) => Some(mk(site, ~present=k, tys))
  | _ => None /* satisfied, overfull, or no presumption */
  };

let ana_elements = (e: Exp.t, info_map): option((Ctx.t, list(Typ.t))) =>
  switch (Id.Map.find_opt(Exp.rep_id(e), info_map)) {
  | Some(Info.InfoExp({ana, ctx, _})) =>
    prod_elements(ctx, ana) |> Option.map(tys => (ctx, tys))
  | _ => None
  };

let syn_of = (e: Exp.t, info_map): option((Ctx.t, Typ.t)) =>
  switch (Id.Map.find_opt(Exp.rep_id(e), info_map)) {
  | Some(Info.InfoExp({elab_syn_ty, ctx, _})) =>
    Some((ctx, Typ.weak_head_normalize(ctx, elab_syn_ty)))
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

/* last content piece inside the (complete) site tile's child */
let anchor_of_site = (seg: Segment.t, site: Id.t): option(Id.t) => {
  let rec find_tile = (ps: Segment.t): option(Tile.t) =>
    List.fold_left(
      (acc, p: Piece.t) =>
        switch (acc, p) {
        | (Some(_), _) => acc
        | (None, Tile(t)) =>
          Id.equal(t.id, site)
            ? Some(t)
            : List.fold_left(
                (acc, child) => acc == None ? find_tile(child) : acc,
                None,
                t.children,
              )
        | (None, _) => None
        },
      None,
      ps,
    );
  switch (find_tile(seg)) {
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

let as_insertions =
    (
      ~seg: Segment.t,
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
  let (existing, fresh) =
    List.fold_left(
      ((existing, fresh), ob) =>
        if (List.exists(ins => holds_site(ins, ob.site), existing)) {
          (
            existing
            |> List.map((ins: CanonicalCompletion.insertion) =>
                 holds_site(ins, ob.site)
                   ? {
                     ...ins,
                     delimiters: comma_delims(deficit(ob)) @ ins.delimiters,
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
                  delimiters: comma_delims(deficit(ob)),
                },
                ...fresh,
              ],
            )
          | None => (existing, fresh)
          };
        },
      (existing, []),
      obs,
    );
  existing @ fresh;
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
    obs
    |> List.map(ob => (ob.site, deficit(ob)))
    |> List.to_seq
    |> Hashtbl.of_seq;
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
           | Some(n) =>
             switch (Util.ListUtil.split_last_opt(t.children)) {
             | Some((init, last)) =>
               Piece.Tile({
                 ...t,
                 children: init @ [last @ owed_pieces(t.id, n)],
               })
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
let at_caret = (z: Zipper.t, obs: list(t)): option(t) =>
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
      obs |> List.filter_map(ob => anchor_of(ob) |> Option.map(a => (a, ob)));
    let is_content = (p: Piece.t): bool =>
      switch (p) {
      | Secondary(_)
      | Grout(_) => false
      | _ => true
      };
    let find = (id: Id.t): option(t) =>
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
