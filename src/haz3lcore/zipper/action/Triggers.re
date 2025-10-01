open Zipper;
open Util;
open OptUtil.Syntax;

/* Syntax replacement operations to automatically run after insertion */

let exp_to_seg =
  ExpToSegment.exp_to_segment(
    ~settings=
      ExpToSegment.Settings.of_core(~inline=true, Language.CoreSettings.on),
  );

let invoked_projector = (name: string, syntax: Segment.t): option(Piece.t) => {
  let* name = Token.of_projector_invoke(name);
  let kind = ProjectorCore.Kind.of_name(name);
  ProjectorPerform.init(kind, syntax);
};

let expand_projector = (z: t): option(t) => {
  switch (z.relatives.siblings |> fst |> List.rev) {
  | [
      Tile({label: ["(", ")"], children: [syntax], _}),
      Tile({label: [name], _}),
      ...rest,
    ]
      when name == "^^probe" =>
    /* Trim only need because of grout/whitespace transmutation when syntax is hole */
    // let syntax =
    //   syntax |> Segment.trim_secondary(Right) |> Segment.trim_secondary(Left);
    Zipper.update_siblings(((_, r)) => (List.rev(syntax @ rest), r), z)
    |> MkRefractor.add_single(
         Segment.root_id(Segment.skel(syntax), syntax),
       )
    |> Option.some
  //TODO(andrew): cleanup
  | [
      Tile({label: ["(", ")"], children: [syntax], _}),
      Tile({label: [name], _}),
      ...rest,
    ]
      when Token.is_projector_invoke(name) =>
    /* Trim only need because of grout/whitespace transmutation when syntax is hole */
    let syntax =
      syntax |> Segment.trim_secondary(Right) |> Segment.trim_secondary(Left);
    let+ piece = invoked_projector(name, syntax);
    Zipper.update_siblings(
      ((_, r)) => ([piece, ...rest] |> List.rev, r),
      z,
    );
  /* Special case for reparsing of projectors placed on holes */
  | [Tile({label: ["()"], _}), Tile({label: [name], _}), ...rest]
      when Token.is_projector_invoke(name) =>
    let+ piece = invoked_projector(name, [Piece.mk_grout(Convex)]);
    Zipper.update_siblings(
      ((_, r)) => ([piece, ...rest] |> List.rev, r),
      z,
    );
  | _ => None
  };
};

let refractor_to_invoke =
    (kind: ProjectorCore.Kind.t, seg: Segment.t): Segment.t => [
  Piece.mk_tile(Form.mk_atom_op(Exp, Token.mk_projector_invoke(kind)), []),
  Piece.mk_tile(Form.get(ApExp), [seg]),
];

let projector_to_invoke = (pr: Base.projector): Segment.t =>
  refractor_to_invoke(pr.kind, Piece.unparenthesize(pr.syntax));

let expand_livelit = (~ctx, z: t): option(t) =>
  switch (z.relatives.siblings |> fst |> List.rev) {
  | [Secondary({content: Whitespace(w), _}), Tile({label: [t], _}), ..._]
      when Token.is_livelit(t) && w == Token.space =>
    let* ll = Language.Ctx.lookup_livelit(ctx, Token.parse_livelit(t));
    let seg = exp_to_seg(ll.model_default);
    let seg =
      switch (ll.model_default) {
      | {term: Tuple(_), _} => Segment.unparenthesize(seg)
      | _ => seg
      };
    let (l, _space) = ListUtil.split_last(fst(z.relatives.siblings));
    let (l, name) = ListUtil.split_last(l);
    let seg = [name, Piece.mk_tile(Form.get(ApExp), [seg])];
    let+ pr = ProjectorPerform.init(Livelit, seg);
    Zipper.update_siblings(((_, r)) => (l @ [pr], r), z);
  | _ => None
  };

/* This is a wrapper intended to effectuate after-insertion conditional
 * operations. This is done here as opposed to in Perform in order to
 * reflect operations we want performed by the parser, which uses
 * Insert.go as its primary driver. Triggers should be zipper to zipper
 * functions which conditionally perform a syntax operation which may
 * (or may not) take caret position into account. Morally these should
 * be mutually exclusive; otherwise the order below is load-bearing. */
let insert = (~ci: option(Language.Info.t), z: t): t => {
  let ctx =
    ci
    |> Option.map(Language.Info.ctx_of)
    |> Option.value(~default=Language.Ctx.empty);
  let triggers = [expand_projector, expand_livelit(~ctx)];
  List.fold_left((z, f) => Option.value(f(z), ~default=z), z, triggers);
};

/* These are just alternate conditional deletion logic. */
let destruct = (z: t): option(t) =>
  switch (z.relatives.siblings |> fst |> ListUtil.last_opt) {
  | Some(Projector({syntax, kind, _})) =>
    let (l, _) = ListUtil.split_last(fst(z.relatives.siblings));
    let last =
      switch (kind, syntax) {
      | (Livelit, Tile({children: [[name, ..._]], _})) => [name]
      | _ => Piece.unparenthesize(syntax)
      };
    Some(Zipper.update_siblings(((_, r)) => (l @ last, r), z));
  | _ => None
  };

/*
  What we want to do here is make the skel, and the recurse into the skel structure,
  at each time checking if the skel root id is in the refractor map. If it is, we need
  to wrap the subseg corresponding to the skel range corresponding to the refactor id
  with the result of calling refractor_to_invoke on the subseg, where the think we're
  wrapping is the result of continuing the recursion down into the seg. kind of confusing.
  basically the idea here is that we want to wrap certain subsegs of the segment, possibly
  contained within other to-be-wrapped subsegs, where the subsegs are given by the term
  structure as which we need to build the skel to get, where the skel can be used to get
  the indicies in the original 'base' segment (see e.g. TermData.segment) corresponding to
  a subterm of a given id. See e.g. Segment.root_id to see the relationship between ids
  and skel, as well as Skel.re obviously.

  As a simple example, consider the segment:
  '1 + 2 * 3'
  where '2 * 3' is a term with id A, and '2' is a term with id B.
  say the refractors map has entries for A and B, both having kind 'probe'.
  then we want this function to convert this segment into:
  '1 + ^^probe(^^probe(2) * 3)'
 where refractor_to_invoke should take care of the actual wrapping

 Possible strategy: Assume for initial simplicity that there are no n-ary forms. that is,
 in the skel, all roots are singleton Abas whose a=int points to a single index into the seg.
 Now, recurse into the skel. Branch on skel. In each case, Get id of root tile. In each branch,
 recurse on the children, getting segments. Now, to recombine those segments to return, we further
 branch based on whether the root tile id is in the refractors map. If it's not, we simply
 create a segment which is the results of the recursive call combine with a singleton segment of
 the root tile, obtained from the original seg, in the appropriate order/combination depending
 on the skel branch we're in. If it is in refractors, then we do the same, except wrapping the
 resulting seg using refractor_to_invoke. so i guess actually we can factor this part out to
 after the skel branching.

   */
let refractor_seg_to_seg =
    (refractors: Id.Map.t(Base.projector), seg: Segment.t)
    : (Id.Map.t(Base.projector), Segment.t) => {
  //TODO(andrew): make this support n-ary ops
  let foo = root => [List.nth(seg, Aba.first_a(root))];
  let rec go = (map, skel: Skel.t): (Id.Map.t(Base.projector), Segment.t) => {
    let (map, res) =
      switch (skel) {
      | Op(root) => (map, foo(root))
      | Pre(root, l) =>
        let (map, seg) = go(map, l);
        (map, foo(root) @ seg);
      | Post(r, root) =>
        let (map, seg) = go(map, r);
        (map, seg @ foo(root));
      | Bin(l, root, r) =>
        let (map1, seg1) = go(map, l);
        let (map2, seg2) = go(map1, r);
        (map2, seg1 @ foo(root) @ seg2);
      };
    let root_id = Segment.root_id(skel, seg);
    switch (Id.Map.find_opt(root_id, refractors)) {
    | Some(pr) => (
        Id.Map.remove(root_id, map),
        refractor_to_invoke(pr.kind, res),
      )
    | None => (map, res)
    };
  };
  Id.Map.is_empty(refractors)
    ? (refractors, seg)
    : {
      let (map, seg) = go(refractors, Segment.skel(seg));
      (map, seg);
    };
};
