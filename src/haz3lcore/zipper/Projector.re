open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type p =
  | Normal
  | Fold;

let to_string: p => string =
  fun
  | Normal => ""
  | Fold => "F";

let toggle_fold: p => p =
  fun
  | Normal => Fold
  | Fold => Normal;

[@deriving (show({with_path: false}), sexp, yojson)]
module Map = {
  open Id.Map;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(p);
  let empty = empty;
  let add = add;
  let remove = remove;
  let find = find_opt;
  let mem = mem;
  let fold = fold;
  let cardinal = cardinal;
};

type t = p;

let rep_id = (seg, r) => r |> Aba.first_a |> List.nth(seg) |> Piece.id;

let rec left_idx = (skel: Skel.t): int => {
  switch (skel) {
  | Op(r)
  | Pre(r, _) => Aba.first_a(r)
  | Post(s, _)
  | Bin(s, _, _) => left_idx(s)
  };
};

let rec right_idx = (skel: Skel.t): int => {
  switch (skel) {
  | Op(r)
  | Post(_, r) => Aba.first_a(r)
  | Pre(_, s)
  | Bin(_, _, s) => right_idx(s)
  };
};

let get_extreme_idxs = (skel: Skel.t): (int, int) => (
  left_idx(skel),
  right_idx(skel),
);

type projector_range = {
  id: Id.t,
  start: int,
  last: int,
};

let get_range = (seg: Segment.t, ps: Map.t): option((Id.t, (int, int))) => {
  let rec go = (skel: Skel.t) => {
    let id = rep_id(seg, Skel.root(skel));
    switch (Id.Map.find_opt(id, ps)) {
    | Some(_) =>
      let (l, r) = get_extreme_idxs(skel);
      Some((id, (l, r)));
    | None =>
      switch (skel) {
      | Op(_) => None
      | Pre(_, r) => go(r)
      | Post(l, _) => go(l)
      | Bin(l, _, r) =>
        switch (go(l)) {
        | Some(x) => Some(x)
        | None => go(r)
        }
      }
    };
  };
  go(Segment.skel(seg));
};

let split_seg =
    (seg: Segment.t, ps: Map.t)
    : option((Segment.t, Segment.t, Segment.t, Id.t)) => {
  switch (get_range(seg, ps)) {
  | None => None
  | Some((id, (start, last))) =>
    //TODO(andrew): numeric edge cases?
    switch (ListUtil.split_sublist_opt(start, last + 1, seg)) {
    | Some((pre, mid, suf)) => Some((pre, mid, suf, id))
    | _ => None
    }
  };
};

let placeholder_tile = (s: string, id: Id.t): Tile.t => {
  id,
  label: [s],
  mold: Mold.mk_op(Any, []),
  shards: [0],
  children: [],
};

let project_mid = (id, p: option(t), mid): Segment.t =>
  //TODO(andrew): prrobably shouldn't just duplicate this id in the general case?
  switch (p) {
  | Some(Fold) => [Tile(placeholder_tile("  ", id))]
  //[Grout({id, shape: Convex})]
  | Some(Normal)
  | None => mid
  };

let project_seg = (p, seg: Segment.t): Segment.t => {
  // print_endline("project_seg");
  switch (split_seg(seg, p)) {
  | Some((pre, mid_og, suf, proj_id)) =>
    /*TODO(andrew): need to find a way to handle multiple projectors per segment,
      i.e. wasn't thinking about ones in disjoint subtrees */
    // print_endline(
    //   "split: segment length: "
    //   ++ string_of_int(List.length(seg))
    //   ++ " as divided into: "
    //   ++ string_of_int(List.length(pre))
    //   ++ " "
    //   ++ string_of_int(List.length(mid_og))
    //   ++ " "
    //   ++ string_of_int(List.length(suf)),
    // );
    pre @ project_mid(proj_id, Map.find(proj_id, p), mid_og) @ suf
  | None =>
    // print_endline("no split");
    seg
  };
};

let rec of_segment = (projectors, seg: Segment.t): Segment.t => {
  seg |> project_seg(projectors) |> List.map(of_piece(projectors));
}
and of_piece = (projectors, p: Piece.t): Piece.t => {
  switch (p) {
  | Tile(t) => Tile(of_tile(projectors, t))
  | Grout(_) => p
  | Secondary(_) => p
  };
}
and of_tile = (projectors, t: Tile.t): Tile.t => {
  {...t, children: List.map(of_segment(projectors), t.children)};
};

type start_entry = {
  proj_id: Id.t,
  t,
  start_id: Id.t,
  last_id: Id.t,
};
/* map indexed by start_id instead of proj_id */
type start_map = Id.Map.t(start_entry);
let guy_of = (id, t, (start, last)) => {
  proj_id: id,
  t,
  start_id: Piece.id(start),
  last_id: Piece.id(last),
};
let guy_of_rev = (id, t, (start, last)) => {
  proj_id: id,
  t,
  start_id: Piece.id(last),
  last_id: Piece.id(start),
};
let proj_info = (term_ranges, id: Id.t, t: t, acc: start_map) => {
  print_endline("proj_info for id: " ++ Id.to_string(id));
  switch (Id.Map.find_opt(id, term_ranges)) {
  | Some(range) =>
    let guy = guy_of(id, t, range);
    Id.Map.add(guy.start_id, guy, acc);
  | _ =>
    print_endline("ERROR: mk_nu_proj_map: no term range for projector");
    acc;
  };
};
let proj_info_rev = (term_ranges, id: Id.t, t: t, acc: start_map) => {
  print_endline("proj_info for id: " ++ Id.to_string(id));
  switch (Id.Map.find_opt(id, term_ranges)) {
  | Some(range) =>
    let guy = guy_of(id, t, range);
    Id.Map.add(guy.last_id, guy, acc);
  | _ =>
    print_endline("ERROR: mk_nu_proj_map: no term range for projector");
    acc;
  };
};

let mk_start_map = (projectors: Map.t, term_ranges: TermRanges.t): start_map =>
  Map.fold(proj_info(term_ranges), projectors, Id.Map.empty);

let mk_last_map = (projectors: Map.t, term_ranges: TermRanges.t): start_map =>
  Map.fold(proj_info_rev(term_ranges), projectors, Id.Map.empty);

let fake_measured =
    (p: Map.t, measured: Measured.t, term_ranges: TermRanges.t): Measured.t =>
  Map.fold(
    (id, _p: t, measured: Measured.t) => {
      switch (
        Measured.find_by_id(id, measured),
        Id.Map.find_opt(id, term_ranges),
      ) {
      | (Some(m), Some((p_start, p_last))) =>
        let p_start = Piece.Tile(placeholder_tile("  ", Piece.id(p_start)));
        let p_last = Piece.Tile(placeholder_tile("  ", Piece.id(p_last)));
        let measured = Measured.add_p(p_start, m, measured);
        let measured = Measured.add_p(p_last, m, measured);
        print_endline("fake_measured: added placeholder tiles:");
        print_endline("root_id:" ++ Id.to_string(id));
        print_endline("start_id:" ++ Id.to_string(Piece.id(p_start)));
        print_endline("last_id:" ++ Id.to_string(Piece.id(p_last)));
        measured;
      | (Some(_), None) =>
        print_endline("fake_measured: no term range for projector");
        measured;
      | _ =>
        print_endline("fake_measured: no measurement for projector");
        measured;
      }
    },
    p,
    measured,
  );

/*
 projector map has ids of projectors
 can use infomap to get ancestors of projectors

 projector map has projector type
 but for each projector we also need:
   - it's extent (to use for measured)
     - clarification: 'range of projector': extent in base syntax
     - vs 'domain of projector': extent in view (eg collapsed entirely for fold)
   - it's view function (to use for view)
   - it's action function (to use for action dispatch
 we might want to pre-derive:
  - parent segment
   - the range of the projector in the segment

 measured side:
 recurse into segment, accumulating ancestor list
 i guess this alternatingly comes from enclosing tiles and segment skels
 i guess inside a segment, we recurse into the skel, tracking going through ancestors
 from the ancestor list until we hit a tile
 whose id is the target id
 then we subsistute in a token consisting only of spaces/linebreaks which
 takes up the space of the projector (which i guess should be a starting
 and ending offset in the frame of the parent editor)


 view side:
 recurse into segment, accumulating ancestor list
 i guess this alternatingly comes from enclosing tiles and segment skels
 i guess inside a segment, we recurse into the skel, tracking going through ancestors
 from the ancestor list until we hit a tile
 whose id is the target id

 maybe to make things easier:
 if for each projector we know its parent segment
 and know what range of the segment 0 <= start < end < length(seg) corresponds to the projector

 ok, new plan:
 in view_of_segment, we recurse the skel looking for projectors. the first (topmost) one we find,
 we find it's range in the segment, and create new segment to render, consisting of the segment
 vefore the projector subrange, a placeholder for the projector subrange, and the segment after
 the projector subrange. we don't handle any drawing for the projector; that goes through Deco

 for first pass we dont need full recursion, so can just have a deco type that is subeditors
 for first pass subeditor will just be stub views, so they can be treated as inline/tokens
 (for higher phases will want to be able to insert full editors, so will likely need editors
 to have programable starting col, so that we can draw them as if they were just a subsegment
 of the parent editor)


 action side:

 for opaque projectors, we will prohibit movement into them,
 and make forming them eject the cursor. as long as we
 ensure the cursor cant move into one, we can dont
 need to worry about whether we're inside one for action permissions

 so for basic movement actions, we make moving left into an
 opaque leaf projector jump to its right, and vice versa

 (phase 1.5 we allow a single cursor state on the projector itself)

 actions: movement
   - phase 1.0: move past subview
   - phase 1.5: move onto subview
   - phase 2: move into subview

 for phase 1.5, we probably need to extend caret position to
 model the cursor being on the projector itself. being in this
 state will dispatch keyboard input to the projector's handler

 old:
   type t =
     | Outer
     | Inner(int, int);

 v2:
   type inner =
    | Token(int, int)
    | SubCell(Id.t);
   type t =
     | Outer
     | Inner(inner);

   v3:
   type base =
     | Outer
     | Inner(int, int);
   type t =
    | Base(base)
    | SubCell(Id.t, t); // supports nested subcells



   primary movement needs to check if the piece we're trying to move into starts/ends
   a subcell. if so, we (phase i) skip over it or (phase ii) move into it, ie set caret
   position to Subcell(Id.t,og_caret_pos)

  2024-02-26
  want to add some entries to Measured
  right now the placeholder approach is that a monotoken is introduced
  with the same id as the projector root term. this seems to work well
  in the case where the projector root tile is convex, as this means that
  the delims on the left and right of the projected segment are shards of
  the same tile. but actually even this creates some issues actually,
  see the FuCkNASTY hackzz in Measured.

  in general we are concerned with caret positions to the left and right
  of the projected segment. caret_point/base_point is excepting there
  because they are looking for ids not found in measured, in the case
  of infix operators on both sides so they can't currently be folded.

  not sure if its a good idea, but one approach is to make sure that
  the ids in Measured include the left and right delims of the projected
  segment instead of the root id.

  but we probably dont want to add multiple tiles to the projection zipper.
  but maybe we could add ids after the fact, copying some measurements
  by querying the measurements of the placeholder tile.

  collapsed segments:

  [1]: root is 1, L is 1, R is 1
  [(1)]: root is [(,)], L is [(], R is [)]
  [1+2] : root is +, L is 1, R is 2
  [let x = 1 in 4] root is [let,=,in], L is [let], R is 4
  (mythical postfix operator:)
  [5!] root is [,], L is 5, L is [!]

  ideally the placeholder tile should correspond to the left and right
  delims of the projected segment so as to make measured accesses
  seamless. so what if:
  placeholder tiles uses two shards, one for the left delim and
  one for the right delim
  then after we run Measured we get the placeholder measurement,
  and create (up to) two new entries in the Measured map, one for
  the left delim and one for the right delim,

  NOTE: currently the FuCkNASTY occurs on the right delim
  of a collapsed "(1+2)" segment, but not a "1" segment

  can fold [fun, ->], but crash (base_point find_p) if we try
  to move right when on left side of collapse

  can't fold "1+2" from any side (crash)


 */
