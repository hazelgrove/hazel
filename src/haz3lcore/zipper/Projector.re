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
  //let update = update;
  //let to_list = x => x |> List.to_seq |> Id.Map.of_seq;
  //let mapi = mapi;
  let fold = fold;
  let cardinal = cardinal;
};

type t = p;

type proj = Map.t;
type proj_ty = t;

type nu_proj_info = {
  proj_id: Id.t, //projector id
  t,
  start_id: Id.t, //first piece id
  last_id: Id.t //last piece id
};
/* map indexed by start_id instead of proj_id */
type nu_proj_map = Id.Map.t(nu_proj_info);

let split_seg =
    (seg: Segment.t, ps: nu_proj_map)
    : option((Segment.t, Segment.t, Segment.t, Id.t, proj_ty)) => {
  let res =
    List.find_map((p: Piece.t) => Id.Map.find_opt(Piece.id(p), ps), seg);
  switch (res) {
  | None => None
  | Some(pr) =>
    let (pre, rest) =
      ListUtil.take_while(p => !Id.Map.mem(Piece.id(p), ps), seg);
    let (mid, suf) =
      rest |> List.rev |> ListUtil.take_while(p => Piece.id(p) != pr.last_id);
    Some((pre, suf |> List.rev, mid |> List.rev, pr.proj_id, pr.t));
  };
};

let guy_of = (id, t, (start, last)) => {
  proj_id: id,
  t,
  start_id: Piece.id(start),
  last_id: Piece.id(last),
};

let proj_info = (term_ranges, id: Id.t, t: proj_ty, acc: nu_proj_map) => {
  print_endline("proj_info for id: " ++ Id.to_string(id));
  switch (Id.Map.find_opt(id, term_ranges)) {
  | Some(range) =>
    //print_endline("proj_info: found term range for projector");
    let guy = guy_of(id, t, range);
    Id.Map.add(guy.start_id, guy, acc);
  | _ =>
    print_endline("ERROR: mk_nu_proj_map: no term range for projector");
    acc;
  };
};

let mk_nu_proj_map =
    (projectors: Map.t, term_ranges: TermRanges.t): nu_proj_map =>
  Map.fold(proj_info(term_ranges), projectors, Id.Map.empty);

let placeholder_tile = (s: string, id: Id.t): Tile.t => {
  id,
  label: [s],
  mold: Mold.mk_op(Any, []),
  shards: [0],
  children: [],
};

let project_mid = (id, p: proj_ty, mid): Segment.t =>
  //TODO(andrew): prrobably shouldn't just duplicate this id in the general case?
  switch (p) {
  | Fold => [Tile(placeholder_tile("%", id))]
  //[Grout({id, shape: Convex})]
  | Normal => mid
  };

let project_seg = (p: nu_proj_map, seg: Segment.t): Segment.t => {
  /* needed input: a map/assoc list of projector first piece ids to projector (and last piece id) (don't even need id here maybe?) */
  /* find first idx in seg where is matches a projector range */
  /* check other side of range exists */
  /* split segment three ways. prefix and suffix left alone */
  /* dispatch on projector type */
  /* Fold will turn middle of suffix into singleton padding token */
  /* stitch seg back together */
  print_endline("project_seg");
  switch (split_seg(seg, p)) {
  | Some((pre, mid_og, suf, proj_id, p)) =>
    print_endline(
      "split: segment length: "
      ++ string_of_int(List.length(seg))
      ++ " as divided into: "
      ++ string_of_int(List.length(pre))
      ++ " "
      ++ string_of_int(List.length(mid_og))
      ++ " "
      ++ string_of_int(List.length(suf)),
    );
    pre @ project_mid(proj_id, p, mid_og) @ suf;
  | None =>
    print_endline("no split");
    seg;
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



 */
