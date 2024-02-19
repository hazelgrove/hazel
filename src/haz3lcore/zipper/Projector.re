[@deriving (show({with_path: false}), sexp, yojson)]
type p =
  | Normal
  | Fold;

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
  let find = find_opt;
};

type t = p;


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

new:
  type inner =
   | Token(int, int)
   | SubCell(Id.t);
  type t =
    | Outer
    | Inner(inner);



*/