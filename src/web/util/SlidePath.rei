/* A slide's hierarchical name: zero or more folder segments followed by a
 * leaf, written as one "/"-separated string ("Basics / Holes"). Slide names
 * are stored and displayed as those strings, so this module is the only place
 * that knows the encoding.
 *
 * A list of paths, in display order, also defines a navigation space. The
 * operations at the bottom answer the two questions a slide picker asks of
 * it: where the arrows may move within a folder, and which sibling segments
 * to offer at each depth of a breadcrumb. Positions are indices into that
 * list, since that is how a picker identifies a slide. Each of those
 * operations is illustrated against one example space:
 *
 *     0  "Basics / Holes"
 *     1  "Basics / Functions"
 *     2  "Tables / Filtering"
 *     3  "Tables / Column Projection"
 *
 * Should this grow more navigation operations, the alternative worth
 * revisiting is an explicit tree of folders with slides at its leaves,
 * expressing those operations as walks over it. That was weighed and set
 * aside here: both callers identify a slide by its index in the flat list, so
 * a tree would have to carry those indices at its leaves anyway. */

[@deriving (show({with_path: false}), eq)]
type t;

/* Parse a name. Segments are trimmed, so spacing around a separator does not
   matter, and a name with no separator is a path with no folders.

     of_string("Basics / Holes") == mk(~folders=["Basics"], "Holes")
     of_string("Basics/Holes")   == mk(~folders=["Basics"], "Holes")
     of_string("Holes")          == mk("Holes") */
let of_string: string => t;

/* Build a path from its parts, normalizing as of_string does.

   mk(~folders=["a / b"], "c") == of_string("a / b / c") */
let mk: (~folders: list(string)=?, string) => t;

/* The canonical name, as stored and displayed.

   to_string(of_string("Basics/Holes")) == "Basics / Holes" */
let to_string: t => string;

/*   leaf(of_string("a / b / c")) == "c" */
let leaf: t => string;

/*   folders(of_string("a / b / c")) == ["a", "b"] */
let folders: t => list(string);

/*   segments(of_string("a / b / c")) == ["a", "b", "c"] */
let segments: t => list(string);

/* The folder the path sits in; None for a path with no folder segment.

   folder(of_string("a / b / c")) == Some("a / b")
   folder(of_string("Holes"))     == None */
let folder: t => option(string);

/* Where the path at `current` sits among the paths sharing its folder.

   folder_position(~current=0, space)
     == {index_in_folder: 0, folder_size: 2}
   folder_position(~current=3, space)
     == {index_in_folder: 1, folder_size: 2} */
type folder_position = {
  index_in_folder: int,
  folder_size: int,
};

let folder_position: (~current: int, list(t)) => folder_position;

/* Move `by` places through the folder holding `current` and return the
   position, in the same list, of the path you land on. Movement stops at the
   folder's first and last path: a step that would leave the folder returns
   `current` unchanged.

     step_in_folder(~current=0, ~by=1, space)  == 1
     step_in_folder(~current=1, ~by=1, space)  == 1  /* end of Basics */
     step_in_folder(~current=2, ~by=-1, space) == 2  /* start of Tables */ */
let step_in_folder: (~current: int, ~by: int, list(t)) => int;

/* One level of a breadcrumb: the segment the current path has at this depth,
   and every segment that could be chosen instead, each paired with the index
   to move to if it is. */
type crumb = {
  selected: string,
  options: list((int, string)),
};

/* The breadcrumb for the path at `current`: one crumb per segment it has,
   outermost first. A crumb's options are the segments at that depth among the
   paths that agree with `current` on every shallower segment, deduplicated
   keeping first appearance -- so choosing a folder moves to the first path in
   it. An out-of-range `current` has no breadcrumb.

     breadcrumb(~current=2, space) == [
       {selected: "Tables", options: [(0, "Basics"), (2, "Tables")]},
       {
         selected: "Filtering",
         options: [(2, "Filtering"), (3, "Column Projection")],
       },
     ]

   A path with no segment at a given depth offers nothing there, which is what
   keeps a name that is a prefix of another from breaking the deeper crumb:
   for ["Tables", "Tables / Tables"], the depth-1 crumb offers only the
   second. */
let breadcrumb: (~current: int, list(t)) => list(crumb);
