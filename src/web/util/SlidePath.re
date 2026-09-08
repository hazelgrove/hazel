open Util;

/* SlidePath: the "/"-separated hierarchical name shared by Documentation-mode
 * slide names ("B2T2 / Table API / Constructors / vcat", src/b2t2/Slides.re)
 * and Tutorial-mode lesson titles ("Basics / Holes"). The leading segments are
 * folders, the last is the leaf.
 *
 * Modes keep storing these names as plain strings — they are IndexedDB keys
 * ("doc:" ++ name, see ScratchMode.Persist) and .hzt `@title` text — so this
 * module is the one place that knows the encoding. It owns both the breadcrumb
 * decomposition the top bar renders (EditorModeView.indicator_select) and the
 * folder-scoped arithmetic the navigation arrows use (TutorialsMode). */

/* `show` is used by Test_SlidePath's Alcotest printer. */
[@deriving show({with_path: false})]
type t = {
  folders: list(string),
  leaf: string,
};

/* Written between segments by to_string. of_string trims each segment, so any
   amount of surrounding whitespace parses to the same path. */
let separator = " / ";

let of_string = (s: string): t => {
  let segs = String.split_on_char('/', s) |> List.map(String.trim);
  switch (List.rev(segs)) {
  | [] => {
      folders: [],
      leaf: "",
    } /* split_on_char never returns [] */
  | [leaf, ...rev_folders] => {
      folders: List.rev(rev_folders),
      leaf,
    }
  };
};

let segments = (p: t): list(string) => p.folders @ [p.leaf];

let to_string = (p: t): string => segments(p) |> String.concat(separator);

let depth = (p: t): int => List.length(p.folders) + 1;

/* The folder a path sits in, "" for a top-level path. */
let folder = (p: t): string => String.concat(separator, p.folders);

let same_folder = (a: t, b: t): bool => a.folders == b.folders;

/* Indices of the paths sharing the folder of the one at `current`, in list
   order. Deliberately scans the whole list, so a folder's paths need not be
   contiguous for grouping to be correct (contiguity only affects the order
   options appear in a dropdown). */
let folder_indices = (~current: int, paths: list(t)): list(int) =>
  switch (List.nth_opt(paths, current)) {
  | None => []
  | Some(cur) =>
    paths
    |> List.mapi((i, p) => (i, p))
    |> List.filter_map(((i, p)) => same_folder(p, cur) ? Some(i) : None)
  };

/* (position of `current` within its folder, that folder's size). */
let folder_position = (~current: int, paths: list(t)): (int, int) => {
  let idxs = folder_indices(~current, paths);
  let pos =
    ListUtil.findi_opt(i => i == current, idxs)
    |> Option.map(fst)
    |> Option.value(~default=0);
  (pos, List.length(idxs));
};

/* The index `by` steps from `current` within its folder, clamped at the
   folder's edges: returns `current` when the step would leave the folder. */
let step_in_folder = (~current: int, ~by: int, paths: list(t)): int => {
  let idxs = folder_indices(~current, paths);
  let (pos, size) = folder_position(~current, paths);
  let pos' = pos + by;
  pos' < 0 || pos' >= size ? current : List.nth(idxs, pos');
};

/* One crumb per segment of the path at `current`. */
type crumb = {
  /* The segment selected at this depth. */
  selected: string,
  /* The sibling segments to offer here — those of the paths sharing
     `current`'s prefix, deduped, each with the index to jump to. */
  options: list((int, string)),
};

/* Decompose a path list into the breadcrumb of sibling-pickers for `current`.
   Options keep first-appearance order (dedup_f), so a folder option jumps to
   the first path in that folder. A path too short to have a segment at a given
   depth is skipped rather than raising, so a name that is a proper prefix of
   another (e.g. "Tables" alongside "Tables / Tables") cannot crash the top bar
   — it is merely unreachable from the deeper dropdown. */
let breadcrumb = (~current: int, paths: list(t)): list(crumb) => {
  let all =
    paths |> List.map(segments) |> List.mapi((i, segs) => (i, segs));
  switch (List.nth_opt(all, current)) {
  | None => []
  | Some((_, parts)) =>
    parts
    |> List.mapi((depth, selected) => {
         let prefix = ListUtil.take(depth, parts);
         let options =
           all
           |> List.filter_map(((i, segs)) =>
                ListUtil.take(depth, segs) == prefix
                  ? List.nth_opt(segs, depth) |> Option.map(seg => (i, seg))
                  : None
              )
           |> ListUtil.dedup_f(((_, a), (_, b)) => a == b);
         {
           selected,
           options,
         };
       })
  };
};
