open Util;

[@deriving (show({with_path: false}), eq)]
type t = {
  folders: list(string),
  leaf: string,
};

/* Rendered between segments, and split on to recover them. */
let separator = " / ";
let separator_char = '/';

let of_string = (s: string): t => {
  let segs =
    String.split_on_char(separator_char, s) |> List.map(String.trim);
  let (folders, leaf) = ListUtil.split_last(segs);
  {
    folders,
    leaf,
  };
};

/* Via of_string, so a part that itself holds a separator gets split. */
let mk = (~folders: list(string)=[], leaf: string): t =>
  of_string(String.concat(separator, folders @ [leaf]));

let leaf = (p: t): string => p.leaf;
let folders = (p: t): list(string) => p.folders;
let segments = (p: t): list(string) => p.folders @ [p.leaf];

let to_string = (p: t): string => segments(p) |> String.concat(separator);

let folder = (p: t): option(string) =>
  switch (p.folders) {
  | [] => None
  | folders => Some(String.concat(separator, folders))
  };

let same_folder = (a: t, b: t): bool =>
  List.equal(String.equal, a.folders, b.folders);

/* Positions sharing the folder of the one at `current`, in list order. Scans
   the whole list, so a folder's paths need not be adjacent. */
let folder_indices = (~current: int, paths: list(t)): list(int) =>
  switch (List.nth_opt(paths, current)) {
  | None => []
  | Some(cur) =>
    paths
    |> List.mapi((i, p) => (i, p))
    |> List.filter_map(((i, p)) => same_folder(p, cur) ? Some(i) : None)
  };

let folder_position = (~current: int, paths: list(t)): (int, int) => {
  let idxs = folder_indices(~current, paths);
  let pos =
    ListUtil.findi_opt(i => i == current, idxs)
    |> Option.map(fst)
    |> Option.value(~default=0);
  (pos, List.length(idxs));
};

let step_in_folder = (~current: int, ~by: int, paths: list(t)): int => {
  let idxs = folder_indices(~current, paths);
  let (pos, size) = folder_position(~current, paths);
  let pos' = pos + by;
  pos' < 0 || pos' >= size ? current : List.nth(idxs, pos');
};

type crumb = {
  selected: string,
  options: list((int, string)),
};

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
                List.equal(String.equal, ListUtil.take(depth, segs), prefix)
                  /* None: no segment at this depth, so nothing to offer. */
                  ? List.nth_opt(segs, depth) |> Option.map(seg => (i, seg))
                  : None
              )
           |> ListUtil.dedup_f(((_, a), (_, b)) => String.equal(a, b));
         {
           selected,
           options,
         };
       })
  };
};
