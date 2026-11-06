open Util;

[@deriving (show({with_path: false}), eq)]
type t = {
  folders: list(string),
  leaf: string,
};

/* Split on to recover the segments, padded when rendering them back. */
let separator_char = '/';
let separator = Printf.sprintf(" %c ", separator_char);

let of_string = (s: string): t => {
  let segs =
    String.split(s, ~on=separator_char) |> List.map(~f=String.strip);
  let (folders, leaf) = ListUtil.split_last(segs);
  {
    folders,
    leaf,
  };
};

/* Via of_string, so a part that itself holds a separator gets split. */
let mk = (~folders: list(string)=[], leaf: string): t =>
  of_string(String.concat(~sep=separator, folders @ [leaf]));

let leaf = (p: t): string => p.leaf;
let folders = (p: t): list(string) => p.folders;
let segments = (p: t): list(string) => p.folders @ [p.leaf];

let to_string = (p: t): string =>
  segments(p) |> String.concat(~sep=separator);

let folder = (p: t): option(string) =>
  switch (p.folders) {
  | [] => None
  | folders => Some(String.concat(~sep=separator, folders))
  };

let same_folder = (a: t, b: t): bool =>
  List.equal(String.equal, a.folders, b.folders);

/* Positions sharing the folder of the one at `current`, in list order. Scans
   the whole list, so a folder's paths need not be adjacent. */
let folder_indices = (~current: int, paths: list(t)): list(int) =>
  switch (List.nth(paths, current)) {
  | None => []
  | Some(cur) =>
    paths
    |> List.mapi(~f=(i, p) => (i, p))
    |> List.filter_map(~f=((i, p)) => same_folder(p, cur) ? Some(i) : None)
  };

type folder_position = {
  index_in_folder: int,
  folder_size: int,
};

let folder_position = (~current: int, paths: list(t)): folder_position => {
  let idxs = folder_indices(~current, paths);
  let index_in_folder =
    ListUtil.findi_opt(i => i == current, idxs)
    |> Option.map(~f=fst)
    |> Option.value(~default=0);
  {
    index_in_folder,
    folder_size: List.length(idxs),
  };
};

let step_in_folder = (~current: int, ~by: int, paths: list(t)): int => {
  let idxs = folder_indices(~current, paths);
  let {index_in_folder, folder_size} = folder_position(~current, paths);
  let stepped = index_in_folder + by;
  stepped < 0 || stepped >= folder_size
    ? current : List.nth_exn(idxs, stepped);
};

type crumb = {
  selected: string,
  options: list((int, string)),
};

let breadcrumb = (~current: int, paths: list(t)): list(crumb) => {
  let all =
    paths |> List.map(~f=segments) |> List.mapi(~f=(i, segs) => (i, segs));
  switch (List.nth(all, current)) {
  | None => []
  | Some((_, parts)) =>
    parts
    |> List.mapi(~f=(depth, selected) => {
         let prefix = ListUtil.take(depth, parts);
         let options =
           all
           |> List.filter_map(~f=((i, segs)) =>
                List.equal(String.equal, ListUtil.take(depth, segs), prefix)
                  /* None: no segment at this depth, so nothing to offer. */
                  ? List.nth(segs, depth) |> Option.map(~f=seg => (i, seg))
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
