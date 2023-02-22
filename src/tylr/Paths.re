open Sexplib.Std;
open Util;

// current a list but could turn into record for specific paths
// (eg cursor vs variable uses)
[@deriving (show({with_path: false}), sexp, yojson)]
type t = list(Path.t);

let merge = List.concat;

// let offsets = List.map((p: Path.t) => p.offset);

let uncons = (ps: t): option((Path.Kid.t, t)) =>
  switch (ps) {
  | []
  | [{kids: [], _}, ..._] => None
  | [{kids: [hd, ...kids], _} as p, ...ps] =>
    ps
    |> List.map(
         fun
         | Path.{kids: [hd', ...kids], _} as p when hd' == hd =>
           Some({...p, kids})
         | _ => None,
       )
    |> OptUtil.sequence
    |> Option.map(List.cons({...p, kids}))
    |> Option.map(ps => (hd, ps))
  };

let link = (ps_kid, ps_p, ps_mel: t) => {
  let ps_kid = List.map(Path.cons(0), ps_kid);
  let ps_p = List.map(Path.of_piece(0), ps_p);
  let ps_mel = List.map(Path.link, ps_mel);
  merge([ps_kid, ps_p, ps_mel]);
};
let knil = (~len, ps_mel: t, ps_p, ps_kid) => {
  let ps_mel = List.map(Path.knil(~len), ps_mel);
  let ps_p = List.map(Path.of_piece(len), ps_p);
  let ps_kid = List.map(Path.cons(len + 1), ps_kid);
  merge([ps_mel, ps_p, ps_kid]);
};

let unlink = ListUtil.partition3_map(Path.unlink);
let unknil = (~len) => ListUtil.partition3_map(Path.unknil(~len));

// let trim = step => List.map(Path.trim(step));
let with_kid = (ps: t, kid: int) =>
  List.partition_map(
    p =>
      switch (Path.with_kid(kid, p)) {
      | Some(p) => Left(p)
      | None => Right(p)
      },
    ps,
  );
let with_piece = (ps: t, index: int) =>
  List.partition_map(
    p =>
      switch (Path.with_piece(index, p)) {
      | Some(p) => Left(p)
      | None => Right(p)
      },
    ps,
  );
let with_space = (ps: t, side: Dir.t) =>
  List.partition_map(
    p =>
      switch (Path.with_space(side, p)) {
      | Some(p) => Left(p)
      | None => Right(p)
      },
    ps,
  );
