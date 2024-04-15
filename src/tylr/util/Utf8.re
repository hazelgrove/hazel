open Util;

let decode = (s: string): list(Uchar.t) =>
  s
  |> Uutf.String.fold_utf_8(
       (decoded, _) =>
         fun
         | `Malformed(_) => failwith("bug: invalid utf-8")
         | `Uchar(u) => [u, ...decoded],
       [],
     )
  |> List.rev;

let length = s => List.length(decode(s));

let encode = (us: list(Uchar.t)) => {
  let b = Buffer.create(List.length(us));
  us |> List.iter(Buffer.add_utf_8_uchar(b));
  Buffer.contents(b);
};

// let sub = (i, j, s) =>
//   decode(s)
//   |> ListUtil.sublist(i, j)
//   |> encode;

let split = (i, s) => {
  let (l, r) = ListUtil.split_n(i, decode(s));
  (encode(l), encode(r));
};
