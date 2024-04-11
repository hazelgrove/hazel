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
