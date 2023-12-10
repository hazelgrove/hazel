[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('focus) = {
  sort: Sort.t,
  prec: Prec.t,
  zipper: Regex.Zipper.t('focus),
};

let mk = (~sort, ~prec, zipper) => {sort, prec, zipper};

let focus = z => fst(z.zipper);

let map = (f, {sort, prec, zipper: (a, ctx)}) =>
  mk(~sort, ~prec, (f(a), ctx));

let map_z = (f, {sort, prec, zipper}) =>
  mk(~sort, ~prec, f(zipper));

let put = foc => map(_ => foc);
