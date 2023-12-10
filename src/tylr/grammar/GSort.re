include GZipper;
type t = GZipper.t(Sort.t);

let sort = focus;
let parent = s => s.sort;

let bounds = (s: t) => {
  let same_sort = Sort.eq(sort(s), parent(s));
  let l = same_sort && nullable(R, s) ? Some(s.prec) : None;
  let r = same_sort && nullable(L, s) ? Some(s.prec) : None;
  (l, r);
};