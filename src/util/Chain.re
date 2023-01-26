open Sexplib.Std;

// invariant: List.length(loops) == List.length(links) + 1
[@deriving (show({with_path: false}), sexp, yojson)]
type t('loop, 'link) = (list('loop), list('link));

let mk = (loops: list('loop), links: list('link)): t('loop, 'link) => {
  assert(List.length(loops) == List.length(links) + 1);
  (loops, links);
};

let of_loop = (lp: 'loop): t('loop, _) => ([lp], []);

let loops: t('loop, _) => list('loop) = fst;
let links: t(_, 'link) => list('link) = snd;

let split_fst =
    ((loops, links): t('loop, 'link))
    : ('loop, (list('link), list('loop))) => {
  assert(loops != []);
  (List.hd(loops), (links, List.tl(loops)));
};
let fst = (c: t('loop, _)): 'loop => fst(split_fst(c));
let map_fst = (f: 'loop => 'loop, c: t('loop, 'link)): t('loop, 'link) => {
  let (a, (links, loops)) = split_fst(c);
  ([f(a), ...loops], links);
};
let put_fst = lp => map_fst(_ => lp);

let split_lst = ((lps, lks)) => {
  assert(lps != []);
  let (lps, a) = ListUtil.split_last(lps);
  ((lps, lks), a);
};
let lst = ((lps, _): t('loop, _)): 'loop => {
  assert(lps != []);
  ListUtil.last(lps);
};
let map_lst = (f: 'loop => 'loop, c: t('loop, 'link)): t('loop, 'link) => {
  let ((lps, lks), a) = split_lst(c);
  (lps @ [f(a)], lks);
};
let put_lst = lp => map_lst(_ => lp);

let rev = (rev_a, rev_b, (lps, lks): t('loop, 'link)): t('loop, 'link) => (
  List.rev_map(rev_a, lps),
  List.rev_map(rev_b, lks),
);

let link =
    (a: 'loop, b: 'link, (loops, links): t('loop, 'link)): t('loop, 'link) => (
  [a, ...loops],
  [b, ...links],
);
let unlink =
    ((loops, links): t('loop, 'link))
    : option(('loop, 'link, t('loop, 'link))) =>
  switch (links) {
  | [] => None
  | [b, ...links] => Some((List.hd(loops), b, (List.tl(loops), links)))
  };

let knil =
    ((loops, links): t('loop, 'link), b: 'link, a: 'loop): t('loop, 'link) => (
  loops @ [a],
  links @ [b],
);
let unknil =
    ((loops, links): t('loop, 'link))
    : option((t('loop, 'link), 'link, 'loop)) =>
  ListUtil.split_last_opt(links)
  |> Option.map(((links, b)) => {
       let (loops, a) = ListUtil.split_last(loops);
       ((loops, links), b, a);
     });

// let rec aba_triples = (c: t('loop, 'link)): list(('loop, 'link, 'loop)) =>
//   switch (c) {
//   | ([a1, a2, ...loops], [b, ...links]) => [
//       (a1, b, a2),
//       ...aba_triples(([a2, ...loops], links)),
//     ]
//   | _ => []
//   };

// let map_a = (f_loop: 'loop => 'c, (loops, links): t('loop, 'link)): t('c, 'link) => (
//   List.map(f_loop, loops),
//   links,
// );
// let map_b = (f_b: 'link => 'c, (loops, links): t('loop, 'link)): t('loop, 'c) => (
//   loops,
//   List.map(f_b, links),
// );
// let map = (f_loop, f_link, c) => c |> List.map(f_loop) |> List.map(f_link);

let to_list =
    (f_loop: 'loop => 'x, f_link: 'link => 'x, c: t('loop, 'link)): list('x) => {
  let (lps, lp) = ListUtil.split_last(loops(c));
  let lks = links(c);
  List.fold_right2(
    (lp, lk, xs) => [f_loop(lp), f_link(lk), ...xs],
    lps,
    lks,
    [f_loop(lp)],
  );
};

let fold_left =
    (
      f_loop: 'loop => 'acc,
      f_link: ('acc, 'link, 'loop) => 'acc,
      (loops, links): t('loop, 'link),
    )
    : 'acc => {
  let (a, loops) = ListUtil.split_first(loops);
  List.fold_left2(f_link, f_loop(a), links, loops);
};
let fold_left_map =
    (
      f_loop: 'loop1 => ('acc, 'loop2),
      f_link: ('acc, 'link1, 'loop1) => ('acc, 'link2, 'loop2),
      c: t('loop1, 'link1),
    )
    : ('acc, t('loop2, 'link2)) =>
  c
  |> fold_left(
       lp1 => {
         let (acc, lp2) = f_loop(lp1);
         (acc, of_loop(lp2));
       },
       ((acc, mapped), lk1, lp1) => {
         let (acc, lk2, lp2) = f_link(acc, lk1, lp1);
         (acc, knil(mapped, lk2, lp2));
       },
     );

let fold_right =
    (
      f_link: ('loop, 'link, 'acc) => 'acc,
      f_loop: 'loop => 'acc,
      (lps, lks): t('loop, 'link),
    ) => {
  let (lps, lp) = ListUtil.split_last(lps);
  List.fold_right2(f_link, lps, lks, f_loop(lp));
};

let cat =
    (cat: ('loop, 'loop) => 'loop, l: t('loop, 'link), r: t('loop, 'link))
    : t('loop, 'link) =>
  l |> fold_right(link, lp => map_fst(cat(lp), r));

let append =
    (l: t('loop, 'link), lk: 'link, r: t('loop, 'link)): t('loop, 'link) =>
  l |> fold_right(link, lp => link(lp, lk, r));