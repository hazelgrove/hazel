/*

 TODO: check memoization
   TODO: is fix_holes doing deep changes
   TODO: print doc diff from previous memoization

 TODO: width intervals
 TODO: wrong-but-fast implementation
 TODO: count number of docs
 TODO: strings are fixed

 TODO: cursor map generation

 */

// TODO: compute actual layout size and use instead of t_of_layout
let rec all: 'annot. Doc.t('annot) => list(Layout.t('annot)) = {
  doc => {
    switch (doc.doc) {
    | Text(string) => [Layout.Text(string)]
    | Cat(d1, d2) =>
      let ls1 = all(d1);
      let ls2 = all(d2);
      List.concat(
        List.map(l1 => List.map(l2 => Layout.Cat(l1, l2), ls2), ls1),
      );
    | Linebreak(_) => [Layout.Linebreak]
    | Align(d) => List.map(l => Layout.Align(l), all(d))
    | Annot(annot, d) => List.map(l => Layout.Annot(annot, l), all(d))
    | Fail(_) => []
    | Choice(d1, d2) => all(d1) @ all(d2)
    };
  };
};

// Note: This union is left biased
let m'_union: 'a. (Doc.m'('a), Doc.m'('a)) => Doc.m'('a) =
  (p1, p2) => {
    let cost_union = ((cost1: Cost.t, _) as t1, (cost2: Cost.t, _) as t2) =>
      if (Cost.leq(cost1, cost2)) {
        t1;
      } else {
        t2;
      };
    PosMap.union(cost_union, p1, p2);
  };

// Random BlockChain
// Constant indent

// All times are for qsort_30 with `make release`
// Original code: 156ms (Count of g: 204607)
// Noop: 0ms
// Alt does just right: 27ms (Count of g: 43897)
// Skip Annot and Align: 24ms (Count of g: 43897)
// cost = Cost.zero: 22ms (Count of g: 43897)
// let pos' = pos + 5: 25ms (Count of g: 43897)
// Omit "Doc.M.add(doc.mem, key, value)": 15ms (Count of g: 45067)
// Omit memo lookup: 7ms (Count of g: 45157)
// No h: 5ms (Count of g: 45157)
// One-function/no-curry: 5.4ms (Count of g: 45157)
// No work in Cat: 2.0ms (Count of g: 45157)
// No pos' in Text: 3.4ms (Count of g: 45157) [undid]
// Constant return value: 1.7ms
// No count++: 0.9ms
// No constant constructors: 0.9ms

// With count++ and both Alt: avg:   2.0ms   per count:  19.8ns (count: 98647)
// Without count++: avg:   2.0ms   per count:  19.8ns (count: 98647)

// fib(25):         avg:   0.8ms   per count:   3.2ns (count: 242785)
// fib(Int1(25)):   avg:   2.0ms   per count:   8.3ns (count: 242785)
// fib(fib_rec_25): avg:   1.2ms   per count:   5.1ns (count: 242785)
// fib with 4 rec:  avg:   1.5ms   per count:   6.3ns (count: 242785)
// fib returning 1: avg:   1.3ms   (count: 242785)
// with count++     avg:   1.5ms   per count:   6.1ns (count: 242785)

// fib-doc(50):     avg:   2.0ms   per count:   9.7ns (count: 206668)
// fib2             avg:   2.2ms   per count:  10.7ns (count: 206668)
// pass width and pos (order doesn't matter)
//                  avg:   2.1ms   per count:  10.3ns (count: 206668)
// make release     avg:   1.5ms   per count:   7.0ns (count: 206668)
// record wrapping  avg:   3.5ms   per count:  16.7ns (count: 206668)
// extra field      avg:   1.6ms   per count:   7.9ns (count: 206668)
//   --record in ctor   avg:   1.8ms   per count:   8.5ns (count: 206668)
//   --pre let fields   avg:   1.7ms   per count:   8.2ns (count: 206668)
//   --re.mem++         avg:   2.0ms   per count:   9.6ns (count: 206668)
// reset            avg:   1.6ms   per count:   7.6ns (count: 206668)
// thread pos       avg:   2.5ms   per count:  12.0ns (count: 206668)
// mem++            avg:   4.0ms   per count:  19.6ns (count: 206668)
// mem if (false)   avg:   4.2ms   per count:  20.3ns (count: 206668)
// if mem < 0       avg:   4.5ms   per count:  22.0ns (count: 206668)
// if mem == gensym^avg:   4.6ms   per count:  22.5ns (count: 206668)
// mem := gensym^   avg:   4.6ms   per count:  22.5ns (count: 206668)
// mem := gg        same


// JS: constant lifted x[0] out of loop

// all 157ms:
// y = 0; z = 1; start = Date.now(); for (i=0; i<1000*1000*10*10; i++) { y=y+1 }; Date.now() - start
// y = 0; x = {tag:1}; start = Date.now(); for (i=0; i<1000*1000*10*10; i++) { y+=x.tag }; Date.now() - start
// y = 0; x = [1]; start = Date.now(); for (i=0; i<1000*1000*10*10; i++) { y+=x[0] }; Date.now() - start
// y = 0; x = [[1]]; start = Date.now(); for (i=0; i<1000*1000*10*10; i++) { y+=x[0][0] }; Date.now() - start

// 780ms
// y = 0; x = 1; start = Date.now(); for (i=0; i<1000*1000*10*10; i++) { y=(y+x)/2; x++ }; Date.now() - start
// y = 0; x = [1]; start = Date.now(); for (i=0; i<1000*1000*10*10; i++) { y=(y+x[0])/2; x[0]++ }; Date.now() - start
// y = 0; x = {tag:1}; start = Date.now(); for (i=0; i<1000*1000*10*10; i++) { y=(y+x.tag)/2; x.tag++ }; Date.now() - start

type my_fib =
  | Text2(ref(int), int)
  | Fail2(ref(int))
  | Cat2(ref(int), my_fib, my_fib)
  | Align2(ref(int), my_fib)
  | Annot2(ref(int), int, my_fib)
  | Choice2(ref(int), my_fib, my_fib);
// let rec fib_of_doc = (doc: Doc.t('annot)): my_fib => {
//   switch (doc.doc) {
//   | Text(string) => Text2(0, string)
//   | Cat(t1, t2) => Cat2(0, t2_of_doc(t1), t2_of_doc(t2))
//   | Linebreak(int) => Linebreak2(0, int)
//   | Align(t2) => Align2(0, t2_of_doc(t2))
//   | Annot(int, t2) => Annot2(0, int, t2_of_doc(t2))
//   | Fail(int) => Fail2(0, int)
//   | Choice(t1, t2) => Choice2(0, t2_of_doc(t1), t2_of_doc(t2))
//   };
// };

let count = ref(0);
let linebreak_cost =
  PosMap.singleton(0, (Cost.mk_height(1), Layout.Linebreak));
type my_int =
  | Int1(int)
  | Int2(int)
  | Int3(int);
let rec fib = (x: my_int): int => {
  switch (x) {
  | Int1(x) =>
    if (x < 2) {
      1;
    } else {
      1 + fib(Int2(x - 1)) + fib(Int2(x - 2));
    }
  | Int2(x) =>
    if (x < 2) {
      1;
    } else {
      1 + fib(Int3(x - 1)) + fib(Int3(x - 2));
    }
  | Int3(x) =>
    if (x < 2) {
      1;
    } else {
      1 + fib(Int1(x - 1)) + fib(Int1(x - 2));
    }
  };
};
let rec make_fib = (x: int): my_fib =>
  if (x < 2) {
    Text2(ref(0), 1);
  } else {
    switch (x mod 4) {
    | 0 => Cat2(ref(0), make_fib(x - 1), make_fib(x - 2))
    | 1 => Align2(ref(0), make_fib(x - 1))
    | 2 => Annot2(ref(0), x, make_fib(x - 1))
    | 3 => Choice2(ref(0), make_fib(x - 1), make_fib(x - 2))
    | _ => failwith(__LOC__)
    };
  };
let fib_rec_25 = make_fib(40);

// let rec fib2 = (x: my_fib): int => {
//   count := count^ + 1;
//   switch (x) {
//   | Text2(_i) => 1
//   | Cat2(f1, f2) =>
//     let _ = fib2(f1);
//     let _ = fib2(f2);
//     1;
//   | Align2(f1) =>
//     let _ = fib2(f1);
//     1;
//   | Annot2(_, f1) =>
//     let _ = fib2(f1);
//     1;
//   | Choice2(f1, f2) =>
//     let _ = fib2(f1);
//     let _ = fib2(f2);
//     1;
//   };
// };

/*

 function fib2(x, width, pos) {
     count[1] = count[1] + 1 | 0;
     caml_call1(Stdlib_printf[2], _t_);
     switch (x[0]) {
     case 0:
         return 1;
     case 1:
         var f2 = x[2]
           , f1 = x[1];
         fib2(f1, width, pos);
         fib2(f2, width, pos);
         return 1;
     case 2:
         var f1$0 = x[1];
         fib2(f1$0, width, pos);
         return 1;
     case 3:
         var f1$1 = x[2];
         fib2(f1$1, width, pos);
         return 1;
     default:
         var f2$0 = x[2]
           , f1$2 = x[1];
         fib2(f1$2, width, pos);
         fib2(f2$0, width, pos);
         return 1
     }
 }
 */

let gensym: ref(int) = ref(0);

let rec fib2 = (~width: int, ~pos: int, x: my_fib): (int, int) => {
  count := count^ + 1;
  switch (x) {
  | Text2(mem, _i) =>
    let old_mem = mem^;
    if (old_mem == gensym^) {
      (0, 0);
    } else {
      let res = 1;
      mem := gensym^;
      (pos, res);
    };
  | Fail2(mem) =>
    let old_mem = mem^;
    if (old_mem == gensym^) {
      (0, 0);
    } else {
      let res = 1;
      mem := gensym^;
      (pos, res);
    };
  | Align2(mem, f) =>
    let old_mem = mem^;
    if (old_mem == gensym^) {
      (0, 0);
    } else {
      let (pos1, res1) = fib2(~width, ~pos, f);
      let res = res1 + 1;
      mem := gensym^;
      (pos1, res);
    };
  | Annot2(mem, _ann, f) =>
    let old_mem = mem^;
    if (old_mem == gensym^) {
      (0, 0);
    } else {
      let (pos1, res1) = fib2(~width, ~pos, f);
      let res = res1 + 1;
      mem := gensym^;
      (pos1, res);
    };
  | Cat2(mem, f1, f2) =>
    let old_mem = mem^;
    if (old_mem == gensym^) {
      (0, 0);
    } else {
      let (pos1, res1) = fib2(~width, ~pos, f1);
      let (pos2, res2) = fib2(~width, ~pos=pos1, f2);
      let res = res1 + res2 + 1;
      mem := gensym^;
      (pos2, res);
    };
  | Choice2(mem, f1, f2) =>
    let old_mem = mem^;
    if (old_mem == gensym^) {
      (0, 0);
    } else {
      let (pos1, res1) = fib2(~width, ~pos, f1);
      let (pos2, res2) = fib2(~width, ~pos=pos1, f2);
      let res = res1 + res2 + 1;
      mem := gensym^;
      (pos2, res);
    };
  };
};

// let rec fib = (x: int): int => {
//   if (x < 2) { 1 }
//   else { 1 + fib(x-1) + fib(x-2) }
// }

let rec fast_layout_of_doc' =
        (doc: Doc.t(unit), ~width: int, ~pos: int)
        : PosMap.t((Cost.t, Layout.t(unit))) => {
  //Printf.printf("fast_layout_of_doc'");
  count := count^ + 1;
  // TODO: lift the switch(doc.doc) outside the lambda
  switch (doc.doc) {
  | Text(_string) =>
    // TODO: cache text length in Text?
    //let pos' = pos + String.length(string); //Unicode.length(string);
    //let pos' = pos + 5;
    // let cost =
    //   if (pos' <= width) {
    //     Cost.zero;
    //   } else {
    //     let overflow = pos' - width;
    //     // overflow_cost = sum i from 1 to overflow
    //     let overflow_cost = overflow * (overflow + 1) / 2;
    //     Cost.mk_overflow(overflow_cost);
    //   };
    //let cost = Cost.zero;
    //PosMap.singleton(pos', (cost, Layout.Text(string)));
    linebreak_cost
  | Cat(d1, d2) =>
    let _l1 = fast_layout_of_doc'(d1, ~width, ~pos);
    let _l2 = fast_layout_of_doc'(d2, ~width, ~pos);
    // PosMap.fold_left(
    //   (pos, z, (cost1, layout1)) => {
    //     let l2 = fast_layout_of_doc'(d2, ~width, ~pos);
    //     let layouts =
    //       PosMap.map(
    //         ((cost2, layout2)) =>
    //           (Cost.add(cost1, cost2), Layout.Cat(layout1, layout2)),
    //         l2,
    //       );
    //     m'_union(z, layouts);
    //   },
    //   PosMap.empty,
    //   l1,
    // );
    linebreak_cost;
  | Linebreak(_) => linebreak_cost
  | Align(d) =>
    // let layout = fast_layout_of_doc'(d, ~width=width - pos, ~pos=0);
    // PosMap.mapk(
    //   (p, (c, l)) => (p + pos, (c, Layout.Align(l))),
    //   layout,
    // );
    let _ = fast_layout_of_doc'(d, ~width, ~pos);
    linebreak_cost;
  | Annot(_annot, d) =>
    let _layout = fast_layout_of_doc'(d, ~width, ~pos);
    //PosMap.map(((c, l)) => (c, Layout.Annot(annot, l)), layout);
    linebreak_cost;
  | Fail(_) => PosMap.empty
  | Choice(d1, d2) =>
    let _l1 = fast_layout_of_doc'(d1, ~width, ~pos);
    let _l2 = fast_layout_of_doc'(d2, ~width, ~pos);
    //m'_union(l1, l2);
    linebreak_cost;
  // let h = (~width: int, ~pos: int): PosMap.t((Cost.t, Layout.t(unit))) => {
  //   // let key = (width, pos);
  //   // switch (Doc.M.find_opt(doc.mem, key)) {
  //   // | Some(value) => value
  //   // | None =>
  //   //   let value = g(~width, ~pos);
  //   //   //Doc.M.add(doc.mem, key, value);
  //   //   value;
  //   // };
  //   g(~width, ~pos)
  // };
  // h;
  };
};

let rec layout_of_doc' = (doc: Doc.t(unit)): Doc.m(Layout.t(unit)) => {
  let g = (~width: int, ~pos: int): Doc.m'(Layout.t(unit)) => {
    // TODO: lift the switch(doc.doc) outside the lambda
    switch (doc.doc) {
    | Text(string) =>
      // TODO: cache text length in Text?
      let pos' = pos + String.length(string); //Unicode.length(string);
      let cost =
        if (pos' <= width) {
          Cost.zero;
        } else {
          let overflow = pos' - width;
          // overflow_cost = sum i from 1 to overflow
          let overflow_cost = overflow * (overflow + 1) / 2;
          Cost.mk_overflow(overflow_cost);
        };
      PosMap.singleton(pos', (cost, Layout.Text(string)));
    | Cat(d1, d2) =>
      let l1 = layout_of_doc'(d1, ~width, ~pos);
      PosMap.fold_left(
        (pos, z, (cost1, layout1)) => {
          let l2 = layout_of_doc'(d2, ~width, ~pos);
          let layouts =
            PosMap.map(
              ((cost2, layout2)) =>
                (Cost.add(cost1, cost2), Layout.Cat(layout1, layout2)),
              l2,
            );
          m'_union(z, layouts);
        },
        PosMap.empty,
        l1,
      );
    | Linebreak(_) =>
      PosMap.singleton(0, (Cost.mk_height(1), Layout.Linebreak))
    | Align(d) =>
      let layout = layout_of_doc'(d, ~width=width - pos, ~pos=0);
      PosMap.mapk(
        (p, (c, l)) => (p + pos, (c, Layout.Align(l))),
        layout,
      );
    | Annot(annot, d) =>
      let layout = layout_of_doc'(d, ~width, ~pos);
      PosMap.map(((c, l)) => (c, Layout.Annot(annot, l)), layout);
    | Fail(_) => PosMap.empty
    | Choice(d1, d2) =>
      let l1 = layout_of_doc'(d1, ~width, ~pos);
      let l2 = layout_of_doc'(d2, ~width, ~pos);
      m'_union(l1, l2);
    };
  };
  let h = (~width: int, ~pos: int): Doc.m'(Layout.t(unit)) => {
    let key = (width, pos);
    switch (Doc.M.find_opt(doc.mem, key)) {
    | Some(value) => value
    | None =>
      let value = g(~width, ~pos);
      Doc.M.add(doc.mem, key, value);
      value;
    };
  };
  h;
};

let fast_layout_of_doc =
    (_doc: Doc.t('annot), ~width: int, ~pos: int): option(Layout.t('annot)) => {
  //let _l: list((int, (Cost.t, Layout.t('annot)))) =
  //  Obj.magic(fast_layout_of_doc'(Obj.magic(doc), ~width, ~pos));
  //Some(snd(snd(List.hd(l))));
  //count := fib(Int1(25));
  gensym := gensym^ + 1;
  ignore(fib2(fib_rec_25, ~width, ~pos));
  None;
};

let layout_of_doc =
    (doc: Doc.t('annot), ~width: int, ~pos: int): option(Layout.t('annot)) => {
  let rec minimum =
          ((pos, (cost, t)): (int, (Cost.t, option('a))))
          : (list((int, (Cost.t, 'a))) => option('a)) => {
    fun
    | [] => t
    | [(x_pos, (x_cost, x)), ...rest] =>
      // Prefer lowest cost, or if same cost, prefer ending at an earlier column
      // (Columns are unique by construction of PosMap.)
      if (Cost.lt(x_cost, cost) || Cost.eq(x_cost, cost) && x_pos < pos) {
        minimum((x_pos, (x_cost, Some(x))), rest);
      } else {
        minimum((pos, (cost, t)), rest);
      };
  };
  // TODO: use options instead of max_int
  // let start_time = Sys.time();
  let l =
    minimum(
      (max_int, (Cost.inf, None)),
      Obj.magic(layout_of_doc'(Obj.magic(doc), ~width, ~pos)),
    );
  // let end_time = Sys.time();
  /*
   Printf.printf(
     "layout_of_doc: %d \t%f\n",
     -1, //fst(Lazy.force(memo_table))##.size,
     //Memoize.WeakPoly.Table.length(fst(Lazy.force(memo_table))),
     1000.0 *. (end_time -. start_time),
   );
   */
  l;
};
