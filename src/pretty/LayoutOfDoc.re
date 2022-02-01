/*

 //  hazelgrove/hazel@layout-into-core

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

type result3 =
  ( int /*size (+1?)*/,
    Array.t(int) /*pos*/,
    Array.t(int) /*cost*/,
    Array.t(Layout.t(unit)),
  );

module Js = Js_of_ocaml.Js;
type memo;

let create_memo: unit => memo =
  Js.Unsafe.js_expr("function create_memo_imp(mem) { return [] }");
let flush_memo: (memo) => unit =
  Js.Unsafe.js_expr(
    "function flush_memo_imp(mem) {
      mem.length = 0;
    }");
let get_memo: (memo, /*key:*/ int) => result3 = // This function is slow (~10% of the runtime)
  Js.Unsafe.js_expr(
    "function get_memo_imp(mem, key) {
      var x = mem[key];
      if (x === undefined) {
        return [0, 1, [0], [0], [0]];
      } else {
        return x;
      }
    }");
let set_memo: (memo, /*key:*/ int, /*value:*/ result3) => unit = // This function is very slow (2x of the runtime)
  Js.Unsafe.js_expr(
    "function set_memo_imp(mem, key, value) {
      mem[key] = value;
    }");

type doc3 =
  | Text3(ref(int), ref(result3), memo, string)
  | Fail3
  | Linebreak3(ref(int), ref(result3), memo)
  | Cat3(ref(int), ref(result3), memo, doc3, doc3)
  | Align3(ref(int), ref(result3), memo, doc3)
  | Annot3(ref(int), ref(result3), memo, int, doc3)
  | Choice3(ref(int), ref(result3), memo, doc3, doc3);

let mk_memo = (): ref(int) => ref(0);
let mk_result = (): ref(result3) => Obj.magic(ref(0));
let mk_text = (s: string): doc3 => Text3(mk_memo(), mk_result(), create_memo(), s);
let mk_fail = (): doc3 => Fail3;
let mk_linebreak = (): doc3 => Linebreak3(mk_memo(), mk_result(), create_memo());
let mk_cat = (d1: doc3, d2: doc3): doc3 => Cat3(mk_memo(), mk_result(), create_memo(), d1, d2);
let mk_align = (d: doc3): doc3 => Align3(mk_memo(), mk_result(), create_memo(), d);
let mk_annot = (a: int, d: doc3): doc3 => Annot3(mk_memo(), mk_result(), create_memo(), a, d);
let mk_choice = (d1: doc3, d2: doc3): doc3 => Choice3(mk_memo(), mk_result(), create_memo(), d1, d2);

module EqHash = {
  type t = Doc.t(unit);
  let equal = (===);
  let hash = Hashtbl.hash;
};

module EqHashtbl = Hashtbl.Make(EqHash);

let rec doc_new_of_old = (old: Doc.t('a)): doc3 => {
  let seen: EqHashtbl.t(doc3) = EqHashtbl.create(0);
  let rec go = (old: Doc.t(unit)): doc3 => {
    switch (EqHashtbl.find_opt(seen, old)) {
    | Some(new_doc) => new_doc
    | None =>
      let new_doc =
        switch (old.doc) {
        | Text(s) => mk_text(s)
        | Fail(_) => mk_fail() // TODO: handle fail argument
        | Cat(old1, old2) => mk_cat(go(old1), go(old2))
        | Linebreak(_) => mk_linebreak()
        | Align(old) => mk_align(go(old))
        | Annot(_annot, old) => mk_annot(99, go(old)) // TODO: handle annotation
        | Choice(old1, old2) => mk_choice(go(old1), go(old2))
        };
      EqHashtbl.add(seen, old, new_doc);
      new_doc;
    };
  };
  go(Obj.magic(old));
};

let count = ref(0);
let mem_count = ref(0);
let linebreak_cost =
  PosMap.singleton(0, (Cost.mk_height(1), Layout.Linebreak));

let rec make_fib = (x: int): doc3 =>
  if (x == 0) {
    mk_linebreak();
  } else if (x == 1) {
    mk_text("abc");
  } else {
    switch (x mod 4) {
    | 0 => mk_annot(x, make_fib(x - 1))
    | 1 => mk_align(make_fib(x - 1))
    | 2 => mk_cat(make_fib(x - 1), make_fib(x - 2)) // must be 2, so that linebreak can happen
    | 3 => mk_choice(make_fib(x - 1), make_fib(x - 2))
    | _ => failwith(__LOC__)
    };
  };
// let doc3_25 = make_fib(40);
let doc3_25 = make_fib(26);

// let rec make_fib_orig = (x: int): Doc.t(int) =>
//   if (x < 2) {
//     Doc.text("a");
//   } else {
//     switch (x mod 4) {
//     | 0 => Doc.hcat(make_fib_orig(x - 1), make_fib_orig(x - 2))
//     | 1 => Doc.align(make_fib_orig(x - 1))
//     | 2 => Doc.annot(x, make_fib_orig(x - 1))
//     | 3 => Doc.choice(make_fib_orig(x - 1), make_fib_orig(x - 2))
//     | _ => failwith(__LOC__)
//     };
//   };
// let fib_orig_rec_25 = make_fib_orig(40);

let gensym: ref(int) = ref(0);

let merge:
  (int, Array.t(int), Array.t(int), int, Array.t(int), Array.t(int)) =>
  (int, Array.t(int), Array.t(int)) =
  Js.Unsafe.js_expr(
    "function merge(js_size1, pos1, res1, js_size2, pos2, res2) {
    //var len = input.length;
    var pre_js_size = js_size1 + js_size2 | 0;
    var js_size = pre_js_size - 1 | 0;
    var pos = new Array(js_size);
    pos[0] = [0];
    var res = new Array(js_size);
    res[0] = [0];
    var i1 = 1;
    var i2 = 1;
    var i = 1;
    while (i1 < js_size1 && i2 < js_size2) {
      if (pos1[i1] < pos2[i2]) {
        //console.log(\"1\");
        pos[i] = pos1[i1];
        res[i] = res1[i1] + 1 | 0;
        i1 = i1 + 1 | 0;
      } else if (pos1[i1] > pos2[i2]) {
        //console.log(\"2\");
        pos[i] = pos2[i2];
        res[i] = res2[i2] + 1 | 0;
        i2 = i2 + 1 | 0;
      } else {
        //console.log(\"3\");
        // TODO: res1[i1] <=> res2[i2]
        pos[i] = pos1[i1];
        res[i] = res1[i1] + 1 | 0;
        i1 = i1 + 1 | 0;
        i2 = i2 + 1 | 0;
      }
      i = i + 1 | 0;
    }
    while (i1 < js_size1) {
        //pos[i] = pos1[i1];
        res[i] = res1[i1] + 1 | 0;
        i1 = i1 + 1 | 0;
        i = i + 1 | 0;
    }
    while (i2 < js_size2) {
        //pos[i] = pos2[i2];
        res[i] = res2[i2] + 1 | 0;
        i2 = i2 + 1 | 0;
        i = i + 1 | 0;
    }
    var size = i;
    return [0, size, pos, res];
  }
    ",
  );

// * Discovery:
//
//       { let a = 0; [|a + 0, a + 1, a + 2, a + 3, a + 4, a + 5, a + 6, a + 7, a + 8, a + 9 |] }
//
//   is faster than
//
//       [|0, 1, 2, 3, 4, 5, 6, 7, 8, 9 |]
//
//   Cause:  js_of_ocaml translates the second code into a global variable and a
//   reference to it. js_of_ocaml also likes to put .slice() on references to
//   array (global?) variables. This can make it slower than just locally
//   computing the result.

///////////////////////////////

// Possible interface types:
//
// mylist
// List.t((int, int))
// Array.t((int, int))
// Array.t(int) (interleaved)
// (Array.t(int), Array.t(int)) (with or without passing both)
// (int, Array.t(int))

// with or without .length (separate param or element of array)
// use size+1 (b/c less aritmetic)
// prealloc the array or not

// TODO: parallel arrays due to heterogenious nature
// TODO: parallel arrays allow reuse of "pos" array


//////////

// Parallel with external size

// 29ns
// 34-36ns (complete layout_map)

// Omits pos argument
let layout_map_align:
  (int, Array.t(Layout.t(unit))) => Array.t(Layout.t(unit)) =
  Js.Unsafe.js_expr(
    "function layout_map_align(size, input) {
      var output = new Array(size);
      output[0] = 0;
      for (var i =  1; i < size; i++) {
        var res = input[i]
        output[i] = Align_share(res);
      }
      return output;
      \"layout_map_align\";
    }
    ",
  );

let layout_map_annot:
  (unit, int, Array.t(Layout.t(unit))) => Array.t(Layout.t(unit)) =
  Js.Unsafe.js_expr(
    "function layout_map_annot(annot, size, input) {
      var output = new Array(size);
      output[0] = 0;
      for (var i =  1; i < size; i++) {
        var res = input[i]
        output[i] = Annot_share(annot, res); // TODO: apply wrapper top
      }
      return output;
      \"layout_map_annot\";
    }
    ",
  );

let layout_merge:
  (
    int,
    Array.t(int),
    Array.t(int),
    Array.t(Layout.t(unit)),
    int,
    Array.t(int),
    Array.t(int),
    Array.t(Layout.t(unit))
  ) =>
  (int, Array.t(int), Array.t(int), Array.t(Layout.t(unit))) =
  Js.Unsafe.js_expr(
    "function layout_merge_imp(size1, pos1, cost1, res1, size2, pos2, cost2, res2) {
      var js_size = size1;
      var end = size1 + size2 | 0;
      end = end - 1 | 0;
      var pos = new Array(end);
      pos[0] = 0;
      var cost = new Array(end);
      cost[0] = 0;
      var res = new Array(end);
      res[0] = 0;
      var i = 1;
      var i1 = 1;
      var i2 = 1;
      while (i1 < size1 && i2 < size2) {
        if (pos1[i1] < pos2[i2]) {
          pos[i] = pos1[i1];
          res[i] = res1[i1];
          cost[i] = cost1[i1];
          i1 = i1 + 1 | 0;
        } else if (pos1[i1] > pos2[i2]) {
          pos[i] = pos2[i2];
          res[i] = res2[i2];
          cost[i] = cost2[i2];
          i2 = i2 + 1 | 0;
        } else {
          // TODO: res1[i1] <=> res2[i2]
          // Note: `<=` makes choice be left biased
          if (cost1[i1] <= cost2[i2]) {
            pos[i] = pos1[i1];
            cost[i] = cost1[i1];
            res[i] = res1[i1];
          } else {
            pos[i] = pos2[i2];
            cost[i] = cost2[i2];
            res[i] = res2[i2];
          }
          i1 = i1 + 1 | 0;
          i2 = i2 + 1 | 0;
        }
        i = i + 1 | 0;
      }
      while (i1 < size1) {
        pos[i] = pos1[i1];
        res[i] = res1[i1];
        cost[i] = cost1[i1];
        i++;
        i1++; // TODO: |0
      }
      while (i2 < size2) {
        pos[i] = pos2[i2];
        res[i] = res2[i2];
        cost[i] = cost2[i2];
        i++;
        i2++;
      }
      return [0, Math.min(i, 11), pos, cost, res];
    }
    ",
  );

let layout_fold:
  (
    int,
    int,
    int,
    doc3,
    int,
    Array.t(int),
    Array.t(int),
    Array.t(Layout.t(unit))
  ) =>
  (int, Array.t(int), Array.t(int), Array.t(Layout.t(unit))) =
  Js.Unsafe.js_expr(
    "function layout_fold_imp(benchmark, width, pos, f2, size1, pos1, cost1, res1) {
      if (size1 == 1) { return [0, 1, [0], [0], [0]]; }
      var xxx = fib3_share(benchmark, width, pos1[1], f2); // TODO: add cost1[i] to each of xxx
      {
        var xxx_len = xxx[1];
        var xxx_cost = xxx[3];
        var xxx_res = xxx[4];
        for (var j = 1; j < xxx_len; j++) {
          xxx_cost[j] = xxx_cost[j] + cost1[j] | 0;
          xxx_res[j] = Cat_share(res1[j], xxx_res[j]);
        }
      }
      var i = 2;
      while (i < size1) {
        var p = pos1[i];
        var yyy = fib3_share(benchmark, width, p, f2);
        var yyy_len = yyy[1];
        var yyy_cost = yyy[3];
        var yyy_res = yyy[4];
        for (var j = 1; j < yyy_len; j++) {
          yyy_cost[j] = yyy_cost[j] + cost1[j] | 0;
          yyy_res[j] = Cat_share(res1[j], yyy_res[j]);
        }
        xxx = layout_merge_share(xxx[1], xxx[2], xxx[3], xxx[4], yyy[1], yyy[2], yyy[3], yyy[4]);
        i = i + 1 | 0;
      }
      return xxx;
    }
    ",
  );

let rec fib3 =
        (~benchmark: bool, ~width: int, ~pos: int, x: doc3)
        : result3 => {
  count := count^ + 1;
  switch (x) {
  | Text3(mem, result, memo, text) =>
    let old_mem = mem^;
    // TODO: optimize the memo here?
    if (old_mem != gensym^) { flush_memo(memo); }
    let memo_key = pos * 80 + width;
    let (memo_s, memo_p, memo_c, memo_r) = get_memo(memo, memo_key);
    if (memo_s != 1) {
      (memo_s, memo_p, memo_c, memo_r)
    } else {
      mem := gensym^;
      // TODO: should we cache the string length in Text3?
      let new_pos = pos + String.length(text);
      let r =
        if (!benchmark) {
          if (new_pos > width) {
            (1, [||], [||], [||])
          } else {
            (2, [|new_pos|], [|0|], [|Layout.Text(text)|])
          }
        } else {
          (
            11,
            [|
              new_pos + 0,
              new_pos + 1,
              new_pos + 2,
              new_pos + 3,
              new_pos + 4,
              new_pos + 5,
              new_pos + 6,
              new_pos + 7,
              new_pos + 8,
              new_pos + 9,
            |],
            [|0, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
            [|
              Layout.Text(text),
              Layout.Text(text),
              Layout.Text(text),
              Layout.Text(text),
              Layout.Text(text),
              Layout.Text(text),
              Layout.Text(text),
              Layout.Text(text),
              Layout.Text(text),
              Layout.Text(text),
            |],
          )
        };
      result := r;
      set_memo(memo, memo_key, r);
      r
    };
  | Fail3 =>
    // We can return without memoization only because there are no pointer equality concerns
    (1, [||], [||], [||])
  | Linebreak3(mem, result, memo) =>
    // TODO: optimize the memo here by always returning (2, [|0|], [|1|], [|Layout.Linebreak|])
    let old_mem = mem^;
    if (old_mem != gensym^) { flush_memo(memo); }
    let memo_key = pos * 80 + width;
    let (memo_s, memo_p, memo_c, memo_r) = get_memo(memo, memo_key);
    if (memo_s != 1) {
      (memo_s, memo_p, memo_c, memo_r)
    } else {
      mem := gensym^;
      let r = 
        if (!benchmark) {
          (2, [|0|], [|1|], [|Layout.Linebreak|])
        } else {
          (
            11,
            [|
              0,
              1,
              2,
              3,
              4,
              5,
              6,
              7,
              8,
              9,
            |],
            [|1, 1, 1, 1, 1, 1, 1, 1, 1, 1|],
            [|
              Layout.Linebreak,
              Layout.Linebreak,
              Layout.Linebreak,
              Layout.Linebreak,
              Layout.Linebreak,
              Layout.Linebreak,
              Layout.Linebreak,
              Layout.Linebreak,
              Layout.Linebreak,
              Layout.Linebreak,
            |],
          )
        };
      result := r;
      set_memo(memo, memo_key, r);
      r
    };
  | Align3(mem, result, memo, f) =>
    let old_mem = mem^;
    if (old_mem != gensym^) { flush_memo(memo); }
    let memo_key = pos * 80 + width;
    let (memo_s, memo_p, memo_c, memo_r) = get_memo(memo, memo_key);
    if (memo_s != 1) {
      (memo_s, memo_p, memo_c, memo_r)
    } else {
      let (out1s, out1p, out1c, out1r) = fib3(~benchmark, ~width=width - pos, ~pos=0, f);
      let out =
        Js.Unsafe.fun_call(
          layout_map_align,
          [|Js.Unsafe.inject(out1s), Js.Unsafe.inject(out1r)|],
        );
      flush_memo(memo);
      mem := gensym^;
      let r = (out1s, out1p, out1c, out);
      result := r;
      set_memo(memo, memo_key, r);
      r
    };
  | Annot3(mem, result, memo, annot, f) =>
    let old_mem = mem^;
    if (old_mem != gensym^) { flush_memo(memo); }
    let memo_key = pos * 80 + width;
    let (memo_s, memo_p, memo_c, memo_r) = get_memo(memo, memo_key);
    if (memo_s != 1) {
      (memo_s, memo_p, memo_c, memo_r)
    } else {
      let (out1s, out1p, out1c, out1r) = fib3(~benchmark, ~width, ~pos, f);
      let out =
        Js.Unsafe.fun_call(
          layout_map_annot,
          [|
            Js.Unsafe.inject(annot),
            Js.Unsafe.inject(out1s),
            Js.Unsafe.inject(out1r),
          |],
        );
      flush_memo(memo);
      mem := gensym^;
      let r = (out1s, out1p, out1c, out);
      result := r;
      set_memo(memo, memo_key, r);
      r
    };
  | Cat3(mem, result, memo, f1, f2) =>
    // TODO: maybe without memoization?
    let old_mem = mem^;
    if (old_mem != gensym^) { flush_memo(memo); }
    let memo_key = pos * 80 + width;
    let (memo_s, memo_p, memo_c, memo_r) = get_memo(memo, memo_key);
    if (memo_s != 1) {
      (memo_s, memo_p, memo_c, memo_r)
    } else {
      let (out1s, out1p, out1c, out1r) = fib3(~benchmark, ~width, ~pos, f1);
      let (out_s, out_p, out_c, out_r) =
        Js.Unsafe.fun_call(
          layout_fold,
          [|
            Js.Unsafe.inject(benchmark),
            Js.Unsafe.inject(width),
            Js.Unsafe.inject(pos),
            Js.Unsafe.inject(f2),
            Js.Unsafe.inject(out1s),
            Js.Unsafe.inject(out1p),
            Js.Unsafe.inject(out1c),
            Js.Unsafe.inject(out1r),
          |],
        );
      mem := gensym^;
      let r = (out_s, out_p, out_c, out_r);
      result := r;
      set_memo(memo, memo_key, r);
      r
    };
  | Choice3(mem, result, memo, f1, f2) =>
    let old_mem = mem^;
    if (old_mem != gensym^) { flush_memo(memo); }
    let memo_key = pos * 80 + width;
    let (memo_s, memo_p, memo_c, memo_r) = get_memo(memo, memo_key);
    if (memo_s != 1) {
      (memo_s, memo_p, memo_c, memo_r)
    } else {
      let (out1s, out1p, out1c, out1r) = fib3(~benchmark, ~width, ~pos, f1);
      let (out2s, out2p, out2c, out2r) = fib3(~benchmark, ~width, ~pos, f2);
      let (out_s, out_p, out_c, out_r) =
        Js.Unsafe.fun_call(
          layout_merge,
          [|
            Js.Unsafe.inject(out1s),
            Js.Unsafe.inject(out1p),
            Js.Unsafe.inject(out1c),
            Js.Unsafe.inject(out1r),
            Js.Unsafe.inject(out2s),
            Js.Unsafe.inject(out2p),
            Js.Unsafe.inject(out2c),
            Js.Unsafe.inject(out2r),
          |],
        );
      mem := gensym^;
      let r = (out_s, out_p, out_c, out_r);
      result := r;
      set_memo(memo, memo_key, r);
      r
    };
  };
};

let _ = Js.export("Cat_share", (x, y) => Layout.Cat(x, y));
let _ = Js.export("Align_share", x => Layout.Align(x));
let _ = Js.export("Annot_share", (x, y) => Layout.Annot(x, y));
let _ = Js.export("fib3_share", fib3);
let _ = Js.export("layout_merge_share", layout_merge);

let rec take = (n: int, lst: list('a)): list('a) => {
  switch (n, lst) {
  | (0, _) => []
  | (_, []) => []
  | (n, [x, ...xs]) => [x, ...take(n - 1, xs)]
  };
};

let rec layout_of_doc' = (doc: Doc.t(unit)): Doc.m(Layout.t(unit)) => {
  let g = (~width: int, ~pos: int): Doc.m'(Layout.t(unit)) => {
    // TODO: lift the switch(doc.doc) outside the lambda
    count := count^ + 1;
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
      let r = (cost, Layout.Text(string));
      [
        (0 + pos', r),
        (1 + pos', r),
        //(2 + pos', r),
      ];
    //PosMap.singleton(pos', (cost, Layout.Text(string)));
    | Cat(d1, d2) =>
      let l1 = take(1, layout_of_doc'(d1, ~width, ~pos));
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
      //Doc.M.add(doc.mem, key, value);
      take(2, value);
    };
  };
  h;
};

let layout_of_doc_25 = (~width: int, ~pos: int): option(Layout.t('annot)) => {
  gensym := gensym^ + 1;
  ignore(fib3(doc3_25, ~benchmark=true, ~width, ~pos));
  None;
};

let new_layout_of_doc =
    (doc: doc3, ~width: int, ~pos: int): option(Layout.t('annot)) => {
  gensym := gensym^ + 1;
  let (layout_s, layout_p, layout_c, layout_r) = fib3(doc, ~benchmark=false, ~width, ~pos);
  let pos = ref(max_int);
  let cost = ref(max_int);
  let res = ref(None);
  for (i in 0 to layout_s - 2) { // remember that _s is 1 greater than the ocaml length (b/c _s is the javascript length)
    if (layout_c[i] < cost^ || layout_c[i] == cost^ && layout_p[i] < pos^) {
      pos := layout_p[i];
      cost := layout_c[i];
      res := Some(layout_r[i]);
    }
  };

  res^;
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
  let l =
    minimum(
      (max_int, (Cost.inf, None)),
      Obj.magic(layout_of_doc'(Obj.magic(doc), ~width, ~pos)),
    );
  l;
};
