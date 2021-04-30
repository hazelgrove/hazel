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
// return list(int,it) avg:   5.2ms   per count:  25.3ns (count: 206668)
//   --List.map (one child) avg:  15.6ms   per count:  75.7ns (count: 206668)
//   --List.map (one child, no closure alloc) avg:  13.3ms   per count:  64.5ns (count: 206668)
// res_inc'         avg:  12.1ms   per count:  58.4ns (count: 206668)
// local map        avg:  12.4ms   per count:  60.1ns (count: 206668)
// res_inc' = id    avg:   3.8ms   per count:  18.6ns (count: 206668)
// res_inc' = cons  avg:   4.9ms   per count:  23.6ns (count: 206668)
// rec res_inc'     avg:   6.3ms   per count:  30.6ns (count: 206668)

// 10x (list int, list int) avg:  60.3ms   per count: 291.9ns (count: 206668)
// 10x mylist               avg:  22.5ms   per count: 108.8ns (count: 206668)

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

let rec make_fib_orig = (x: int): Doc.t(int) =>
  if (x < 2) {
    Doc.text("a");
  } else {
    switch (x mod 4) {
    | 0 => Doc.hcat(make_fib_orig(x - 1), make_fib_orig(x - 2))
    | 1 => Doc.align(make_fib_orig(x - 1))
    | 2 => Doc.annot(x, make_fib_orig(x - 1))
    | 3 => Doc.choice(make_fib_orig(x - 1), make_fib_orig(x - 2))
    | _ => failwith(__LOC__)
    };
  };
let fib_orig_rec_25 = make_fib_orig(40);

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
type mylist =
  | Nil
  | Cons(int, int, mylist);

let gensym: ref(int) = ref(0);

// mylist  avg:   5.8ms   per count:  27.9ns (count: 206668)
// let rec res_inc' = input => {
//   switch (input) {
//   | Nil => Nil
//   | Cons(pos, res, rest) => Cons(pos, res + 1, res_inc'(rest))
//   };
// };

// let rec fib2 = (~width: int, ~pos: int, x: my_fib): mylist => {
//   count := count^ + 1;
//   switch (x) {
//   | Text2(mem, _i) =>
//     // TODO: without memoization
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       Cons(0, 0, Nil);
//     } else {
//       let res = 1;
//       mem := gensym^;
//       Cons(pos, res, Nil);
//     };
//   | Fail2(mem) =>
//     // TODO: fail without memoization
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       Cons(0, 0, Nil);
//     } else {
//       let res = 1;
//       mem := gensym^;
//       Cons(pos, res, Nil);
//     };
//   | Align2(mem, f) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       Cons(0, 0, Nil);
//     } else {
//       let out1 = fib2(~width, ~pos, f);
//       let out = res_inc'(out1);
//       mem := gensym^;
//       out;
//     };
//   | Annot2(mem, _ann, f) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       Cons(0, 0, Nil);
//     } else {
//       let out1 = fib2(~width, ~pos, f);
//       let out = res_inc'(out1);
//       mem := gensym^;
//       out;
//     };
//   | Cat2(mem, f1, f2) =>
//     // TODO: maybe without memoization?
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       Cons(0, 0, Nil);
//     } else {
//       let out1 = fib2(~width, ~pos, f1);
//       let _ = fib2(~width, ~pos, f2);
//       let out = res_inc'(out1);
//       mem := gensym^;
//       out;
//     };
//   | Choice2(mem, f1, f2) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       Cons(0, 0, Nil);
//     } else {
//       let out1 = fib2(~width, ~pos, f1);
//       let _ = fib2(~width, ~pos, f2);
//       // let out = res_inc'(out1);
//       let out = res_inc'(out1);
//       mem := gensym^;
//       out;
//     };
//   };
// };

// let rec res_inc' = input => {
//   switch (input) {
//   | [] => []
//   | [res, ...rest] => [res + 1, ...res_inc'(rest)]
//   };
// };

//******* Current
// let rec res_inc' = input => {
//   switch (input) {
//   | Nil => Nil
//   | Cons(p, r, rest) => Cons(p, r + 1, res_inc'(rest))
//   };
// };

// let rec fib2 = (~width: int, ~pos: int, x: my_fib): mylist => {
//   count := count^ + 1;
//   switch (x) {
//   | Text2(mem, _i) =>
//     // TODO: without memoization
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       Cons(0, 0, Nil);
//     } else {
//       let res = 1;
//       mem := gensym^;
//       Cons(pos, res,
//       Cons(pos, res,
//       Cons(pos, res,
//       Cons(pos, res,
//       Cons(pos, res,
//       Cons(pos, res,
//       Cons(pos, res,
//       Cons(pos, res,
//       Cons(pos, res,
//       Cons(pos, res,
//       Nil))))))))))
//       //([pos,pos,pos,pos,pos,pos,pos,pos,pos,pos], [res,res,res,res,res,res,res,res,res,res,]);
//     };
//   | Fail2(mem) =>
//     // TODO: fail without memoization
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       Cons(0, 0, Nil);
//     } else {
//       let res = 1;
//       mem := gensym^;
//       Cons(pos, res,
//       Cons(pos, res,
//       Cons(pos, res,
//       Cons(pos, res,
//       Cons(pos, res,
//       Cons(pos, res,
//       Cons(pos, res,
//       Cons(pos, res,
//       Cons(pos, res,
//       Cons(pos, res,
//       Nil))))))))))
//       //([pos,pos,pos,pos,pos,pos,pos,pos,pos,pos], [res,res,res,res,res,res,res,res,res,res,]);
//     };
//   | Align2(mem, f) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       Cons(0, 0, Nil);
//     } else {
//       let out1 = fib2(~width, ~pos, f);
//       let out = res_inc'(out1);
//       mem := gensym^;
//       out;
//     };
//   | Annot2(mem, _ann, f) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       Cons(0, 0, Nil);
//     } else {
//       let out1 = fib2(~width, ~pos, f);
//       let out = res_inc'(out1);
//       mem := gensym^;
//       out;
//     };
//   | Cat2(mem, f1, f2) =>
//     // TODO: maybe without memoization?
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       Cons(0, 0, Nil);
//     } else {
//       let out1 = fib2(~width, ~pos, f1);
//       let _ = fib2(~width, ~pos, f2);
//       let out = res_inc'(out1);
//       mem := gensym^;
//       out;
//     };
//   | Choice2(mem, f1, f2) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       Cons(0, 0, Nil);
//     } else {
//       let out1 = fib2(~width, ~pos, f1);
//       let _ = fib2(~width, ~pos, f2);
//       // let out = res_inc'(out1);
//       let out = res_inc'(out1);
//       mem := gensym^;
//       out;
//     };
//   };
// };

// 30ns
// let rec res_inc' = input => {
//   switch (input) {
//   | [] => []
//   | [(pos, res), ...input] => [(pos, res + 1), ...res_inc'(input)]
//   };
// };
// function res_inc$0(input) {
//     /*<<src/pretty/LayoutOfDoc.re 243 28>>*/
//     if (input) {
//         var input$0 = input[2]
//           , match = input[1]
//           , res = match[2]
//           , pos = match[1];
//         /*<<src/pretty/LayoutOfDoc.re 246 50>>*/
//         return [0, [0, pos, res + 1 | 0], /*<<src/pretty/LayoutOfDoc.re 246 50>>*/
//         res_inc$0(input$0)]
//     }
//     /*<<src/pretty/LayoutOfDoc.re 245 10>>*/
//     return 0
//     /*<<src/pretty/LayoutOfDoc.re 248 1>>*/
// }

// [(pos, res)]             avg:   5.9ms   per count:  28.4ns (count: 206668)
// [(pos, res), (pos, res)] avg:   8.2ms   per count:  39.8ns (count: 206668)
// let rec res_inc'' = (out, input) => {
//   switch (input) {
//   | [] => out
//   | [(pos, res), ...input] => res_inc''([(pos, res + 1), ...out], input)
//   };
// };
// let res_inc' = input => res_inc''([], input);
// function res_inc$0(out, input) {
//     var out$0 = out
//       , input$0 = input;
//     /*<<src/pretty/LayoutOfDoc.re 266 36>>*/
//     for (; ; ) {
//         /*<<src/pretty/LayoutOfDoc.re 266 36>>*/
//         if (input$0) {
//             var input$1 = input$0[2]
//               , match = input$0[1]
//               , res = match[2]
//               , pos = match[1]
//               , out$1 = [0, [0, pos, res + 1 | 0], out$0]
//               , out$0 = out$1
//               , input$0 = input$1;
//             continue
//         }
//         /*<<src/pretty/LayoutOfDoc.re 268 10>>*/
//         return out$0
//     }
//     /*<<src/pretty/LayoutOfDoc.re 271 1>>*/
// }
// /*<<src/pretty/LayoutOfDoc.re 178 17>>*/
// function res_inc$1(input) {
//     /*<<src/pretty/LayoutOfDoc.re 272 24>>*/
//     return /*<<src/pretty/LayoutOfDoc.re 272 24>>*/
//     res_inc$0(0, input)
//     /*<<src/pretty/LayoutOfDoc.re 272 44>>*/
// }

// TODO: array?
// avg:  12.1ms   per count:  58.7ns (count: 206668)
// let res_inc' = (input: List.t((int, int))): List.t((int, int)) => {
//   let inref = ref(input);
//   let out = ref([]);
//   while (inref^ != []) {
//     switch (inref^) {
//     | [] => failwith(__LOC__)
//     | [(pos, res), ...rest] =>
//       out := [(pos, res + 1), ...out^];
//       inref := rest;
//     };
//   };
//   out^;
// };
// avg:  25.4ms   per count: 122.9ns (count: 206668)
// let res_inc' = (input: Array.t((int, int))): Array.t((int, int)) => {
//   Array.map(res_inc, input);
// };
// avg:  21.0ms   per count: 101.5ns (count: 206668)
// let res_inc' = (input: Array.t((int, int))): Array.t((int, int)) => {
//   let out = Array.make(Array.length(input), (0, 0));
//   for (i in 0 to Array.length(input) - 1) {
//     let (pos, res) = input[i];
//     out[i] = (pos, res + 1);
//   };
//   out;
// };
// avg:   4.0ms   per count:  19.4ns (count: 206668)
// module Js = Js_of_ocaml.Js;
// let res_inc': Array.t((int, int)) => Array.t((int, int)) =
//   Js.Unsafe.js_expr("function res_inc$0(input) { return input; }
//     ");
// w/o prealloc array avg:   8.3ms   per count:  39.9ns (count: 206668)
// w/ prealloc array  avg:   6.4ms   per count:  31.2ns (count: 206668)
// 10x avg:  18.7ms   per count:  90.4ns (count: 206668)
// module Js = Js_of_ocaml.Js;
// let res_inc': Array.t((int, int)) => Array.t((int, int)) =
//   Js.Unsafe.js_expr(
//     "function res_inc$0(input) {
//     var len = input.length;
//     var output = new Array(len);
//     output[0] = [0];
//     for (var i = 1; i < len; i++) {
//       var inp = input[i];
//       var res = inp[2]
//       var pos = inp[1];
//       var res1 = [0, pos, res + 1 | 0]
//       output[i] = res1;
//     }
//     return output;
//   }
//     ",
//   );

// let rec fib2 = (~width: int, ~pos: int, x: my_fib): Array.t((int, int)) => {
//   count := count^ + 1;
//   switch (x) {
//   | Text2(mem, _i) =>
//     // TODO: without memoization
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       [|(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),|];
//     } else {
//       let res = 1;
//       mem := gensym^;
//       [|(pos, res),(pos, res),(pos, res),(pos, res),(pos, res),(pos, res),(pos, res),(pos, res),(pos, res),(pos, res),|];
//     };
//   | Fail2(mem) =>
//     // TODO: fail without memoization
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       [|(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),|];
//     } else {
//       let res = 1;
//       mem := gensym^;
//       [|(pos, res),(pos, res),(pos, res),(pos, res),(pos, res),(pos, res),(pos, res),(pos, res),(pos, res),(pos, res),|];
//     };
//   | Align2(mem, f) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       [|(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),|];
//     } else {
//       let out1 = fib2(~width, ~pos, f);
//       let out = Js.Unsafe.fun_call(res_inc', [|Js.Unsafe.inject(out1)|]);
//       mem := gensym^;
//       out;
//     };
//   | Annot2(mem, _ann, f) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       [|(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),|];
//     } else {
//       let out1 = fib2(~width, ~pos, f);
//       let out = Js.Unsafe.fun_call(res_inc', [|Js.Unsafe.inject(out1)|]);
//       mem := gensym^;
//       out;
//     };
//   | Cat2(mem, f1, f2) =>
//     // TODO: maybe without memoization?
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       [|(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),|];
//     } else {
//       let out1 = fib2(~width, ~pos, f1);
//       let _ = fib2(~width, ~pos, f2);
//       let out = Js.Unsafe.fun_call(res_inc', [|Js.Unsafe.inject(out1)|]);
//       mem := gensym^;
//       out;
//     };
//   | Choice2(mem, f1, f2) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       [|(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),(0, 0),|];
//     } else {
//       let out1 = fib2(~width, ~pos, f1);
//       let _ = fib2(~width, ~pos, f2);
//       // let out = res_inc'(out1);
//       let out = Js.Unsafe.fun_call(res_inc', [|Js.Unsafe.inject(out1)|]);
//       mem := gensym^;
//       out;
//     };
//   };
// };

// TODO: parallel arrays due to heterogenious nature
// TODO: parallel arrays allow reuse of "pos" array

// // interleaved array avg:   5.8ms   per count:  27.9ns (count: 206668)
// 10x avg:  11.3ms   per count:  54.5ns (count: 206668)
// module Js = Js_of_ocaml.Js;
// let res_inc': Array.t(int) => Array.t(int) =
//   Js.Unsafe.js_expr(
//     "function res_inc$0(input) {
//     var len = input.length;
//     var output = new Array(len);
//     output[0] = [0];
//     for (var i = 1; i < len; i+=2) {
//       var pos = input[i];
//       var res = input[i+1]
//       output[i] = pos;
//       output[i+1] = res + 1 | 0;
//     }
//     return output;
//   }
//     ",
//   );

// let rec fib2 = (~width: int, ~pos: int, x: my_fib): Array.t(int) => {
//   count := count^ + 1;
//   switch (x) {
//   | Text2(mem, _i) =>
//     // TODO: without memoization
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       [|0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, |];
//     } else {
//       let res = 1;
//       mem := gensym^;
//       [|pos,res, pos,res, pos,res, pos,res, pos,res, pos,res, pos,res, pos,res, pos,res, pos,res, |];
//     };
//   | Fail2(mem) =>
//     // TODO: fail without memoization
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       [|0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, |];
//     } else {
//       let res = 1;
//       mem := gensym^;
//       [|pos,res, pos,res, pos,res, pos,res, pos,res, pos,res, pos,res, pos,res, pos,res, pos,res, |];
//     };
//   | Align2(mem, f) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       [|0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, |];
//     } else {
//       let out1 = fib2(~width, ~pos, f);
//       let out = Js.Unsafe.fun_call(res_inc', [|Js.Unsafe.inject(out1)|]);
//       mem := gensym^;
//       out;
//     };
//   | Annot2(mem, _ann, f) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       [|0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, |];
//     } else {
//       let out1 = fib2(~width, ~pos, f);
//       let out = Js.Unsafe.fun_call(res_inc', [|Js.Unsafe.inject(out1)|]);
//       mem := gensym^;
//       out;
//     };
//   | Cat2(mem, f1, f2) =>
//     // TODO: maybe without memoization?
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       [|0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, |];
//     } else {
//       let out1 = fib2(~width, ~pos, f1);
//       let _ = fib2(~width, ~pos, f2);
//       let out = Js.Unsafe.fun_call(res_inc', [|Js.Unsafe.inject(out1)|]);
//       mem := gensym^;
//       out;
//     };
//   | Choice2(mem, f1, f2) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       [|0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, 0,0, |];
//     } else {
//       let out1 = fib2(~width, ~pos, f1);
//       let _ = fib2(~width, ~pos, f2);
//       // let out = res_inc'(out1);
//       let out = Js.Unsafe.fun_call(res_inc', [|Js.Unsafe.inject(out1)|]);
//       mem := gensym^;
//       out;
//     };
//   };
// };

// parallel array    avg:   6.8ms   per count:  32.9ns (count: 206668)
// no pos array      avg:   5.5ms   per count:  26.6ns (count: 206668)
// 10x avg:   9.1ms   per count:  44.2ns (count: 206668)
// 10x passthrough align or annot: avg:   6.8ms   per count:  33.1ns (count: 206668)
// module Js = Js_of_ocaml.Js;
// let res_inc': Array.t(int) => Array.t(int) =
//   Js.Unsafe.js_expr(
//     "function res_inc$0(input) {
//     var len = input.length;
//     var output = new Array(len);
//     output[0] = [0];
//     for (var i = 1; i < len; i++) {
//       var res = input[i]
//       output[i] = res + 1 | 0;
//     }
//     return output;
//   }
//     ",
//   );

// let rec fib2 =
//         (~width: int, ~pos: int, x: my_fib): (Array.t(int), Array.t(int)) => {
//   count := count^ + 1;
//   switch (x) {
//   | Text2(mem, _i) =>
//     // TODO: without memoization
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       ([|0,0,0,0,0,0,0,0,0,0,|], [|0,0,0,0,0,0,0,0,0,0,|]);
//     } else {
//       let res = 1;
//       mem := gensym^;
//       ([|pos,pos,pos,pos,pos,pos,pos,pos,pos,pos,|], [|res,res,res,res,res,res,res,res,res,res,|]);
//     };
//   | Fail2(mem) =>
//     // TODO: fail without memoization
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       ([|0,0,0,0,0,0,0,0,0,0,|], [|0,0,0,0,0,0,0,0,0,0,|]);
//     } else {
//       let res = 1;
//       mem := gensym^;
//       ([|pos,pos,pos,pos,pos,pos,pos,pos,pos,pos,|], [|res,res,res,res,res,res,res,res,res,res,|]);
//     };
//   | Align2(mem, f) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       ([|0,0,0,0,0,0,0,0,0,0,|], [|0,0,0,0,0,0,0,0,0,0,|]);
//     } else {
//       let (out1p, out1r) = fib2(~width, ~pos, f);
//       //let out = Js.Unsafe.fun_call(res_inc', [|Js.Unsafe.inject(out1r)|]);
//       let out = out1r;
//       mem := gensym^;
//       (out1p, out);
//     };
//   | Annot2(mem, _ann, f) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       ([|0,0,0,0,0,0,0,0,0,0,|], [|0,0,0,0,0,0,0,0,0,0,|]);
//     } else {
//       let (out1p, out1r) = fib2(~width, ~pos, f);
//       //let out = Js.Unsafe.fun_call(res_inc', [|Js.Unsafe.inject(out1r)|]);
//       let out = out1r;
//       mem := gensym^;
//       (out1p, out);
//     };
//   | Cat2(mem, f1, f2) =>
//     // TODO: maybe without memoization?
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       ([|0,0,0,0,0,0,0,0,0,0,|], [|0,0,0,0,0,0,0,0,0,0,|]);
//     } else {
//       let (out1p, out1r) = fib2(~width, ~pos, f1);
//       let _ = fib2(~width, ~pos, f2);
//       let out = Js.Unsafe.fun_call(res_inc', [|Js.Unsafe.inject(out1r)|]);
//       mem := gensym^;
//       (out1p, out);
//     };
//   | Choice2(mem, f1, f2) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       ([|0,0,0,0,0,0,0,0,0,0,|], [|0,0,0,0,0,0,0,0,0,0,|]);
//     } else {
//       let (out1p, out1r) = fib2(~width, ~pos, f1);
//       let _ = fib2(~width, ~pos, f2);
//       // let out = res_inc'(out1);
//       let out = Js.Unsafe.fun_call(res_inc', [|Js.Unsafe.inject(out1r)|]);
//       mem := gensym^;
//       (out1p, out);
//     };
//   };
// };

// 10x with +1           avg:   6.7ms   per count:  32.3ns (count: 206668)
// return int length     avg:   7.2ms   per count:  34.6ns (count: 206668)
// w/o .length = ...     avg:   7.2ms   per count:  34.6ns (count: 206668)
// w/  .length = ...     avg:  11.3ms   per count:  54.5ns (count: 206668)
// res_inc return int length avg:   7.6ms   per count:  36.8ns (count: 206668)
// merge len=5           avg:   7.6ms   per count:  36.6ns (count: 206668)
// merge len=10          avg:   8.9ms   per count:  43.0ns (count: 206668)
// TODO: put length in first element of array
// TODO: RLE of the arrays
// TODO: use size+1 instead of size (b/c less arithmetic)
// module Js = Js_of_ocaml.Js;
// let res_inc': (int, Array.t(int)) => (int, Array.t(int)) =
//   Js.Unsafe.js_expr(
//     "function res_inc$0(size, input) {
//     //var len = input.length;
//     var js_size = size + 1 | 0;
//     var output = new Array(js_size);
//     output[0] = [0];
//     for (var i = 1; i < js_size; i++) {
//       var res = input[i]
//       output[i] = res + 1 | 0;
//     }
//     return [0, size, output];
//   }
//     ",
//   );

// let merge: (int, Array.t(int), int, Array.t(int)) => (int, Array.t(int)) =
//   Js.Unsafe.js_expr(
//     "function merge(size1, res1, size2, res2) {
//     //var len = input.length;
//     var js_size1 = size1 + 1 | 0;
//     var js_size2 = size2 + 1 | 0;
//     var js_size_1 = size1 + size2 | 0;
//     var js_size = js_size_1 + 1 | 0;
//     var res = new Array(js_size);
//     res[0] = [0];
//     var i1 = 1;
//     var i2 = 1;
//     var i = 1;
//     while (i1 < js_size1 && i2 < js_size2) {
//       if (res1[i1] < res2[i2]) {
//         res[i] = res1[i1] + 1 | 0;
//         i1 = i1 + 1 | 0;
//         i = i + 1 | 0;
//       } else if (res1[i1] > res2[i2]) {
//         res[i] = res2[i2] + 1 | 0;
//         i2 = i2 + 1 | 0;
//         i = i + 1 | 0;
//       } else {
//         res[i] = res1[i1] + 1 | 0;
//         i1 = i1 + 1 | 0;
//         i2 = i2 + 1 | 0;
//         i = i + 1 | 0;
//       }
//     }
//     while (i1 < js_size1) {
//       res[i] = res1[i1] + 1 | 0;
//       i1 = i1 + 1 | 0;
//       i = i + 1 | 0;
//     }
//     while (i2 < js_size2) {
//         res[i] = res2[i2] + 1 | 0;
//         i2 = i2 + 1 | 0;
//         i = i + 1 | 0;
//     }
//     var size = i - 1 | 0;
//     return [0, size, res];
//   }
//     ",
//   );

// //let foo = (x: int): (int, int) => (x, x);

// let rec fib2 =
//         (~width: int, ~pos: int, x: my_fib)
//         : (int, Array.t(int), Array.t(int)) => {
//   count := count^ + 1;
//   switch (x) {
//   | Text2(mem, _i) =>
//     // TODO: without memoization
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       (
//         10,
//         [|1111, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//         [|2222, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//       );
//     } else {
//       let res = 1;
//       mem := gensym^;
//       (
//         10,
//         [|
//           pos + 1,
//           pos + 2,
//           pos + 3,
//           pos + 4,
//           pos + 5,
//           pos + 6,
//           pos + 7,
//           pos + 8,
//           pos + 9,
//           pos + 10,
//         |],
//         [|res+1, res+2, res+3, res+4, res+5, res+6, res+7, res+8, res+9, res+10|],
//       );
//     };
//   | Fail2(mem) =>
//     // TODO: fail without memoization
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       (
//         10,
//         [|3333, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//         [|4444, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//       );
//     } else {
//       let res = 1;
//       mem := gensym^;
//       (
//         10,
//         [|
//           pos + 1,
//           pos + 2,
//           pos + 3,
//           pos + 4,
//           pos + 5,
//           pos + 6,
//           pos + 7,
//           pos + 8,
//           pos + 9,
//           pos + 10,
//         |],
//         [|res+1, res+2, res+3, res+4, res+5, res+6, res+7, res+8, res+9, res+10|],
//       );
//     };
//   | Align2(mem, f) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       (
//         10,
//         [|5555, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//         [|6666, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//       );
//     } else {
//       let (out1s, out1p, out1r) = fib2(~width, ~pos, f);
//       //let out = Js.Unsafe.fun_call(res_inc', [|Js.Unsafe.inject(out1r)|]);
//       let out = out1r;
//       mem := gensym^;
//       (out1s, out1p, out);
//     };
//   | Annot2(mem, _ann, f) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       (
//         10,
//         [|77770, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//         [|88880, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//       );
//     } else {
//       let (out1s, out1p, out1r) = fib2(~width, ~pos, f);
//       //let out = Js.Unsafe.fun_call(res_inc', [|Js.Unsafe.inject(out1r)|]);
//       let out = out1r;
//       mem := gensym^;
//       (out1s, out1p, out);
//     };
//   | Cat2(mem, f1, f2) =>
//     // TODO: maybe without memoization?
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       (
//         10,
//         [|999999990, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//         [|121212120, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//       );
//     } else {
//       let (out1s, out1p, out1r) = fib2(~width, ~pos, f1);
//       let _ = fib2(~width, ~pos, f2);
//       let (out_s, out_r) =
//         Js.Unsafe.fun_call(
//           res_inc',
//           [|Js.Unsafe.inject(out1s), Js.Unsafe.inject(out1r)|],
//         );
//       mem := gensym^;
//       (out_s, out1p, out_r);
//     };
//   | Choice2(mem, f1, f2) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       (
//         10,
//         [|131313130, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//         [|141414140, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//       );
//     } else {
//       let (out1s, out1p, out1r) = fib2(~width, ~pos, f1);
//       let (out2s, _, out2r) = fib2(~width, ~pos, f2);
//       //let out = res_inc'(out1);
//       let (out_s, out_r) =
//         Js.Unsafe.fun_call(
//           merge,
//           [|Js.Unsafe.inject(out1s), Js.Unsafe.inject(out1r), Js.Unsafe.inject(out2s), Js.Unsafe.inject(out2r), |],
//         );
//       mem := gensym^;
//       (out_s, out1p, out_r);
//     };
//   };
// };

// avg:  12.0ms   per count:  58.1ns (count: 206668)
// avg:   8.2ms   per count:  39.8ns (count: 206668)
// avg:   8.4ms   per count:  40.5ns (count: 206668)
// 10 avg:   9.5ms   per count:  46.1ns (count: 206668)

module Js = Js_of_ocaml.Js;
// let res_inc': (int, Array.t(int)) => (int, Array.t(int)) =
//   Js.Unsafe.js_expr(
//     "function res_inc$0(js_size, input) {
//     //var len = input.length;
//     var output = new Array(js_size);
//     output[0] = [0];
//     for (var i = 1; i < js_size; i++) {
//       var res = input[i]
//       output[i] = res + 1 | 0;
//     }
//     return [0, js_size, output];
//   }
//     ",
//   );

// let res_inc': Array.t(int) => Array.t(int) =
//   Js.Unsafe.js_expr(
//     "function res_inc$0(input) {
//     //var len = input.length;
//     var js_size = input[1];
//     var output = new Array(js_size);
//     output[0] = [0];
//     output[1] = js_size;
//     for (var i = 2; i < js_size; i++) {
//       var res = input[i];
//       output[i] = res + 1 | 0;
//     }
//     return [0, output];
//   }
//     ",
//   );

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

// avg:  14.9ms   per count:  71.9ns (count: 206668)
// let merge:
//   (Array.t(int), Array.t(int), Array.t(int), Array.t(int)) =>
//   (Array.t(int), Array.t(int)) =
//   Js.Unsafe.js_expr(
//     "function merge(pos1, res1, pos2, res2) {
//     //var len = input.length;
//     var js_size1 = pos1[1];
//     var js_size2 = pos2[1];
//     var pre_js_size = js_size1 + js_size2 | 0;
//     var js_size = pre_js_size - 2 | 0;
//     var pos = new Array(js_size);
//     pos[0] = [0];
//     var res = new Array(js_size);
//     res[0] = [0];
//     var i1 = 2;
//     var i2 = 2;
//     var i = 2;
//     while (i1 < js_size1 && i2 < js_size2) {
//       if (pos1[i1] < pos2[i2]) {
//         //console.log(\"1\");
//         pos[i] = pos1[i1];
//         res[i] = res1[i1] + 1 | 0;
//         i1 = i1 + 1 | 0;
//       } else if (pos1[i1] > pos2[i2]) {
//         //console.log(\"2\");
//         pos[i] = pos2[i2];
//         res[i] = res2[i2] + 1 | 0;
//         i2 = i2 + 1 | 0;
//       } else {
//         //console.log(\"3\");
//         // TODO: res1[i1] <=> res2[i2]
//         pos[i] = pos1[i1];
//         res[i] = res1[i1] + 1 | 0;
//         i1 = i1 + 1 | 0;
//         i2 = i2 + 1 | 0;
//       }
//       i = i + 1 | 0;
//     }
//     while (i1 < js_size1) {
//         //pos[i] = pos1[i1];
//         res[i] = res1[i1] + 1 | 0;
//         i1 = i1 + 1 | 0;
//         i = i + 1 | 0;
//     }
//     while (i2 < js_size2) {
//         //pos[i] = pos2[i2];
//         res[i] = res2[i2] + 1 | 0;
//         i2 = i2 + 1 | 0;
//         i = i + 1 | 0;
//     }
//     pos[1] = i;
//     return [0, pos, res];
//   }
//     ",
//   );

// let merge:
//   (int, Array.t(int), Array.t(int), int, Array.t(int), Array.t(int)) =>
//   (int, Array.t(int), Array.t(int)) =
//   Js.Unsafe.js_expr(
//     "function merge(size1, pos1, res1, size2, pos2, res2) {
//     //var len = input.length;
//     var js_size1 = size1 + 1 | 0;
//     var js_size2 = size2 + 1 | 0;
//     var i1 = 1;
//     var i2 = 1;
//     var i = 1;
//     while (i1 < js_size1 && i2 < js_size2) {
//       if (pos1[i1] < pos2[i2]) {
//         i1 = i1 + 1 | 0;
//         i = i + 1 | 0;
//       } else if (pos1[i1] > pos2[i2]) {
//         //console.log(\"2\");
//         i2 = i2 + 1 | 0;
//         i = i + 1 | 0;
//       } else {
//         i1 = i1 + 1 | 0;
//         i2 = i2 + 1 | 0;
//         i = i + 1 | 0;
//       }
//     }
//     var js_size = i;
//     var size = i - 1 | 0;
//     var pos = new Array(js_size);
//     pos[0] = [0];
//     var res = new Array(js_size);
//     res[0] = [0];
//     i1 = 1;
//     i2 = 1;
//     i = 1;
//     while (i < js_size) {
//       if (pos1[i1] < pos2[i2]) {
//         //console.log(\"1\");
//         pos[i] = pos1[i1];
//         res[i] = res1[i1] + 1 | 0;
//         i1 = i1 + 1 | 0;
//         i = i + 1 | 0;
//       } else if (pos1[i1] > pos2[i2]) {
//         //console.log(\"2\");
//         pos[i] = pos2[i2];
//         res[i] = res2[i2] + 1 | 0;
//         i2 = i2 + 1 | 0;
//         i = i + 1 | 0;
//       } else {
//         //console.log(\"3\");
//         // TODO: res1[i1] <=> res2[i2]
//         pos[i] = pos1[i1];
//         res[i] = res1[i1] + 1 | 0;
//         i1 = i1 + 1 | 0;
//         i2 = i2 + 1 | 0;
//         i = i + 1 | 0;
//       }
//     }
//     while (i1 < js_size1) {
//         //pos[i] = pos1[i1];
//         res[i] = res1[i1] + 1 | 0;
//         i1 = i1 + 1 | 0;
//         i = i + 1 | 0;
//     }
//     while (i2 < js_size2) {
//         //pos[i] = pos2[i2];
//         res[i] = res2[i2] + 1 | 0;
//         i2 = i2 + 1 | 0;
//         i = i + 1 | 0;
//     }
//     //var size = i - 1 | 0;
//     return [0, size, pos, res];
//   }
//     ",
//   );

//let foo = (x: int): (int, int) => (x, x);

// let rec fib2 =
//         (~width: int, ~pos: int, x: my_fib)
//         : (int, Array.t(int), Array.t(int)) => {
//   count := count^ + 1;
//   switch (x) {
//   | Text2(mem, _i) =>
//     // TODO: without memoization
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       (
//         11,
//         [|1111, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//         [|2222, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//       );
//     } else {
//       let res = 1;
//       mem := gensym^;
//       (
//         11,
//         [|
//           pos + 0,
//           pos + 1,
//           pos + 2,
//           pos + 3,
//           pos + 4,
//           pos + 5,
//           pos + 6,
//           pos + 7,
//           pos + 8,
//           pos + 9,
//         |],
//         [|
//           res + 0,
//           res + 1,
//           res + 2,
//           res + 3,
//           res + 4,
//           res + 5,
//           res + 6,
//           res + 7,
//           res + 8,
//           res + 9,
//         |],
//       );
//     };
//   | Fail2(mem) =>
//     // TODO: fail without memoization
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       (
//         11,
//         [|3333, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//         [|4444, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//       );
//     } else {
//       let res = 1;
//       mem := gensym^;
//       (
//         11,
//         [|
//           pos + 0,
//           pos + 1,
//           pos + 2,
//           pos + 3,
//           pos + 4,
//           pos + 5,
//           pos + 6,
//           pos + 7,
//           pos + 8,
//           pos + 9,
//         |],
//         [|
//           res + 0,
//           res + 1,
//           res + 2,
//           res + 3,
//           res + 4,
//           res + 5,
//           res + 6,
//           res + 7,
//           res + 8,
//           res + 9,
//         |],
//       );
//     };
//   | Align2(mem, f) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       (
//         11,
//         [|5555, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//         [|6666, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//       );
//     } else {
//       let (out1s, out1p, out1r) = fib2(~width, ~pos, f);
//       //let out = Js.Unsafe.fun_call(res_inc', [|Js.Unsafe.inject(out1r)|]);
//       let out = out1r;
//       mem := gensym^;
//       (out1s, out1p, out);
//     };
//   | Annot2(mem, _ann, f) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       (
//         11,
//         [|77770, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//         [|88880, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//       );
//     } else {
//       let (out1s, out1p, out1r) = fib2(~width, ~pos, f);
//       //let out = Js.Unsafe.fun_call(res_inc', [|Js.Unsafe.inject(out1r)|]);
//       let out = out1r;
//       mem := gensym^;
//       (out1s, out1p, out);
//     };
//   | Cat2(mem, f1, f2) =>
//     // TODO: maybe without memoization?
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       (
//         11,
//         [|999999990, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//         [|121212120, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//       );
//     } else {
//       let (out1s, out1p, out1r) = fib2(~width, ~pos, f1);
//       let _ = fib2(~width, ~pos, f2);
//       let (out_s, out_r) =
//         Js.Unsafe.fun_call(
//           res_inc',
//           [|
//             Js.Unsafe.inject(out1s),
//             Js.Unsafe.inject(out1s),
//             Js.Unsafe.inject(out1r),
//           |],
//         );
//       mem := gensym^;
//       (out_s, out1p, out_r);
//     };
//   | Choice2(mem, f1, f2) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       (
//         11,
//         [|131313130, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//         [|141414140, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//       );
//     } else {
//       let (out1s, out1p, out1r) = fib2(~width, ~pos, f1);
//       let (out2s, out2p, out2r) = fib2(~width, ~pos, f2);
//       //let out = res_inc'(out1);
//       let (out_s, out_p, out_r) =
//         Js.Unsafe.fun_call(
//           merge,
//           [|
//             Js.Unsafe.inject(out1s),
//             Js.Unsafe.inject(out1p),
//             Js.Unsafe.inject(out1r),
//             Js.Unsafe.inject(out2s),
//             Js.Unsafe.inject(out2p),
//             Js.Unsafe.inject(out2r),
//           |],
//         );
//       mem := gensym^;
//       (out_s, out_p, out_r);
//     };
//   };
// };

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

// "map" functions

// Array of pairs
let map: Array.t((int, int)) => Array.t((int, int)) =
  Js.Unsafe.js_expr(
    "function res_inc$0(input) {
    var len = input.length;
    var output = new Array(len);
    output[0] = [0];
    for (var i = 1; i < len; i++) {
      var inp = input[i];
      var res = inp[2]
      var pos = inp[1];
      var res1 = [0, pos, res + 1 | 0]
      output[i] = res1;
    }
    return output;
  }
    ",
  );

// TODO: merge
// TODO: fold

// Interleaved (w/ stored length?)
let map: Array.t(int) => Array.t(int) =
  Js.Unsafe.js_expr(
    "function res_inc$0(input) {
    var len = input.length;
    var output = new Array(len);
    output[0] = [0];
    for (var i = 1; i < len; i+=2) {
      var pos = input[i];
      var res = input[i+1]
      output[i] = pos;
      output[i+1] = res + 1 | 0;
    }
    return output;
  }
    ",
  );

// TODO: merge
// TODO: fold

// Interleaved with external size
// let map: Array.t(int) => Array.t(int) =
//   Js.Unsafe.js_expr(
//     "function res_inc$0(input) {
//     var len = input.length;
//     var output = new Array(len);
//     output[0] = [0];
//     for (var i = 1; i < len; i+=2) {
//       var pos = input[i];
//       var res = input[i+1]
//       output[i] = pos;
//       output[i+1] = res + 1 | 0;
//     }
//     return output;
//   }
//     ",
//   );

// // TODO: merge
// // TODO: fold

// // Parallel with external size
// let res_inc': (int, Array.t(int)) => (int, Array.t(int)) =
//   Js.Unsafe.js_expr(
//     "function res_inc$0(size, input) {
//     //var len = input.length;
//     var js_size = size + 1 | 0;
//     var output = new Array(js_size);
//     output[0] = [0];
//     for (var i = 1; i < js_size; i++) {
//       var res = input[i]
//       output[i] = res + 1 | 0;
//     }
//     return [0, size, output];
//   }
//     ",
//   );

// let merge:
//   (int, Array.t(int), Array.t(int), int, Array.t(int), Array.t(int)) =>
//   (int, Array.t(int), Array.t(int)) =
//   Js.Unsafe.js_expr(
//     "function merge(js_size1, pos1, res1, js_size2, pos2, res2) {
//     //var len = input.length;
//     var pre_js_size = js_size1 + js_size2 | 0;
//     var js_size = pre_js_size - 1 | 0;
//     var pos = new Array(js_size);
//     pos[0] = [0];
//     var res = new Array(js_size);
//     res[0] = [0];
//     var i1 = 1;
//     var i2 = 1;
//     var i = 1;
//     while (i1 < js_size1 && i2 < js_size2) {
//       if (pos1[i1] < pos2[i2]) {
//         //console.log(\"1\");
//         pos[i] = pos1[i1];
//         res[i] = res1[i1] + 1 | 0;
//         i1 = i1 + 1 | 0;
//       } else if (pos1[i1] > pos2[i2]) {
//         //console.log(\"2\");
//         pos[i] = pos2[i2];
//         res[i] = res2[i2] + 1 | 0;
//         i2 = i2 + 1 | 0;
//       } else {
//         //console.log(\"3\");
//         // TODO: res1[i1] <=> res2[i2]
//         pos[i] = pos1[i1];
//         res[i] = res1[i1] + 1 | 0;
//         i1 = i1 + 1 | 0;
//         i2 = i2 + 1 | 0;
//       }
//       i = i + 1 | 0;
//     }
//     while (i1 < js_size1) {
//         //pos[i] = pos1[i1];
//         res[i] = res1[i1] + 1 | 0;
//         i1 = i1 + 1 | 0;
//         i = i + 1 | 0;
//     }
//     while (i2 < js_size2) {
//         //pos[i] = pos2[i2];
//         res[i] = res2[i2] + 1 | 0;
//         i2 = i2 + 1 | 0;
//         i = i + 1 | 0;
//     }
//     var size = i;
//     return [0, size, pos, res];
//   }
//     ",
//   );

// TODO: fold

////////////////////////////////////////////////// RESTART /////////////////////////////////////

// skeliton w/o map, merge or flatmap: 28ns

// let rec fib2 =
//         (~width: int, ~pos: int, x: my_fib)
//         : (int, Array.t(int), Array.t(int)) => {
//   count := count^ + 1;
//   switch (x) {
//   | Text2(mem, _i) =>
//     // TODO: without memoization
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       (
//         11,
//         [|1111, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//         [|2222, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//       );
//     } else {
//       let res = 0;
//       mem := gensym^;
//       (
//         11,
//         [|
//           pos + 0,
//           pos + 1,
//           pos + 2,
//           pos + 3,
//           pos + 4,
//           pos + 5,
//           pos + 6,
//           pos + 7,
//           pos + 8,
//           pos + 9,
//         |],
//         [|
//           res + 0,
//           res + 1,
//           res + 2,
//           res + 3,
//           res + 4,
//           res + 5,
//           res + 6,
//           res + 7,
//           res + 8,
//           res + 9,
//         |],
//       );
//     };
//   | Fail2(mem) =>
//     // TODO: fail without memoization
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       (
//         11,
//         [|3333, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//         [|4444, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//       );
//     } else {
//       let res = 1;
//       mem := gensym^;
//       (
//         11,
//         [|
//           pos + 0,
//           pos + 1,
//           pos + 2,
//           pos + 3,
//           pos + 4,
//           pos + 5,
//           pos + 6,
//           pos + 7,
//           pos + 8,
//           pos + 9,
//         |],
//         [|
//           res + 0,
//           res + 1,
//           res + 2,
//           res + 3,
//           res + 4,
//           res + 5,
//           res + 6,
//           res + 7,
//           res + 8,
//           res + 9,
//         |],
//       );
//     };
//   | Align2(mem, f) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       (
//         11,
//         [|5555, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//         [|6666, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//       );
//     } else {
//       let (out1s, out1p, out1r) = fib2(~width, ~pos, f);
//       //let out = Js.Unsafe.fun_call(res_inc', [|Js.Unsafe.inject(out1r)|]);
//       let out = out1r;
//       mem := gensym^;
//       (out1s, out1p, out);
//     };
//   | Annot2(mem, _ann, f) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       (
//         11,
//         [|77770, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//         [|88788, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//       );
//     } else {
//       let (out1s, out1p, out1r) = fib2(~width, ~pos, f);
//       //let out = Js.Unsafe.fun_call(res_inc', [|Js.Unsafe.inject(out1r)|]);
//       let out = out1r;
//       mem := gensym^;
//       (out1s, out1p, out);
//     };
//   | Cat2(mem, f1, f2) =>
//     // TODO: maybe without memoization?
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       (
//         11,
//         [|999999990, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//         [|121212120, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//       );
//     } else {
//       let (out1s, out1p, out1r) = fib2(~width, ~pos, f1);
//       let _ = fib2(~width, ~pos, f2);
//       // let (out_s, out_r) =
//       //   Js.Unsafe.fun_call(
//       //     res_inc',
//       //     [|
//       //       Js.Unsafe.inject(out1s),
//       //       Js.Unsafe.inject(out1s),
//       //       Js.Unsafe.inject(out1r),
//       //     |],
//       //   );
//       mem := gensym^;
//       (out1s, out1p, out1r);
//     };
//   | Choice2(mem, f1, f2) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       (
//         11,
//         [|131313130, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//         [|141414140, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//       );
//     } else {
//       let (out1s, out1p, out1r) = fib2(~width, ~pos, f1);
//       let (_out2s, _out2p, _out2r) = fib2(~width, ~pos, f2);
//       //let out = res_inc'(out1);
//       // let (out_s, out_p, out_r) =
//       //   Js.Unsafe.fun_call(
//       //     merge,
//       //     [|
//       //       Js.Unsafe.inject(out1s),
//       //       Js.Unsafe.inject(out1p),
//       //       Js.Unsafe.inject(out1r),
//       //       Js.Unsafe.inject(out2s),
//       //       Js.Unsafe.inject(out2p),
//       //       Js.Unsafe.inject(out2r),
//       //     |],
//       //   );
//       mem := gensym^;
//       (out1s, out1p, out1r);
//     };
//   };
// };
// let rec fib2 =
//         (~width: int, ~pos: int, x: my_fib)
//         : (int, Array.t(int), Array.t(int)) => {
//   count := count^ + 1;
//   switch (x) {
//   | Text2(mem, _i) =>
//     // TODO: without memoization
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       (
//         11,
//         [|1111, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//         [|2222, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//       );
//     } else {
//       let res = 0;
//       mem := gensym^;
//       (
//         11,
//         [|
//           pos + 0,
//           pos + 1,
//           pos + 2,
//           pos + 3,
//           pos + 4,
//           pos + 5,
//           pos + 6,
//           pos + 7,
//           pos + 8,
//           pos + 9,
//         |],
//         [|
//           res + 0,
//           res + 1,
//           res + 2,
//           res + 3,
//           res + 4,
//           res + 5,
//           res + 6,
//           res + 7,
//           res + 8,
//           res + 9,
//         |],
//       );
//     };
//   | Fail2(mem) =>
//     // TODO: fail without memoization
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       (
//         11,
//         [|3333, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//         [|4444, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//       );
//     } else {
//       let res = 1;
//       mem := gensym^;
//       (
//         11,
//         [|
//           pos + 0,
//           pos + 1,
//           pos + 2,
//           pos + 3,
//           pos + 4,
//           pos + 5,
//           pos + 6,
//           pos + 7,
//           pos + 8,
//           pos + 9,
//         |],
//         [|
//           res + 0,
//           res + 1,
//           res + 2,
//           res + 3,
//           res + 4,
//           res + 5,
//           res + 6,
//           res + 7,
//           res + 8,
//           res + 9,
//         |],
//       );
//     };
//   | Align2(mem, f) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       (
//         11,
//         [|5555, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//         [|6666, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//       );
//     } else {
//       let (out1s, out1p, out1r) = fib2(~width, ~pos, f);
//       let out = Js.Unsafe.fun_call(layout_map, [|Js.Unsafe.inject(out1r)|]);
//       //let out = out1r;
//       mem := gensym^;
//       (out1s, out1p, out);
//     };
//   | Annot2(mem, _ann, f) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       (
//         11,
//         [|77770, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//         [|88788, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//       );
//     } else {
//       let (out1s, out1p, out1r) = fib2(~width, ~pos, f);
//       let out = Js.Unsafe.fun_call(layout_map, [|Js.Unsafe.inject(out1r)|]);
//       //let out = out1r;
//       mem := gensym^;
//       (out1s, out1p, out);
//     };
//   | Cat2(mem, f1, f2) =>
//     // TODO: maybe without memoization?
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       (
//         11,
//         [|999999990, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//         [|121212120, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//       );
//     } else {
//       let (out1s, out1p, out1r) = fib2(~width, ~pos, f1);
//       let _ = fib2(~width, ~pos, f2);
//       // let (out_s, out_r) =
//       //   Js.Unsafe.fun_call(
//       //     res_inc',
//       //     [|
//       //       Js.Unsafe.inject(out1s),
//       //       Js.Unsafe.inject(out1s),
//       //       Js.Unsafe.inject(out1r),
//       //     |],
//       //   );
//       mem := gensym^;
//       (out1s, out1p, out1r);
//     };
//   | Choice2(mem, f1, f2) =>
//     let old_mem = mem^;
//     if (old_mem == gensym^) {
//       (
//         11,
//         [|131313130, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//         [|141414140, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
//       );
//     } else {
//       let (out1s, out1p, out1r) = fib2(~width, ~pos, f1);
//       let (_out2s, _out2p, _out2r) = fib2(~width, ~pos, f2);
//       let out = Js.Unsafe.fun_call(layout_map, [|Js.Unsafe.inject(out1r)|]);
//       // let (out_s, out_p, out_r) =
//       //   Js.Unsafe.fun_call(
//       //     merge,
//       //     [|
//       //       Js.Unsafe.inject(out1s),
//       //       Js.Unsafe.inject(out1p),
//       //       Js.Unsafe.inject(out1r),
//       //       Js.Unsafe.inject(out2s),
//       //       Js.Unsafe.inject(out2p),
//       //       Js.Unsafe.inject(out2r),
//       //     |],
//       //   );
//       mem := gensym^;
//       (out1s, out1p, out1r);
//     };
//   };
// };

//////////

// Parallel with external size

// 29ns
// 34-36ns (complete layout_map)

// Omits pos argument
let layout_map_align: (int, Array.t(Layout.t(unit))) => Array.t(Layout.t(unit)) =
  Js.Unsafe.js_expr(
    "function layout_map_imp(size, input) {
      //var js_size = size + 1 | 0;
      //console.log(\"size: %o\", size);
      //console.log(\"input: %o\", input);
      //throw 5;
      var output = new Array(size);
      output[0] = 0;
      for (var i =  1; i < size; i++) {
        var res = input[i]
        output[i] = Align_share(res);
      }
      //return [0, size, output];
      return output;
      //\"layout_map_imp\";
    }
    ",
  );

let layout_map_annot: (unit, int, Array.t(Layout.t(unit))) => Array.t(Layout.t(unit)) =
  Js.Unsafe.js_expr(
    "function layout_map_imp(annot, size, input) {
      //var js_size = size + 1 | 0;
      //console.log(\"size: %o\", size);
      //console.log(\"input: %o\", input);
      //throw 5;
      var output = new Array(size);
      output[0] = 0;
      for (var i =  1; i < size; i++) {
        var res = input[i]
        output[i] = Annot_share(annot, res); // TODO: apply wrapper
      }
      //return [0, size, output];
      return output;
      //\"layout_map_imp\";
    }
    ",
  );

let layout_merge:
  (int, Array.t(int), Array.t(int), Array.t(Layout.t(unit)), int, Array.t(int), Array.t(int), Array.t(Layout.t(unit))) =>
  (int, Array.t(int), Array.t(int), Array.t(Layout.t(unit))) =
  Js.Unsafe.js_expr(
    "function layout_merge_imp(size1, pos1, cost1, res1, size2, pos2, cost2, res2) {
      //\"layout_merge\";
    var js_size = size1; // |0 (37-39)
    var end = size1 + size2 | 0;
    end = end - 1 | 0;
    var pos = new Array(end);
    pos[0] = 0;
    var cost = new Array(end);
    pos[0] = 0;
    var res = new Array(end);
    res[0] = 0;
    var i = 1;
    var i1 = 1;
    var i2 = 1;
    while (i1 < size1 && i2 < size2) {
      if (pos1[i1] < pos2[i2]) {
        //console.log(\"1\");
        pos[i] = pos1[i1];
        res[i] = res1[i1]; // TODO: wrap?
        i1 = i1 + 1 | 0;
      } else if (pos1[i1] > pos2[i2]) {
        //console.log(\"2\");
        pos[i] = pos2[i2];
        res[i] = res2[i2]; // TODO: wrap?
        i2 = i2 + 1 | 0;
      } else {
        //console.log(\"3\");
        // TODO: res1[i1] <=> res2[i2]
        // Note: this makes choice be left biased
        if (cost1[i1] <= cost2[i2]) {
          pos[i] = pos1[i1];
          cost[i] = cost1[i1];
          res[i] = res1[i1]; // TODO: wrap?
        } else {
          pos[i] = pos2[i2];
          cost[i] = cost2[i2];
          res[i] = res2[i2]; // TODO: wrap?
        }
        i1 = i1 + 1 | 0;
        i2 = i2 + 1 | 0;
      }
      i = i + 1 | 0;
    }
    while (i1 < size1) {
      pos[i] = pos1[i1];
      res[i] = res1[i1]; // TODO: wrap?
      i++;
      i1++; // TODO: |0
    }
    while (i2 < size2) {
      pos[i] = pos2[i2];
      res[i] = res2[i2]; // TODO: wrap?
      i++;
      i2++;
    }
    //pos.length = js_size;
    // res.length = js_size;
    //res[0] = [0];
    // var i1 = 1;
    // var i2 = 1;
    // var i = 1;
    // while (i1 < js_size1 && i2 < js_size2) {
    //   if (pos1[i1] < pos2[i2]) {
    //     //console.log(\"1\");
    //     pos[i] = pos1[i1];
    //     res[i] = res1[i1] + 1 | 0;
    //     i1 = i1 + 1 | 0;
    //   } else if (pos1[i1] > pos2[i2]) {
    //     //console.log(\"2\");
    //     pos[i] = pos2[i2];
    //     res[i] = res2[i2] + 1 | 0;
    //     i2 = i2 + 1 | 0;
    //   } else {
    //     //console.log(\"3\");
    //     // TODO: res1[i1] <=> res2[i2]
    //     pos[i] = pos1[i1];
    //     res[i] = res1[i1] + 1 | 0;
    //     i1 = i1 + 1 | 0;
    //     i2 = i2 + 1 | 0;
    //   }
    //   i = i + 1 | 0;
    // }
    // while (i1 < js_size1) {
    //     //pos[i] = pos1[i1];
    //     res[i] = res1[i1] + 1 | 0;
    //     i1 = i1 + 1 | 0;
    //     i = i + 1 | 0;
    // }
    // while (i2 < js_size2) {
    //     //pos[i] = pos2[i2];
    //     res[i] = res2[i2] + 1 | 0;
    //     i2 = i2 + 1 | 0;
    //     i = i + 1 | 0;
    // }
    //var size = i;
    //return [0, size, pos, res];
    // TODO: compute real costs
    return [0, size1, pos, cost, res];
  }
    ",
  );

let layout_fold:
  (int, int, my_fib, int, Array.t(int), Array.t(int), Array.t(Layout.t(unit))) =>
  (int, Array.t(int), Array.t(int), Array.t(Layout.t(unit))) =
  Js.Unsafe.js_expr(
    "function layout_fold_imp(width, pos, f2, size1, pos1, cost1, res1) {
      //\"layout_fold\";
      if (size1 == 1) { return [0, 1, [0], [0], [0]]; }
      var xxx = fib2_share(width, pos1[1], f2);
      var i = 2;
      while (i < size1) {
        var p = pos1[i];
        var yyy = fib2_share(width, p, f2);
        xxx = layout_merge_share(xxx[1], xxx[2], xxx[3], xxx[4], yyy[1], yyy[2], yyy[3], yyy[4]);
        i = i + 1 | 0;
      }
      return xxx;
    }
    ",
  );

let rec fib2 =
        (~width: int, ~pos: int, x: my_fib)
        : (int, Array.t(int), Array.t(int), Array.t(Layout.t(unit))) => {
  count := count^ + 1;
  switch (x) {
  | Text2(mem, i) =>
    // TODO: without memoization
    let old_mem = mem^;
    if (old_mem == gensym^) {
      (
        11,
        [|1111, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
        [|0, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
        //[|2222, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
        [|
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
        |],
      );
    } else {
      let res = 0;
      let t = "Z";
      mem := gensym^;
      (
        11,
        [|
          pos + 0,
          pos + 1,
          pos + 2,
          pos + 3,
          pos + 4,
          pos + 5,
          pos + 6,
          pos + 7,
          pos + 8,
          pos + 9,
        |],
        [|0, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
        [|
          Layout.Text(t),
          Layout.Text(t),
          Layout.Text(t),
          Layout.Text(t),
          Layout.Text(t),
          Layout.Text(t),
          Layout.Text(t),
          Layout.Text(t),
          Layout.Text(t),
          Layout.Text(t),
          // 33331,
          // res + 1,
          // res + 2,
          // res + 3,
          // res + 4,
          // res + 5,
          // res + 6,
          // res + 7,
          // res + 8,
          // res + 9,
        |],
      );
    };
  | Fail2(mem) =>
    // TODO: fail without memoization
    let old_mem = mem^;
    if (old_mem == gensym^) {
      (
        11,
        [|3333, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
        [|0, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
        // [|4444, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
        [|
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
        |],
      );
    } else {
      let res = 1;
      let t = "Z";
      mem := gensym^;
      (
        11,
        [|
          pos + 0,
          pos + 1,
          pos + 2,
          pos + 3,
          pos + 4,
          pos + 5,
          pos + 6,
          pos + 7,
          pos + 8,
          pos + 9,
        |],
        [|0, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
        [|
          Layout.Text(t),
          Layout.Text(t),
          Layout.Text(t),
          Layout.Text(t),
          Layout.Text(t),
          Layout.Text(t),
          Layout.Text(t),
          Layout.Text(t),
          Layout.Text(t),
          Layout.Text(t),
          // res + 0,
          // res + 1,
          // res + 2,
          // res + 3,
          // res + 4,
          // res + 5,
          // res + 6,
          // res + 7,
          // res + 8,
          // res + 9,
        |],
      );
    };
  | Align2(mem, f) =>
    let old_mem = mem^;
    if (old_mem == gensym^) {
      (
        11,
        [|5555, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
        [|0, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
        // [|6666, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
        [|
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
        |],
      );
    } else {
      let (out1s, out1p, out1c, out1r) = fib2(~width, ~pos, f);
      let out =
        Js.Unsafe.fun_call(
          layout_map_align,
          [|Js.Unsafe.inject(out1s), Js.Unsafe.inject(out1r)|],
        );
      // let out = out1r;
      mem := gensym^;
      (out1s, out1p, out1c, out);
    };
  | Annot2(mem, annot, f) =>
    let old_mem = mem^;
    if (old_mem == gensym^) {
      (
        11,
        [|77770, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
        [|0, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
        // [|88788, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
        [|
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
        |],
      );
    } else {
      let (out1s, out1p, out1c, out1r) = fib2(~width, ~pos, f);
      let out =
        Js.Unsafe.fun_call(
          layout_map_annot,
          [|Js.Unsafe.inject(annot), Js.Unsafe.inject(out1s), Js.Unsafe.inject(out1r)|],
        );
      // let out = out1r;
      mem := gensym^;
      (out1s, out1p, out1c, out);
    };
  | Cat2(mem, f1, f2) =>
    // TODO: maybe without memoization?
    let old_mem = mem^;
    if (old_mem == gensym^) {
      (
        11,
        [|999999990, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
        [|0, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
        // [|121212120, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
        [|
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
        |],
      );
    } else {
      let (out1s, out1p, out1c, out1r) = fib2(~width, ~pos, f1);
      // let _ = fib2(~width, ~pos, f2);
      let (out_s, out_p, out_c, out_r) =
        Js.Unsafe.fun_call(
          layout_fold,
          [|
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
      (out_s, out_p, out_c, out_r);
    };
  | Choice2(mem, f1, f2) =>
    let old_mem = mem^;
    if (old_mem == gensym^) {
      (
        11,
        [|131313130, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
        [|0, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
        // [|141414140, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
        [|
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
          Layout.Text("Z"),
        |],
      );
    } else {
      let (out1s, out1p, out1c, out1r) = fib2(~width, ~pos, f1);
      let (out2s, out2p, out2c, out2r) = fib2(~width, ~pos, f2);
      //let out = Js.Unsafe.fun_call(layout_map, [|Js.Unsafe.inject(out1r)|]);
      //let out = out1r;
      // let (out_s, out_p, out_r) = (out1s, out1p, out1r);
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
      (out_s, out_p, out_c, out_r);
      // (out1s, out1p, out1r);
    };
  };
};

let _ = Js.export("Align_share", fun (x) => Layout.Align(x));
let _ = Js.export("Annot_share", fun (x, y) => Layout.Annot(x, y));
let _ = Js.export("fib2_share", fib2);
let _ = Js.export("layout_merge_share", layout_merge);

// let rec fib = (x: int): int => {
//   if (x < 2) { 1 }
//   else { 1 + fib(x-1) + fib(x-2) }
// }

// avg:   3.3ms   per count:  15.9ns (count: 206668)
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

let fast_layout_of_doc =
    (_doc: Doc.t('annot), ~width: int, ~pos: int): option(Layout.t('annot)) => {
  //let _l: list((int, (Cost.t, Layout.t('annot)))) =
  //  Obj.magic(fast_layout_of_doc'(Obj.magic(doc), ~width, ~pos));
  //Some(snd(snd(List.hd(l))));
  //count := fib(Int1(25));
  //Printf.printf("fast_layout_of_doc\n");
  gensym := gensym^ + 1;
  ignore(fib2(fib_rec_25, ~width, ~pos));
  None;
};

let orig_layout_of_doc =
    (_doc: Doc.t('annot), ~width: int, ~pos: int): option(Layout.t('annot)) => {
  //let _l: list((int, (Cost.t, Layout.t('annot)))) =
  //  Obj.magic(fast_layout_of_doc'(Obj.magic(doc), ~width, ~pos));
  //Some(snd(snd(List.hd(l))));
  //count := fib(Int1(25));
  gensym := gensym^ + 1;
  ignore(fast_layout_of_doc(Obj.magic(fib_orig_rec_25), ~width, ~pos));
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

// new (10 per): avg:  48.1ms   per count: 101.8ns (count: 472384)
// new (2 per): avg:  9.9ms   per count:  41.9ns (count: 236192)
// orig (1 per): avg:  52.8ms   per count: 255.6ns (count: 206668)
// orig (2 per (via take)): avg: 1067.7ms   per count: 339.4ns (count: 3145725)
