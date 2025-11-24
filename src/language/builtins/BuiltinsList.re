module Fresh = IdTagged.FreshGrammar;
open Fresh.Typ;
open BuiltinsUtil;
open BuiltinsADT;

// NOTE[Matt]: All Hazel builtin functions MUST BE FIXPOINTS even if they are not recursive
// to ensure that the environments are handled correctly.

let builtins =
  [
    {
      str: {|fix length -> fun xs -> case xs
               | [] => 0
               | x :: xs => 1 + length(xs)
             end|},
      name: "length",
      arg: List(unknown(Internal)),
      ret: Atom(Int),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("length"),
              fn(
                Pat.var("xs"),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), int(0)),
                    (
                      Pat.cons(Pat.wild(), Pat.var("xs")),
                      bin_op(
                        Int(Plus),
                        int(1),
                        ap(Forward, var("length"), var("xs")),
                      ),
                    ),
                  ],
                ),
                None,
                Some("length+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix map -> fun xs f -> case xs
               | [] => []
               | x :: xs => f(x) :: map(f, xs)
             end|},
      name: "map",
      arg:
        Prod([
          list(unknown(Internal)),
          arrow(unknown(Internal), unknown(Internal)),
        ]),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("map"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("f")]),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), list_lit([])),
                    (
                      Pat.cons(Pat.var("x"), Pat.var("xs")),
                      cons(
                        ap(Forward, var("f"), var("x")),
                        ap(
                          Forward,
                          var("map"),
                          tuple([var("xs"), var("f")]),
                        ),
                      ),
                    ),
                  ],
                ),
                None,
                Some("map+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix filter -> fun xs f -> case xs
             | [] => []
             | x :: xs => if f(x) then x :: filter(f, xs) else filter(f, xs)
           end|},
      name: "filter",
      arg:
        Prod([list(unknown(Internal)), arrow(unknown(Internal), bool())]),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("filter"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("f")]),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), list_lit([])),
                    (
                      Pat.cons(Pat.var("x"), Pat.var("xs")),
                      if_(
                        ap(Forward, var("f"), var("x")),
                        cons(
                          var("x"),
                          ap(
                            Forward,
                            var("filter"),
                            tuple([var("xs"), var("f")]),
                          ),
                        ),
                        ap(
                          Forward,
                          var("filter"),
                          tuple([var("xs"), var("f")]),
                        ),
                      ),
                    ),
                  ],
                ),
                None,
                Some("filter+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix fold_left -> fun xs f acc -> case xs
             | [] => acc
             | x :: xs => fold_left(xs, f, f(acc, x))
           end|},
      name: "fold_left",
      arg:
        Prod([
          list(unknown(Internal)),
          arrow(
            prod([unknown(Internal), unknown(Internal)]),
            unknown(Internal),
          ),
          unknown(Internal),
        ]),
      ret: Typ.term_of(unknown(Internal)),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("fold_left"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("f"), Pat.var("acc")]),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), var("acc")),
                    (
                      Pat.cons(Pat.var("x"), Pat.var("xs")),
                      ap(
                        Forward,
                        var("fold_left"),
                        tuple([
                          var("xs"),
                          var("f"),
                          ap(
                            Forward,
                            var("f"),
                            tuple([var("acc"), var("x")]),
                          ),
                        ]),
                      ),
                    ),
                  ],
                ),
                None,
                Some("fold_left+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix flat_map -> fun (xs, f) -> fold_left(xs, fun (acc,x) -> acc @ f(x), []))|},
      name: "flat_map",
      arg:
        Prod([
          list(unknown(Internal)),
          arrow(unknown(Internal), list(unknown(Internal))),
        ]),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("flat_map"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("f")]),
                ap(
                  Forward,
                  var("fold_left"),
                  tuple([
                    var("xs"),
                    fn(
                      Pat.tuple([Pat.var("acc"), Pat.var("x")]),
                      Exp.list_concat(
                        var("acc"),
                        ap(Forward, var("f"), var("x")),
                      ),
                      None,
                      None,
                    ),
                    list_lit([]),
                  ]),
                ),
                None,
                Some("flat_map+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix zip -> fun (xs, ys) -> case (xs, ys)
             | [], _ => []
             | _, [] => []
             | (x :: xs, y :: ys) => (x,y) :: zip(xs,ys)
           end|},
      name: "zip",
      arg: Prod([list(unknown(Internal)), list(unknown(Internal))]),
      ret: List(prod([unknown(Internal), unknown(Internal)])),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("zip"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("ys")]),
                match(
                  tuple([var("xs"), var("ys")]),
                  [
                    (
                      Pat.tuple([Pat.list_lit([]), Pat.wild()]),
                      list_lit([]),
                    ),
                    (
                      Pat.tuple([Pat.wild(), Pat.list_lit([])]),
                      list_lit([]),
                    ),
                    (
                      Pat.tuple([
                        Pat.cons(Pat.var("x"), Pat.var("xs")),
                        Pat.cons(Pat.var("y"), Pat.var("ys")),
                      ]),
                      cons(
                        tuple([var("x"), var("y")]),
                        ap(
                          Forward,
                          var("zip"),
                          tuple([var("xs"), var("ys")]),
                        ),
                      ),
                    ),
                  ],
                ),
                None,
                Some("zip+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix unzip -> fun xs -> case xs
             |[]=> ([], [])
             | ((a,b) :: xs) => let (as, bs) = unzip(xs) in ((a :: as), (b ::bs))
           end|},
      name: "unzip",
      arg: List(prod([unknown(Internal), unknown(Internal)])),
      ret: Prod([list(unknown(Internal)), list(unknown(Internal))]),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("unzip"),
              fn(
                Pat.var("xs"),
                match(
                  var("xs"),
                  [
                    (
                      Pat.list_lit([]),
                      tuple([list_lit([]), list_lit([])]),
                    ),
                    (
                      Pat.cons(
                        Pat.tuple([Pat.var("a"), Pat.var("b")]),
                        Pat.var("xs"),
                      ),
                      let_(
                        Pat.tuple([Pat.var("as"), Pat.var("bs")]),
                        ap(Forward, var("unzip"), var("xs")),
                        tuple([
                          cons(var("a"), var("as")),
                          cons(var("b"), var("bs")),
                        ]),
                      ),
                    ),
                  ],
                ),
                None,
                Some("unzip+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix reverse -> fun xs -> case xs
             | [] => []
             | x :: xs => reverse(xs) @ [x]
           end|},
      /* Faster (possibly) than tail-rec variant in medium-length cases since @ is builtin */
      name: "reverse",
      arg: List(unknown(Internal)),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("reverse"),
              fn(
                Pat.var("xs"),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), list_lit([])),
                    (
                      Pat.cons(Pat.var("x"), Pat.var("xs")),
                      list_concat(
                        ap(Forward, var("reverse"), var("xs")),
                        list_lit([var("x")]),
                      ),
                    ),
                  ],
                ),
                None,
                Some("reverse+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix take -> fun (xs, n) ->
             if n <= 0 then []
             else case xs
             | [] => []
             | x :: xs => x :: take(xs, n - 1)
           end|},
      name: "take",
      arg: Prod([list(unknown(Internal)), int()]),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("take"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("n")]),
                if_(
                  bin_op(Int(LessThanOrEqual), var("n"), int(0)),
                  list_lit([]),
                  match(
                    var("xs"),
                    [
                      (Pat.list_lit([]), list_lit([])),
                      (
                        Pat.cons(Pat.var("x"), Pat.var("xs")),
                        cons(
                          var("x"),
                          ap(
                            Forward,
                            var("take"),
                            tuple([
                              var("xs"),
                              bin_op(Int(Minus), var("n"), int(1)),
                            ]),
                          ),
                        ),
                      ),
                    ],
                  ),
                ),
                None,
                Some("take+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix drop -> fun (xs, n) ->
             if n <= 0 then xs else case xs
             | [] => []
             | x :: xs => drop(xs, n - 1)
           end|},
      name: "drop",
      arg: Prod([list(unknown(Internal)), int()]),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("drop"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("n")]),
                if_(
                  bin_op(Int(LessThanOrEqual), var("n"), int(0)),
                  var("xs"),
                  match(
                    var("xs"),
                    [
                      (Pat.list_lit([]), list_lit([])),
                      (
                        Pat.cons(Pat.var("x"), Pat.var("xs")),
                        ap(
                          Forward,
                          var("drop"),
                          tuple([
                            var("xs"),
                            bin_op(Int(Minus), var("n"), int(1)),
                          ]),
                        ),
                      ),
                    ],
                  ),
                ),
                None,
                Some("drop+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix range -> fun (start, end) -> if start > end then [] else start :: range(start + 1, end)|},
      name: "range",
      arg: Prod([int(), int()]),
      ret: List(int()),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("range"),
              fn(
                Pat.tuple([Pat.var("start"), Pat.var("end")]),
                if_(
                  bin_op(Int(GreaterThan), var("start"), var("end")),
                  list_lit([]),
                  cons(
                    var("start"),
                    ap(
                      Forward,
                      var("range"),
                      tuple([
                        bin_op(Int(Plus), var("start"), int(1)),
                        var("end"),
                      ]),
                    ),
                  ),
                ),
                None,
                Some("range+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix enumerate ->
             fun xs -> (fix enumerate_helper -> fun (xs, index) -> case xs
             | [] => []
             | x :: xs => (index, x) :: enumerate_helper((xs, index + 1))
           end)((xs, 0))
           |},
      name: "enumerate",
      arg: List(unknown(Internal)),
      ret: List(prod([int(), unknown(Internal)])),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("enumerate"),
              fn(
                Pat.var("xs"),
                ap(
                  Forward,
                  fix_f(
                    Pat.var("enumerate_helper"),
                    fn(
                      Pat.tuple([Pat.var("xs"), Pat.var("index")]),
                      match(
                        var("xs"),
                        [
                          (Pat.list_lit([]), list_lit([])),
                          (
                            Pat.cons(Pat.var("x"), Pat.var("xs")),
                            cons(
                              tuple([var("index"), var("x")]),
                              ap(
                                Forward,
                                var("enumerate_helper"),
                                tuple([
                                  var("xs"),
                                  bin_op(Int(Plus), var("index"), int(1)),
                                ]),
                              ),
                            ),
                          ),
                        ],
                      ),
                      None,
                      None,
                    ),
                    None,
                  ),
                  tuple([var("xs"), int(0)]),
                ),
                None,
                Some("enumerate+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix any -> fun (xs, pred) -> case xs
             | [] => false
             | x :: xs => if pred(x) then true else any(xs, pred)
           end|},
      name: "any",
      arg:
        Prod([list(unknown(Internal)), arrow(unknown(Internal), bool())]),
      ret: Atom(Bool),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("any"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("pred")]),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), bool(false)),
                    (
                      Pat.cons(Pat.var("x"), Pat.var("xs")),
                      if_(
                        ap(Forward, var("pred"), var("x")),
                        bool(true),
                        ap(
                          Forward,
                          var("any"),
                          tuple([var("xs"), var("pred")]),
                        ),
                      ),
                    ),
                  ],
                ),
                None,
                Some("any+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix all -> fun (xs, pred) -> case xs
             | [] => true
             | x :: xs => if pred(x) then all(xs, pred) else false
           end|},
      name: "all",
      arg:
        Prod([list(unknown(Internal)), arrow(unknown(Internal), bool())]),
      ret: Atom(Bool),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("all"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("pred")]),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), bool(true)),
                    (
                      Pat.cons(Pat.var("x"), Pat.var("xs")),
                      if_(
                        ap(Forward, var("pred"), var("x")),
                        ap(
                          Forward,
                          var("all"),
                          tuple([var("xs"), var("pred")]),
                        ),
                        bool(false),
                      ),
                    ),
                  ],
                ),
                None,
                Some("all+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix intersperse -> fun (xs, sep) -> case xs
             | [] => []
             | [x] => [x]
             | x :: xs => x :: sep :: intersperse(xs, sep)
           end|},
      name: "intersperse",
      arg: Prod([list(unknown(Internal)), unknown(Internal)]),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("intersperse"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("sep")]),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), list_lit([])),
                    (Pat.list_lit([Pat.var("x")]), list_lit([var("x")])),
                    (
                      Pat.cons(Pat.var("x"), Pat.var("xs")),
                      cons(
                        var("x"),
                        cons(
                          var("sep"),
                          ap(
                            Forward,
                            var("intersperse"),
                            tuple([var("xs"), var("sep")]),
                          ),
                        ),
                      ),
                    ),
                  ],
                ),
                None,
                Some("intersperse+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix cons -> fun (x, xs) -> x :: xs|},
      name: "cons",
      arg: Prod([unknown(Internal), list(unknown(Internal))]),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("cons"),
              fn(
                Pat.tuple([Pat.var("x"), Pat.var("xs")]),
                cons(var("x"), var("xs")),
                None,
                Some("cons+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix hd -> fun xs -> case xs
             | [] => error("hd: empty list")
             | x :: _ => x
           end|},
      name: "hd",
      arg: List(unknown(Internal)),
      ret: Unknown(Internal),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("hd"),
              fn(
                Pat.var("xs"),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), undefined()),
                    (Pat.cons(Pat.var("x"), Pat.wild()), var("x")),
                  ],
                ),
                None,
                Some("hd+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix tl -> fun xs -> case xs
             | [] => error("tl: empty list")
             | _ :: xs => xs
           end|},
      name: "tl",
      arg: List(unknown(Internal)),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("tl"),
              fn(
                Pat.var("xs"),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), undefined()),
                    (Pat.cons(Pat.wild(), Pat.var("xs")), var("xs")),
                  ],
                ),
                None,
                Some("tl+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix is_empty -> fun xs -> case xs
             | [] => true
             | _ => false
           end|},
      name: "is_empty",
      arg: List(unknown(Internal)),
      ret: Atom(Bool),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("is_empty"),
              fn(
                Pat.var("xs"),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), bool(true)),
                    (Pat.wild(), bool(false)),
                  ],
                ),
                None,
                Some("is_empty+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix nth -> fun (xs, n) -> case xs
             | [] => error("nth: index out of bounds")
             | x :: xs => if n == 0 then x else nth(xs, n - 1)
           end|},
      name: "nth",
      arg: Prod([list(unknown(Internal)), int()]),
      ret: Unknown(Internal),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("nth"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("n")]),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), undefined()),
                    (
                      Pat.cons(Pat.var("x"), Pat.var("xs")),
                      if_(
                        bin_op(Poly(Equals), var("n"), int(0)),
                        var("x"),
                        ap(
                          Forward,
                          var("nth"),
                          tuple([
                            var("xs"),
                            bin_op(Int(Minus), var("n"), int(1)),
                          ]),
                        ),
                      ),
                    ),
                  ],
                ),
                None,
                Some("nth+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix fold_right -> fun (xs, f, acc) -> case xs
             | [] => acc
             | x :: xs => f(x, fold_right(xs, f, acc))
           end|},
      name: "fold_right",
      arg:
        Prod([
          list(unknown(Internal)),
          arrow(
            prod([unknown(Internal), unknown(Internal)]),
            unknown(Internal),
          ),
          unknown(Internal),
        ]),
      ret: Unknown(Internal),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("fold_right"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("f"), Pat.var("acc")]),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), var("acc")),
                    (
                      Pat.cons(Pat.var("x"), Pat.var("xs")),
                      ap(
                        Forward,
                        var("f"),
                        tuple([
                          var("x"),
                          ap(
                            Forward,
                            var("fold_right"),
                            tuple([var("xs"), var("f"), var("acc")]),
                          ),
                        ]),
                      ),
                    ),
                  ],
                ),
                None,
                Some("fold_right+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix append -> fun (xs, ys) -> xs @ ys|},
      name: "append",
      arg: Prod([list(unknown(Internal)), list(unknown(Internal))]),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("append"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("ys")]),
                list_concat(var("xs"), var("ys")),
                None,
                Some("append+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix concat -> fun xss -> case xss
             | [] => []
             | xs :: xss => xs @ concat(xss)
           end|},
      name: "concat",
      arg: List(list(unknown(Internal))),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("concat"),
              fn(
                Pat.var("xss"),
                match(
                  var("xss"),
                  [
                    (Pat.list_lit([]), list_lit([])),
                    (
                      Pat.cons(Pat.var("xs"), Pat.var("xss")),
                      list_concat(
                        var("xs"),
                        ap(Forward, var("concat"), var("xss")),
                      ),
                    ),
                  ],
                ),
                None,
                Some("concat+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix mapi -> fun xs f -> mapi_helper(xs, f, 0)
           fix mapi_helper -> fun (xs, f, i) -> case xs
             | [] => []
             | x :: xs => f(i, x) :: mapi_helper(xs, f, i + 1)
           end|},
      name: "mapi",
      arg:
        Prod([
          list(unknown(Internal)),
          arrow(prod([int(), unknown(Internal)]), unknown(Internal)),
        ]),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("mapi"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("f")]),
                ap(
                  Forward,
                  fix_f(
                    Pat.var("mapi_helper"),
                    fn(
                      Pat.tuple([
                        Pat.var("xs"),
                        Pat.var("f"),
                        Pat.var("i"),
                      ]),
                      match(
                        var("xs"),
                        [
                          (Pat.list_lit([]), list_lit([])),
                          (
                            Pat.cons(Pat.var("x"), Pat.var("xs")),
                            cons(
                              ap(
                                Forward,
                                var("f"),
                                tuple([var("i"), var("x")]),
                              ),
                              ap(
                                Forward,
                                var("mapi_helper"),
                                tuple([
                                  var("xs"),
                                  var("f"),
                                  bin_op(Int(Plus), var("i"), int(1)),
                                ]),
                              ),
                            ),
                          ),
                        ],
                      ),
                      None,
                      None,
                    ),
                    None,
                  ),
                  tuple([var("xs"), var("f"), int(0)]),
                ),
                None,
                Some("mapi+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix filteri -> fun xs f -> filteri_helper(xs, f, 0)
           fix filteri_helper -> fun (xs, f, i) -> case xs
             | [] => []
             | x :: xs => if f(i, x) then x :: filteri_helper(xs, f, i + 1) else filteri_helper(xs, f, i + 1)
           end|},
      name: "filteri",
      arg:
        Prod([
          list(unknown(Internal)),
          arrow(prod([int(), unknown(Internal)]), bool()),
        ]),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("filteri"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("f")]),
                ap(
                  Forward,
                  fix_f(
                    Pat.var("filteri_helper"),
                    fn(
                      Pat.tuple([
                        Pat.var("xs"),
                        Pat.var("f"),
                        Pat.var("i"),
                      ]),
                      match(
                        var("xs"),
                        [
                          (Pat.list_lit([]), list_lit([])),
                          (
                            Pat.cons(Pat.var("x"), Pat.var("xs")),
                            if_(
                              ap(
                                Forward,
                                var("f"),
                                tuple([var("i"), var("x")]),
                              ),
                              cons(
                                var("x"),
                                ap(
                                  Forward,
                                  var("filteri_helper"),
                                  tuple([
                                    var("xs"),
                                    var("f"),
                                    bin_op(Int(Plus), var("i"), int(1)),
                                  ]),
                                ),
                              ),
                              ap(
                                Forward,
                                var("filteri_helper"),
                                tuple([
                                  var("xs"),
                                  var("f"),
                                  bin_op(Int(Plus), var("i"), int(1)),
                                ]),
                              ),
                            ),
                          ),
                        ],
                      ),
                      None,
                      Some("filteri_helper+"),
                    ),
                    None,
                  ),
                  tuple([var("xs"), var("f"), int(0)]),
                ),
                None,
                Some("filteri+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix mem -> fun (xs, x) -> any(xs, fun t -> x == t)|},
      name: "mem",
      arg: Prod([list(unknown(Internal)), unknown(Internal)]),
      ret: Atom(Bool),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("mem"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("x")]),
                ap(
                  Forward,
                  var("any"),
                  tuple([
                    var("xs"),
                    fn(
                      Pat.var("t"),
                      bin_op(Poly(Equals), var("x"), var("t")),
                      None,
                      None,
                    ),
                  ]),
                ),
                None,
                Some("mem+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix partition -> fun (xs, pred) -> case xs
             | [] => ([], [])
             | x :: xs => let (trues, falses) = partition(xs, pred) in
               if pred(x) then (x :: trues, falses) else (trues, x :: falses)
           end|},
      name: "partition",
      arg:
        Prod([list(unknown(Internal)), arrow(unknown(Internal), bool())]),
      ret: Prod([list(unknown(Internal)), list(unknown(Internal))]),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("partition"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("pred")]),
                match(
                  var("xs"),
                  [
                    (
                      Pat.list_lit([]),
                      tuple([list_lit([]), list_lit([])]),
                    ),
                    (
                      Pat.cons(Pat.var("x"), Pat.var("xs")),
                      let_(
                        Pat.tuple([Pat.var("trues"), Pat.var("falses")]),
                        ap(
                          Forward,
                          var("partition"),
                          tuple([var("xs"), var("pred")]),
                        ),
                        if_(
                          ap(Forward, var("pred"), var("x")),
                          tuple([
                            cons(var("x"), var("trues")),
                            var("falses"),
                          ]),
                          tuple([
                            var("trues"),
                            cons(var("x"), var("falses")),
                          ]),
                        ),
                      ),
                    ),
                  ],
                ),
                None,
                Some("partition+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix rev_append -> fun (xs, ys) -> case xs
             | [] => ys
             | x :: xs => rev_append(xs, x :: ys)
           end|},
      name: "rev_append",
      arg: Prod([list(unknown(Internal)), list(unknown(Internal))]),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("rev_append"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("ys")]),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), var("ys")),
                    (
                      Pat.cons(Pat.var("x"), Pat.var("xs")),
                      ap(
                        Forward,
                        var("rev_append"),
                        tuple([var("xs"), cons(var("x"), var("ys"))]),
                      ),
                    ),
                  ],
                ),
                None,
                Some("rev_append+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix fold_left2 -> fun (xs, ys, f, acc) -> case (xs, ys)
             | ([], _) => acc
             | (_, []) => acc
             | (x :: xs, y :: ys) => fold_left2(xs, ys, f, f(acc, x, y))
           end|},
      name: "fold_left2",
      arg:
        Prod([
          list(unknown(Internal)),
          list(unknown(Internal)),
          arrow(
            prod([
              unknown(Internal),
              unknown(Internal),
              unknown(Internal),
            ]),
            unknown(Internal),
          ),
          unknown(Internal),
        ]),
      ret: Unknown(Internal),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("fold_left2"),
              fn(
                Pat.tuple([
                  Pat.var("xs"),
                  Pat.var("ys"),
                  Pat.var("f"),
                  Pat.var("acc"),
                ]),
                match(
                  tuple([var("xs"), var("ys")]),
                  [
                    (Pat.tuple([Pat.list_lit([]), Pat.wild()]), var("acc")),
                    (
                      Pat.tuple([Pat.wild(), Pat.list_lit([])]),
                      var("acc"),
                    ),
                    (
                      Pat.tuple([
                        Pat.cons(Pat.var("x"), Pat.var("xs")),
                        Pat.cons(Pat.var("y"), Pat.var("ys")),
                      ]),
                      ap(
                        Forward,
                        var("fold_left2"),
                        tuple([
                          var("xs"),
                          var("ys"),
                          var("f"),
                          ap(
                            Forward,
                            var("f"),
                            tuple([var("acc"), var("x"), var("y")]),
                          ),
                        ]),
                      ),
                    ),
                  ],
                ),
                None,
                Some("fold_left2+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix fold_right2 -> fun (xs, ys, f, acc) -> case (xs, ys)
             | ([], _) => acc
             | (_, []) => acc
             | (x :: xs, y :: ys) => f(x, y, fold_right2(xs, ys, f, acc))
           end|},
      name: "fold_right2",
      arg:
        Prod([
          list(unknown(Internal)),
          list(unknown(Internal)),
          arrow(
            prod([
              unknown(Internal),
              unknown(Internal),
              unknown(Internal),
            ]),
            unknown(Internal),
          ),
          unknown(Internal),
        ]),
      ret: Unknown(Internal),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("fold_right2"),
              fn(
                Pat.tuple([
                  Pat.var("xs"),
                  Pat.var("ys"),
                  Pat.var("f"),
                  Pat.var("acc"),
                ]),
                match(
                  tuple([var("xs"), var("ys")]),
                  [
                    (Pat.tuple([Pat.list_lit([]), Pat.wild()]), var("acc")),
                    (
                      Pat.tuple([Pat.wild(), Pat.list_lit([])]),
                      var("acc"),
                    ),
                    (
                      Pat.tuple([
                        Pat.cons(Pat.var("x"), Pat.var("xs")),
                        Pat.cons(Pat.var("y"), Pat.var("ys")),
                      ]),
                      ap(
                        Forward,
                        var("f"),
                        tuple([
                          var("x"),
                          var("y"),
                          ap(
                            Forward,
                            var("fold_right2"),
                            tuple([
                              var("xs"),
                              var("ys"),
                              var("f"),
                              var("acc"),
                            ]),
                          ),
                        ]),
                      ),
                    ),
                  ],
                ),
                None,
                Some("fold_right2+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix map2 -> fun (xs, ys, f) -> case (xs, ys)
             | ([], _) => []
             | (_, []) => []
             | (x :: xs, y :: ys) => f(x, y) :: map2(xs, ys, f)
           end|},
      name: "map2",
      arg:
        Prod([
          list(unknown(Internal)),
          list(unknown(Internal)),
          arrow(
            prod([unknown(Internal), unknown(Internal)]),
            unknown(Internal),
          ),
        ]),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("map2"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("ys"), Pat.var("f")]),
                match(
                  tuple([var("xs"), var("ys")]),
                  [
                    (
                      Pat.tuple([Pat.list_lit([]), Pat.wild()]),
                      list_lit([]),
                    ),
                    (
                      Pat.tuple([Pat.wild(), Pat.list_lit([])]),
                      list_lit([]),
                    ),
                    (
                      Pat.tuple([
                        Pat.cons(Pat.var("x"), Pat.var("xs")),
                        Pat.cons(Pat.var("y"), Pat.var("ys")),
                      ]),
                      cons(
                        ap(
                          Forward,
                          var("f"),
                          tuple([var("x"), var("y")]),
                        ),
                        ap(
                          Forward,
                          var("map2"),
                          tuple([var("xs"), var("ys"), var("f")]),
                        ),
                      ),
                    ),
                  ],
                ),
                None,
                Some("map2+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix all2 -> fun (xs, ys, pred) -> case (xs, ys)
             | ([], _) => true
             | (_, []) => true
             | (x :: xs, y :: ys) => if pred(x, y) then all2(xs, ys, pred) else false
           end|},
      name: "all2",
      arg:
        Prod([
          list(unknown(Internal)),
          list(unknown(Internal)),
          arrow(prod([unknown(Internal), unknown(Internal)]), bool()),
        ]),
      ret: Atom(Bool),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("all2"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("ys"), Pat.var("pred")]),
                match(
                  tuple([var("xs"), var("ys")]),
                  [
                    (Pat.tuple([Pat.list_lit([]), Pat.wild()]), bool(true)),
                    (
                      Pat.tuple([Pat.wild(), Pat.list_lit([])]),
                      bool(true),
                    ),
                    (
                      Pat.tuple([
                        Pat.cons(Pat.var("x"), Pat.var("xs")),
                        Pat.cons(Pat.var("y"), Pat.var("ys")),
                      ]),
                      if_(
                        ap(
                          Forward,
                          var("pred"),
                          tuple([var("x"), var("y")]),
                        ),
                        ap(
                          Forward,
                          var("all2"),
                          tuple([var("xs"), var("ys"), var("pred")]),
                        ),
                        bool(false),
                      ),
                    ),
                  ],
                ),
                None,
                Some("all2+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix any2 -> fun (xs, ys, pred) -> case (xs, ys)
             | ([], _) => false
             | (_, []) => false
             | (x :: xs, y :: ys) => if pred(x, y) then true else any2(xs, ys, pred)
           end|},
      name: "any2",
      arg:
        Prod([
          list(unknown(Internal)),
          list(unknown(Internal)),
          arrow(prod([unknown(Internal), unknown(Internal)]), bool()),
        ]),
      ret: Atom(Bool),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("any2"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("ys"), Pat.var("pred")]),
                match(
                  tuple([var("xs"), var("ys")]),
                  [
                    (
                      Pat.tuple([Pat.list_lit([]), Pat.wild()]),
                      bool(false),
                    ),
                    (
                      Pat.tuple([Pat.wild(), Pat.list_lit([])]),
                      bool(false),
                    ),
                    (
                      Pat.tuple([
                        Pat.cons(Pat.var("x"), Pat.var("xs")),
                        Pat.cons(Pat.var("y"), Pat.var("ys")),
                      ]),
                      if_(
                        ap(
                          Forward,
                          var("pred"),
                          tuple([var("x"), var("y")]),
                        ),
                        bool(true),
                        ap(
                          Forward,
                          var("any2"),
                          tuple([var("xs"), var("ys"), var("pred")]),
                        ),
                      ),
                    ),
                  ],
                ),
                None,
                Some("any2+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix find -> fun (xs, pred) -> case xs
             | [] => error("find: element not found")
             | x :: xs => if pred(x) then x else find(xs, pred)
           end|},
      name: "find",
      arg:
        Prod([list(unknown(Internal)), arrow(unknown(Internal), bool())]),
      ret: Unknown(Internal),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("find"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("pred")]),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), undefined()),
                    (
                      Pat.cons(Pat.var("x"), Pat.var("xs")),
                      if_(
                        ap(Forward, var("pred"), var("x")),
                        var("x"),
                        ap(
                          Forward,
                          var("find"),
                          tuple([var("xs"), var("pred")]),
                        ),
                      ),
                    ),
                  ],
                ),
                None,
                Some("find+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix take_while -> fun (xs, pred) -> case xs
             | [] => []
             | x :: xs => if pred(x) then x :: take_while(xs, pred) else []
           end|},
      name: "take_while",
      arg:
        Prod([list(unknown(Internal)), arrow(unknown(Internal), bool())]),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("take_while"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("pred")]),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), list_lit([])),
                    (
                      Pat.cons(Pat.var("x"), Pat.var("xs")),
                      if_(
                        ap(Forward, var("pred"), var("x")),
                        cons(
                          var("x"),
                          ap(
                            Forward,
                            var("take_while"),
                            tuple([var("xs"), var("pred")]),
                          ),
                        ),
                        list_lit([]),
                      ),
                    ),
                  ],
                ),
                None,
                Some("take_while+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix drop_while -> fun (xs, pred) -> case xs
             | [] => []
             | x :: xs => if pred(x) then drop_while(xs, pred) else xs
           end|},
      name: "drop_while",
      arg:
        Prod([list(unknown(Internal)), arrow(unknown(Internal), bool())]),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("drop_while"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("pred")]),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), list_lit([])),
                    (
                      Pat.cons(Pat.var("x"), Pat.var("xs")),
                      if_(
                        ap(Forward, var("pred"), var("x")),
                        ap(
                          Forward,
                          var("drop_while"),
                          tuple([var("xs"), var("pred")]),
                        ),
                        cons(var("x"), var("xs")),
                      ),
                    ),
                  ],
                ),
                None,
                Some("drop_while+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix filter_map -> fun (xs, f) -> case xs
             | [] => []
             | x :: xs => case f(x)
               | None => filter_map(xs, f)
               | Some(y) => y :: filter_map(xs, f)
             end
           end|},
      name: "filter_map",
      arg:
        Prod([
          list(unknown(Internal)),
          arrow(unknown(Internal), unknown(Internal)),
        ]),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("filter_map"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("f")]),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), list_lit([])),
                    (
                      Pat.cons(Pat.var("x"), Pat.var("xs")),
                      match(
                        ap(Forward, var("f"), var("x")),
                        [
                          (
                            Option.pat_none,
                            ap(
                              Forward,
                              var("filter_map"),
                              tuple([var("xs"), var("f")]),
                            ),
                          ),
                          (
                            Pat.ap(Option.pat_some, Pat.var("y")),
                            cons(
                              var("y"),
                              ap(
                                Forward,
                                var("filter_map"),
                                tuple([var("xs"), var("f")]),
                              ),
                            ),
                          ),
                        ],
                      ),
                    ),
                  ],
                ),
                None,
                Some("filter_map+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix nth_opt -> fun (xs, n) -> case xs
             | [] => None
             | x :: xs => if n == 0 then Some(x) else nth_opt(xs, n - 1)
           end|},
      name: "nth_opt",
      arg: Prod([list(unknown(Internal)), int()]),
      ret: Unknown(Internal),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("nth_opt"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("n")]),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), Option.none),
                    (
                      Pat.cons(Pat.var("x"), Pat.var("xs")),
                      if_(
                        bin_op(Poly(Equals), var("n"), int(0)),
                        ap(Forward, Option.some, var("x")),
                        ap(
                          Forward,
                          var("nth_opt"),
                          tuple([
                            var("xs"),
                            bin_op(Int(Minus), var("n"), int(1)),
                          ]),
                        ),
                      ),
                    ),
                  ],
                ),
                None,
                Some("nth_opt+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix find_opt -> fun (xs, pred) -> case xs
             | [] => None
             | x :: xs => if pred(x) then Some(x) else find_opt(xs, pred)
           end|},
      name: "find_opt",
      arg:
        Prod([list(unknown(Internal)), arrow(unknown(Internal), bool())]),
      ret: Unknown(Internal),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("find_opt"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("pred")]),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), Option.none),
                    (
                      Pat.cons(Pat.var("x"), Pat.var("xs")),
                      if_(
                        ap(Forward, var("pred"), var("x")),
                        ap(Forward, Option.some, var("x")),
                        ap(
                          Forward,
                          var("find_opt"),
                          tuple([var("xs"), var("pred")]),
                        ),
                      ),
                    ),
                  ],
                ),
                None,
                Some("find_opt+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix find_index -> fun (xs, pred) -> find_index_helper(xs, pred, 0)
           fix find_index_helper -> fun (xs, pred, i) -> case xs
             | [] => None
             | x :: xs => if pred(x) then Some(i) else find_index_helper(xs, pred, i + 1)
           end|},
      name: "find_index",
      arg:
        Prod([list(unknown(Internal)), arrow(unknown(Internal), bool())]),
      ret: Unknown(Internal),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("find_index"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("pred")]),
                ap(
                  Forward,
                  fix_f(
                    Pat.var("find_index_helper"),
                    fn(
                      Pat.tuple([
                        Pat.var("xs"),
                        Pat.var("pred"),
                        Pat.var("i"),
                      ]),
                      match(
                        var("xs"),
                        [
                          (Pat.list_lit([]), Option.none),
                          (
                            Pat.cons(Pat.var("x"), Pat.var("xs")),
                            if_(
                              ap(Forward, var("pred"), var("x")),
                              ap(Forward, Option.some, var("i")),
                              ap(
                                Forward,
                                var("find_index_helper"),
                                tuple([
                                  var("xs"),
                                  var("pred"),
                                  bin_op(Int(Plus), var("i"), int(1)),
                                ]),
                              ),
                            ),
                          ),
                        ],
                      ),
                      None,
                      Some("find_index_helper+"),
                    ),
                    None,
                  ),
                  tuple([var("xs"), var("pred"), int(0)]),
                ),
                None,
                Some("find_index+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix find_map -> fun (xs, f) -> case xs
             | [] => None
             | x :: xs => case f(x)
               | None => find_map(xs, f)
               | Some(y) => Some(y)
             end
           end|},
      name: "find_map",
      arg:
        Prod([
          list(unknown(Internal)),
          arrow(unknown(Internal), unknown(Internal)),
        ]),
      ret: Unknown(Internal),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("find_map"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("f")]),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), Option.none),
                    (
                      Pat.cons(Pat.var("x"), Pat.var("xs")),
                      match(
                        ap(Forward, var("f"), var("x")),
                        [
                          (
                            Option.pat_none,
                            ap(
                              Forward,
                              var("find_map"),
                              tuple([var("xs"), var("f")]),
                            ),
                          ),
                          (
                            Pat.ap(Option.pat_some, Pat.var("y")),
                            ap(Forward, Option.some, var("y")),
                          ),
                        ],
                      ),
                    ),
                  ],
                ),
                None,
                Some("find_map+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix find_mapi -> fun (xs, f) -> find_mapi_helper(xs, f, 0)
           fix find_mapi_helper -> fun (xs, f, i) -> case xs
             | [] => None
             | x :: xs => case f(i, x)
               | None => find_mapi_helper(xs, f, i + 1)
               | Some(y) => Some(y)
             end
           end|},
      name: "find_mapi",
      arg:
        Prod([
          list(unknown(Internal)),
          arrow(prod([int(), unknown(Internal)]), unknown(Internal)),
        ]),
      ret: Unknown(Internal),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("find_mapi"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("f")]),
                ap(
                  Forward,
                  fix_f(
                    Pat.var("find_mapi_helper"),
                    fn(
                      Pat.tuple([
                        Pat.var("xs"),
                        Pat.var("f"),
                        Pat.var("i"),
                      ]),
                      match(
                        var("xs"),
                        [
                          (Pat.list_lit([]), Option.none),
                          (
                            Pat.cons(Pat.var("x"), Pat.var("xs")),
                            match(
                              ap(
                                Forward,
                                var("f"),
                                tuple([var("i"), var("x")]),
                              ),
                              [
                                (
                                  Option.pat_none,
                                  ap(
                                    Forward,
                                    var("find_mapi_helper"),
                                    tuple([
                                      var("xs"),
                                      var("f"),
                                      bin_op(Int(Plus), var("i"), int(1)),
                                    ]),
                                  ),
                                ),
                                (
                                  Pat.ap(Option.pat_some, Pat.var("y")),
                                  ap(Forward, Option.some, var("y")),
                                ),
                              ],
                            ),
                          ),
                        ],
                      ),
                      None,
                      Some("find_mapi_helper+"),
                    ),
                    None,
                  ),
                  tuple([var("xs"), var("f"), int(0)]),
                ),
                None,
                Some("find_mapi+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix init -> fun (n, f) -> init_helper(n, f, 0)
           fix init_helper -> fun (n, f, i) -> if i >= n then [] else f(i) :: init_helper(n, f, i + 1)
           end|},
      name: "init",
      arg: Prod([int(), arrow(int(), unknown(Internal))]),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("init"),
              fn(
                Pat.tuple([Pat.var("n"), Pat.var("f")]),
                ap(
                  Forward,
                  fix_f(
                    Pat.var("init_helper"),
                    fn(
                      Pat.tuple([
                        Pat.var("n"),
                        Pat.var("f"),
                        Pat.var("i"),
                      ]),
                      if_(
                        bin_op(
                          Int(GreaterThanOrEqual),
                          var("i"),
                          var("n"),
                        ),
                        list_lit([]),
                        cons(
                          ap(Forward, var("f"), var("i")),
                          ap(
                            Forward,
                            var("init_helper"),
                            tuple([
                              var("n"),
                              var("f"),
                              bin_op(Int(Plus), var("i"), int(1)),
                            ]),
                          ),
                        ),
                      ),
                      None,
                      Some("init_helper+"),
                    ),
                    None,
                  ),
                  tuple([var("n"), var("f"), int(0)]),
                ),
                None,
                Some("init+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix assoc -> fun (xs, key) -> case xs
             | [] => error("assoc: key not found")
             | (k, v) :: xs => if k == key then v else assoc(xs, key)
           end|},
      name: "assoc",
      arg:
        Prod([
          list(prod([unknown(Internal), unknown(Internal)])),
          unknown(Internal),
        ]),
      ret: Unknown(Internal),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("assoc"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("key")]),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), undefined()),
                    (
                      Pat.cons(
                        Pat.tuple([Pat.var("k"), Pat.var("v")]),
                        Pat.var("xs"),
                      ),
                      if_(
                        bin_op(Poly(Equals), var("k"), var("key")),
                        var("v"),
                        ap(
                          Forward,
                          var("assoc"),
                          tuple([var("xs"), var("key")]),
                        ),
                      ),
                    ),
                  ],
                ),
                None,
                Some("assoc+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix assoc_opt -> fun (xs, key) -> case xs
             | [] => None
             | (k, v) :: xs => if k == key then Some(v) else assoc_opt(xs, key)
           end|},
      name: "assoc_opt",
      arg:
        Prod([
          list(prod([unknown(Internal), unknown(Internal)])),
          unknown(Internal),
        ]),
      ret: Unknown(Internal),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("assoc_opt"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("key")]),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), Option.none),
                    (
                      Pat.cons(
                        Pat.tuple([Pat.var("k"), Pat.var("v")]),
                        Pat.var("xs"),
                      ),
                      if_(
                        bin_op(Poly(Equals), var("k"), var("key")),
                        ap(Forward, Option.some, var("v")),
                        ap(
                          Forward,
                          var("assoc_opt"),
                          tuple([var("xs"), var("key")]),
                        ),
                      ),
                    ),
                  ],
                ),
                None,
                Some("assoc_opt+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix mem_assoc -> fun (xs, key) -> case xs
             | [] => false
             | (k, _) :: xs => if k == key then true else mem_assoc(xs, key)
           end|},
      name: "mem_assoc",
      arg:
        Prod([
          list(prod([unknown(Internal), unknown(Internal)])),
          unknown(Internal),
        ]),
      ret: Atom(Bool),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("mem_assoc"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("key")]),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), bool(false)),
                    (
                      Pat.cons(
                        Pat.tuple([Pat.var("k"), Pat.wild()]),
                        Pat.var("xs"),
                      ),
                      if_(
                        bin_op(Poly(Equals), var("k"), var("key")),
                        bool(true),
                        ap(
                          Forward,
                          var("mem_assoc"),
                          tuple([var("xs"), var("key")]),
                        ),
                      ),
                    ),
                  ],
                ),
                None,
                Some("mem_assoc+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix remove_assoc -> fun (xs, key) -> case xs
             | [] => []
             | (k, v) :: xs => if k == key then remove_assoc(xs, key) else (k, v) :: remove_assoc(xs, key)
           end|},
      name: "remove_assoc",
      arg:
        Prod([
          list(prod([unknown(Internal), unknown(Internal)])),
          unknown(Internal),
        ]),
      ret: List(prod([unknown(Internal), unknown(Internal)])),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("remove_assoc"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("key")]),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), list_lit([])),
                    (
                      Pat.cons(
                        Pat.tuple([Pat.var("k"), Pat.var("v")]),
                        Pat.var("xs"),
                      ),
                      if_(
                        bin_op(Poly(Equals), var("k"), var("key")),
                        ap(
                          Forward,
                          var("remove_assoc"),
                          tuple([var("xs"), var("key")]),
                        ),
                        cons(
                          tuple([var("k"), var("v")]),
                          ap(
                            Forward,
                            var("remove_assoc"),
                            tuple([var("xs"), var("key")]),
                          ),
                        ),
                      ),
                    ),
                  ],
                ),
                None,
                Some("remove_assoc+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix partition_map -> fun (xs, f) -> case xs
             | [] => ([], [])
             | x :: xs => let (lefts, rights) = partition_map(xs, f) in
               case f(x)
               | Left(y) => (y :: lefts, rights)
               | Right(y) => (lefts, y :: rights)
             end
           end|},
      name: "partition_map",
      arg:
        Prod([
          list(unknown(Internal)),
          arrow(unknown(Internal), unknown(Internal)),
        ]),
      ret: Prod([list(unknown(Internal)), list(unknown(Internal))]),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("partition_map"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("f")]),
                match(
                  var("xs"),
                  [
                    (
                      Pat.list_lit([]),
                      tuple([list_lit([]), list_lit([])]),
                    ),
                    (
                      Pat.cons(Pat.var("x"), Pat.var("xs")),
                      let_(
                        Pat.tuple([Pat.var("lefts"), Pat.var("rights")]),
                        ap(
                          Forward,
                          var("partition_map"),
                          tuple([var("xs"), var("f")]),
                        ),
                        match(
                          ap(Forward, var("f"), var("x")),
                          [
                            (
                              Pat.ap(Either.pat_left, Pat.var("y")),
                              tuple([
                                cons(var("y"), var("lefts")),
                                var("rights"),
                              ]),
                            ),
                            (
                              Pat.ap(Either.pat_right, Pat.var("y")),
                              tuple([
                                var("lefts"),
                                cons(var("y"), var("rights")),
                              ]),
                            ),
                          ],
                        ),
                      ),
                    ),
                  ],
                ),
                None,
                Some("partition_map+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix sort -> fun (cmp, xs) ->
    let merge: ((?, ?) -> Int, [?], [?]) -> [?] =
fun cmp, xs, ys ->
let go: ([?], [?], [?]) -> [?] =
  fun xs, ys, acc ->
    case (xs, ys)
      | [], [] => rev(acc)
        | [], ys => rev_append(acc, ys)
          | xs, [] => rev_append(acc, xs)
            | x::xs, y::ys =>
            case cmp(x, y)
            | Lt => go(xs, y::ys, x::acc)
            | Eq => go(xs, y::ys, x::acc)
            | Gt => go(x::xs, ys, y::acc)
            end in
            go(xs, ys, []) in
      let split: [?] -> ([?], [?]) = fun xs ->
  case xs
  | [] => ([], [])
    | [x] => ([x], [])
      | x::y::ys =>
        let (xs, ys) = split(ys) in
        (x::xs, y::ys)
        end in
        let merge_sort: [?] -> [?] = fun xs ->
        case xs
    | [] => []
    | [x] => [x]
      | _ =>
        let (left, right) = split(xs) in
        merge(cmp, merge_sort(left), merge_sort(right))
        end in
        merge_sort(xs)|},
      name: "sort",
      arg:
        Prod([
          arrow(prod([unknown(Internal), unknown(Internal)]), Ord.t),
          list(unknown(Internal)),
        ]),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("sort"),
              fn(
                Pat.tuple([Pat.var("cmp"), Pat.var("xs")]),
                let_(
                  Pat.var("merge"),
                  fn(
                    Pat.tuple([
                      Pat.var("cmp"),
                      Pat.var("xs"),
                      Pat.var("ys"),
                    ]),
                    let_(
                      Pat.var("go"),
                      fix_f(
                        Pat.var("go"),
                        fn(
                          Pat.tuple([
                            Pat.var("xs"),
                            Pat.var("ys"),
                            Pat.var("acc"),
                          ]),
                          match(
                            tuple([var("xs"), var("ys")]),
                            [
                              (
                                Pat.tuple([
                                  Pat.list_lit([]),
                                  Pat.list_lit([]),
                                ]),
                                ap(Forward, var("reverse"), var("acc")),
                              ),
                              (
                                Pat.tuple([Pat.list_lit([]), Pat.wild()]),
                                ap(
                                  Forward,
                                  var("rev_append"),
                                  tuple([var("acc"), var("ys")]),
                                ),
                              ),
                              (
                                Pat.tuple([Pat.wild(), Pat.list_lit([])]),
                                ap(
                                  Forward,
                                  var("rev_append"),
                                  tuple([var("acc"), var("xs")]),
                                ),
                              ),
                              (
                                Pat.tuple([
                                  Pat.cons(Pat.var("x"), Pat.var("xs")),
                                  Pat.cons(Pat.var("y"), Pat.var("ys")),
                                ]),
                                match(
                                  ap(
                                    Forward,
                                    var("cmp"),
                                    tuple([var("x"), var("y")]),
                                  ),
                                  [
                                    (
                                      Ord.lt_pat,
                                      ap(
                                        Forward,
                                        var("go"),
                                        tuple([
                                          var("xs"),
                                          cons(var("y"), var("ys")),
                                          cons(var("x"), var("acc")),
                                        ]),
                                      ),
                                    ),
                                    (
                                      Ord.eq_pat,
                                      ap(
                                        Forward,
                                        var("go"),
                                        tuple([
                                          var("xs"),
                                          cons(var("y"), var("ys")),
                                          cons(var("x"), var("acc")),
                                        ]),
                                      ),
                                    ),
                                    (
                                      Ord.gt_pat,
                                      ap(
                                        Forward,
                                        var("go"),
                                        tuple([
                                          cons(var("x"), var("xs")),
                                          var("ys"),
                                          cons(var("y"), var("acc")),
                                        ]),
                                      ),
                                    ),
                                  ],
                                ),
                              ),
                            ],
                          ),
                          None,
                          Some("go+"),
                        ),
                        None,
                      ),
                      ap(
                        Forward,
                        var("go"),
                        tuple([var("xs"), var("ys"), list_lit([])]),
                      ),
                    ),
                    None,
                    Some("merge+"),
                  ),
                  let_(
                    Pat.var("split"),
                    fix_f(
                      Pat.var("split"),
                      fn(
                        Pat.var("xs"),
                        match(
                          var("xs"),
                          [
                            (
                              Pat.list_lit([]),
                              tuple([list_lit([]), list_lit([])]),
                            ),
                            (
                              Pat.list_lit([Pat.var("x")]),
                              tuple([list_lit([var("x")]), list_lit([])]),
                            ),
                            (
                              Pat.cons(
                                Pat.var("x"),
                                Pat.cons(Pat.var("y"), Pat.var("ys")),
                              ),
                              let_(
                                Pat.tuple([Pat.var("xs"), Pat.var("ys")]),
                                ap(Forward, var("split"), var("ys")),
                                tuple([
                                  cons(var("x"), var("xs")),
                                  cons(var("y"), var("ys")),
                                ]),
                              ),
                            ),
                          ],
                        ),
                        None,
                        Some("split+"),
                      ),
                      None,
                    ),
                    let_(
                      Pat.var("merge_sort"),
                      fix_f(
                        Pat.var("merge_sort"),
                        fn(
                          Pat.var("xs"),
                          match(
                            var("xs"),
                            [
                              (Pat.list_lit([]), list_lit([])),
                              (
                                Pat.list_lit([Pat.var("x")]),
                                list_lit([var("x")]),
                              ),
                              (
                                Pat.wild(),
                                let_(
                                  Pat.tuple([
                                    Pat.var("left"),
                                    Pat.var("right"),
                                  ]),
                                  ap(Forward, var("split"), var("xs")),
                                  ap(
                                    Forward,
                                    var("merge"),
                                    tuple([
                                      var("cmp"),
                                      ap(
                                        Forward,
                                        var("merge_sort"),
                                        var("left"),
                                      ),
                                      ap(
                                        Forward,
                                        var("merge_sort"),
                                        var("right"),
                                      ),
                                    ]),
                                  ),
                                ),
                              ),
                            ],
                          ),
                          None,
                          Some("merge_sort+"),
                        ),
                        None,
                      ),
                      ap(Forward, var("merge_sort"), var("xs")),
                    ),
                  ),
                ),
                None,
                Some("sort+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix slice -> fun (start, end, xs) -> take(drop(xs, start), end - start)|},
      name: "slice",
      arg: Prod([int(), int(), list(unknown(Internal))]),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("slice"),
              fn(
                Pat.tuple([
                  Pat.var("start"),
                  Pat.var("end"),
                  Pat.var("xs"),
                ]),
                ap(
                  Forward,
                  var("take"),
                  tuple([
                    ap(
                      Forward,
                      var("drop"),
                      tuple([var("xs"), var("start")]),
                    ),
                    bin_op(Int(Minus), var("end"), var("start")),
                  ]),
                ),
                None,
                Some("slice+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix hd_opt -> fun xs -> case xs
             | [] => None
             | x :: _ => Some(x)
           end|},
      name: "hd_opt",
      arg: List(unknown(Internal)),
      ret: Unknown(Internal),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("hd_opt"),
              fn(
                Pat.var("xs"),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), Option.none),
                    (
                      Pat.cons(Pat.var("x"), Pat.wild()),
                      ap(Forward, Option.some, var("x")),
                    ),
                  ],
                ),
                None,
                Some("hd_opt+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|fix tl_opt -> fun xs -> case xs
             | [] => None
             | _ :: xs => Some(xs)
           end|},
      name: "tl_opt",
      arg: List(unknown(Internal)),
      ret: Unknown(Internal),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("tl_opt"),
              fn(
                Pat.var("xs"),
                match(
                  var("xs"),
                  [
                    (Pat.list_lit([]), Option.none),
                    (
                      Pat.cons(Pat.wild(), Pat.var("xs")),
                      ap(Forward, Option.some, var("xs")),
                    ),
                  ],
                ),
                None,
                Some("tl_opt+"),
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {|any|}, //alias
      name: "contains",
      arg:
        Prod([list(unknown(Internal)), arrow(unknown(Internal), bool())]),
      ret: Atom(Bool),
      imp: {
        Fresh.(Exp.(var("any")));
      },
    },
    {
      str: {|concat|}, //alias
      name: "flatten",
      arg: List(list(unknown(Internal))),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(Exp.(var("concat")));
      },
    },
    {
      str: {|reverse|}, //alias
      name: "rev",
      arg: List(unknown(Internal)),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(Exp.(var("reverse")));
      },
    },
    {
      str: {|any|}, //alias
      name: "exists",
      arg:
        Prod([list(unknown(Internal)), arrow(unknown(Internal), bool())]),
      ret: Atom(Bool),
      imp: {
        Fresh.(Exp.(var("any")));
      },
    },
    {
      str: {|all|}, //alias
      name: "for_all",
      arg:
        Prod([list(unknown(Internal)), arrow(unknown(Internal), bool())]),
      ret: Atom(Bool),
      imp: {
        Fresh.(Exp.(var("all")));
      },
    },
    {
      str: {|any2|}, //alias
      name: "exists2",
      arg:
        Prod([
          list(unknown(Internal)),
          list(unknown(Internal)),
          arrow(prod([unknown(Internal), unknown(Internal)]), bool()),
        ]),
      ret: Atom(Bool),
      imp: {
        Fresh.(Exp.(var("any2")));
      },
    },
    {
      str: {|all2|}, //alias
      name: "for_all2",
      arg:
        Prod([
          list(unknown(Internal)),
          list(unknown(Internal)),
          arrow(prod([unknown(Internal), unknown(Internal)]), bool()),
        ]),
      ret: Atom(Bool),
      imp: {
        Fresh.(Exp.(var("all2")));
      },
    },
    {
      str: {|flat_map|}, //alias
      name: "concat_map",
      arg:
        Prod([
          list(unknown(Internal)),
          arrow(unknown(Internal), list(unknown(Internal))),
        ]),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(Exp.(var("flat_map")));
      },
    },
    {
      str: {|unzip|}, //alias
      name: "split",
      arg: List(prod([unknown(Internal), unknown(Internal)])),
      ret: Prod([list(unknown(Internal)), list(unknown(Internal))]),
      imp: {
        Fresh.(Exp.(var("unzip")));
      },
    },
    {
      str: {|zip|}, //alias
      name: "combine",
      arg: Prod([list(unknown(Internal)), list(unknown(Internal))]),
      ret: List(prod([unknown(Internal), unknown(Internal)])),
      imp: {
        Fresh.(Exp.(var("zip")));
      },
    },
    {
      str: {|fix unique -> fun xs ->
              (xs, fun (seen, x) -> if mem(seen, x) then seen else seen @ [x], []))|},
      name: "unique",
      arg: List(unknown(Internal)),
      ret: List(unknown(Internal)),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("unique"),
              fn(
                Pat.var("xs"),
                ap(
                  Forward,
                  var("fold_left"),
                  tuple([
                    var("xs"),
                    fn(
                      Pat.tuple([Pat.var("seen"), Pat.var("x")]),
                      if_(
                        ap(
                          Forward,
                          var("mem"),
                          tuple([var("seen"), var("x")]),
                        ),
                        var("seen"),
                        ap(
                          Forward,
                          var("append"),
                          tuple([var("seen"), list_lit([var("x")])]),
                        ),
                      ),
                      None,
                      None,
                    ),
                    list_lit([]),
                  ]),
                ),
                None,
                None,
              ),
              None,
            )
          )
        );
      },
    },
    {
      str: {hazel|fix pivot_table -> fun (table, new_col, index, value) ->
             let indices = map(table, index) |> unique in
             let new_cols = map(table, new_col) |> unique in

             map(indices, fun idx ->
                (index=idx) ...
                (map(new_cols, fun col ->
                  (label=col,
                    value=filter(table, fun r -> index(r) == idx && new_col(r) == col)
                    |>value)
                ) |> from_lvs))|hazel},
      name: "pivot_table",
      arg:
        Prod([
          list(unknown(Internal)),
          arrow(unknown(Internal), unknown(Internal)),
          arrow(unknown(Internal), unknown(Internal)),
          arrow(unknown(Internal), unknown(Internal)),
        ]),
      ret: List(unknown(Internal)),
      imp: {
        open Fresh;
        open Exp;
        let indices =
          ap(
            Reverse,
            var("unique"),
            ap(Forward, var("map"), tuple([var("table"), var("index")])),
          );
        let new_cols =
          ap(
            Reverse,
            var("unique"),
            ap(
              Forward,
              var("map"),
              tuple([var("table"), var("new_col")]),
            ),
          );

        // filter(table, fun r -> index(r) == idx && new_col(r) == col)
        let filtered_values =
          ap(
            Forward,
            var("filter"),
            tuple([
              var("table"),
              fn(
                Pat.var("r"),
                bin_op(
                  Bool(And),
                  bin_op(
                    Poly(Equals),
                    ap(Forward, var("index"), var("r")),
                    var("idx"),
                  ),
                  bin_op(
                    Poly(Equals),
                    ap(Forward, var("new_col"), var("r")),
                    var("col"),
                  ),
                ),
                None,
                None,
              ),
            ]),
          );
        /*   (label=col,
             value=filter(table, fun r -> index(r) == idx && new_col(r) == col)
             |>value) */
        let lvs =
          tuple([
            tup_label(label("label"), var("col")),
            tup_label(
              label("value"),
              ap(Forward, var("value"), filtered_values),
            ),
          ]);

        let from_lvs =
          ap(
            Reverse,
            var("from_lvs"),
            ap(
              Forward,
              var("map"),
              tuple([
                var("new_cols"),
                fn(Pat.var("col"), lvs, None, None),
              ]),
            ),
          );

        let mapped =
          ap(
            Forward,
            var("map"),
            tuple([
              var("indices"),
              fn(
                Pat.var("idx"),
                tuple_extension(
                  tuple([tup_label(label("index"), var("idx"))]),
                  from_lvs,
                ),
                None,
                None,
              ),
            ]),
          );

        let fn =
          fn(
            Pat.tuple([
              Pat.var("table"),
              Pat.var("new_col"),
              Pat.var("index"),
              Pat.var("value"),
            ]),
            let_(
              Pat.var("indices"),
              indices,
              let_(Pat.var("new_cols"), new_cols, mapped),
            ),
            None,
            None,
          );
        fix_f(Pat.var("pivot_table"), fn, None);
      },
    },
    {
      str: {|fix group_on_key -> fun (xs, f) -> fold_left(xs, fun (acc, x) ->
        let update_groups = fix update_groups -> fun (acc, key, x) ->
          case acc
          | [] => [(key, [x])]
          | (k, g) :: acc => if k == key then (k, x :: g) :: acc else (k, g) :: update_groups(acc, key, x)
          end in update_groups(acc, f(x), x), [])|},
      name: "group_on_key",
      arg:
        Prod([
          list(unknown(Internal)),
          arrow(unknown(Internal), unknown(Internal)),
        ]),
      ret: List(prod([unknown(Internal), list(unknown(Internal))])),
      imp: {
        Fresh.(
          Exp.(
            fix_f(
              Pat.var("group_on_key"),
              fn(
                Pat.tuple([Pat.var("xs"), Pat.var("f")]),
                ap(
                  Forward,
                  var("fold_left"),
                  tuple([
                    var("xs"),
                    let_(
                      Pat.var("update_groups"),
                      fix_f(
                        Pat.var("update_groups"),
                        fn(
                          Pat.tuple([
                            Pat.var("acc"),
                            Pat.var("key"),
                            Pat.var("x"),
                          ]),
                          match(
                            var("acc"),
                            [
                              (
                                Pat.list_lit([]),
                                list_lit([
                                  tuple([
                                    var("key"),
                                    list_lit([var("x")]),
                                  ]),
                                ]),
                              ),
                              (
                                Pat.cons(
                                  Pat.tuple([Pat.var("k"), Pat.var("g")]),
                                  Pat.var("acc_tail"),
                                ),
                                if_(
                                  bin_op(
                                    Poly(Equals),
                                    var("k"),
                                    var("key"),
                                  ),
                                  cons(
                                    tuple([
                                      var("k"),
                                      list_concat(
                                        var("g"),
                                        list_lit([var("x")]),
                                      ),
                                    ]),
                                    var("acc_tail"),
                                  ),
                                  cons(
                                    tuple([var("k"), var("g")]),
                                    ap(
                                      Forward,
                                      var("update_groups"),
                                      tuple([
                                        var("acc_tail"),
                                        var("key"),
                                        var("x"),
                                      ]),
                                    ),
                                  ),
                                ),
                              ),
                            ],
                          ),
                          None,
                          Some("update_groups+"),
                        ),
                        None,
                      ),
                      fn(
                        Pat.tuple([Pat.var("acc"), Pat.var("x")]),
                        ap(
                          Forward,
                          var("update_groups"),
                          tuple([
                            var("acc"),
                            ap(Forward, var("f"), var("x")),
                            var("x"),
                          ]),
                        ),
                        None,
                        None,
                      ),
                    ),
                    list_lit([]),
                  ]),
                ),
                None,
                Some("group_on_key+"),
              ),
              None,
            )
          )
        );
      },
    },
  ]
  // De-alias all aliases
  |> Util.ListUtil.map_with_history((prev, curr) =>
       switch (curr.imp) {
       | {term: Var(v), _} =>
         switch (List.find_opt(b => b.name == v, prev)) {
         | Some(b: hazel_fn) => {
             ...curr,
             imp: b.imp,
           }
         | None => curr
         }
       | _ => curr
       }
     );
