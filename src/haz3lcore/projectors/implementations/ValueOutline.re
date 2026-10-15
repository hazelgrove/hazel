/* Fold-aware layout of probe sample values in the drawer.

   A value's lists, tuples (labeled or not) and constructor applications are
   groups. An open group prints like the drawer's pretty-printer: opening
   delimiter on the label line, one item per line, closing delimiter on its
   own line. A folded group prints a summary on the rest of its line.
   Anything else is a leaf, printed flat by the Hazel printer, or
   pretty-printed when it is opened.

   Which groups are open: a fit that opens groups breadth-first while the
   rows stay within the drawer, with the user's toggles on top. Folds are
   keyed by path into the value ("0.3" is item 3 of item 0) because every
   run makes fresh ids; a probe's samples share them. */

open Util;
open Language;

module SMap = Map.Make(String);
module SSet = Set.Make(String);

type kind =
  | List
  | Tuple
  | Ctor(string);

type node = {
  exp: Exp.t,
  shape,
  mutable text: option(string),
  /* exact flat width once known, else -1 */
  mutable width: int,
  /* a flat width this node is known to exceed */
  mutable wider_than: int,
  /* pretty-printed form at a width: (width, segment, rows) */
  mutable pretty: option((int, Segment.t, int)),
}
and shape =
  | Leaf
  | Group(kind, array(item))
and item = {
  label: option(string),
  node,
};

let cols = Unicode.Width.columns_of_string;
let ellipsis = "…";
let count_note = (k: int): string => ellipsis ++ "+" ++ string_of_int(k);
let label_sep = " = ";
let prefix = (label: option(string)): string =>
  switch (label) {
  | Some(l) => l ++ label_sep
  | None => ""
  };

let delims =
  fun
  | List => ("[", "]")
  | Tuple => ("(", ")")
  | Ctor(name) => (name ++ "(", ")");

let rec strip_parens = (e: Exp.t): Exp.t =>
  switch (Exp.term_of(e)) {
  | Parens(e') => strip_parens(e')
  | _ => e
  };

let label_name = (l: Exp.t): option(string) =>
  switch (Exp.term_of(strip_parens(l))) {
  | Label(s) => Some(s)
  | _ => None
  };

let rec build = (e: Exp.t): node => {
  let e = strip_parens(e);
  let mk = shape => {
    exp: e,
    shape,
    text: None,
    width: (-1),
    wider_than: (-1),
    pretty: None,
  };
  let item = (e: Exp.t): item =>
    switch (Exp.term_of(strip_parens(e))) {
    | TupLabel(l, v) when label_name(l) != None => {
        label: label_name(l),
        node: build(v),
      }
    | _ => {
        label: None,
        node: build(e),
      }
    };
  let group = (kind, es) =>
    mk(Group(kind, Array.of_list(List.map(item, es))));
  switch (Exp.term_of(e)) {
  | ListLit([_, ..._] as es) => group(List, es)
  | Tuple([_, ..._] as es) => group(Tuple, es)
  | Ap(Forward, ctor, arg) =>
    switch (Exp.term_of(strip_parens(ctor))) {
    | Constructor(name, _) =>
      switch (Exp.term_of(strip_parens(arg))) {
      | Tuple([_, ..._] as es) => group(Ctor(name), es)
      | _ => group(Ctor(name), [arg])
      }
    | _ => mk(Leaf)
    }
  | _ => mk(Leaf)
  };
};

let one_line = (s: string): string =>
  String.map(c => c == '\n' ? ' ' : c, s);

let leaf_text = (u: ProjectorBase.utility, n: node): string =>
  switch (n.text) {
  | Some(t) => t
  | None =>
    let t = u.term_to_seg(~inline=true, Exp(n.exp)) |> u.seg_to_string;
    let t = one_line(t);
    n.text = Some(t);
    t;
  };

/* The flat width if it is at most `cap`, else some number above `cap`.
   Stops early, so a huge list costs only what fits. */
let rec flat_width = (u, n: node, cap: int): int =>
  switch (n.shape) {
  | Leaf =>
    if (n.width < 0) {
      n.width = cols(leaf_text(u, n));
    };
    n.width;
  | Group(_) when n.width >= 0 => n.width
  | Group(_) when n.wider_than >= cap => cap + 1
  | Group(kind, items) =>
    let (o, c) = delims(kind);
    let acc = ref(cols(o) + cols(c));
    let i = ref(0);
    let len = Array.length(items);
    while (i^ < len && acc^ <= cap) {
      let it = items[i^];
      acc := acc^ + (i^ > 0 ? 2 : 0) + cols(prefix(it.label));
      if (acc^ <= cap) {
        acc := acc^ + flat_width(u, it.node, cap - acc^);
      };
      incr(i);
    };
    if (acc^ <= cap) {
      n.width = acc^;
    } else {
      n.wider_than = max(n.wider_than, cap);
    };
    acc^;
  };

let fits = (u, n, b) => b >= 0 && flat_width(u, n, b) <= b;

let rec flat_text = (u, n: node): string =>
  switch (n.shape) {
  | Leaf => leaf_text(u, n)
  | Group(kind, items) =>
    let (o, c) = delims(kind);
    o
    ++ String.concat(
         ", ",
         Array.to_list(items)
         |> List.map(it => prefix(it.label) ++ flat_text(u, it.node)),
       )
    ++ c;
  };

/* ===== Summaries: a node on one line within b columns ===== */

let join_width = (parts: list(string)): int =>
  List.fold_left((acc, s) => acc + cols(s), 0, parts)
  + 2
  * max(0, List.length(parts) - 1);

let rec summary = (u, n: node, b: int): string =>
  if (fits(u, n, b)) {
    flat_text(u, n);
  } else if (b < 1) {
    "";
  } else {
    switch (n.shape) {
    /* names and numbers don't shorten; strings and code do */
    | Leaf =>
      switch (Exp.term_of(n.exp)) {
      | Atom(String(_))
      | Fun(_)
      | Closure(_)
      | TypFun(_)
      | Cons(_)
      | ListConcat(_)
      | BinOp(_)
      | UnOp(_)
      | Ap(_)
      | If(_)
      | Let(_)
      | Match(_) =>
        let (seg, _) = ProbeUtil.abbreviated_seg_of(u, b, n.exp);
        let s = one_line(u.seg_to_string(seg));
        cols(s) <= b ? s : ellipsis;
      | _ => ellipsis
      }
    | Group(kind, items) =>
      let (o, c) = delims(kind);
      let inner = b - cols(o) - cols(c);
      let all_labeled = Array.for_all((it: item) => it.label != None, items);
      inner < 1
        ? ellipsis
        : o
          ++ (
            switch (kind) {
            | List => prefix_body(u, items, inner)
            | Tuple when all_labeled => prefix_body(u, items, inner)
            | Tuple
            | Ctor(_) => slot_body(u, items, inner)
            }
          )
          ++ c;
    };
  }

/* Whole items in order while they fit, one partial item, then …+N. */
and prefix_body = (u, items: array(item), b: int): string => {
  let n = Array.length(items);
  let all_labeled = Array.for_all((it: item) => it.label != None, items);
  let rec go = (i: int, shown: list(string)): string =>
    if (i == n) {
      String.concat(", ", shown);
    } else {
      let rest = n - i - 1;
      let tail = rest > 0 ? [count_note(rest)] : [];
      let it = items[i];
      let pre = prefix(it.label);
      let room = b - join_width(shown @ [pre] @ tail);
      if (fits(u, it.node, room)) {
        go(i + 1, shown @ [pre ++ flat_text(u, it.node)]);
      } else {
        let partial =
          room >= 1 ? summary(u, it.node, room) |> Option.some : None;
        /* unlabeled, a partial must say more than "…" or [<…>] */
        switch (partial) {
        | Some(s) when s != "" && (it.label != None || cols(s) >= 4) =>
          String.concat(", ", shown @ [pre ++ s] @ tail)
        | _ =>
          let rec trim = (shown: list(string)) =>
            switch (shown) {
            | [] => []
            | _ =>
              let k = n - List.length(shown);
              join_width(shown @ [count_note(k)]) <= b
                ? shown : trim(fst(ListUtil.split_last(shown)));
            };
          let shown = trim(shown);
          let note = count_note(n - List.length(shown));
          /* a record's field count says little: (…) rather than (…+3) */
          shown == [] && all_labeled
            ? ellipsis
            : join_width(shown @ [note]) <= b
                ? String.concat(", ", shown @ [note]) : ellipsis;
        };
      };
    };
  go(0, []);
}

/* Positional items (tuples, constructor arguments): every slot first,
   then whole values where they fit, then partial ones. */
and slot_body = (u, items: array(item), b: int): string => {
  let pres = Array.map((it: item) => prefix(it.label), items);
  let vals = Array.map(_ => ellipsis, items);
  let whole = Array.map(_ => false, items);
  let width = () =>
    join_width(Array.to_list(Array.mapi((i, p) => p ++ vals[i], pres)));
  if (width() > b) {
    ellipsis;
  } else {
    Array.iteri(
      (i, it: item) => {
        let room = b - width() + cols(vals[i]);
        if (fits(u, it.node, room)) {
          vals[i] = flat_text(u, it.node);
          whole[i] = true;
        };
      },
      items,
    );
    Array.iteri(
      (i, it: item) =>
        if (!whole[i]) {
          let room = b - width() + cols(vals[i]);
          if (room > 1) {
            let s = summary(u, it.node, room);
            if (cols(s) > 1) {
              vals[i] = s;
            };
          };
        },
      items,
    );
    String.concat(
      ", ",
      Array.to_list(Array.mapi((i, p) => p ++ vals[i], pres)),
    );
  };
};

/* ===== Layout ===== */

type body =
  /* a leaf, or a group shown whole */
  | Text(Lazy.t(string))
  /* a folded group, or a leaf too wide for its line */
  | Summary(string, Lazy.t(string))
  /* an opened wide leaf, pretty-printed: path, segment, rows */
  | Pretty(string, Segment.t, int)
  /* hidden items of the sequence at path; clicking shows `next` items */
  | More(string, int, int);

type out =
  | Row(option(string), body, string)
  | Block(block)
  /* short items packed across rows */
  | Packed(list(list(packed)))
and block = {
  key: string,
  label: option(string),
  opener: string,
  kids: list(out),
  closer: string,
  suffix: string,
}
and packed =
  | Item(string)
  | PackedMore(string, int, int);

let rec rows = (o: out): int =>
  switch (o) {
  | Row(_, Pretty(_, _, r), _) => max(1, r)
  | Row(_) => 1
  | Block(b) => List.fold_left((acc, k) => acc + rows(k), 2, b.kids)
  | Packed(rs) => List.length(rs)
  };

let child_key = (key: string, i: int): string =>
  key == "" ? string_of_int(i) : key ++ "." ++ string_of_int(i);

let key_depth = (key: string): int =>
  key == "" ? 0 : List.length(String.split_on_char('.', key));

type cfg = {
  width: int,
  is_open: (string, node) => bool,
  /* items a sequence shows */
  limit: (string, node) => int,
  /* items `…+N` adds when clicked */
  step: (string, node) => int,
  /* folded groups, in document order (reversed), with their depth */
  folded: ref(list((string, int))),
};

let row_count = (seg: Segment.t): int =>
  Measured.of_segment(seg, ProjectorCore.Shape.Map.empty, Id.Map.empty)
  |> Measured.total_rows;

let pretty = (u: ProjectorBase.utility, n: node, width: int) =>
  switch (n.pretty) {
  | Some((w, seg, r)) when w == width => (seg, r)
  | _ =>
    let seg =
      u.term_to_seg(~inline=false, Exp(n.exp))
      |> PrettySegment.prettify(~width);
    let r = row_count(seg);
    n.pretty = Some((width, seg, r));
    (seg, r);
  };

let packable = (u, items: array(item), shown: int, line_w: int): bool =>
  shown >= 2
  && {
    let ok = ref(true);
    for (i in 0 to shown - 1) {
      let it = items[i];
      let leaf =
        switch (it.node.shape) {
        | Leaf => true
        | Group(_) => false
        };
      if (it.label != None
          || !leaf
          || flat_width(u, it.node, line_w)
          + 1 > line_w) {
        ok := false;
      };
    };
    ok^;
  };

let pack =
    (u, items, ~shown, ~hidden, ~key, ~next, line_w): list(list(packed)) => {
  /* an item carries its comma; the count, when there is one, is last */
  let toks =
    List.init(shown, i =>
      Item(
        flat_text(u, items[i].node)
        ++ (i < shown - 1 || hidden > 0 ? "," : ""),
      )
    )
    @ (hidden > 0 ? [PackedMore(key, hidden, next)] : []);
  let text_w =
    fun
    | Item(s) => cols(s)
    | PackedMore(_, h, _) => cols(count_note(h));
  let (rows, cur, _) =
    List.fold_left(
      ((rows, cur, col), tok) => {
        let w = text_w(tok);
        cur != [] && col + 1 + w > line_w
          ? ([List.rev(cur), ...rows], [tok], w)
          : (rows, [tok, ...cur], col + (cur == [] ? 0 : 1) + w);
      },
      ([], [], 0),
      toks,
    );
  List.rev(cur == [] ? rows : [List.rev(cur), ...rows]);
};

let rec emit =
        (
          u,
          cfg: cfg,
          n: node,
          ~key: string,
          ~indent: int,
          ~label: option(string),
          ~suffix: string,
        )
        : out => {
  let pre = prefix(label);
  let avail = cfg.width - indent - cols(pre) - cols(suffix);
  if (fits(u, n, avail)) {
    Row(label, Text(lazy(flat_text(u, n))), suffix);
  } else {
    switch (n.shape) {
    | Leaf =>
      cfg.is_open(key, n)
        ? {
          let (seg, r) =
            pretty(u, n, max(20, cfg.width - indent - cols(pre)));
          Row(label, Pretty(key, seg, r), suffix);
        }
        : Row(
            label,
            Summary(key, lazy(summary(u, n, max(1, avail)))),
            suffix,
          )
    | Group(kind, items) =>
      if (!cfg.is_open(key, n)) {
        cfg.folded := [(key, key_depth(key)), ...cfg.folded^];
        Row(
          label,
          Summary(key, lazy(summary(u, n, max(3, avail)))),
          suffix,
        );
      } else {
        let (opener, closer) = delims(kind);
        let count = Array.length(items);
        let shown = max(1, min(count, cfg.limit(key, n)));
        let hidden = count - shown;
        let next = min(count, shown + cfg.step(key, n));
        let line_w = cfg.width - indent - 2;
        let kids =
          if (kind == List && packable(u, items, shown, line_w)) {
            [Packed(pack(u, items, ~shown, ~hidden, ~key, ~next, line_w))];
          } else {
            List.init(shown, i =>
              emit(
                u,
                cfg,
                items[i].node,
                ~key=child_key(key, i),
                ~indent=indent + 2,
                ~label=items[i].label,
                ~suffix=i == count - 1 ? "" : ",",
              )
            )
            @ (hidden > 0 ? [Row(None, More(key, hidden, next), "")] : []);
          };
        Block({
          key,
          label,
          opener,
          kids,
          closer,
          suffix,
        });
      }
    };
  };
};

/* ===== Fit ===== */

type base = {
  opened: SSet.t,
  limits: SMap.t(int),
};

let run = (u, root, ~width, ~is_open, ~limit, ~step) => {
  let folded = ref([]);
  let cfg = {
    width,
    is_open,
    limit,
    step,
    folded,
  };
  let o = emit(u, cfg, root, ~key="", ~indent=0, ~label=None, ~suffix="");
  (o, List.rev(folded^));
};

let fit = (u, root: node, ~width: int, ~rows as h: int): base => {
  let opened = ref(SSet.singleton(""));
  let limits = ref(SMap.empty);
  let go = () =>
    run(
      u,
      root,
      ~width,
      ~is_open=(k, _) => SSet.mem(k, opened^),
      ~limit=
        (k, _) =>
          SMap.find_opt(k, limits^) |> Option.value(~default=max_int),
      ~step=(_, _) => 0,
    );
  /* a packed row holds at most width / 3 items ("1, "), so a list longer
     than that many rows' worth is cut before the first layout */
  let most = h * max(1, width / 3);
  switch (root.shape) {
  | Group(List, items) when Array.length(items) > most =>
    limits := SMap.add("", most, limits^)
  | _ => ()
  };
  let (o, folded) = go();
  let folded = ref(folded);
  switch (root.shape) {
  | Group(List, items) when rows(o) > h =>
    let rec bisect = (lo, hi) =>
      if (lo >= hi) {
        lo;
      } else {
        let mid = (lo + hi + 1) / 2;
        limits := SMap.add("", mid, limits^);
        rows(fst(go())) <= h ? bisect(mid, hi) : bisect(lo, mid - 1);
      };
    let lim = bisect(1, min(most, Array.length(items)));
    limits := SMap.add("", lim, limits^);
    folded := snd(go());
  | _ => ()
  };
  /* a group that can't open within the drawer even cut short stays folded */
  switch (root.shape) {
  | Group(_) when rows(fst(go())) > h =>
    opened := SSet.empty;
    folded := [];
  | _ => ()
  };
  let depth = ref(1);
  while (List.exists(((_, d)) => d >= depth^, folded^)) {
    let d = depth^;
    List.filter(((_, d')) => d' == d, folded^)
    |> List.iter(((k, _)) => {
         opened := SSet.add(k, opened^);
         let (o, f) = go();
         if (rows(o) <= h) {
           folded := f;
         } else {
           opened := SSet.remove(k, opened^);
         };
       });
    incr(depth);
  };
  {
    opened: opened^,
    limits: limits^,
  };
};

/* A sequence the user opens shows a chunk at a time. */
let chunk = (u, n: node, ~line_w: int): int =>
  switch (n.shape) {
  | Group(kind, items) =>
    let count = Array.length(items);
    if (count <= 14) {
      max_int;
    } else if (kind == List && packable(u, items, min(count, 400), line_w)) {
      /* about six packed rows */
      let col = ref(0);
      let rows = ref(1);
      let i = ref(0);
      while (i^ < count && rows^ <= 6) {
        let w = flat_width(u, items[i^].node, line_w) + 1;
        if (col^ > 0 && col^ + 1 + w > line_w) {
          incr(rows);
          col := 0;
        };
        if (rows^ <= 6) {
          col := col^ + (col^ > 0 ? 1 : 0) + w;
          incr(i);
        };
      };
      max(1, i^ - 1);
    } else {
      10;
    };
  | Leaf => max_int
  };

/* ===== Entry point, with memos keyed on the sample value itself ===== */

type folds = {
  opened: list((string, bool)),
  shown: list((string, int)),
};

let no_folds = {
  opened: [],
  shown: [],
};

let memo_size = 64;
let trees: ref(list((Exp.t, node))) = ref([]);
let fits_memo: ref(list(((Exp.t, int, int), base))) = ref([]);
let layouts: ref(list(((Exp.t, int, int, folds), out))) = ref([]);

let remember = (memo, k, v) => {
  memo := [(k, v), ...ListUtil.take(memo_size - 1, memo^)];
  v;
};

let tree = (exp: Exp.t): node =>
  switch (List.find_opt(((e, _)) => e === exp, trees^)) {
  | Some((_, n)) => n
  | None =>
    remember(
      trees,
      exp,
      build(exp |> DHExp.strip_ascriptions |> Exp.strip_projectors),
    )
  };

let base_for = (u, exp, ~width, ~rows): base =>
  switch (
    List.find_opt(
      (((e, w, r), _)) => e === exp && w == width && r == rows,
      fits_memo^,
    )
  ) {
  | Some((_, b)) => b
  | None =>
    remember(
      fits_memo,
      (exp, width, rows),
      fit(u, tree(exp), ~width, ~rows),
    )
  };

let layout =
    (
      u: ProjectorBase.utility,
      exp: Exp.t,
      ~width: int,
      ~rows: int,
      ~folds: folds,
    )
    : out =>
  switch (
    List.find_opt(
      (((e, w, r, f), _)) =>
        e === exp && w == width && r == rows && f == folds,
      layouts^,
    )
  ) {
  | Some((_, o)) => o
  | None =>
    let root = tree(exp);
    let base = base_for(u, exp, ~width, ~rows);
    let opened = SMap.of_seq(List.to_seq(folds.opened));
    let shown = SMap.of_seq(List.to_seq(folds.shown));
    let line_w = key => width - 2 * (key_depth(key) + 1);
    let is_open = (k, _) =>
      switch (SMap.find_opt(k, opened)) {
      | Some(b) => b
      | None => SSet.mem(k, base.opened)
      };
    let limit = (k, n) =>
      switch (SMap.find_opt(k, shown), SMap.find_opt(k, base.limits)) {
      | (Some(s), _) => s
      | (None, Some(l)) => l
      | (None, None) =>
        SMap.find_opt(k, opened) == Some(true)
          ? chunk(u, n, ~line_w=line_w(k)) : max_int
      };
    let step = (k, n) =>
      switch (SMap.find_opt(k, base.limits)) {
      | Some(l) => l
      | None =>
        let c = chunk(u, n, ~line_w=line_w(k));
        c == max_int ? 10 : c;
      };
    let (o, _) = run(u, root, ~width, ~is_open, ~limit, ~step);
    remember(layouts, (exp, width, rows, folds), o);
  };
