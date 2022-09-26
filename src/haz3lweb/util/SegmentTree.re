type ordinal = int;

type status =
  | Empty
  | Partial
  | Full;

type interval = (float, float);

type t = {
  root: node,
  // interval endpoints specified at initialization
  values: array(float),
  // ordinals of sorted endpoints
  ordinals: Hashtbl.t(float, ordinal),
}
and node = {
  // invariant: unit intervals are leaves
  interval: (ordinal, ordinal),
  shape: node_shape,
  // status and count could be externalized in a more
  // generic implementation of this data structure
  // but we only need this data structure for a single
  // purpose at the moment
  status,
  count: int,
}
and node_shape =
  | Leaf
  | Branch(Lazy.t(node), Lazy.t(node));

let mk = (values: list(float)) => {
  let (values, ordinals) = {
    let sorted_values = List.sort_uniq(Float.compare, values);
    (
      Array.of_list(sorted_values),
      sorted_values
      |> List.mapi((i, y) => (y, i))
      |> List.to_seq
      |> Hashtbl.of_seq,
    );
  };

  let rec mk_node = (a: ordinal, b: ordinal): node => {
    let shape =
      if (a + 1 == b) {
        Leaf;
      } else {
        let mid = (a + b) / 2;
        Branch(lazy(mk_node(a, mid)), lazy(mk_node(mid, b)));
      };
    {shape, interval: (a, b), count: 0, status: Empty};
  };

  {values, ordinals, root: mk_node(0, Array.length(values) - 1)};
};

let update_status = (node: node): node => {
  let status =
    if (node.count > 0) {
      Full;
    } else {
      switch (node.shape) {
      | Leaf => Empty
      | Branch(l, r) =>
        switch (Lazy.force(l).status, Lazy.force(r).status) {
        | (Empty, Empty) => Empty
        | _ => Partial
        }
      };
    };
  {...node, status};
};

type op =
  | Insert
  | Delete;
let string_of_op =
  fun
  | Insert => "insert"
  | Delete => "delete";

/**
 * Implementation follows algorithm described in Sections 8.3 + 8.5
 * of Computational Geometry: An Introduction by Preparata & Shamos.
 * Section 8.3 describes the general use of segment trees to compute
 * features of a collection of rectangles; Section 8.5 describes the
 * specialization of that approach to compute the contour of a union
 * of rectangles. The general framework laid out in 8.3 is unnecessary
 * for our purposes (so our implementation could be streamlined) but I
 * implemented it anyway so that the text serves as documentation that
 * closely maps to our code.
 */
let perform = (op, (a, b): interval, tree: t): t => {
  let rec go = (op, (a, b) as interval, node: node): node => {
    let (a', b') = node.interval;
    let node =
      if (a <= a' && b' <= b) {
        {
          // allocate to this node
          ...node,
          count:
            switch (op) {
            | Insert => node.count + 1
            | Delete => node.count - 1
            },
        };
      } else {
        switch (node.shape) {
        | Leaf => node
        | Branch(l, r) =>
          let mid = (a' + b') / 2;
          let l = a >= mid ? l : (lazy(go(op, interval, Lazy.force(l))));
          let r = b <= mid ? r : (lazy(go(op, interval, Lazy.force(r))));
          {...node, shape: Branch(l, r)};
        };
      };
    update_status(node);
  };
  switch (
    Hashtbl.find_opt(tree.ordinals, a),
    Hashtbl.find_opt(tree.ordinals, b),
  ) {
  | (None, _)
  | (_, None) =>
    let msg =
      Printf.sprintf(
        "SegmentTree.%s: expected interval with endpoints specified at initialization",
        string_of_op(op),
      );
    raise(Invalid_argument(msg));
  | (Some(a), Some(b)) =>
    let interval = a < b ? (a, b) : (b, a);
    let new_root = go(op, interval, tree.root);
    {...tree, root: new_root};
  };
};
let insert = perform(Insert);
let delete = perform(Delete);

/**
 * Preparata & Shamos use the terminology "contribution" and specify
 * the following implementation in the procedure CONTR in Section 8.5.
 */
let complement_intersection = ((a, b): interval, tree: t): list(interval) => {
  let rec go =
          (~stack=[], (a, b) as interval, node: node)
          : list((ordinal, ordinal)) => {
    let (a', b') = node.interval;
    switch (node.status) {
    | Full => stack
    | Empty when a <= a' && b' <= b =>
      // merge continuous segments
      switch (stack) {
      | [(a'', b''), ...rest] when a' == b'' => [(a'', b'), ...rest]
      | _ => [(a', b'), ...stack]
      }
    | _ =>
      switch (node.shape) {
      | Leaf => stack
      | Branch(l, r) =>
        let mid = (a' + b') / 2;
        let stack = a >= mid ? stack : go(~stack, interval, Lazy.force(l));
        b <= mid ? stack : go(~stack, interval, Lazy.force(r));
      }
    };
  };
  switch (
    Hashtbl.find_opt(tree.ordinals, a),
    Hashtbl.find_opt(tree.ordinals, b),
  ) {
  | (None, _)
  | (_, None) => failwith("invalid argument")
  | (Some(a), Some(b)) =>
    let interval = a < b ? (a, b) : (b, a);
    go(interval, tree.root)
    |> List.map(((ordinal1, ordinal2)) => {
         let (o1, o2) = a < b ? (ordinal1, ordinal2) : (ordinal2, ordinal1);
         (tree.values[o1], tree.values[o2]);
       });
  };
};
