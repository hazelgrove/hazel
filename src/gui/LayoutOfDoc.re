// TODO: use a weak hash table
// TODO: use a hash table that is generic over the 'tag, or have a better way to represent this
type memo = {
  left: int,
  right: int,
  first_left: int,
  last_right: int,
};

// TODO: move `cost` to own module?
// TODO: is there already a type for this?
type cost =
  | Inf
  | Fin(int);

// TODO: does Reason have 'type classes'? operators? infix?
let cost_plus = (c1: cost, c2: cost): cost => {
  switch (c1, c2) {
  | (Inf, _) => Inf
  | (_, Inf) => Inf
  | (Fin(i1), Fin(i2)) => Fin(i1 + i2)
  };
};

let cost_le = (c1: cost, c2: cost): bool => {
  switch (c1, c2) {
  | (Inf, Inf) => true
  | (Inf, Fin(_)) => false
  | (Fin(_), Inf) => true
  | (Fin(i1), Fin(i2)) => i1 <= i2
  };
};

let cost_sub1 = (c: cost): cost => {
  switch (c) {
  | Inf => Inf
  | Fin(i) => Fin(i - 1)
  };
};

let range = (_: int, _: int): list(int) => {
  failwith("unimplemented");
};

let min = (_: list('a)): 'a => {
  failwith("unimplemented");
};

// TODO: use ptr equality for Doc.t but structural equality for memo in hash table
let mk_layout_of_doc =
    (width: int): (Doc.t('tag) => (cost, Layout.t('tag))) => {
  /*
   let memo:
     Hashtbl.t(Doc.t('tag), Hashtbl.t(memo, (cost, Layout.t('tag)))) =
     Hashtbl.create(0);
     */
  let rec layout_of_doc =
          (memo: memo): (Doc.t('tag) => (cost, Layout.t('tag))) =>
    fun
    | VZero => (Fin(0), VZero)
    | VCat(d1, d2) => {
        let (c1, l1) = layout_of_doc(memo, d1);
        let (c2, l2) = layout_of_doc(memo, d2);
        (cost_plus(c1, c2), VCat(l1, l2));
      }
    | HCat(d1, d2) => {
        let go = (i: int): (cost, Layout.t('tag)) => {
          let (c1, l1) = layout_of_doc({...memo, last_right: i}, d1);
          let (c2, l2) = layout_of_doc({...memo, first_left: i}, d2);
          (cost_sub1(cost_plus(c1, c2)), HCat(l1, l2));
        };
        // TODO: left biased
        // TODO: do we allow zero width last and first lines?
        // TODO: what about when left >= right?
        min(List.map(go, range(memo.left, memo.right)));
      }
    | String(string) => (Fin(1), String(string)) // TODO: overlength strings
    | Choice(d1, d2) => {
        let (c1, l1) = layout_of_doc(memo, d1);
        let (c2, l2) = layout_of_doc(memo, d2);
        // TODO: left biased
        if (cost_le(c1, c2)) {
          (c1, l1);
        } else {
          (c2, l2);
        };
      }
    | Tagged(tag, d) => {
        let (c, l) = layout_of_doc(memo, d);
        (c, Tagged(tag, l));
      }
    | SingleLine(d) => {
        let (c, l) = layout_of_doc(memo, d);
        switch (c) {
        | Fin(0 | 1) => (c, l)
        | _ => (Inf, l)
        };
      };
  d =>
    layout_of_doc(
      {left: 0, first_left: 0, right: width, last_right: width},
      d,
    );
};
