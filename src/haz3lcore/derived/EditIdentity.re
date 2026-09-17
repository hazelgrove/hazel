/* Structural replacements own only their selected region. Reuse unchanged
   pieces there; never search the whole program for equal-looking syntax.
   A changed construct gets the parser's id, while unchanged descendants,
   prefixes and suffixes retain their existing ids and objects. */
let rec same_content = (a: Segment.t, b: Segment.t): bool =>
  phys_equal(a, b)
  || List.length(a) == List.length(b)
  && List.for_all2_exn(a, b, ~f=same_piece)
and same_piece = (a: Piece.t, b: Piece.t): bool =>
  phys_equal(a, b)
  || (
    switch (a, b) {
    | (Tile(a), Tile(b)) =>
      same_shape(a, b) && same_content_children(a.children, b.children)
    | (Secondary(a), Secondary(b)) => Poly.equal(a.content, b.content)
    | (Grout(a), Grout(b)) => Grout.equal_shape(a.shape, b.shape)
    /* Projectors own state beyond their printed syntax. Do not transplant
       a new instance onto an old one merely because they print alike. */
    | (Projector(_), Projector(_)) =>
      Id.equal(Piece.id(a), Piece.id(b)) && Piece.equal(a, b)
    | _ => false
    }
  )
and same_content_children = (a, b) =>
  List.length(a) == List.length(b)
  && List.for_all2_exn(a, b, ~f=same_content)
and same_shape = (a: Tile.t, b: Tile.t): bool =>
  Poly.equal(a.form, b.form)
  && Sort.equal(a.sort, b.sort)
  && List.equal(Int.equal, a.shards, b.shards);

let rec reuse = (old: Segment.t, fresh: Segment.t): Segment.t =>
  if (same_content(old, fresh)) {
    old;
  } else {
    let rec prefix = (old, fresh, acc) =>
      switch (old, fresh) {
      | ([a, ...aa], [b, ...bb]) when same_piece(a, b) =>
        prefix(aa, bb, [a, ...acc])
      | _ => (List.rev(acc), old, fresh)
      };
    let (pre, old, fresh) = prefix(old, fresh, []);
    let (suf, old, fresh) = prefix(List.rev(old), List.rev(fresh), []);
    let (old, fresh) = (List.rev(old), List.rev(fresh));
    let middle =
      List.length(old) == List.length(fresh)
        ? List.map2_exn(old, fresh, ~f=reuse_piece) : fresh;
    pre @ middle @ List.rev(suf);
  }
and reuse_piece = (old: Piece.t, fresh: Piece.t): Piece.t =>
  if (same_piece(old, fresh)) {
    old;
  } else {
    switch (old, fresh) {
    | (Tile(a), Tile(b))
        when
          same_shape(a, b)
          && List.length(a.children) == List.length(b.children) =>
      Tile({
        ...b,
        children: List.map2_exn(a.children, b.children, ~f=reuse),
      })
    | _ => fresh
    };
  };

/* Recover sharing after a structural operation that kept ids but rebuilt
   records. Strict comparison includes ids (unlike derived Piece.equal). */
let index = (seg: Segment.t): Id.Map.t(Piece.t) => {
  let rec add = (acc, ps) =>
    List.fold(
      ps,
      ~init=acc,
      ~f=(acc, p: Piece.t) => {
        let acc = Id.Map.add(Piece.id(p), p, acc);
        switch (p) {
        | Tile(t) => List.fold(t.children, ~init=acc, ~f=add)
        | _ => acc
        };
      },
    );
  add(Id.Map.empty, seg);
};

let restore = (before: Segment.t, after: Segment.t): Segment.t => {
  let originals = index(before);
  let rec seg = ps => {
    let next = List.map(ps, ~f=piece);
    Segment.ptr_eq(ps, next) ? ps : next;
  }
  and piece = p =>
    switch (Id.Map.find_opt(Piece.id(p), originals)) {
    | Some(old) when phys_equal(old, p) || Poly.compare(old, p) == 0 => old
    | _ =>
      switch (p) {
      | Tile(t) =>
        let children = List.map(t.children, ~f=seg);
        List.for_all2_exn(children, t.children, ~f=phys_equal)
          ? p
          : Tile({
              ...t,
              children,
            });
      | _ => p
      }
    };
  let next = seg(after);
  Segment.ptr_eq(before, next) ? before : next;
};
