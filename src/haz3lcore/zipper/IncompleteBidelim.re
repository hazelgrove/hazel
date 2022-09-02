type t = Id.Map.t(list(int));

let t = ref(Id.Map.empty);

let contains = (id, i): bool =>
  switch (Id.Map.find_opt(id, t^)) {
  | None => false
  | Some(is) => List.mem(i, is)
  };

let clear = () => {
  t := Id.Map.empty;
};

// assumes seg is fully assembled
let set = (seg: Base.segment): unit =>
  t :=
    seg
    |> List.filter_map(
         fun
         | Piece.Tile(t) => {
             let (l_shard, r_shard) = Tile.(l_shard(t), r_shard(t));
             let l = l_shard == 0 ? [] : [l_shard - 1];
             let r = r_shard == List.length(t.label) - 1 ? [] : [r_shard];
             let lr = l @ r;
             lr == [] ? None : Some((t.id, l @ r));
           }
         | Grout(_)
         | Whitespace(_) => None,
       )
    |> List.to_seq
    |> Id.Map.of_seq;
