[@deriving sexp]
type t = {id: Id.t};

let init = {id: Id.init(Elaborator)};

let get_id = ({id, _}) => id;
let put_id = (id, _) => {id: id};
let with_id = (f, es) => {
  let (x, id) = es |> get_id |> f;
  (x, es |> put_id(id));
};
