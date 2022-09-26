[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Touch(Id.t)
  | Delete(Id.t);

// used to record effects over the course of a single action
let s: ref(list(t)) = ref([]);
let s_clear = () => s := [];
let s_touch = (ids: list(Id.t)) =>
  s := List.map(id => Touch(id), ids) @ s^;

let s_touched = (id: Id.t): bool => List.mem(Touch(id), s^);
