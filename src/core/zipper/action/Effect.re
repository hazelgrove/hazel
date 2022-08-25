type t =
  | Touch(Id.t)
  | Delete(Id.t);

let s: ref(list(t)) = ref([]);
let s_clear = () => s := [];
let s_touch = (ids: list(Id.t)) =>
  s := List.map(id => Touch(id), ids) @ s^;
