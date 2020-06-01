type t =
  | State;

type coordinate = {
  x: int,
  y: int,
};
let mouse_position = ref({x: 0, y: 0});
