type t =
  | State;

type coordinate = {
  x: float,
  y: float,
};
let mouse_position = ref({x: 0.0, y: 0.0});
