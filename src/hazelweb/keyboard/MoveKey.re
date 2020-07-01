[@deriving sexp]
type t =
  | ArrowLeft
  | ArrowRight
  | ArrowUp
  | ArrowDown
  | Home
  | End;

let of_key =
  fun
  | "ArrowLeft" => Some(ArrowLeft)
  | "ArrowRight" => Some(ArrowRight)
  | "ArrowDown" => Some(ArrowDown)
  | "ArrowUp" => Some(ArrowUp)
  | "Home" => Some(Home)
  | "End" => Some(End)
  | _ => None;
