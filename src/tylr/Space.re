type char =
  | Space
  | Newline;
type t = list(char);

module Z = {
  type t = int;
};

let empty = [];
