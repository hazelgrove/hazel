let uppercase_char: char => bool = (
  fun
  | 'A' .. 'Z' => true
  | _ => false:
    char => bool
);

let lowercase_char: char => bool = (
  fun
  | 'a' .. 'z' => true
  | _ => false:
    char => bool
);

let digit_char: char => bool = (
  fun
  | '0' .. '9' => true
  | _ => false:
    char => bool
);
