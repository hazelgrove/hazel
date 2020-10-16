let uppercase_char : char -> bool = function
  | 'A' .. 'Z' -> true
  | _ -> false

let lowercase_char : char -> bool = function
  | 'a' .. 'z' -> true
  | _ -> false

let digit_char : char -> bool = function '0' .. '9' -> true | _ -> false
