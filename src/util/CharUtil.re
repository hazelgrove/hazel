let is_alpha_lower = c =>
  Char.(code('a') <= code(c) && code(c) <= code('z'));
let is_alpha_upper = c =>
  Char.(code('A') <= code(c) && code(c) <= code('Z'));
let is_alpha = c => is_alpha_lower(c) || is_alpha_upper(c);

let is_digit = c => Char.(code('0') <= code(c) && code(c) <= code('9'));

let is_alphanum = c => is_alpha(c) || is_digit(c);
