// maybe turn into sum for user-defined ops
type t('builtin) =
  | BuiltIn('builtin);

// use regex
let of_string: string => option(t);
let unsafe_of_string: string => t;
