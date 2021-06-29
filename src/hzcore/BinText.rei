// maybe turn into sum for user-defined ops
type t = pri string;

// use regex
let of_string: string => option(t);
let unsafe_of_string : string => t;
