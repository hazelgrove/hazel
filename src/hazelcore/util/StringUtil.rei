let is_empty: String.t => bool;

let cat: list(string) => string;
let sep: list(string) => string;

let replicat: (int, string) => string;

/**
 * A string of length n has caret positions 0 through n,
 * where 0 places the caret at the start and n places
 * the caret at the end. Split s at caret_index.
 *
 * TODO rename to split
 */
let split_string: (int, string) => (string, string);

let insert: (int, string, string) => string;

let backspace: (int, string) => string;

let delete: (int, string) => string;

let escaped_enter: string => string;

let utf8_length: CamomileLibrary.UTF8.t => int;

let find_and_replace: (string, string, string) => (string, string);
