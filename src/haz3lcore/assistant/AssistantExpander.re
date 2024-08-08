/* We decorate buffers whose content will result in an
 * expansion with a trailing "...". Note that this ...
 * (at least in the current implementation) is not literally
 * inserted into the syntax so will not be reflected
 * in the decoration metrics */

let last = t => String.sub(t, String.length(t) - 1, 1);

let is_expander = (label: Label.t) =>
  switch (label) {
  | [t] => last(t) == " " || last(t) == "("
  | _ => false
  };

let mark = (label: Label.t): Label.t =>
  is_expander(label) ? List.map(t => t ++ "…", label) : label;
