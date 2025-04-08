open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

/* Non-Empty lists */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t('a) = {
  head: 'a,
  tail: list('a),
};

let singleton = head => {
  head,
  tail: [],
};

let option_t_of_list: list('a) => option(t('a)) =
  fun
  | [] => None
  | [head, ...tail] =>
    Some({
      head,
      tail,
    });

let list_of_option_t: option(t('a)) => list('a) =
  fun
  | None => []
  | Some({head, tail}) => [head, ...tail];

let head = ({head, _}) => head;

let tail = ({tail, _}) => tail;

let cons = (type a, x: a, {head, tail}) => {
  head: x,
  tail: [head, ...tail],
};

let append = (x, y) => {
  head: x.head,
  tail: x.tail @ [y.head, ...y.tail],
};

let nth = (n, {head, tail}) =>
  if (n == 0) {
    Some(head);
  } else {
    ListUtil.nth_opt(n - 1, tail);
  };

let length = ({tail, _}) => List.length(tail) + 1;
