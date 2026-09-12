open Util;

/* What the cursor inspector knows about a Fumola subterm.

   The Fumola counterpart of DrvInfo, and smaller than it: Fumola has no
   statics in Hazel, so there is no type to report and no sort to check a
   term against -- Fumola(Exp) is a single closed sort. What is left is the
   syntactic class and the ancestry, which is what a reader wants and what
   was missing entirely. A hole still reports as an error, because a hole is
   the one thing about a Fumola program Hazel can judge on its own. */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type ancestors = list(Id.t);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type error =
  | BadToken(string)
  | MultiHole;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type status =
  | NotInHole
  | InHole(error);

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  cls: FumolaCls.t,
  ancestors,
  status,
};

let cls_of: t => FumolaCls.t = ({cls, _}) => cls;

let id_of: t => Id.t = ({id, _}) => id;

let ancestors_of: t => ancestors = ({ancestors, _}) => ancestors;

let error_of: t => option(error) =
  ({status, _}) =>
    switch (status) {
    | NotInHole => None
    | InHole(err) => Some(err)
    };

let is_error: t => bool =
  ({status, _}) =>
    switch (status) {
    | NotInHole => false
    | InHole(_) => true
    };

let status_of_hole: FumolaTermBase.hole => status =
  fun
  | Invalid(token) => InHole(BadToken(token))
  | MultiHole(_) => InHole(MultiHole)
  | EmptyHole => NotInHole;

let mk = (~id, ~ancestors, ~status=NotInHole, cls: FumolaCls.t): t => {
  id,
  cls,
  ancestors,
  status,
};

let of_exp = (~ancestors, e: FumolaTermBase.t): t => {
  let status =
    switch (e.term) {
    | Hole(h) => status_of_hole(h)
    | _ => NotInHole
    };
  mk(
    ~id=IdTagged.rep_id(e),
    ~ancestors,
    ~status,
    FumolaCls.of_exp_term(e.term),
  );
};

let of_dec = (~ancestors, d: FumolaTermBase.dec): t => {
  let status =
    switch (d.term) {
    | DHole(h) => status_of_hole(h)
    | _ => NotInHole
    };
  mk(~id=IdTagged.rep_id(d), ~ancestors, ~status, FumolaCls.of_dec(d));
};

let of_pat = (~ancestors, p: FumolaTermBase.pat): t => {
  let status =
    switch (p.term) {
    | PHole(h) => status_of_hole(h)
    | _ => NotInHole
    };
  mk(~id=IdTagged.rep_id(p), ~ancestors, ~status, FumolaCls.of_pat(p));
};
