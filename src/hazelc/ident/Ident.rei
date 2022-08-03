include  (module type of Ident0) with type t = Ident0.t;

module Map: (module type of Util.MapSexp.Make(Ident0));
