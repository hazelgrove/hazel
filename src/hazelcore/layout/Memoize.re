// Memoization on one particular key type
module Make = (Table: Hashtbl.S) => {
  type key = Table.key;
  let make: 'b. (key => 'b, key) => 'b =
    f => {
      let table = Table.create(0);
      key => {
        switch (Table.find_opt(table, key)) {
        | Some(value) => value
        | None =>
          let value = f(key);
          Table.add(table, key, value);
          value;
        };
      };
    };
};

// Memoization on polymorphic key types
module MakePoly = (H: (Hashtbl.HashedType) => Hashtbl.S) => {
  module Key: Hashtbl.HashedType = {
    // We use `Obj.magic` to convert all value to `unit`.  This allows
    // functions that use the memoization table to have a polymorphic key.
    type t = unit;
    let hash = Hashtbl.hash;
    let equal = (==);
  };
  module Make = Make((H(Key)));
  let make: ('a => 'b, 'a) => 'b =
    f => {
      let table: Make.key => 'b = Make.make(key => f(Obj.magic(key)));
      let f' = (key: 'a): 'b => {
        let key': Make.key = Obj.magic(key);
        table(key');
      };
      f';
    };
};

module Strong = (Key: Hashtbl.HashedType) => Make((Hashtbl.Make(Key)));
module Weak = (Key: Hashtbl.HashedType) => Make((Ephemeron.K1.Make(Key))) /* }*/;
module StrongPoly = MakePoly(Hashtbl.Make);
module WeakPoly = MakePoly(Ephemeron.K1.Make);
