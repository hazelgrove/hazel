include
   (module type of KindSystem.Kind_core) with
    type s('idx) = KindSystem.Kind_core.s('idx);
include  (module type of KindSystem.Kind) with type t = KindSystem.Kind.t;
