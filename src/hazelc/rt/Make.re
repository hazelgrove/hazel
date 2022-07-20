open Grain;

module type M = {
  let name: ident;
  let path: string;
};

module WithoutImpl = (M: M) => {
  include Lib.Make({
    let name = M.name;
    let path = ImportStd(M.path);
  });

  /**
    Implementation module.
   */
  let impl_md = () => None;
};

module WithImpl = (M: M) => {
  module Here = WithoutImpl(M);

  include Here;
  include Impl.Make(Here);

  let impl_imports = ref([]);
  let impl_decl = ref([]);

  let add_impl_import = import => {
    impl_imports := [TImport(import), ...impl_decl^];
  };

  let add_impl_fn = fn => {
    impl_decl := [TDecl(DStmt(Impl.get_impl(fn))), ...impl_decl^];
    Impl.call(fn);
  };

  /**
    Implementation module.
   */
  let impl_md = () => {
    let tstmts = List.rev(impl_imports^) @ List.rev(impl_decl^);
    FileModule.mk(Here.path, (tstmts |> List.map(tstmt => tstmt), []))
    |> Option.some;
  };
};
