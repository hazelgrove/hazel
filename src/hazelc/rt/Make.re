open Grain;

module type I = FileModuleStatic.I;
module type M = {let path: Path.t;};

/**
  Functor to make stubs for a handwritten module.
 */
module Stub = (M: M) => {
  include FileModuleStatic.Stub.Make({
    let fmodl = M.path |> FileModule.Stub.mk;
  });

  /**
    Implementation module.
   */
  let impl = () => fmodl |> FileModule.of_stub;
};

module Gen = (M: M) =>
  Impl.Make(
    (
      FileModuleStatic.Stub.Make({
        let fmodl = M.path |> FileModule.Stub.mk;
      })
    ),
  );
