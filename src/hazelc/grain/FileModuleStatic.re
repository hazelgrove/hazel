module type I = {
  let name: Ident.t;
  let from: Path.t;
};

module type M = {let fmodl: FileModule.t;};
module type S = {
  include M;
  module Use: (I) => ImportStatic.S;
};

module Make = (M: M) => {
  include M;

  module Use = (I: I) => {
    let imp = FileModule.import(fmodl, ~name=I.name, ~from=I.from);

    include ImportStatic.Of({
      let v = `Import(imp);
    });
  };
};

module Full = {
  module type M = {let fmodl: FileModule.full;};
  module type S = {
    include M;
    module Use: (I) => ImportStatic.S;
  };

  module Make = (M: M) => {
    include M;

    module Use = (I: I) => {
      let imp = FileModule.Full.import(fmodl, ~name=I.name, ~from=I.from);

      include ImportStatic.Of({
        let v = `Import(imp);
      });
    };
  };
};

module Stub = {
  module type M = {let fmodl: FileModule.stub;};
  module type S = {
    include M;
    module Use: (I) => ImportStatic.S;
  };

  module Make = (M: M) => {
    include M;

    module Use = (I: I) => {
      let imp = FileModule.Stub.import(fmodl, ~name=I.name, ~from=I.from);

      include ImportStatic.Of({
        let v = `Import(imp);
      });
    };
  };
};
