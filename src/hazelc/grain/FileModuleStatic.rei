module type I = {
  let name: Ident.t;
  let from: Path.t;
};

module type M = {let fmodl: FileModule.t;};
module type S = {
  include M;
  module Use: (I) => ImportStatic.S;
};

module Make: (M) => S;

module Full: {
  module type M = {let fmodl: FileModule.full;};
  module type S = {
    include M;
    module Use: (I) => ImportStatic.S;
  };

  module Make: (M) => S;
};

module Stub: {
  module type M = {let fmodl: FileModule.stub;};
  module type S = {
    include M;
    module Use: (I) => ImportStatic.S;
  };

  module Make: (M) => S;
};
