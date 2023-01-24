// I really hate to move all components to CH.re, but I have to,
// because they all rec'd together.

module Ids = CH.Ids;

module UExp = {
  include CH.CExp;

  let mk = (ids: list(Id.t), term): t => {
    mk(Ids.mk(ids), term);
  };
};

module UPat = {
  include CH.CPat;
  let mk = (ids: list(Id.t), term): t => {
    mk(Ids.mk(ids), term);
  };
};

module UTyp = {
  include CH.CTyp;
  let mk = (ids: list(Id.t), term): t => {
    mk(Ids.mk(ids), term);
  };
};

module URul = {
  include CH.CRul;
  let mk = (ids: list(Id.t), term): t => {
    mk(Ids.mk(ids), term);
  };
};

module Any = CH.CAny;
