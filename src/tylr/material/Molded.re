module Base = {
  type t('mtrl) = {
    mtrl: 'mtrl,
    mold: Mold.t,
  };
};
include Base;

let map = (f, m) => {...m, mtrl: f(m)};

module Label = {
  type t = Base.t(Mtrl.Label.t);
};

module Sort = {
  type t = Base.t(Mtrl.Sort.t);
};
