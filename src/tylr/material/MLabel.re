module Base = {
  type t = Mtrl.t(Label.t);
};
include Base;

module Molded = {
  include Molded;
  type t = Molded.t(Base.t);
};
