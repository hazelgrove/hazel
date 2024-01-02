module Base = {
  type t = Mtrl.t(Sort.t);
};
include Base;

module Molded = {
  include Molded;
  type t = Molded.t(Base.t);
}