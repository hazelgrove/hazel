open IdTagged;
include Grammar.Factory({
  type t = IdTag.t;
  let default_value = (): IdTag.t => IdTag.temp;
});
