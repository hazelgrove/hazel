open Haz3lcore;
open Util;
open OptUtil.Syntax;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    id: Id.t,
    name: string,
    file_system: FileSystem.Model.t,
    agent: Agent.Model.t,
    // agent: Agent.Model.t
    // ...
  };
};

module Persistent = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    id: Id.t,
    name: string,
    file_system: FileSystem.Persistent.t,
    // agent: Agent.Persistent.t
    // ...
  };

  let persist = (model: Model.t): t => {
    {
      id: model.id,
      name: model.name,
      file_system: FileSystem.Persistent.persist(model.file_system),
    };
  };

  let unpersist = (~settings, p: t): Model.t => {
    {
      id: p.id,
      name: p.name,
      file_system: FileSystem.Persistent.unpersist(~settings, p.file_system),
    };
  };
};

module Utils = {
  let mk_new_project = (name: string): Model.t => {
    let id = Id.mk();
    {
      id,
      name,
      file_system: FileSystem.Utils.init(),
    };
  };
};
