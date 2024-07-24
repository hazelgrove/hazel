open Util;
//open Virtual_dom.Vdom;

/* This module, along with ZipperBase, exists to resolve
 * cyclic dependencies between Zipper and Projector. All
 * projector functionality should be added to Projector.re
 * or above unless it would create such a cycle */

/* Enumeration of different kinds of projectors. This is
 * used as a key when adding new projectors to an editor. */
[@deriving (show({with_path: false}), sexp, yojson)]
type kind = Base.kind;

/* The projector map is store alongside the zipper and
 * maps syntax UUIDs to instantiated projectors. This
 * is how projector placement and models are persisted */
[@deriving (show({with_path: false}), sexp, yojson)]
module Map = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type entry = {
    kind,
    model: string,
  };
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(entry);
  open Id.Map;
  let empty = empty;
  let find = find_opt;
  let mem = mem;
  let mapi = mapi;
  let update = update;
};

/* Projectors currently have two options for placeholder
 * shapes: A inline display of a given length, or a block
 * display with given length & height. Both of these can
 * depend on the projector model and info package */
[@deriving (show({with_path: false}), sexp, yojson)]
type shape = ProjNew.shape;

/* The type of syntax which a projector can replace.
 * Right now projectors can replace a single piece */
[@deriving (show({with_path: false}), sexp, yojson)]
type syntax = ProjNew.syntax;

/* Global actions available to handlers in all projectors */
type external_action = ProjNew.external_action;

/* External info fed to all projectors. Eventually
 * dynamic information will be added here. Projector
 * position and dimensions in base editor could be
 * added here if needed */
[@deriving (show({with_path: false}), sexp, yojson)]
type info = ProjNew.info;

let info_init = (p: syntax) =>
  ProjNew.{id: Piece.id(p), syntax: p, ci: None};

/* To add a new projector, implement this module signature */
module type Projector = ProjNew.Projector;

/* Projector model and action are serialized so that
 * they may be used by the Editor without it having
 * specialized knowledge of projector internals */
type serialized_model = ProjNew.serialized_model;
type serialized_action = ProjNew.serialized_action;

/* A cooked projector is the same as the base module
 * signature except model & action are serialized */
module type Cooked = ProjNew.Cooked;

module Cook = ProjNew.Cook;
