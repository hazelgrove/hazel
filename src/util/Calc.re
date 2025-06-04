/*
     A helper module for making things that look incremental (but aren't
     because we haven't integrated incrementality yet). Eventually this module
     will hopefully be made redundant by the Bonsai tree.
 */

// ================================================================================
// t('a) is the basic datatype that stores a value and whether it has been updated

[@deriving (show({with_path: false}), sexp, yojson)]
type t('a) =
  | OldValue('a)
  | NewValue('a);

let combine = (x: t('a), y: t('b)): t(('a, 'b)) =>
  switch (x, y) {
  | (OldValue(x), OldValue(y)) => OldValue((x, y))
  | (OldValue(x) | NewValue(x), OldValue(y) | NewValue(y)) =>
    NewValue((x, y))
  };

let make_old = (x: t('a)): t('a) =>
  switch (x) {
  | OldValue(x)
  | NewValue(x) => OldValue(x)
  };

let get_value = (x: t('a)): 'a =>
  switch (x) {
  | OldValue(x)
  | NewValue(x) => x
  };

let map_if_new = (f: 'a => 'a, x: t('a)): t('a) =>
  switch (x) {
  | OldValue(x) => OldValue(x)
  | NewValue(x) => OldValue(f(x))
  };

let is_new = (x: t('a)): bool =>
  switch (x) {
  | OldValue(_) => false
  | NewValue(_) => true
  };

// ================================================================================
// saved('a) is used to store a value that has been calculated in the model
[@deriving (show({with_path: false}), sexp, yojson)]
type saved('a) =
  | Pending
  | Calculated('a);

let get_saved = (default, x: saved('a)): 'a =>
  switch (x) {
  | Pending => default
  | Calculated(x) => x
  };

let map_saved = (f: 'a => 'b, x: saved('a)): saved('b) =>
  switch (x) {
  | Pending => Pending
  | Calculated(x) => Calculated(f(x))
  };

/* Using update, we can make a value of saved('a) that recalculates whenever
   the value of t('a) changes. */
let update = (x: t('a), f: 'a => 'b, y: saved('b)): t('b) =>
  switch (y, x) {
  | (Pending, OldValue(x)) => NewValue(f(x))
  | (Pending | Calculated(_), NewValue(x)) => NewValue(f(x))
  | (Calculated(y), OldValue(_)) => OldValue(y)
  };

/* Using set, we can compare some value to the previously saved value, and create
   a new t('a) that indicates whether the value has changed. */
let set = (~eq: ('a, 'a) => bool=(==), x: 'a, y: saved('a)) =>
  switch (y) {
  | Pending => NewValue(x)
  | Calculated(x') when eq(x, x') => OldValue(x)
  | Calculated(_) => NewValue(x)
  };

/* Save takes a value of t('a) that has been recalculated and stores it in a
   saved so it can be put back in the model */
let save = (x: t('a)): saved('a) =>
  switch (x) {
  | OldValue(x)
  | NewValue(x) => Calculated(x)
  };

// ================================================================================
// Helper functions:

let to_option = (x: t(option('a))): option(t('a)) => {
  switch (x) {
  | OldValue(Some(x)) => Some(OldValue(x))
  | NewValue(Some(x)) => Some(NewValue(x))
  | OldValue(None) => None
  | NewValue(None) => None
  };
};

module Syntax = {
  let (let.calc) = update;
  let (and.calc) = combine;
};
