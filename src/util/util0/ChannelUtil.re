module Syntax = {
  let (let&i) = (ch, fn) =>
    Fun.protect(~finally=() => close_in(ch), () => {fn(ch)});
  let (let&o) = (ch, fn) =>
    Fun.protect(~finally=() => close_out(ch), () => {fn(ch)});
};
