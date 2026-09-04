/* SPIKE (wasm-eval-bench): aggregator for the Bonsai/Virtual_dom half of the
   old [util] library. Re-exports all of [Util] as well, so a module that
   previously said [open Util] and used both halves only needs to say
   [open Util_web] instead. */

include Util;

module BonsaiUtil = BonsaiUtil;
module ClipboardUtil = ClipboardUtil;
module ColumnMenuListener = ColumnMenuListener;
module ContextMenuListener = ContextMenuListener;
module KeyHandlers = KeyHandlers;
module Menu = Menu;
module MenuListener = MenuListener;
module SafeTriangle = SafeTriangle;
module SvgUtil = SvgUtil;
module WebUtil = WebUtil;
