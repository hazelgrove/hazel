open Util;

/*
 ------------------------------------------------
 Generic Parameter Set Scaffolding:
 ------------------------------------------------

 module ParameterName = {
     type t = The actual contents of the parameter

     --
     let val_1 = ...;
     let val_2 = ...;
     let val_3 = ...;
     ...
     let val_k = ...;
     // definitions of hardcoded values to place into the parameter set
     --
     let self: list(t) = [val_1, val_2, val_3, ...val_k];
 }

 We can then take the cartesian product between *all* parameter sets,
 creating n_1 * n_2 * ... * n_k unique cases
 Where n_i is the number of values in the i-th parameter set

 */

module SketchPrompt = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    sketch: string, // the initial sketch
    prompt: string // the task request for the agent to complete
  };

  let scenario_1 = {
    sketch: EmojiPaint.self,
    prompt: "think about how to update the program with the following new abilities: the ability to indicate a sub-grid of the canvas and copy that subgrid as a kind of 'stamp' to the palette, and then to stamp that stamp onto the grid by specifying an upper-left coordinate to begin the stamp at. let's proceed in a type-directed-ish way; how should be update the types, and then how should we update the functions, possibly adding new ones? the program is written in hazel, a low-resource language which you probably haven't seen much if any of before, so try to be sparring in any syntax used beyond that represented in the code example.",
  };

  let scenario_2 = {
    sketch: ListFuns.self,
    prompt: "add a function to get the nth element of a list",
  };

  let scenario_3 = {
    sketch: "",
    prompt: "write a simple rock paper scissors game",
  };

  let self = [scenario_1, scenario_2, scenario_3];
};

module ToolKit = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = CompositionTools.t; // the tools available to the agent

  let all_tools = CompositionTools.tools;

  // Create tool set abalations.
  // Each set of tools within this list will be removed from a tool kit for a given run.
  let exclude_these = [];

  let ablate = (tools, exclude_these): list(t) => {
    exclude_these
    |> List.map(exclude_this => {
         tools |> List.filter(tool => !List.mem(tool, exclude_this))
       });
  };

  // We can perform an ablation study on the tool kit
  // What tools are necessary for the agent?
  // What tools significantly improve the agent's performance?

  let self: list(t) = [all_tools] @ ablate(all_tools, exclude_these);
};

module LLM = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = string; // the LLM to use

  let self: list(t) = [];
};
