let codegen_grain: (~opts: Codegen_grain.opts, Mir.Anf.block) => Grain.prog;

module Grain = Codegen_grain;
