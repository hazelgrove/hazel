type color = {
  var_name: string,
  color: string,
};

module LightMode = {
  /* Base Colors - Core foundational colors */
  let base_colors = [
    {
      var_name: "NONE",
      color: "oklch(0% 0 0 / 0%)",
    }, /* transparent */
    {
      var_name: "SAND",
      color: "oklch(99% 0.012 90)",
    }, /* code background */
    {
      var_name: "STONE",
      color: "oklch(52% 0.03 220)",
    }, /* code text */
    {
      var_name: "BLACK",
      color: "oklch(0% 0 0)",
    } /* use sparingly */
  ];

  /* Shale Colors - Focal syntax and top level UI */
  let shale_colors = [
    {
      var_name: "BR1",
      color: "oklch(85% 0.07 90)",
    }, /* caret shard, token buffer */
    {
      var_name: "BR2",
      color: "oklch(from var(--BR1) 70% c h)",
    }, /* exp shard arms */
    {
      var_name: "BR3",
      color: "oklch(from var(--BR1) 60% c h)",
    }, /* top ui accent */
    {
      var_name: "BR4",
      color: "oklch(from var(--BR1) 50% c h)",
    } /* top ui accent */
  ];

  /* Clay Colors - Peripheral syntax */
  let clay_colors = [
    {
      var_name: "T1",
      color: "oklch(97% 0.025 90)",
    }, /* buffer shards */
    {
      var_name: "T2",
      color: "oklch(from var(--T1) 94% c h)",
    }, /* projector shards */
    {
      var_name: "T3",
      color: "oklch(from var(--T1) 91% c h)",
    }, /* result background */
    {
      var_name: "T4",
      color: "oklch(from var(--T1) 88% c h)",
    } /* darker background */
  ];

  /* Molten Colors - Under construction */
  let molten_colors = [
    {
      var_name: "Y0",
      color: "oklch(95% 0.05 90)",
    }, /* menu fill */
    {
      var_name: "Y1",
      color: "oklch(91% 0.11 95)",
    }, /* selections */
    {
      var_name: "Y2",
      color: "oklch(88% 0.2 95)",
    }, /* explicit hole */
    {
      var_name: "Y3",
      color: "oklch(71% 0.2 95)",
    } /* incomplete shards */
  ];

  /* Magma Colors - Error and opportunity states */
  let magma_colors = [
    {
      var_name: "R0",
      color: "oklch(85% 0.1 30)",
    }, /* broken shard fill */
    {
      var_name: "R1",
      color: "oklch(60% 0.3 30)",
    }, /* caret, error stroke */
    {
      var_name: "R2",
      color: "oklch(40% 0.3 30)",
    } /* error text, broken shard text */
  ];

  /* Glass Colors - Type system */
  let glass_colors = [
    {
      var_name: "TYP",
      color: "oklch(60% 0.2 300)",
    }, /* type colors */
    {
      var_name: "PAT",
      color: "oklch(from var(--TYP) l c calc(h - 1 * 75))",
    }, /* pattern colors */
    {
      var_name: "TPAT",
      color: "var(--PAT)",
    }, /* type pattern colors */
    {
      var_name: "LABEL",
      color: "oklch(60% 0.2 180)",
    } /* label colors */
  ];

  /* Aura Colors - Documentation highlighting */
  let aura_colors = [
    {
      var_name: "highlight-a",
      color: "oklch(0.95 0.07 360)",
    }, /* primary highlight */
    {
      var_name: "highlight-b",
      color: "oklch(from var(--highlight-a) l c calc(h - 1 * 120))",
    }, /* secondary highlight */
    {
      var_name: "highlight-c",
      color: "oklch(from var(--highlight-a) l c calc(h - 2 * 120))",
    } /* tertiary highlight */
  ];

  /* Moss Colors - Success and affirmation states */
  let moss_colors = [
    {
      var_name: "G0",
      color: "oklch(70% 0.15 150)",
    }, /* page title, passing tests */
    {
      var_name: "G1",
      color: "oklch(85% 0.15 150)",
    }, /* passing tests hover */
    {
      var_name: "G2",
      color: "oklch(80% 0.05 150)",
    }, /* comments */
    {
      var_name: "GB0",
      color: "oklch(70% 0.05 120)",
    }, /* nut menu active */
    {
      var_name: "GB1",
      color: "oklch(45% 0.05 120)",
    } /* nut menu fill */
  ];

  /* UI Colors - Interface elements */
  let ui_colors = [
    {
      var_name: "primary-accent",
      color: "var(--G0)",
    }, /* primary UI accent */
    {
      var_name: "nut-menu",
      color: "var(--GB1)",
    }, /* navigation menu background */
    {
      var_name: "nut-menu-active",
      color: "var(--GB0)",
    }, /* active menu state */
    {
      var_name: "menu-bkg",
      color: "var(--Y0)",
    }, /* menu background */
    {
      var_name: "menu-item-hover-bkg",
      color: "var(--SAND)",
    }, /* menu item hover */
    {
      var_name: "menu-item-text",
      color: "var(--STONE)",
    }, /* menu text */
    {
      var_name: "menu-outline",
      color: "var(--BR2)",
    }, /* menu borders */
    {
      var_name: "menu-icon",
      color: "var(--BR4)",
    }, /* menu icons */
    {
      var_name: "menu-group-name",
      color: "var(--menu-icon)",
    }, /* menu section headers */
    {
      var_name: "menu-scroll-thumb",
      color: "var(--menu-outline)",
    }, /* scrollbar */
    {
      var_name: "menu-scroll-track",
      color: "var(--BR1)",
    }, /* scrollbar track */
    {
      var_name: "menu-divider",
      color: "var(--CREASE)",
    }, /* menu separators */
    {
      var_name: "menu-shadow",
      color: "var(--SHADOW)",
    }, /* menu shadows */
    {
      var_name: "ui-bkg",
      color: "var(--T1)",
    }, /* main UI background */
    {
      var_name: "ui-header-text",
      color: "var(--BR3)",
    }, /* header text */
    {
      var_name: "toggle-knob",
      color: "var(--SAND)",
    } /* toggle switches */
  ];

  /* Code Colors - Tokens and decorations */
  let code_colors = [
    {
      var_name: "main-bkg",
      color: "var(--T3)",
    }, /* main code background */
    {
      var_name: "cell-active",
      color: "var(--SAND)",
    }, /* active cell background */
    {
      var_name: "main-scroll-thumb",
      color: "var(--BR1)",
    }, /* scrollbar */
    {
      var_name: "main-scroll-track",
      color: "var(--NONE)",
    }, /* scrollbar track */
    {
      var_name: "cell-selected-accent",
      color: "var(--R1)",
    }, /* selection accent */
    {
      var_name: "caret-color",
      color: "var(--R1)",
    }, /* text cursor */
    {
      var_name: "error-hole-stroke",
      color: "var(--R1)",
    }, /* error indicators */
    {
      var_name: "token-exp",
      color: "var(--STONE)",
    }, /* expression tokens */
    {
      var_name: "token-pat",
      color: "var(--PAT)",
    }, /* pattern tokens */
    {
      var_name: "token-typ",
      color: "var(--TYP)",
    }, /* type tokens */
    {
      var_name: "token-tpat",
      color: "var(--TPAT)",
    }, /* type pattern tokens */
    {
      var_name: "token-label",
      color: "var(--LABEL)",
    }, /* label tokens */
    {
      var_name: "token-string-lit",
      color: "var(--Y3)",
    }, /* string literals */
    {
      var_name: "token-comment",
      color: "var(--G2)",
    }, /* comments */
    {
      var_name: "token-incomplete",
      color: "var(--Y3)",
    }, /* incomplete code */
    {
      var_name: "token-inconsistent",
      color: "var(--token-exp)",
    }, /* inconsistent code */
    {
      var_name: "token-buffer",
      color: "var(--BR1)",
    }, /* buffer tokens */
    {
      var_name: "token-explicit-hole",
      color: "var(--Y2)",
    }, /* explicit holes */
    {
      var_name: "token-explicit-hole-shadow",
      color: "var(--BLACK)",
    }, /* hole shadows */
    {
      var_name: "token-secondary",
      color: "var(--shard-exp)",
    }, /* secondary text */
    {
      var_name: "token-rul",
      color: "var(--token-exp)",
    }, /* rule tokens */
    {
      var_name: "token-any",
      color: "var(--R2)",
    } /* any type tokens */
  ];

  /* Shard Colors - Code decoration backgrounds */
  let shard_colors = [
    {
      var_name: "shard-caret-exp",
      color: "var(--T2)",
    }, /* expression caret shards */
    {
      var_name: "shard-lines-exp",
      color: "var(--BR1)",
    }, /* expression shard borders */
    {
      var_name: "shard-exp",
      color: "var(--T2)",
    }, /* expression shards */
    {
      var_name: "shard-caret-pat",
      color: "oklch(from var(--token-pat) 95% calc(c/4) h)",
    }, /* pattern caret shards */
    {
      var_name: "shard-caret-typ",
      color: "oklch(from var(--token-typ) 95% calc(c/4) h)",
    }, /* type caret shards */
    {
      var_name: "shard-caret-tpat",
      color: "oklch(from var(--token-tpat) 95% calc(c/4) h)",
    }, /* type pattern caret shards */
    {
      var_name: "shard-pat",
      color: "var(--shard-caret-pat)",
    }, /* pattern shards */
    {
      var_name: "shard-typ",
      color: "var(--shard-caret-typ)",
    }, /* type shards */
    {
      var_name: "shard-tpat",
      color: "var(--shard-caret-tpat)",
    }, /* type pattern shards */
    {
      var_name: "shard-selected",
      color: "var(--Y1)",
    }, /* selected shards */
    {
      var_name: "shard-buffer",
      color: "var(--T1)",
    }, /* buffer shards */
    {
      var_name: "shard_projector",
      color: "var(--T2)",
    }, /* projector shards */
    {
      var_name: "shard-rul",
      color: "var(--shard-exp)",
    }, /* rule shards */
    {
      var_name: "shard-lines-rul",
      color: "var(--shard-lines-exp)",
    }, /* rule shard borders */
    {
      var_name: "shadow-selected",
      color: "var(--R0)",
    }, /* selection shadows */
    {
      var_name: "shard-any",
      color: "var(--shard-exp)",
    }, /* any type shards */
    {
      var_name: "shadow-any",
      color: "var(--R0)",
    } /* any type shadows */
  ];

  /* Hole Colors - Empty and error holes */
  let hole_colors = [
    {
      var_name: "empty-hole-stroke",
      color: "var(--BR1)",
    }, /* empty hole borders */
    {
      var_name: "empty-hole-fill",
      color: "var(--Y0)",
    }, /* empty hole backgrounds */
    {
      var_name: "error-hole-fill",
      color: "var(--ERRHOLE)",
    } /* error hole backgrounds */
  ];

  /* Backpack Colors - Selection and targeting */
  let backpack_colors = [
    {
      var_name: "backpack-selection",
      color: "var(--shard-selected)",
    }, /* selection backgrounds */
    {
      var_name: "backpack-joiner",
      color: "var(--backpack-selection)",
    }, /* connection lines */
    {
      var_name: "backpack-genie",
      color: "var(--backpack-selection)",
    }, /* genie indicators */
    {
      var_name: "backpack-selection-outline",
      color: "var(--light-page-color)",
    }, /* selection borders */
    {
      var_name: "backback-targets",
      color: "var(--Y3)",
    } /* target indicators */
  ];

  /* Projector Colors - Code projection system */
  let projector_colors = [
    {
      var_name: "textarea-indicated",
      color: "var(--SAND)",
    }, /* indicated text areas */
    {
      var_name: "textarea-text",
      color: "var(--BR3)",
    } /* textarea text */
  ];

  /* Dynamics Colors - Runtime and evaluation */
  let dynamics_colors = [
    {
      var_name: "cell-result-text",
      color: "var(--BR4)",
    }, /* result text */
    {
      var_name: "cell-result-border",
      color: "var(--BR1)",
    }, /* result borders */
    {
      var_name: "cell-result-hidden",
      color: "var(--BR1)",
    }, /* hidden results */
    {
      var_name: "eval-exception",
      color: "var(--test-fail-active)",
    }, /* evaluation errors */
    {
      var_name: "eval-exception-stroke",
      color: "var(--R2)",
    }, /* error outlines */
    {
      var_name: "step-hole-color",
      color: "var(--G0)",
    } /* stepper holes */
  ];

  /* Context Inspector Colors - Code analysis UI */
  let ci_colors = [
    {
      var_name: "ci-icon-bkg",
      color: "var(--BR3)",
    }, /* inspector icons */
    {
      var_name: "ci-status-text",
      color: "var(--BR4)",
    }, /* status text */
    {
      var_name: "ci-status-error-text",
      color: "var(--R2)",
    }, /* error text */
    {
      var_name: "ci-status-error-bkg",
      color: "var(--test-fail-active)",
    }, /* error backgrounds */
    {
      var_name: "context-inspector-colon",
      color: "var(--BR2)",
    } /* separator colons */
  ];

  /* Exercise Mode Colors - Educational features */
  let exercise_colors = [
    {
      var_name: "cell-caption",
      color: "var(--BR2)",
    }, /* exercise captions */
    {
      var_name: "cell-result",
      color: "var(--T3)",
    }, /* exercise results */
    {
      var_name: "cell-exercises-border",
      color: "var(--BR2)",
    }, /* exercise borders */
    {
      var_name: "test-panel-bkg",
      color: "var(--menu-bkg)",
    }, /* test panel background */
    {
      var_name: "test-percent-text",
      color: "var(--SAND)",
    }, /* test percentage text */
    {
      var_name: "test-pass",
      color: "var(--G0)",
    }, /* passing tests */
    {
      var_name: "test-pass-active",
      color: "var(--G1)",
    }, /* active passing tests */
    {
      var_name: "test-fail",
      color: "var(--R1)",
    }, /* failing tests */
    {
      var_name: "test-fail-active",
      color: "var(--R0)",
    }, /* active failing tests */
    {
      var_name: "test-indet",
      color: "var(--BR2)",
    }, /* indeterminate tests */
    {
      var_name: "test-indet-active",
      color: "var(--BR1)",
    } /* active indeterminate tests */
  ];

  /* Special Colors - Miscellaneous utility colors */
  let special_colors = [
    {
      var_name: "textarea-v-stripe",
      color: "oklch(78% 0.14 6 / 55%)",
    }, /* vertical stripes */
    {
      var_name: "textarea-h-stripe",
      color: "oklch(87% 0.07 246)",
    }, /* horizontal stripes */
    {
      var_name: "textarea-h-strip-selected",
      color: "oklch(68% 0.14 76 / 30%)",
    }, /* selected stripes */
    {
      var_name: "SHADOW",
      color: "oklch(50% 0.1 90 / 33%)",
    }, /* general shadows */
    {
      var_name: "ERRHOLE",
      color: "oklch(96% 0.02 47)",
    }, /* error hole backgrounds */
    {
      var_name: "CREASE",
      color: "oklch(0% 0 0 / 40%)",
    } /* crease/divider lines */
  ];

  /* Projector Colors - Interactive code analysis system */
  let projector_colors_extended = [
    {
      var_name: "live-env-bkg",
      color: "var(--T3)",
    }, /* live environment background */
    {
      var_name: "num-closures",
      color: "var(--Y1)",
    }, /* number of closures indicator */
    {
      var_name: "num-closures-indicated",
      color: "var(--R1)",
    }, /* indicated number of closures */
    {
      var_name: "exp-ap",
      color: "hsl(265, 75%, 80%)",
    }, /* expression application */
    {
      var_name: "pat-ap",
      color: "hsl(220, 75%, 80%)",
    }, /* pattern application */
    {
      var_name: "exp-indicated",
      color: "var(--G0)",
    }, /* indicated expression state */
    {
      var_name: "pat-indicated",
      color: "var(--PAT)",
    }, /* indicated pattern state */
    {
      var_name: "exp-ap-indicated",
      color: "var(--TYP)",
    }, /* indicated expression application */
    {
      var_name: "exp-base",
      color: "hsl(120, 40%, 85%)",
    }, /* base expression background */
    {
      var_name: "pat-base",
      color: "hsl(170, 40%, 85%)",
    }, /* base pattern background */
    {
      var_name: "exp-shadow",
      color: "oklch(0.55 0.15 150)",
    }, /* expression shadow */
    {
      var_name: "pat-shadow",
      color: "oklch(0.5 0.1 245)",
    }, /* pattern shadow */
    {
      var_name: "exp-ap-shadow",
      color: "oklch(0.5 0.1 300)",
    }, /* expression application shadow */
    {
      var_name: "exp-cell",
      color: "hsl(115, 30%, 70%)",
    }, /* expression cell background */
    {
      var_name: "pat-cell",
      color: "hsl(165, 30%, 70%)",
    }, /* pattern cell background */
    {
      var_name: "main-base",
      color: "hsl(281, 80%, 95%)",
    }, /* type projector main background */
    {
      var_name: "main-shadow",
      color: "hsl(281, 40%, 25%)",
    }, /* type projector shadow */
    {
      var_name: "main-indicated",
      color: "var(--TYP)",
    } /* type projector indicated state */
  ];

  /* Combined color configuration */
  let vars =
    List.concat([
      base_colors,
      shale_colors,
      clay_colors,
      molten_colors,
      magma_colors,
      glass_colors,
      aura_colors,
      moss_colors,
      ui_colors,
      code_colors,
      shard_colors,
      hole_colors,
      backpack_colors,
      projector_colors,
      projector_colors_extended,
      dynamics_colors,
      ci_colors,
      exercise_colors,
      special_colors,
    ]);
};

module DarkMode = {
  let vars =
    [
      ("NONE", "oklch(0% 0 0 / 0%)"),
      ("SAND", "oklch(25% 0.015 240)"),
      ("STONE", "oklch(75% 0.03 250)"),
      ("BLACK", "oklch(0% 0 0)"),
      ("BR1", "oklch(30% 0.04 250)"),
      ("BR2", "oklch(from var(--BR1) 40% c h)"),
      ("BR3", "oklch(from var(--BR1) 55% c h)"),
      ("BR4", "oklch(from var(--BR1) 70% c h)"),
      ("T1", "oklch(15% 0.02 250)"),
      ("T2", "oklch(from var(--T1) 18% c h)"),
      ("T3", "oklch(from var(--T1) 22% c h)"),
      ("T4", "oklch(from var(--T1) 26% c h)"),
      ("Y0", "oklch(22% 0.06 95)"),
      ("Y1", "oklch(0.32 0.09 169.5)"),
      ("Y2", "oklch(45% 0.15 95)"),
      ("Y3", "oklch(0.65 0.18 118.38)"),
      ("R0", "oklch(40% 0.1 30)"),
      ("R1", "oklch(55% 0.25 30)"),
      ("R2", "oklch(70% 0.3 30)"),
      ("TYP", "oklch(70% 0.18 300)"),
      ("PAT", "oklch(from var(--TYP) l c calc(h - 1 * 75))"),
      ("TPAT", "var(--PAT)"),
      ("LABEL", "oklch(75% 0.15 210)"),
      ("highlight-a", "oklch(65% 0.1 260)"),
      ("highlight-b", "oklch(from var(--highlight-a) l c calc(h - 1 * 120))"),
      ("highlight-c", "oklch(from var(--highlight-a) l c calc(h - 2 * 120))"),
      ("G0", "oklch(65% 0.15 150)"),
      ("G1", "oklch(75% 0.15 150)"),
      ("G2", "oklch(60% 0.05 150)"),
      ("GB0", "oklch(60% 0.05 200)"),
      ("GB1", "oklch(35% 0.05 200)"),
      ("primary-accent", "var(--G0)"),
      ("nut-menu", "var(--G2)"),
      ("nut-menu-active", "var(--GB0)"),
      ("menu-bkg", "var(--T1)"),
      ("menu-item-hover-bkg", "oklch(95% 0.015 240)"),
      ("menu-item-text", "var(--STONE)"),
      ("menu-outline", "var(--BR2)"),
      ("menu-icon", "var(--BR4)"),
      ("menu-group-name", "var(--menu-icon)"),
      ("menu-scroll-thumb", "var(--menu-outline)"),
      ("menu-scroll-track", "var(--BR1)"),
      ("menu-divider", "var(--CREASE)"),
      ("menu-shadow", "var(--SHADOW)"),
      ("ui-bkg", "var(--T2)"),
      ("ui-header-text", "var(--BR4)"),
      ("toggle-knob", "var(--SAND)"),
      ("main-bkg", "var(--T3)"),
      ("cell-active", "oklch(30% 0.02 240)"),
      ("main-scroll-thumb", "var(--BR3)"),
      ("main-scroll-track", "var(--NONE)"),
      ("cell-selected-accent", "var(--R1)"),
      ("caret-color", "var(--R1)"),
      ("error-hole-stroke", "var(--R1)"),
      ("token-exp", "var(--STONE)"),
      ("token-pat", "var(--PAT)"),
      ("token-typ", "var(--TYP)"),
      ("token-tpat", "var(--TPAT)"),
      ("token-label", "var(--LABEL)"),
      ("token-string-lit", "var(--Y3)"),
      ("token-comment", "var(--G2)"),
      ("token-incomplete", "var(--Y3)"),
      ("token-inconsistent", "var(--token-exp)"),
      ("token-buffer", "var(--BR3)"),
      ("token-explicit-hole", "var(--Y2)"),
      ("token-explicit-hole-shadow", "var(--BLACK)"),
      ("token-secondary", "var(--shard-exp)"),
      ("token-rul", "var(--token-exp)"),
      ("token-any", "var(--R2)"),
      ("shard-caret-exp", "var(--T2)"),
      ("shard-lines-exp", "var(--BR2)"),
      ("shard-exp", "var(--T2)"),
      ("shard-caret-pat", "oklch(from var(--token-pat) 40% calc(c/3) h)"),
      ("shard-caret-typ", "oklch(from var(--token-typ) 40% calc(c/3) h)"),
      ("shard-caret-tpat", "oklch(from var(--token-tpat) 40% calc(c/3) h)"),
      ("shard-pat", "var(--shard-caret-pat)"),
      ("shard-typ", "var(--shard-caret-typ)"),
      ("shard-tpat", "var(--shard-caret-tpat)"),
      ("shard-selected", "var(--Y1)"),
      ("shard-buffer", "var(--T1)"),
      ("shard_projector", "var(--T2)"),
      ("shard-rul", "var(--shard-exp)"),
      ("shard-lines-rul", "var(--shard-lines-exp)"),
      ("shadow-selected", "var(--R0)"),
      ("shard-any", "var(--shard-exp)"),
      ("shadow-any", "var(--R0)"),
      ("empty-hole-stroke", "var(--BR3)"),
      ("empty-hole-fill", "var(--T2)"),
      ("error-hole-fill", "var(--ERRHOLE)"),
      ("backpack-selection", "var(--shard-selected)"),
      ("backpack-joiner", "var(--backpack-selection)"),
      ("backpack-genie", "var(--backpack-selection)"),
      ("backpack-selection-outline", "oklch(80% 0.02 250)"),
      ("backback-targets", "var(--Y2)"),
      ("textarea-indicated", "var(--SAND)"),
      ("textarea-text", "var(--STONE)"),
      ("live-env-bkg", "var(--T3)"),
      ("num-closures", "var(--Y1)"),
      ("num-closures-indicated", "var(--R1)"),
      ("exp-ap", "hsl(265, 75%, 50%)"),
      ("pat-ap", "hsl(220, 75%, 50%)"),
      ("exp-indicated", "var(--G0)"),
      ("pat-indicated", "var(--PAT)"),
      ("exp-ap-indicated", "var(--TYP)"),
      ("exp-base", "hsl(210, 20%, 35%)"),
      ("pat-base", "hsl(230, 20%, 35%)"),
      ("exp-shadow", "oklch(0.3 0.1 230)"),
      ("pat-shadow", "oklch(0.25 0.1 260)"),
      ("exp-ap-shadow", "oklch(0.25 0.1 300)"),
      ("exp-cell", "hsl(215, 20%, 40%)"),
      ("pat-cell", "hsl(240, 20%, 40%)"),
      ("main-base", "hsl(250, 30%, 15%)"),
      ("main-shadow", "hsl(250, 30%, 5%)"),
      ("main-indicated", "var(--TYP)"),
      ("cell-result-text", "var(--BR4)"),
      ("cell-result-border", "var(--BR2)"),
      ("cell-result-hidden", "var(--BR1)"),
      ("eval-exception", "var(--test-fail-active)"),
      ("eval-exception-stroke", "var(--R2)"),
      ("step-hole-color", "var(--G0)"),
      ("ci-icon-bkg", "var(--BR2)"),
      ("ci-status-text", "var(--STONE)"),
      ("ci-status-error-text", "var(--R2)"),
      ("ci-status-error-bkg", "var(--test-fail-active)"),
      ("context-inspector-colon", "var(--BR3)"),
      ("cell-caption", "var(--BR3)"),
      ("cell-result", "var(--T3)"),
      ("cell-exercises-border", "var(--BR3)"),
      ("test-panel-bkg", "var(--menu-bkg)"),
      ("test-percent-text", "var(--SAND)"),
      ("test-pass", "var(--G0)"),
      ("test-pass-active", "var(--G1)"),
      ("test-fail", "var(--R1)"),
      ("test-fail-active", "var(--R0)"),
      ("test-indet", "var(--BR2)"),
      ("test-indet-active", "var(--BR1)"),
      ("textarea-v-stripe", "oklch(35% 0.14 230 / 55%)"),
      ("textarea-h-stripe", "oklch(30% 0.07 240)"),
      ("textarea-h-strip-selected", "oklch(45% 0.14 230 / 30%)"),
      ("SHADOW", "oklch(10% 0.08 250 / 50%)"),
      ("ERRHOLE", "oklch(40% 0.02 47)"),
      ("CREASE", "oklch(100% 0 0 / 25%)"),
    ]
    |> List.map(((var_name, color)) =>
         {
           var_name,
           color,
         }
       );
};
let color_theme = (vars: list(color)): Language.Exp.t => {
  open Language;
  open IdTagged.FreshGrammar.Exp;
  let lits =
    List.map(
      ({var_name, color}) => tuple([string(var_name), string(color)]),
      vars,
    );
  list_lit(lits);
};

let segment = {
  open Language;
  open Haz3lcore;
  let light = color_theme(LightMode.vars);
  let dark = color_theme(DarkMode.vars);
  let exp =
    IdTagged.FreshGrammar.(
      Exp.(
        let_(
          Pat.var("light"),
          light,
          let_(
            Pat.var("dark"),
            dark,
            if_(bool(true), var("light"), var("dark")),
          ),
        )
      )
    );

  ExpToSegment.exp_to_segment(
    ~settings=
      ExpToSegment.Settings.editable(~inline=false, ~multiline_lists=true),
    exp,
  )
  |> PersistentSegment.persist;
};
