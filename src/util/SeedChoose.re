/* Choose the value for a `^^seed(N)` projector at initialization time, given the
   source default N. Installed by each frontend (the CLI prompts and can draw OS
   entropy; the web keeps the default). The pure default keeps N, so a program is
   reproducible wherever no chooser is installed and the library stays free of
   platform dependencies. Mirrors UrlFetch. */
let choose: ref((~default: int) => int) = ref((~default) => default);
