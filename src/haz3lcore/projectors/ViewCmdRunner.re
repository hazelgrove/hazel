/* The view runner lives in the language library now, so the eval worker
   (which links only language) can run livelit views; this keeps its name
   here for the projectors and the tests. */
include Language.ViewCmdRunner;
