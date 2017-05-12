This describes how lbForth is normally built.

- To build a target, load its build.fth file.  Every target should
  have one.

- build.fth sets up some information about the target, and loads the
  generic compile.fth.

- compile.fth loads the generic metacompiler framework.

  - The generic metacompiler loads a library for accessing a target image.

  - And creates vocabularies for metacompiling words.

- compile.fth proceeds to add metacompiling words that target lbForth.

- And a target assembler.

- It then loads kernel.fth and cold.fth to build a target image.

- Finally, forward references are resolved, and the image is saved to
  disk.
