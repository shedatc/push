push
====

Push is a wrapper around the `rsync` program to make it project-aware. The basic
idea is to make it automatically locate the project root by looking for its
configuration directory (`.push/`). Imagine the following directory layout:

     /
      `- home/
          `- sheda/
              `- lab/
                  `- some-wonderful-project/
                      `- .push/
                      `- src/
                          `- foo/
                              `- bar/
                                 |`- hello.c
                                  `- world.c

The `.push/` directory is used to locate the root of the project. So that
running `push` from, say, the `bar/` directory will run `rsync` from
`some-wonderful-project/` and thus synchronize the project as a whole.
