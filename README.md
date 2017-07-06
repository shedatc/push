Introduction
============

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

    $ pwd
    /home/sheda/lab/git/push
    $ ./push
    push: Missing root.
    CallStack (from HasCallStack):
      error, called at push.hs:126:34 in main:Main
    $ cd lab/some-wonderful-project/src/foo/bar
    $ /home/sheda/lab/git/push/push
    Root: /home/sheda/lab/git/push/lab/some-wonderful-project
    push: Missing parameter: target-host
    CallStack (from HasCallStack):
      error, called at push.hs:32:44 in main:Main
    $

Configuration
=============

Configuration go to the `.push/` directory at the root of the project. The
configuration structure is directly mapped onto the filesystem: parameters are
just files with their content being the value. So to set the hostname to which
files must be sent, just `echo foo.org > .push/target-host`.

- `.push/target-host` The target's hostname.

- `.push/remote-path` The destination path on the target host.

- `.push/rsync-options` The base `rsync` options to use. Default to `-vv -a
  --no-group --no-perms -T /tmp`.

- `.push/additional-rsync-options` The `rsync` options to add to the base ones.

- `.push/no-delete` If present, don't use the `--delete` option.

- `.push/includes` Add inclusion patterns to the filter rules.

- `.push/excludes` Add exclusion patterns to the filter rules.

Filtering
=========

The `.push/includes` and `.push/excludes` parameters control what is sent to the
target host. Default is to exclude everything.
