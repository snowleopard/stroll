# Stroll

[![Build status](https://img.shields.io/github/actions/workflow/status/snowleopard/stroll/ci.yml?branch=master)](https://github.com/snowleopard/stroll/actions)

Stroll is an experiment in developing a language-agnostic build system that does
not require the user to specify dependencies between individual build tasks. By "language-agnostic" I mean the user does not need to learn a new language or
special file format for describing build tasks -- existing scripts or
executable files can be used as build tasks directly without modification.

I call this build system Stroll because its build algorithm reminds me of
strolling through a park where you've never been before, trying to figure out an
optimal path to your target destination, and likely going in circles
occasionally until you've built up a complete mental map of the park.

See an introductory blog post here: https://blogs.ncl.ac.uk/andreymokhov/stroll/.

## How to try

First, build and install Stroll, e.g. using Stack:

```bash
stack install
```

Stroll uses the [`fsatrace` utility](https://github.com/jacereda/fsatrace) for
tracking file-system operations when executing build tasks. You therefore need
to have `fsatrace` on your `PATH`.

Now you can try the included example:

```bash
$ cd examples/windows # or examples/linux
$ stroll build
Executing build/lib.bat...
Executing build/main.bat...
Done
```

Stroll will create `.stroll` files when executing build scripts. For example,
the file `build/lib.bat.stroll` might look like this:

```
exit-code: ExitSuccess
operations:
  bin/lib/lib.o:
    write: 16b8739d090e7c2ccd1bf38130c2d3b82071182bd4a53177c7bde08fcb0a719e
  build/lib.bat:
    read: 4e0ec561c9a00e3184f8cc3ad847cdecb0e7174ddb51144f81eb18c779927df5
  src/lib/lib.h:
    read: aea5eaf14b630a535e0d6ddd15adc721a43a112c2ef645d8a02d2e55cd5ce231
  src/lib/lib.c:
    read: 244f7e2a4de64201b6a44637de36d6dba9225c58d5f29debaf0f39f53ea770cf
```

To visualise the dependency graph you can run Stroll with the `-g` flag,
redirecting the output to a program that can render `dot` files:

```bash
$ stroll -g build | dot -Tpng -Gdpi=600 -o graph.png
```
