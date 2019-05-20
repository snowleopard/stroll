# Stroll

Stroll is an experiment in developing a *zero configuration build system* that does not
require the user to specify any dependencies between individual build tasks.

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
cd examples/windwows # or examples/linux
stroll build
Executing build\compile.bat...
Executing build\link.bat...
Done
```

Stroll will create `.stroll` files when executing build scripts. For example, the file
`build\compile.bat.stroll` might look like this:

```
exit-code: ExitSuccess
operations:
  obj\Main.hi:
    write: e37964e1cbe289493b797de2dcc149f901fb5c95ab8c5f3ffa6f36b8da8ae5b9
  build\compile.bat:
    read: 55e11faefaffbb93e3c88884c1c26599c37935c8afe9f5b203b94cab69f1e975
  src\HelloWorld.hs:
    read: 71ad4c978a7377555b6c18bccca25e0655b63073e9f0ef91e55149488a94b7c3
  obj\Main.o:
    write: 08398c8e8c1c577ad9459f254346bbc767f6d25b93bf675c28fb2e7f648e7d3c
```
