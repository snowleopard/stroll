# Stroll

Stroll is an experiment in developing a *zero configuration build system* that does not
require the user to specify any dependencies between individual build tasks.

## How to try

First, build and install Stroll, e.g. using Stack:

```bash
stack install
```

Then try the included example:

```bash
$ cd examples/windwows # or examples/linux
$ stroll build
Executing build\compile.bat
Executing build\link.bat
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
    read: 7de7012e6bce950cfb007b7477bb9213a76bb45485be2281335454ab4ebe220e
  src\HelloWorld.hs:
    read: 1e1c31aee06bc1fff5d38d16edfbd8f26e2d5220bf19c98dce6b8d1891b55e06
  obj\Main.o:
    write: 08398c8e8c1c577ad9459f254346bbc767f6d25b93bf675c28fb2e7f648e7d3c
```
