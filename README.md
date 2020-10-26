# ASCIInema Autorecorder

An automatic [`asciinema`](https://asciinema.org/) cast recorder.

Instead of having to manually type in the commands while `asciinema rec` is recording, you can specify what needs to happen declaratively.

This is an example spec file that shows how to use `autorecorder` in a cast (very meta):

[![asciicast](https://asciinema.org/a/362623.png)](https://asciinema.org/a/362623)

```
packages:
- autorecorder
- asciinema
working-dir: .
file: echo.cast
environment:
  ASCIINEMA_CONFIG_HOME:
    "./config/asciinema"
input:
- type: "autorecorder record echo.yaml echo.cast --progress\n"
- wait: 10000
- type: "asciinema play echo.cast\n"
- wait: 10000
```

I would put a link to the output cast here as well but https://asciinema.org is currently having trouble with uploads and logins.
See the `examples` directory for more examples.

For the full spec file format, try recording a cast for an invalid spec file, and the error message will show you the format (in colour!).


## Calling from nix

Import the `mkCastDerivation` function, then call it on your spec:

```
let
  mkCastDerivation = import (
      builtins.fetchGit {
        url = "https://github.com/NorfairKing/autorecorder";
        rev = "da5bf9d61108a4a89addc8203b1579a364ce8c01";
        ref = "master";
      } + "/nix/cast.nix"
    ) { inherit pkgs; };
in
  mkCastDerivation {
    name = "my-cast";
    src = ./my-cast-spec.yaml;
  }
```
