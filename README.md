# raco mod

Per-project Racket dependencies, resolved with Go's [Minimal Version Selection](https://research.swtch.com/vgo-mvs) and kept out of your global installation. Still using `info.rkt` as manifest format, so the same package still installs with plain `raco pkg`.

## Installation

```sh
raco pkg install --auto raco-mod
```

## Usage

Get dependency for the project (`info.rkt`)

```sh
raco mod get megaparsack-lib@latest  # records #:version in info.rkt
```

We can run subcommands with the current project's dependencies, e.g. run `racket`

```sh
raco mod exec racket
```

Open DrRacket with the project's dependencies, keeping DrRacket plugins installed in user scope

```sh
raco mod drracket
```
