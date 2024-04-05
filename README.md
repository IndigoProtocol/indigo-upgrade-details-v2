# Indigo V2 Upgrade Details
This repository is meant to be able to display how the validators and minting policies were generated for V2 of Indigo Protocol as well as the V1 -> V2 upgrade. You can locate the UPLC of the v2 contracts in the `indigo-v2/scripts` directory.

## Prerequisite

Make sure `nix` is installed.

Subsequent instructions assume you are inside the dev env shell.
To get there do `nix develop` (if this doesn't work and your nix is older then do `nix-shell`).

## Build

```sh
cabal build
```

## Run Upgrade Details

``` sh
cabal run upgrade-details
```

## Publishing named-debrujin from UPLC
The Plutus Simple Model package that we use to be able to test the validators requires named-debrujin files rather than raw UPLC to be able to run tests. To be able to compile the named-debrujin from the UPLC we can run the following script for each UPLC.

``` sh
aiken uplc encode --to named-debruijn $UPLC_FILE > $NAMED_DEBRUJIN_FILE
```