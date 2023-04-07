Personal finance in plain text.

Based on https://github.com/adept/full-fledged-hledger, but with nix and improved folder structure - all immutable inputs are in inputs/, all code is in scripts/, all mutable outputs are in outputs/.

To add a bank put csv and rules to inputs/bank3/YEAR.csv, write extra_deps and export_all rules to scripts/run.hs, rerun scripts/run.sh


```sh
# run with nix
nix develop -c ./run.sh

# or install stack and lts-18.13
stack --system-ghc --resolver lts-18.13 ghci --package shake --package directory
```
