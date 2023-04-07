#!/usr/bin/env bash
set -euo pipefail

SCRIPTS="${BASH_SOURCE%/*}"

# ghc-8.10.7 - lts-18.12
stack --system-ghc --resolver lts-18.12 script --package shake --package directory --optimize "$SCRIPTS/run.hs"
