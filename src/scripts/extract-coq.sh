#!/bin/bash

set -eou pipefail

SCRIPTPATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

coqtop -compile "$SCRIPTPATH/../semantics.v" > "$SCRIPTPATH/../semantics.ml"
refmt "$SCRIPTPATH/../semantics.ml" > "$SCRIPTPATH/../semantics.re"
rm "$SCRIPTPATH/../semantics.ml"

