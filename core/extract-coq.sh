#!/bin/bash

set -eou pipefail

SCRIPTPATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

mkdir -p "$SCRIPTPATH/ml"
coqtop -compile "$SCRIPTPATH/hazel_core" > "$SCRIPTPATH/ml/hazel_core.ml"

