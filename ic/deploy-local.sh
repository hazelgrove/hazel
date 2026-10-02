#!/usr/bin/env bash
# Start the project's local network if it is not running, then deploy both
# canisters. Local only: nothing here touches the IC mainnet.
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"
icp network status >/dev/null 2>&1 || icp network start -d
icp deploy
