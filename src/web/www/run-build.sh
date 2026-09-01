#!/usr/bin/env bash
cd "$(dirname "$0")/../../.."
exec node src/web/www/build-prebundle.mjs
