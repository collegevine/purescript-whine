#!/bin/bash
set -e

ROOT=$(dirname $(dirname ${BASH_SOURCE[0]}))

pushd $ROOT
npx spago build
$ROOT/dist/bundle.sh

cp README.md $ROOT/dist/npm/
pushd $ROOT/dist/npm
npm publish
