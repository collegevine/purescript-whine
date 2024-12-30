#!/bin/bash
set -e

ROOT=$(dirname $(dirname ${BASH_SOURCE[0]}))

pushd $ROOT
npx spago build
$ROOT/dist/bundle.sh

pushd $ROOT/dist/npm
npm publish
