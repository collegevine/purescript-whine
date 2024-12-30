#!/bin/bash
set -e

ROOT=$(dirname $(dirname ${BASH_SOURCE[0]}))
npx esbuild $ROOT/output/Whine.Runner.Client.Main/index.js --bundle --outfile=$ROOT/dist/vscode-extension/extension.js --platform=node --format=cjs --external:vscode
npx esbuild $ROOT/dist/npm/entryPoint.js --bundle --outfile=$ROOT/dist/npm/index.js --platform=node --format=cjs

version=$($ROOT/dist/npm/index.js --version)
description="PureScript linter, extensible, with configurable rules, and one-off escape hatches"

for file in $ROOT/dist/npm/package.json $ROOT/dist/vscode-extension/package.json; do
  sed -i "s/\"version\": \".*\"/\"version\": \"$version\"/g" $file
  sed -i "s/\"description\": \".*\"/\"description\": \"$description\"/g" $file
done

cp $ROOT/LICENSE.txt $ROOT/dist/vscode-extension
pushd $ROOT/dist/vscode-extension
npx vsce package --out ./purescript-whine-$version.vsix
popd
