# Whine - a configurable PureScript linter with escape hatches all around

## Getting started

1. Install Whine with `npm install whine`
2. Create a config file with `echo "rulePackages: [whine-core]" > ./whine.yaml`
3. Run Whine with `npx whine`
4. Install VSCode extension VSIX from the [release page](https://github.com/collegevine/purescript-whine/releases)

## Configuring rules

In `whine.yaml` every rule can be configured, with configuration options
depending on the rule.

Besides rule-specific configuration, every rule may have `include` and `exclude`
fields specifying globs for included and excluded files respectively. If
`include` is not present, all files are included. If `exclude` is not present,
no files are excluded.

Top level may also specify `include` and `exclude` for globally including or
excluding files respectively. If `include` is not present, it defaults to
`src/**/*.purs`. If `exclude` is not present, it's considered empty.

For example:

```
rulePackages:
  - whine-core

exclude:
  - src/LegacyCode/**/*.purs
  - src/One/Specific/Module.purs

rules:
  UndesirableModules:
    Unsafe.Coerce: Do not use unsafe coercions
    Data.List.Lazy: Do not use lazy lists, use Data.List instead

  CommaFirstArrays:
    exclude:
      - src/Another/Specific/Module.purs
```

## One-off escape hatches

Rules can be disabled a la carte via directives in the code.

A `disable` directive appended to a code line will disable just the one
violation of the rule that starts on that line.

A `disable` directive appearing alone on its own line starts a span where the
rule is disabled. The span can be optionally closed with an `enable` directive.
If not closed, the span ends at the end of the file.

For example:

```haskell
module MyModule where

import Prelude
import Unsafe.Coerce (unsafeCoerce) -- #disable UndesirableModules
import Data.Array (length)

function :: Array Int -> Int
function = length

-- #disable CommaFirstArrays
lengthOfThisArray = function [
  1,
  2,
  3
]
-- #enable CommaFirstArrays

lengthOfAnotherOne = function
  [ 1, 2, 3
  , 4
  , 5
  ]
```