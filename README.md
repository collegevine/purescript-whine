# Whine - a configurable linter with escape hatches all around

A work in progress, this project is a PureScript linter based on pluggable rules
and powerful configuration via an external config file, allowing to
include/exclude specific files (or globs) and enable/disable/configure specific
rules. Plus, escape hatches for one-off opt-outs when you really need it.
Inspired by Rubocop.

At the moment, the UX is minimal:
1. The program takes a set of globs as command-line parameters, e.g. `node /where/is/whine.js ./src/**/*.purs`
    * The plan is to properly parse command-line parameters to allow for switches.
1. The set of rules and their IDs are specified in program code, in the `Main` module.
    * This is likely to stay that way for a long time, but the dream is to allow
      pluggable rule packages, implemented via the upcoming `spago script`
      feature.
1. The output is a list of rule violations found + non-zero exit code if there
   are any violations.
    * No particular plans for this one, but one could envision reporting
      violations as JSON or something. Would be handy to implement a VSCode
      extension.
