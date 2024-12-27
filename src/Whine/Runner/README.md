Logically this folder should be a separate library, `whine-runner`, so that
`whine-core` contains only stuff needed for authoring rules, while
`whine-runner` provides the environment - reads the config, instantiates rules,
prints results, and so on.

However, this is currently impossible, as the PureScript Registry doesn't
support publishing non-root packages from monorepos yet, so I wouldn't be able
to publish both `whine-core` and `whine-runner`. So for now this is all in the
same library.
