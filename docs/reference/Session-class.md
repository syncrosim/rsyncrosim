# SyncroSim Session class

A SyncroSim Session object contains a link to a SyncroSim installation.
`SsimLibrary`, `Project` and `Scenario` objects contain a `Session` used
to query and modify the object.

## Slots

- `filepath`:

  The path to the SyncroSim installation

- `silent`:

  If `FALSE`, all SyncroSim output with non-zero exit status is printed.
  Helpful for debugging. Default is `TRUE`

- `printCmd`:

  If `TRUE`, arguments passed to the SyncroSim console are also printed.
  Helpful for debugging. Default is `FALSE`

- `condaFilepath`:

  The path to the Conda installation. Default is `"default"`

## See also

See
[`session`](https://syncrosim.github.io/rsyncrosim/reference/session.md)
for options when creating a Session.
