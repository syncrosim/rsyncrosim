# SyncroSim Library class

`SsimLibrary` object representing a SyncroSim Library. A SsimLibrary is
the highest level of organization in the SyncroSim workflow and contains
at least one `Project`.

## Slots

- `session`:

  `Session` object

- `filepath`:

  character string. The path to the SsimLibrary on disk

- `datasheetNames`:

  character string. The name and scope of all Datasheets in the
  SsimLibrary.

## See also

See
[`ssimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/ssimLibrary.md)
for options when creating or loading a SyncroSim SsimLibrary.
