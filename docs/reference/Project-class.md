# SyncroSim Project class

`Project` object representing a SyncroSim Project. A Project is the
intermediate level of organization in the SyncroSim workflow, between
the
[`ssimLibrary`](https://syncrosim.github.io/rsyncrosim/reference/ssimLibrary.md)
and the
[`scenario`](https://syncrosim.github.io/rsyncrosim/reference/scenario.md).
It contains information relevant to a group of Scenarios.

## Slots

- `session`:

  `Session` object. The Session associated with the Project's
  SsimLibrary

- `filepath`:

  character string. The path to the Project's SsimLibrary on disk

- `datasheetNames`:

  Names and scopes of datasheets in the Project's Library

- `projectId`:

  integer. The Project id

## See also

See
[`project`](https://syncrosim.github.io/rsyncrosim/reference/project.md)
for options when creating or loading a SyncroSim Project.
