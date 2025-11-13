# SyncroSim Scenario class

`Scenario` object representing a SyncroSim Scenario. A Scenario is the
lowest level of organization in the SyncroSim workflow, and is often
used to isolate information on a single Datasheet.

## Slots

- `session`:

  `Session` object. The Session associated with the Scenario

- `filepath`:

  character string. The path to the Scenario's SsimLibrary on disk

- `datasheetNames`:

  character string. Names and scope of all Datasheets in Scenario's
  SsimLibrary

- `projectId`:

  integer. The Project id

- `scenarioId`:

  integer. The Scenario id

- `parentId`:

  integer. For a result Scenario, this is the id of the parent Scenario.
  0 indicates this is not a result Scenario

- `folderId`:

  integer. The folder in which the Scenario exists. If the Scenario
  exists at the root of the project, then this value is NULL.

## See also

See
[`scenario`](https://syncrosim.github.io/rsyncrosim/reference/scenario.md)
for options when creating or loading a SyncroSim Scenario.
