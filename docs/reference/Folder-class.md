# SyncroSim Folder class

`Folder` object representing a SyncroSim Folder. A Folder is used to
organize SyncroSim Scenarios within a `Project`, and can be nested
within other Folders at the project-level. These are used mostly in the
SyncroSim User Interface.

## Slots

- `session`:

  `Session` object. The Session associated with the Folder's SsimLibrary

- `filepath`:

  character string. The path to the Folder's SsimLibrary on disk

- `folderId`:

  integer. The Folder id

- `parentId`:

  integer. The parent Folder id (if the folder is nested)

- `projectId`:

  integer. The Project id

## See also

See
[`folder`](https://syncrosim.github.io/rsyncrosim/reference/folder.md)
for options when creating or loading a SyncroSim Folder
