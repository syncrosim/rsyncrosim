# Copyright (c) 2024 Apex Resource Management Solution Ltd. (ApexRMS). All
# rights reserved.
# MIT License
#' @include AAAClassDefinitions.R
NULL

#' Restore Library
#'
#' Restores a SyncroSim library from a backup file.
#'
#' @param ssimLibraryBackup character string. Path to a library backup file
#' @param folder character string. Optional path to a folder to restore
#' the library into. If NULL, restores to default location. If the folder
#' specified does not exist, it will be created.
#' @param session SyncroSim session.
#' @return
#' Invisibly returns \code{TRUE} upon success (i.e. successful restore)
#' and \code{FALSE} upon failure.
#'
#' @examples
#' \dontrun{
#' # Specify file path and name of SsimLibrary backup file
#' myLibraryBackupName <- file.path(tempdir(), "testlib.ssimbak")
#'
#' # Set up a SyncroSim Session and create SsimLibrary from backup file
#' mySession <- session()
#' myLibrary <- restore(ssimLibraryBackup = myLibraryBackupName,
#' session = mySession)
#'
#' }
#'
#' @export

#' @rdname restore
setGeneric("restore",
           function(ssimLibraryBackup,
                    folder = NULL,
                    session = NULL) standardGeneric("restore"))

#' @rdname restore
setMethod("restore", signature(ssimLibraryBackup = "character"),
          function(ssimLibraryBackup, folder, session) {

            if (!file.exists(ssimLibraryBackup)) {
              stop(paste0("Library not found: ", ssimLibraryBackup))
            }

            args <- list(restore = NULL, lib = ssimLibraryBackup,
                         folder = folder)

            tt <- command(args = args, session = session)

            if (any(grepl("Library successfully restored", tt, fixed = TRUE))) {
              message(tt)
              return(invisible(TRUE))
            } else {
              message(paste0("Library restoration failed:\n", tt))
              return(invisible(FALSE))
            }
          })
