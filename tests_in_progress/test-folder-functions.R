### ApexRMS
### 2024-07-11
### Below script tests the following functions:
### * folder
### * dateModified
### * filepath
### * name
### * readOnly
### * folderId
### * projectId
### * parentId
### * scenarioId


# Setup ----
myLibraryName1 <- file.path(tempdir(),"testlib")
myLibraryName2 <- file.path(tempdir(),"mylib")
mySession <- session("C:/gitprojects/ssimbin3/")

# Tests ----
# Create new library in custom session
myLib <- ssimLibrary(myLibraryName1, session = mySession, overwrite = T)

# Create new project
myProj <- project(myLib, project = "Proj 1")

# Create new scenario from library
myScen <- scenario(myLib, scenario = "Test Scenario 1")

# Create new scenario from project
myScen2 <- scenario(myProj, scenario = "Test Scenario 2")

# Create a new scenario based off another scenario
myScen3 <- scenario(myProj, scenario = "Test Scenario 3", sourceScenario = myScen2)

# Create a new project
myProj2 <- project(myLib, project = "Proj 2")

# Put scenario in second project
myScen4 <- scenario(myProj2, scenario = "Test Scenario 4")

# Get list of folders in each project (shouldn't be any yet)
folder(myProj)
folder(myProj2)

# Add a new folder to first project
folder(myProj, folder = "Test Folder 1")

# Check that folder has been created
folder(myProj)

# Grab folder object by Id - check that folderId, parentId, and projectId make sense
folder(myProj, folder = 13)

# Assign folder object to variable
myFolder <- folder(myProj, folder = "Test Folder 1")

# Below should return the same thing
folder(myFolder, summary = T)
folder(myProj, folder = 13, summary = T)

# Ensure that above returns an existing folder object (Id = 13)
folderId(myFolder)

# Create nested folder within existing folder
myNestedFolder <- folder(myProj, folder = "Test Nested Folder 1", 
                         parentFolder = "Test Folder 1")

# Ensure parent ID = 13, folder ID = 14, project Id = 1
folderId(myNestedFolder)
parentId(myNestedFolder)
projectId(myNestedFolder)

# Create another folder with same name in second project
myFolder2 <- folder(myProj2, folder = "Test Folder 1")

# Ensure folderId =/= 13 and projectId =/= 1
folderId(myFolder2)
projectId(myFolder2)

# Create second folder with same name in same project
myFolder3 <- folder(myProj2, folder = "Test Folder 1", create = T)

folderId(myFolder3)
projectId(myFolder3)

# Check folders in both projects
folder(myProj)
folder(myProj2)

# Add scenarios to folders
folderId(myScen4)
folderId(myScen4) <- folderId(myFolder3)
folderId(myScen4)

# Get summary of a folder
folder(myFolder3, summary = T)

# Check date modified 
# TODO: why does this return NULL??
dateModified(myFolder3)

# Check filepath
filepath(myFolder3)

# Check and modify name
name(myFolder3)
name(myFolder3) <- "New Test Folder Name"
name(myFolder3)

# Check and modify readOnly
readOnly(myFolder3)
readOnly(myFolder3) <- TRUE
readOnly(myFolder3)

# Check projectId
projectId(myFolder3)
