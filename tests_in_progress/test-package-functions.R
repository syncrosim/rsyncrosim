### ApexRMS
### 2024-07-11
### Below script tests the following functions:
### * installPackage
### * uninstallPackage
### * addPackage
### * removePackage
### * packages

# Setup ----
myLibraryName1 <- file.path(tempdir(),"testlib")
myLibraryName2 <- file.path(tempdir(),"mylib")
mySession <- session("C:/gitprojects/ssimbin3/")

# Test ----
# Test multi-install 
installPackage(session = mySession, packages = c("stsim", "demosales"), 
               versions = c("4.0.1", "2.0.0"))

# Expect stsim v4.0.1 and demosales v2.0.0 to be in installed packages
packages(mySession, installed = T)

# Test install without specifying version
installPackage(session = mySession, packages = "stsimecodep")

# Test create new library with package specified 
myLibrary1 <- ssimLibrary(name = myLibraryName1, session = mySession, 
                          packages = "stsim")

# Expect latest installed version of "stsim" to be in packages list
packages(myLibrary1)

# Test add stsimecodep to existing library
addPackage(myLibrary1, packages = "stsimecodep")

# Expect latest installed version of stsimecodep to be in packages list
packages(myLibrary1)

# Test add package with version specified to existing library
addPackage(myLibrary1, packages = "demosales", versions = "2.0.0")

# Expect demosales v2.0.0 to be in packages list
packages(myLibrary1)

# Test remove stsim from existing library
removePackage(myLibrary1, packages = "stsim")

# Expect stsim to not be in packages list
packages(myLibrary1)

# Test add non-latest version of stsim to existing library
addPackage(myLibrary1, packages = "stsim", versions = "4.0.0")

# Expect that stsim v4.0.0 is in packages list
packages(myLibrary1)

# Update stsim version in existing library by specifying newer package version
addPackage(myLibrary1, packages = "stsim", versions = "4.0.1")

# Expect that stsim v4.0.1 is now in packages list
packages(myLibrary1)

# Test create new library without adding any packages
myLibrary2 <- ssimLibrary(name = myLibraryName2, session = mySession)

# Expect that only core package is in packages list
packages(myLibrary2)

# What happens if you add installed argument when calling packages on lib object
packages(myLibrary2, installed = T) # ignores

# Test add multiple packages to lib at once
addPackage(myLibrary2, packages = c("stsim", "demosales"))

# Expect latest versions of stsim and demosales to be in packages list
packages(myLibrary2)

# Test remove multiple packages at once
removePackage(myLibrary2, packages= c("stsim", "demosales"))

# Expect stsim and demosales to be removed from list
packages(myLibrary2)

# What happens if you try to add a package that doesn't exist?
addPackage(myLibrary2, packages = "test")

#What happens if you try to remove core?
removePackage(myLibrary2, packages = "core")

# Test uninstall
uninstallPackage(packages = "stsim", versions = "4.0.0", session = mySession)

# Expect installed packages to not include stsim v4.0.0
packages(mySession)

# Test uninstall all versions of specified package
uninstallPackage(packages = "stsim", session = mySession)

# Expect no stsim
packages(mySession)

# What happens if you try to add package that is on server, but not local?
addPackage(myLibrary2, packages = "stsim", versions = "4.0.0") # have to install first

# Old code below ----
# test_that("package added", {
#   skip_on_cran()
#   expect_equal(installPackage(session = mySession, name = "burnP3Plus"), TRUE)
#   expect_equal(installPackage(session = mySession, name = "helloworld"), FALSE)
#   expect_equal(installPackage(session = mySession, name = "C:/Users/GabrielleEdnie/Documents/SyncroSim/Designer/helloworldPipeline.ssimpkg"), TRUE)
#   expect_equal(installPackage(name = "burnP3Plus"), FALSE)
# })
# 
# test_that("test errors", {
#   skip_on_cran()
#   expect_error(installPackage(session = mySession))
#   expect_error(installPackage(session = "mySession"))
#   expect_error(installPackage(name = "helloworld", session = "mySession"))
#   #expect_message(installPackage(name = "test"), "The package 'test' was not found in the package index.")
#   expect_message(installPackage(session = mySession, name = "C:/Users/GabrielleEdnie/Documents/SyncroSim/Designer/hdPipeline.ssimpkg"), "Cannot find file: C:/Users/GabrielleEdnie/Documents/SyncroSim/Designer/hdPipeline.ssimpkg")
#   expect_error(expect_equal(installPackage(session = mySession, name = "helloworld"), TRUE))
# })

#capture.output(installPackage(session = mySession, name = "C:/Users/GabrielleEdnie/Documents/SyncroSim/Designer/hdPipeline.ssimpkg"))
#value((installPackage(session = mySession, name = "C:/Users/GabrielleEdnie/Documents/SyncroSim/Designer/helloworldPipeline.ssimpkg"))) #uses future package
#expect_output((installPackage(session = mySession, name = "C:/Users/GabrielleEdnie/Documents/SyncroSim/Designer/helloworldPipeline.ssimpkg")), "Package installed from file <", name, ">")
