test_that("Session is ok", {
  mySsim = session() # Creates a session using the default installation of syncrosim
  expect_is(mySsim, "Session")
  expect_equal(file.exists(filepath(mySsim)),TRUE) # Lists the folder location of syncrosim session
  expect_output(str(version(mySsim)),"chr [1:2]",fixed=T) # Lists the version of syncrosim session
  expect_equal(names(modules(mySsim)),c("name","displayName","version")) # Dataframe of the modules installed with this verions of SyncroSim.
  expect_equal(names(models(mySsim)),c("name","displayName","shortName")) # Dataframe of the models installed with this version of syncrosim, listing all of its properties as columns
  expect_output('removeModules<-'(mySsim,"hi"),"Module hi is not installed, so cannot be removed.")
})


test_that("command is ok", {
  expect_equal(command("help")[1],"System Console [Arguments]")
  expect_equal(command(c("list","help"),mySsim,printCmd=T)[1],"Lists existing items")
})


