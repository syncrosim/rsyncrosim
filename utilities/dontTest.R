setwd("C:/gitprojects/rsyncrosim")

scripts <- list.files(path = "./R", pattern = ".R", full.names = T)

ChangedScripts <- data.frame(File = character(),
                             Changes = integer())

for(i in 1:length(scripts)){
  RScript <- readLines(scripts[i])
  if(length(grep(pattern = "\\\\donttest", x = RScript) > 0)) {
    DontRun <- gsub(pattern = "\\\\donttest", replacement = "\\\\dontrun", x = RScript)
    writeLines(text = DontRun, con = scripts[i])
  }
  ChangedScripts[i, "File"] <- scripts[i]
  ChangedScripts[i, "Changes"] <- length(grep(pattern = "\\\\donttest", x = RScript))
}

ChangedScripts

