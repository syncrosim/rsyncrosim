devtools::document();devtools::load_all()

mySsim = session("C:/svnprojects/SyncroSim-1/WinForm/bin/x86/Debug/SyncroSim.Console.exe",silent=T)   # Creates a silent session using a particular version (i.e. folder) of syncrosim

myArgs = list(list=NULL,columns=NULL,lib="C:/Temp/ST-Sim-Command-Line.ssim",sheet="STSim_TransitionTarget",pid=1)
x= command(args=myArgs,mySsim)
sheetDefinition = .dataframeFromSSim(x)

#Now build blank data from from sheetDefinition
showOptional=T #if F, omit optional columns

