devtools::document();devtools::load_all()

mySsim = session("C:/svnprojects/SyncroSim-1/WinForm/bin/x86/Debug/SyncroSim.Console.exe",silent=T)   # Creates a silent session using a particular version (i.e. folder) of syncrosim

myArgs = list(list=NULL,columns=NULL,lib="C:/Temp/NewLibrary.ssim",sheet="STSim_Stratum",pid=1)
x= command(args=myArgs,mySsim,printCmd=T)
.dataframeFromSSim(x)


