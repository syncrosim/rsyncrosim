devtools::document();devtools::load_all()

myClient=externalClient()

#How to send a valid command?
sendResp = writeLines("shutdown", connection(myClient))

#Clean up stray connections
gc()
closeAllConnections()

#Handling multiple connections?
showConnections()
isOpen(connection(myClient))
close(connection(myClient)) #remove the connection
showConnections()
connection(myClient) = connection() #get a new connection - but server is still listening for the old connection
connection(myClient)
#TO DO: understand how multiple connections work.
#TO DO: When I close connections, the server seems to think I am still connected. ??

showConnections()


setMethod('readServer', signature(x="ServerController"), function(x) {
  m = readLines(connection(x))
  s = m.decode('utf-8')
})
setGeneric('remoteCall',function(x,...) standardGeneric('remoteCall'))
setMethod('remoteCall', signature(x="ServerController"), function(x,message) {
  #message='shutdown';x=myClient
  #isOpen(connection(x))

  msg = enc2utf8(message)

  ret=readLines(connection(x),1)
  sendResp = writeLines(message, connection(x))

  ret = None
  while (T){
    res = self.read_server()
    #  cmd = self.get_command_name(res)
    #  if cmd == remote_msg_breakpoint_hit:
    #    split = res.split('|', 4)
    #  self.on_breakpoint_hit(split[1], split[2], split[3])
    #  self.server.sendall(remote_msg_breakpoint_continue)
    #  elif cmd == remote_msg_call_complete:
    #    split = res.split('|', 2)
    #  if split[1] == 'FAILURE':
    #    raise RuntimeError('Server returned failure: %s' % split[2])
    #  else:
    #    ret = split[2]
    #  break
    #  return ret
    return(x)
  })





mySsim = session("C:/svnprojects/SyncroSim-1/WinForm/bin/x86/Debug/SyncroSim.Console.exe",silent=T)   # Creates a silent session using a particular version (i.e. folder) of syncrosim

libPath = "C:/Users/Josie Hughes/Documents/ApexLocal/Sockets/Test Lib/Test Lib.ssim"

#install.packages("pryr")
library(pryr)
pryr::otype(con)

myArgs = list(list=NULL,scenarios=NULL,lib=libPath)
command(args=myArgs,mySsim,printCmd=T)

myArgs = list(run=NULL,lib=libPath,sid=1)
command(args=myArgs,mySsim,printCmd=T)

