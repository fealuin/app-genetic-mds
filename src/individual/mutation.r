mutate<-function(I,ratio=1,type='flipPoints'){
  n=I$getNrow()
  m=I$getNcol()
  if(type=='flipPoints'){
    I$setData(2*I$getData()[sample(n,size=ratio*n),])
  }

}
