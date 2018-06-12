initialize<-function(individual,type='random',max=1){
  n<-individual$getNrow()
  m<-individual$getNcol()
  if(type=='random'){
    return(matrix(runif(n*m,max=max),ncol=m))
  }
}
