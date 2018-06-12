source('individual/individual.r')

Population<-function(size=100,n=10,m=2){

  thisEnv <- environment()
  individuals<-list()

  me<-list(
    thisEnv = thisEnv,
    getEnv = function(){
      return(get("thisEnv",thisEnv))
    },
    getIndividuals=function(){
      return(get("individuals",thisEnv))
    },
    setIndividuals=function(I){
      return(assign("individuals",I,thisEnv))
    },
    getIndividual=function(i){
      return(me$getIndividuals()[[i]])
    },

    #setIndividual=function(i,I){
    #  return(assign(paste("individuals[[",i,"]]"),I,thisEnv))
    #},
    initialize=function(type="random",max=100){
      init=list()
      for (i in 1:size){
        init[[i]]=Individual(n,m)
      }
      me$setIndividuals(init)
      lapply(me$getIndividuals(),function(x) x$initialize(type,max))
      return("ok")
    },
    setFitness=function(D){
      lapply(me$getIndividuals(),function(x) x$setFitness(D))
      return("ok")
    },
    getFitness=function() {
      return(unlist(lapply(me$getIndividuals(),function(x) x$getFitness())))
    },
    orderByFitness=function(decreasing=FALSE){
      return(me$setIndividuals(me$getIndividuals()[order(me$getFitness(),decreasing=decreasing)]))
    },
    getMutation=function(p=0.4,ratio=0.5,type="flipPoints") {
      mutation=Population(size,n,m)
      mutation$setIndividuals(me$getIndividuals())
      lapply(mutation$getIndividuals(),function(x) if(runif(1)<p){x$getMutation(type=type)})
      #for(i in size){
      #  if(runif(1)<p){
      #    mutation$setIndividual(i,me$getIndividual(i)$getMutation(type=type))
      #  }
      #}
      return(mutation)
    }

  )

  class(me) <- append(class(me),"Population")
  return(me)
}

pop=Population(n=214)
pop$initialize()
pop$setFitness(D)
pop2<-pop$getMutation()
pop2$setFitness(D)
pop$getFitness()==pop2$getFitness()
