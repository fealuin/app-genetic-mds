  #Individual class
  source('individual/fitness.r')
  source('individual/mutation.r')
  source('individual/crossover.r')
  source('individual/initialization.r')

  Individual<-function(n=10,m=2) {
    thisEnv <- environment()
    data<-matrix(nrow=n,ncol=m)
    n<-n
    m<-m
    fitness=0
    me<-list(
      thisEnv = thisEnv,
      getEnv = function(){
        return(get("thisEnv",thisEnv))
      },
      getNrow = function(){
        return(get("n",thisEnv))
      },
      getNcol = function(){
        return(get("m",thisEnv))
      },
      #Data
      getData = function(){
        return(get("data",thisEnv))
      },
      setData=function(M){
        return (assign("data",M,thisEnv))
      },
      #Initialization
      initialize=function(type='random',max=1){
        return (assign("data",initialize(me,type,max),thisEnv))
      },
      #Fitness
      getFitness=function(){
        return(get("fitness",thisEnv))
      },
      setFitness=function(D,type='rawStress'){
        return(assign("fitness",fitness(D,data,type),thisEnv))
      },
      #Mutation
      getMutation=function(type='flipPoints'){
        return(mutate(I=me,type=type))
      },
      getCrossOver=function(I,p=0.2,type="random"){
        return(crossOver(me,I,p,type))
      }

    )
    class(me) <- append(class(me),"Individual")
    return(me)
  }

juanito<-Individual()
juanito$initialize(max=100)
juanito$getData()
juanito$getFitness()
juanito$setFitness(D)
juanito$getFitness()
