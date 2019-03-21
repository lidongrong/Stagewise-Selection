StagewiseRegression<-function(y,x){
  x=matrix(x,nrow=dim(x)[1],ncol=dim(x)[2])
  y=matrix(y)
  
  
  beta=vector(dim(x)[2],mode="numeric")
  correlation_vector=vector(dim(x)[2],mode="numeric")
  
  for(i in 2:dim(x)[2]){
    x[,i]=(x[,i]-mean(x[,i]))/sd(x[,i])
  }
  
  print(x)
  
  y=(y-mean(y))/sd(y)
  
  r=y
  r=matrix(r)
  
  epsilon=0.1
  
  
  
  for(i in 1:dim(x)[2]){
    correlation_vector=cor(r,x[,i])
  }
  
  while(sum(abs(correlation_vector))>=dim(x)[2]*0.005){
    
    index=which.max(correlation_vector)
    
    minus=0.2*sign(crossprod(c(x[,index]),c(r)))
    
    print(r)
    print(minus)
    
    beta[index]=beta[index]+minus
    
    
    
    
    r=r-minus*x[,index]
    
    for(i in 1:dim(x)[2]){
      correlation_vector=cor(r,x[,i])
    }
    
    #print(r)
    #print(dim(x))
    #print(correlation_vector)
    
  }
  return(beta)
}
