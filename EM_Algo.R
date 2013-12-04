MultinomialEM=function(H,K,tau){
  n=nrow(H)
  
  d=ncol(H)
  #randomly select three histograms of gray scale 
  t0=H[sample(nrow(H),size=K[i]),]
  
  #scale these samples
  t0=t0/rowSums(t0)
  
  #initialize a, c
  
  a=matrix(0,n,K[i])
  
  C=rep(1/K[i],K[i])
  
  repeat {
    
    #store matrix a into old value
    a.old=a
    
    phi=exp(H%*%log(t(t0)))
    
    a=t(t(phi)*C)/rowSums(t(t(phi)*C))
    
    C=colSums(a)/n
    b=t(a)%*%H
    
    t0=b/rowSums(b)
    
    delta=norm(a-a.old,"O")
    
    if (delta<tau) break()
  }
  
  index=max.col(a)
  
  #compute m matrix
  m=matrix(NA,200,200)
  for (j in 0:n-1){
    m[(j%/%200)+1,(j%%200)+1]=index[j+1]
  }
  
  return(m)
}


H<-matrix(readBin("histograms.bin", "double", 640000), 40000, 16)

H[which(H==0)]=0.01

K=c(3,4,5)

tau=0.1

for (i in 1:3){
  m=MultinomialEM(H,K[i],tau)
}

