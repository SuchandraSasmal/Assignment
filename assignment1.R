tennis<-function(p)
{
  #A: 1, B: 2
  seq<-numeric()
  j<-numeric()
  m<-numeric()
  j1<-numeric()
  m1<-numeric()
  count=0
  for(i in 1:5)
  {
    winner<-sample(x=1:2,size=1,prob=c(p,1-p))
    count=count+1
    seq[i]<-winner
    j<-which(seq==1)
    m<-which(seq==2)
    j1[i]<-length(j)
    m1[i]<-length(m)
    if(m1[i]>j1[i] || j1[i]>m1[i])
    {
      break
    }
    
  }
 return(count)
  
}
