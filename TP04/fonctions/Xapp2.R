ReLogQua<-function(xapp)
{
  n<-dim(xapp)[1]
  p<-dim(xapp)[2]
  
  #nouvelle dimension
  new_p<-2*p+(p-1)*p/2
  xapp_carre<-matrix(0, n, new_p)
  for(i in 1:p)
  {
    xapp_carre[,i]<-xapp[,i]
  }
  
  for(i in 1:(p-1))
  {
    j<-i+1
    while(j <= p)
    {
      xapp_carre[,p+i+j-2]<-xapp[,i]*xapp[,j]
      j<-j+1
    }
  }
  
  for (i in 1:p)
  {
    xapp_carre[,new_p-p+i]<-xapp[,i]*xapp[,i]
  }
  xapp_carre
}