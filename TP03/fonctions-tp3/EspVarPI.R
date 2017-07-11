EspVarPi<-function(data)
{
  
#la derniere colonne enregistre le pourcentage des individus sur ensemble
#calculer nbr de variable p et variable expliquee
  
  p<-dim(data)[2] - 1
  res<-NULL
  res$tab<-matrix(0, 2, p+1)
  nb_total<-dim(data)[1]
  #Calculer l'esperacne
  for (i in 1:2)
  {
    #Extraire des donnees
    dat<-data[which(data$z==i),]
    nb_classe<-dim(dat)[1]
    for (j in 1:p)
    {
      res$tab[i,j]<-mean(dat[,j])
    }
    res$tab[i,p+1]<-nb_classe/nb_total
    if(i == 1)
    {
      res$cov1<-cov(dat[,-(p+1)])
    }
    else if (i == 2)
    {
      res$cov2<-cov(dat[,-(p+1)])
    }
  }
  #retourner le tableau qui contient esp var et Pi
  res
}
