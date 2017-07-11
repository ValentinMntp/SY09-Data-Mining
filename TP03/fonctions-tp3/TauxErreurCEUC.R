TauxErreur <- function(data)
{
  error<-matrix(0,20,2)
  #effectuer 20 fois decoupage de donnees et calculer le taux d'erreur
  for (i in 1:20)
  {
    dat<-separ1(data[,1:2],data$z)
    nb_app<-length(dat$zapp)
    nb_tst<-length(dat$ztst)
    
    #calculer le taux d'erreur d'apprentissage
    predict<-kppv.val(dat$Xapp, dat$zapp, K, dat$Xapp)
    for(j in 1:nb_app)
    {
      if (predict[j] != dat$zapp[j])
      {
        error[i,1] <- error[i,1] + 1
      }
    }
    error[i,1] <- error[i,1]/nb_app
    
    #calculer le taux d'erreur de test
    predict<-kppv.val(dat$Xapp, dat$zapp, K, dat$Xtst)
    for(j in 1:nb_tst)
    {
      if (predict[j] != dat$ztst[j])
      {
        error[i,2] <- error[i,2] + 1
      }
    }
    error[i,2] <- error[i,2]/nb_app
  }
  error
}