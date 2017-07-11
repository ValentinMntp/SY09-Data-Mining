"
  Effectue 20 séparations aléatoires des données X et du vecteur d'étiquettes z en ensemble d'apprentissage, 
  de validation et de test. Pour chaque séparation, la méthode des K plus proches voisins est utilisée pour 
  effectuer des prédictions. Ces prédictions sont ensuite comparées aux étiquettes réelles afin de retourner
  une matrice comprenant les taux d'erreur pour chaque séparation.

  Retour : une matrice err 20x2 dont la première colonne contient les erreurs sur l'ensemble de test et la 
  seconde contient les erreurs sur l'ensemble d'apprentissage.
"

perfKppv<- function(data){
  i=1
  err<-matrix(ncol=2,nrow=20)
  p <- ncol(data)
  while(i<=20){
    donn.sep <- separ2(data[,-p-1], data[,p])
    Xapp <- donn.sep$Xapp
    zapp <- donn.sep$zapp
    Xval <- donn.sep$Xval
    zval <- donn.sep$zval
    Xtst <- donn.sep$Xtst
    ztst <- donn.sep$ztst
    Kapp<- kppv.tune(Xapp,zapp,Xval,zval,c(1:10))
    Ktst<- kppv.tune(Xtst,ztst,Xval,zval,c(1:10))
    labelApp<- kppv.val(Xapp,zapp,Kapp,Xapp)
    labelTst<- kppv.val(Xval,zval,Ktst,Xtst)
    err[i,1] <- 1.0 - sum(labelTst==ztst)/length(ztst)
    err[i,2] <- 1.0 - sum(labelApp==zapp)/length(zapp)
    i<-i+1
  }
  err
}
