"
  Effectue 20 séparations aléatoires des données X et du vecteur d'étiquettes z en ensemble d'apprentissage 
  et de test. Pour chaque séparation, la méthode du classificateur Euclidien est utilisée pour effectuer des 
  prédictions. Ces prédictions sont ensuite comparées aux étiquettes réelles afin de retourner une matrice 
  comprenant les taux d'erreur pour chaque séparation.

  Retour : une matrice 20x2 dont la première colonne contient les erreurs sur l'ensemble de test et la 
  seconde contient les erreurs sur l'ensemble d'apprentissage.
"

perfCeuc<- function(data){
  i=1
  err<-matrix(ncol=2,nrow=20)
  p <- ncol(data)
  while(i<=20){
    donn.sep <- separ1(data[,-p-1], data[,p])
    Xapp <- donn.sep$Xapp
    zapp <- donn.sep$zapp
    Xtst <- donn.sep$Xtst
    ztst <- donn.sep$ztst
    mu<- ceuc.app(Xapp,zapp)
    labelTst<- ceuc.val(mu,Xtst)
    labelApp<- ceuc.val(mu,Xapp)
    err[i,1] <- 1 - sum(labelTst==ztst)/length(ztst)
    err[i,2] <- 1 - sum(labelApp==zapp)/length(zapp)
    i<-i+1
  }
  err[is.na(err)]<-0
  err
}
