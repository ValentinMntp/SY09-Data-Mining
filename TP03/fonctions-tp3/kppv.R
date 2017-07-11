"
  Méthode déterminant le nombre de voisins Kopt donnant les meilleures performances sur un ensemble de 
validation Xval étiqueté par zval. 
Prend en paramètres un tableau individu-variables d'apprentissage Xapp et un vecteur zapp
des étiquettes associées et un vecteur d'entiers nppv.

Retour : Valeur de K optimale pour les paramètres donnés.
"

kppv.tune <- function(Xapp, zapp, Xval, zval, nppv)
{
  nval <- nrow(Xval)
  napp <- nrow(Xapp)
  p<- ncol(Xapp)
  class <- unique(zapp)
  nbClass<-length(class)
  err <- matrix(nrow=length(nppv),ncol=2)
  i<-1
  for(k in nppv){
    zexp<-kppv.val(Xapp,zapp,k,Xval)
    err[i,] <- c(k,table(zval==zexp)['FALSE']/length(zval))
    if(is.na(err[i,2])) err[i,2]<-0
    i<-i+1
  }
  err[which.min(err[,2]),1]
}

"
  Méthode faisant le classement d'un tableau individus-variables Xtst par la méthode des K plus proches voisins.
  Prend en paramètres un tableau individus-variables d'apprentisage Xapp, étiqueté par le vecteur zapp, et un
  entier K représentant le nombre de voisins à considérer.

  Retour : un vecteur d'étiquettes du tableau Xtst
"

kppv.val <- function(Xapp, zapp, K, Xtst) 
{
  dist <- distXY(as.matrix(Xtst),as.matrix(Xapp))
  dist_ranked<-t(apply(dist, 1, rank))
  kNeighbors <- matrix(nrow=nrow(dist_ranked), ncol=K)
  for(i in 1:nrow(dist_ranked)){
    kNeighbors[i,]<-as.integer(sort.int(dist_ranked[i,], index.return = TRUE)$ix[1:K])
  }
  kppv <- matrix(zapp[as.vector(t(kNeighbors))], ncol=K, byrow=TRUE);
  if(K == 1){
    zexp <- as.integer(names(sapply(sapply(kppv,table),which.max)))
  }else{
    zexp <- as.integer(names(sapply(apply(kppv,1,table),which.max)))
  }
  zexp
}


