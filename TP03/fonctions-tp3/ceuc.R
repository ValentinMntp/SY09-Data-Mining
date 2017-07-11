"
  Méthode d'apprentissage des paramètres du classificateur Euclidien. 
Prend en paramètres un tableau individu-variables d'apprentissage Xapp et un vecteur zapp
des étiquettes associées.

Retour : Paramètres estimés du classificateur euclidien dans une matrice g x p
"

ceuc.app <- function(Xapp, zapp)
{
  class<-unique(zapp)
  nbClass<- length(class)
  napp<-nrow(Xapp)
  inClass<-matrix(0,napp,nbClass)
  p<- ncol(Xapp)
  for(i in 1:napp){
    inClass[i,zapp[i]]<-1
  }
  sizeClass<-apply(inClass,2,sum)
  gravityCenters<- matrix(0,nbClass,p)
  for(k in 1:nbClass){
    center<- apply((Xapp*inClass[,k]),2,sum) /sizeClass[k]
    gravityCenters[k,]<-center
  }
  gravityCenters
}

"
  Méthode effectuant le classement d'un tableau individus-variables Xtst par la méthode du classifieur Euclidien
  à l'aide d'une matrice mu des paramètres estimés du classifieur.

  Retour : un vecteur contenant les étiquettes des individus de Xtst.
"

ceuc.val <- function(mu, Xtst){
  ntst<- nrow(Xtst)
  p<-ncol(Xtst)
  nbClass <- nrow(mu)
  labels <- rep(0,ntst)
  dist<- distXY(as.matrix(Xtst),as.matrix(mu))
  labels<-apply(dist,1,which.min)
  c(t(labels))
}

