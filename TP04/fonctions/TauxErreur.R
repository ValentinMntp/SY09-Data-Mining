TauxErreurADQ <- function(data, N)
{
  p <- ncol(data)
  error<-matrix(0,N,1)
  #effectuer 20 fois decoupage de donnees et calculer le taux d'erreur
  for (i in 1:N)
  {
    dat<-separ1(data[,-p],data[,p])
    nb_app<-length(dat$zapp)
    nb_tst<-length(dat$ztst)
    
    
    param <- adq.app(dat$Xapp, dat$zapp)
    
    #calculer le taux d'erreur de test
    out<-ad.val(param, dat$Xtst)
    for(j in 1:nb_tst)
    {
      if (out$pred[j] != dat$ztst[j])
      {
        error[i,1] <- error[i,1] + 1
      }
    }
    error[i,1] <- error[i,1]/nb_tst
  }
  if(p == 3){
    prob.ad(param, dat$Xapp, dat$zapp, niveaux = c(0.3,0.5,0.7), titre='ADQ, niveau = (0.3,0.5,0.7)')
  }
  
  error
}

TauxErreurADL <- function(data, N)
{
  p <- ncol(data)
  error<-matrix(0,N,1)
  #effectuer 20 fois decoupage de donnees et calculer le taux d'erreur
  for (i in 1:N)
  {
    dat<-separ1(data[,-p],data[,p])
    nb_app<-length(dat$zapp)
    nb_tst<-length(dat$ztst)
    
    
    param <- adl.app(dat$Xapp, dat$zapp)
    
    #calculer le taux d'erreur de test
    out<-ad.val(param, dat$Xtst)
    for(j in 1:nb_tst)
    {
      if (out$pred[j] != dat$ztst[j])
      {
        error[i,1] <- error[i,1] + 1
      }
    }
    error[i,1] <- error[i,1]/nb_tst
  }
  if(p == 3){
    prob.ad(param, dat$Xapp, dat$zapp, niveaux = c(0.3,0.5,0.7), titre='ADL, niveau = (0.3,0.5,0.7)')
  }  
  error
}


TauxErreurNBA <- function(data, N)
{
  p <- ncol(data)
  error<-matrix(0,N,1)
  #effectuer 20 fois decoupage de donnees et calculer le taux d'erreur
  for (i in 1:N)
  {
    dat<-separ1(data[,-p],data[,p])
    nb_app<-length(dat$zapp)
    nb_tst<-length(dat$ztst)
    
    
    param <- nba.app(dat$Xapp, dat$zapp)
    
    #calculer le taux d'erreur de test
    out<-ad.val(param, dat$Xtst)
    for(j in 1:nb_tst)
    {
      if (out$pred[j] != dat$ztst[j])
      {
        error[i,1] <- error[i,1] + 1
      }
    }
    error[i,1] <- error[i,1]/nb_tst
  }
  if(p == 3){
    prob.ad(param, dat$Xapp, dat$zapp, niveaux = c(0.3,0.5,0.7), titre='NBA, niveau = (0.3,0.5,0.7)')
  }
  error
}

TauxErreurTREE <- function(data, N){
  i=1
  p <- ncol(data)
  err <- c()
  while(i <= N){
    donn.sep <- separ1(data[,-p],data[,p])
    Xapp <- donn.sep$Xapp
    zapp <- donn.sep$zapp
    Xtst <- donn.sep$Xtst9:81
    ztst <- donn.sep$ztst
    zapp <- as.factor(zapp)
    tr <- tree(zapp~., data = Xapp)
    pred <- predict(tr, Xtst)
    zpred <- apply(pred, 1, which.max)
    err <- c(err, 1 - sum(ztst==zpred)/length(ztst))
    i<-i+1
  }
  err
}

TauxErreurLOG1 <- function(data, intr, N){
  p <- ncol(data)
  error<-matrix(0,N,1)
  #effectuer 20 fois decoupage de donnees et calculer le taux d'erreur
  for (i in 1:N)
  {
    dat<-separ1(data[,-p],data[,p])
    nb_app<-length(dat$zapp)
    nb_tst<-length(dat$ztst)
    
    
    param <- log.app(dat$Xapp, dat$zapp, intr, 1e-5)
    #calculer le taux d'erreur de test
    out<-log.val(param$beta, dat$Xtst)
    for(j in 1:nb_tst)
    {
      if (out$pred[j] != dat$ztst[j])
      {
        error[i,1] <- error[i,1] + 1
      }
    }
    error[i,1] <- error[i,1]/nb_tst
  }
  if(p == 3){
    if (intr == F){
      prob.log(param$beta, dat$Xapp, dat$zapp, niveaux = c(0.3,0.5,0.7), titre='Log1, niveau = (0.3,0.5,0.7), intr=F')
    }
    else{
      prob.log(param$beta, dat$Xapp, dat$zapp, niveaux = c(0.3,0.5,0.7), titre='Log1, niveau = (0.3,0.5,0.7), intr=V')
      
    }
  }
  error
}

TauxErreurLOG2 <- function(data, intr, N){
  error<-matrix(0,N,1)
  p <- ncol(data)
  #effectuer 20 fois decoupage de donnees et calculer le taux d'erreur
  for (i in 1:N)
  {
    dat<-separ1(data[,-p],data[,p])
    nb_app<-length(dat$zapp)
    nb_tst<-length(dat$ztst)
    
    
    param <- logquad.app(dat$Xapp, dat$zapp, intr, 1e-5)
    #calculer le taux d'erreur de test
    out<-logquad.val(param$beta, dat$Xtst)
    for(j in 1:nb_tst)
    {
      if (out$pred[j] != dat$ztst[j])
      {
        error[i,1] <- error[i,1] + 1
      }
    }
    error[i,1] <- error[i,1]/nb_tst
    if (intr == F){
      prob.log2(param$beta, dat$Xapp, dat$zapp, niveaux = c(0.3,0.5,0.7), titre='Log2, niveau = (0.3,0.5,0.7), intr=F')
    }
    else{
      prob.log2(param$beta, dat$Xapp, dat$zapp, niveaux = c(0.3,0.5,0.7), titre='Log2, niveau = (0.3,0.5,0.7), intr=V')
    }
  }
  
  error
}



# errFOREST <- function(data,N){
  error<-matrix(0,N,1)
  p <- ncol(data)
  for (i in 1:N){
    dat<-separ1(data[,-p],data[,p])
    Xapp <- dat$Xapp
    zapp <- dat$zapp
    Xtst <- dat$Xtst
    ztst <- dat$ztst
    nb_tst<-length(dat$ztst)
    rf <- randomForest(x = Xapp, y = as.factor(zapp))
    zpred <- predict(rf, Xtst)
    for(j in 1:nb_tst)
    {
      if (zpred[j] != ztst[j])
      {
        error[i,1] <- error[i,1] + 1
      }
    }
    error[i,1] <- error[i,1]/nb_tst
  }
  error
}
