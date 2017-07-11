logquad.app <- function(Xapp, zapp, intr, epsi)
{
  n <- dim(Xapp)[1]

  
  Xapp <- as.matrix(Xapp)
  Xapp <- ReLogQua(Xapp)
  p <- dim(Xapp)[2]
  if (intr == T)
  {
    Xapp <- cbind(rep(1,n),Xapp)
    p <- p + 1
  }
  
  targ <- matrix(as.numeric(zapp),nrow=n)
  targ[which(targ==2),] <- 0
  tXap <- t(Xapp)
  
  beta <- matrix(0,nrow=p,ncol=1)
  
  conv <- F
  iter <- 0
  while (conv == F)
  {
    iter <- iter + 1
    bold <- beta
    
    prob <- postprob(beta, Xapp)
    MatW <- diag(c(prob*(1-prob)))
    beta <- beta + solve(tXap%*%MatW%*%Xapp)%*%tXap%*%(targ-prob)
    
    if (norm(beta-bold)<epsi)
    {
      conv <- T
    }
  }
  
  prob <- postprob(beta, Xapp)
  out <- NULL
  out$beta <- beta
  out$iter <- iter
  out$logL <- tXap%*%(targ-prob)
  out
}

logquad.val <- function(beta, Xtst)
{
  m <- dim(Xtst)[1]
  p <- dim(beta)[1]
  
  Xtst <- as.matrix(Xtst)
  Xtst <- ReLogQua(Xtst)
  pX <- dim(Xtst)[2]
  if (pX == (p-1))
  {
    Xtst  <- cbind(rep(1,m),Xtst)
  }
  
  prob <- postprob(beta,Xtst)
  pred <- max.col(cbind(prob,1-prob))
  out <- NULL
  out$prob <- prob
  out$pred <- pred
  return(out)
}

