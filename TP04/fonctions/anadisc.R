adq.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	g <- max(unique(zapp))

	param <- NULL
	param$MCov <- array(0, c(p,p,g))
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)

	for (k in 1:g)
	{
		indk <- which(zapp==k)

		param$MCov[,,k] <- cov(Xapp[indk[],])*(length(indk)/(length(indk)-1))
		param$mean[k,] <- apply(Xapp[indk[],],2,sum)/length(indk)
		param$prop[k] <- length(indk)/n
	}

	param
}






adl.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	g <- max(unique(zapp))

	param <- NULL
	MCov <- array(0, c(p,p))
	param$MCov <- array(0, c(p,p,g))
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)

	for (k in 1:g)
	{
		indk <- which(zapp==k)

		MCov <- MCov + cov(Xapp[indk[],])*length(indk)
		param$mean[k,] <- apply(Xapp[indk[],],2,sum)/length(indk)
		param$prop[k] <- length(indk)/n
	}
	MCov <- MCov/(n-g)
	for (k in 1:g)
	{
		param$MCov[,,k] <- MCov
	}

	param
}





nba.app <- function(Xapp, zapp)
{
	n <- dim(Xapp)[1]
	p <- dim(Xapp)[2]
	g <- max(unique(zapp))

	param <- NULL
	param$MCov <- array(0, c(p,p,g))
	param$mean <- array(0, c(g,p))
	param$prop <- rep(0, g)

	for (k in 1:g)
	{
		indk <- which(zapp==k)

		param$MCov[,,k] <- diag(diag(cov(Xapp[indk[],])))
		param$mean[k,] <- apply(Xapp[indk[],],2,sum)/length(indk) 
		param$prop[k] <- length(indk)/n
	}

	param
}




ad.val <- function(param, Xtst){
  n <- dim(Xtst)[1]
  p <- dim(Xtst)[2]
  g <- length(param$prop)

  out <- NULL

  prob <- matrix(0, nrow=n, ncol=g)

  for (k in 1:g){
    prob[,k] <- param$prop[k]*mvdnorm(Xtst, param$mean[k,], param$MCov[,,k])
  }
  prob <- prob/(param$prop[1]*mvdnorm(Xtst, param$mean[1,], param$MCov[,,1])+param$prop[2]*mvdnorm(Xtst, param$mean[2,], param$MCov[,,2]))
  pred <- max.col(prob)

  out$prob <- prob
  out$pred <- pred

  out
}
