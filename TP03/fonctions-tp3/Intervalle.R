"
  Retourne un estimateur ponctuel de la moyenne et un intervalle de confiance pour le vecteur v donné en paramètre.
"

intervalle_confiance <- function(v){
  m = mean(v)
  s = sd(v)
  n = length(v)
  error <- qnorm(0.95)*s/sqrt(n)
  left <- m-error
  right <- m + error
  intervalle <- c(left, m, right)
  names(intervalle) <- c("left", "erreur", "right")
  intervalle
}

plot_intervalle <- function(data){
  err <- NULL
  intervalle_confiance(TauxErreurADQ(data,100)) -> err$adq
  intervalle_confiance(TauxErreurADL(data,100)) -> err$adl
  intervalle_confiance(TauxErreurNBA(data,100)) -> err$nba
  intervalle_confiance(TauxErreurLOG1(data,F,100)) -> err$log1F
  intervalle_confiance(TauxErreurLOG1(data,T,100)) -> err$log1T
  intervalle_confiance(TauxErreurLOG2(data,F,100)) -> err$log2F
  intervalle_confiance(TauxErreurLOG2(data,T,100)) -> err$log2T
  intervalle_confiance(TauxErreurTREE(data,100)) -> err$tree
  
  print(err)
  
  F <- c(err$adq["erreur"],err$adl["erreur"],err$nba["erreur"],err$tree["erreur"],err$log1F["erreur"],err$log1T["erreur"],err$log2F["erreur"],err$log2T["erreur"])
  L <- c(err$adq["left"],err$adl["left"],err$nba["left"],err$tree["left"],err$log1F["left"],err$log1T["left"],err$log2F["left"],err$log2T["left"])
  U <- c(err$adq["right"],err$adl["right"],err$nba["right"],err$tree["right"],err$log1F["right"],err$log1T["right"],err$log2F["right"],err$log2T["right"])
  df <- data.frame(x=c("ADQ","ADL","NBA","Tree","Log1F","Log1T","Log2F","Log2T"), F=F,L=L,U=U)
  ggplot(df, aes(x = x, y = F)) + geom_point(size = 4) + geom_errorbar(aes(ymax = U, ymin = L))
  
}