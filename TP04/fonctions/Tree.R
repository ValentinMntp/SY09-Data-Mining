zapp <- as.factor(zapp)
frmla <- zapp ~ V1 + V2
tr <- tree(frmla, data = Xapp, control=tree.control(nobs=dim(Xapp)[1],mindev = 0.0001))
cv.tree(tr)
# Regarder la valeur minimale de $dev et récuperer la valeur k de $size associée

tr2 <- prune.misclass(tr, best = k)
pred <- predict(tr2, Xtst)
zpred <- apply(pred, 1, which.max)
err <- 1 - sum((zpred == ztst))/length(ztst)
