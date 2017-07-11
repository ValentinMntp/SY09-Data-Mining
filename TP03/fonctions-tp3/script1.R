source('fonctions-tp3/ceuc.R') 
source('fonctions-tp3/distXY.R')
source('fonctions-tp3/front.ceuc.R')
source('fonctions-tp3/front.kppv.R')
source('fonctions-tp3/kppv.R')
source('fonctions-tp3/separ1.R')
source('fonctions-tp3/separ2.R')
source('fonctions-tp3/EspVarPI.R')
source('fonctions-tp3/errorCEUC.R')
source('fonctions-tp3/errorKPPV.R')
source('fonctions-tp3/Intervalle.R')

data40 <- read.csv("data/Synth1-40.csv", header=T)
data100 <- read.csv("data/Synth1-100.csv", header=T)
data500 <- read.csv("data/Synth1-500.csv", header=T)
data1000 <- read.csv("data/Synth1-1000.csv", header=T)
X <- data40[,-3]
z <- data40[,3]

# Classifieur euclidien
print("Classifieur Euclidien")
donn.sep <- separ1(X, z)
Xapp <- donn.sep$Xapp
zapp <- donn.sep$zapp
Xtst <- donn.sep$Xtst
ztst <- donn.sep$ztst

muprint("mu =")
print(mu)
zpred <- ceuc.val(mu, Xtst)
print("zpred =")
print(zpred)
front.ceuc(mu, X,z, 1000)


# K plus proches voisins
print("K plus proches voisins")
donn.sep <- separ2(X, z)
Xapp <- donn.sep$Xapp
zapp <- donn.sep$zapp
Xval <- donn.sep$Xval
zval <- donn.sep$zval
Xtst <- donn.sep$Xtst
ztst <- donn.sep$ztst

Kopt <- kppv.tune(Xapp, zapp, Xval, zval, 1:10)
print("Kopt =")
print(Kopt)
zpred <- kppv.val(Xapp, zapp, 2, Xtst)
print("zpred =")
print(zpred)
front.kppv(Xapp, zapp, Kopt)


