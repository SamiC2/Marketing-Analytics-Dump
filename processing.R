library(readxl)
mydf = read.csv('PersonalityData3.csv')
mydf_real = mydf[,2:13]

#p1
eigenval=eigen(cor(mydf_real[,1:12]))
eigenval$values

#p2 and 3
library(psych)
fit <- principal(mydf_real[, 1:12],nfactors=5,rotate="none")
fit$loadings

#p4-9
fit2 <- principal(mydf_real[, 1:12],nfactors=3,rotate="varimax")
