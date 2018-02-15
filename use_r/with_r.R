X<-matrix(runif(400*400), ncol=400)

ptm <- proc.time()
X %*% X
proc.time()-ptm

Y<-matrix(runif(4000*4000), ncol=4000)

ptm <- proc.time()
Y %*% Y
proc.time()-ptm
