X=outer(rep(1:2,c(7,13)),1:2,"==")*1
pdf(file="Pind2sample.pdf",8,8)
par(cex=1.5)
XX=X%*%solve(t(X)%*%X)%*%t(X)
XX=-XX[nrow(XX):1,]
image(XX,col=c("grey","white"),axes = FALSE,asp=1,xaxt="n",yaxt="n",xlab="",ylab="observations",main="observations")
axis(2,c(0,7/20,1),c("n=#F+#M","#F","1"), lwd =0,pos=.01)
axis(3,c(0,13/20,1),c("1","#F","n=#F+#M"), lwd =0,pos=.99)
dev.off()




X=outer(rep(1:10,rep(2,10)),1:10,"==")*1
pdf(file="Ppair2sample.pdf",8,8)
par(cex=1.5)
XX=X%*%solve(t(X)%*%X)%*%t(X)
XX=-XX[nrow(XX):1,]
image(XX,col=c("grey","white"),axes = FALSE,asp=1,xaxt="n",yaxt="n",xlab="",ylab="observations",main="observations")
axis(2,c(0,.5,1),c("n","to","1"), lwd =0,pos=.01)
axis(3,c(0,.5,1),c("1","to","n"), lwd =0,pos=.99)
dev.off()



X=matrix(rnorm(40),20,2)
pdf(file="Preg.pdf",8,8)
par(cex=1.5)
XX=X%*%solve(t(X)%*%X)%*%t(X)
XX=-XX[nrow(XX):1,]
image(XX,cex.lab=1.5,col=grey((1:10)/10),axes = FALSE,asp=1,xaxt="n",yaxt="n",xlab="",ylab="observations",main="observations")
axis(2,c(0,.5,1),c("n","to","1"), lwd =0,pos=.01,cex.lab=3)
axis(3,c(0,.5,1),c("1","to","n"), lwd =0,pos=.99)
dev.off()


#################
X=cbind(1,rep(0:1,c(6,4)))
X0=X[,1,drop=FALSE]
X1=X[,2,drop=FALSE]
P0=diag(10)-X0%*%solve(t(X0)%*%X0)%*%t(X0)

XS=solve(t(X1)%*%P0%*%X1)%*%t(X1)%*%P0
XS

X=cbind(1,c(rep(0,5),rep(1,5)),rep(c(0,1,0,1),c(2,3,1,4)))
X0=X[,1:2,drop=FALSE]
X1=X[,3,drop=FALSE]
P0=diag(10)-X0%*%solve(t(X0)%*%X0)%*%t(X0)

XS=solve(t(X1)%*%P0%*%X1)%*%t(X1)%*%P0
XS



#############################

pdf(file="Ppair2sample.pdf",8,8)
XX=X%*%solve(t(X)%*%X)%*%t(X)
XX=-XX[nrow(XX):1,]
image(XX,col=c("grey","white"),axes = FALSE,asp=1,xaxt="n",yaxt="n",xlab="",ylab="observations",main="observations")
axis(2,c(0,.5,1),c("n","to","1"), lwd =0,pos=.01)
axis(3,c(0,.5,1),c("1","to","n"), lwd =0,pos=.99)
dev.off()
