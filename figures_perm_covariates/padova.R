set.seed=1
com=rnorm(20)
dati=data.frame(x=rep(0:1,10),y1=rnorm(20)+com*4,y2=rnorm(20)+com*4)

library(flip)

b<-1000
res=flip(y1+y2~x,data=dati,perm=b)

plot(t2p(res@permT,FALSE))



alpha<-.1
g<-t2p(res@permT,FALSE)
u<-t(apply(g,1,sort))
usort<-apply(t(apply(u,1,sort)),2,sort)

quanbeta <- 1
too.high <- TRUE
while(too.high)
{
  count=sum(apply(u,1,function(t) any(t<usort[quanbeta,]))) 
  #count=sum(apply(u,1,function(t) any(t< c(quanbeta*alpha/2,alpha)  )))
  #count=sum(apply(u,1,function(t) min(t)< c(quanbeta*alpha/2)  ))
  if(count>(alpha*b)) 
  {
    too.high <- FALSE
    #critical <- if(quanbeta>1) usort[quanbeta-1,] else   rep(0,ncol(usort))
  } else {
    #quanbeta <- quanbeta+.001
    quanbeta <- quanbeta+1
  }
}
#quanbeta<-quanbeta-.001
sum(apply(u,1,function(t) any(t<usort[quanbeta-1,])  ))
#sum(apply(u,1,function(t) any(t< c(quanbeta*alpha/2,alpha)  )))
#sum(apply(u,1,function(t) min(t)< c(quanbeta*alpha/2)  ))

plot(g, frame=F, xlim=c(0,1), ylim=c(0,1), asp=1, xlab=expression(p[1]), ylab=expression(p[2]))
critical<-c(alpha/2,alpha)
segments(x0=min(critical), x1=min(critical), y0<-1, y1<-max(critical), lwd=4, col="blue")
segments(x0=max(critical), x1=1, y0<-min(critical), y1<-min(critical), lwd=4, col="blue")
segments(x0=min(critical), x1=max(critical), y0<-max(critical), y1<-max(critical), lwd=4, col="blue")
segments(x0=max(critical), x1=max(critical), y0<-min(critical), y1<-max(critical), lwd=4, col="blue")


critical<-usort[quanbeta-1,]
segments(x0=min(critical), x1=min(critical), y0<-1, y1<-max(critical), lwd=4, col="red")
segments(x0=max(critical), x1=1, y0<-min(critical), y1<-min(critical), lwd=4, col="red")
segments(x0=min(critical), x1=max(critical), y0<-max(critical), y1<-max(critical), lwd=4, col="red")
segments(x0=max(critical), x1=max(critical), y0<-min(critical), y1<-max(critical), lwd=4, col="red")




#################### FWER
load("/home/livio/Dropbox/lavorincorso/permAdjust/dataExample/esMaxT/resultsAdjust.Rdata")
load("/home/livio/Dropbox/lavorincorso/permAdjust/dataExample/esMaxT/resHolm.Rdata")

Q=(1:100)/1000
MaxT=sapply(Q,function(q)sum(res@res[,"Adjust:maxT"]<q))
Holm=sapply(Q,function(q)sum(resHolm<q))

pdf("compareFWER.pdf")

plot(Q,pmax(MaxT,Holm),col=0,cex.lab=1.5,xlab="Sign. level",ylab="# rejections")
lines(Q,MaxT,col="red",lwd=5) #type="s"
lines(Q,Holm,col="blue",lwd=5)
legend(.04,700,legend=c("rotation & min-p","parametric & Holm"),lwd=5,lty=1,col=c("red","blue"),bty="n")
dev.off()






################# sim 1 sample
library(flip)
#### H0
pdf("simTypeIHomo.pdf")
par(cex=1.8)
Y=matrix(rnorm(10*1000),10,1000)
res=flip(Y,tail=1)
p.t.test=round(apply(Y,2,function(y) t.test(y,alternative="greater")$p.value),4)
plot.ecdf(p.t.test,col="black",cex=.3,main="Homoscedastic Errors",ylab="Empirical Type I error",xlab="Sign. level (alpha)")
plot.ecdf(p.value(res),col="red",add=TRUE,cex=.3)
legend(.4,.3,legend=c("permutation","t-test"),lwd=5,lty=1,col=c("red","black"),bty="n")
text(.7,.4,"Type I error")
dev.off()
##
pdf("simTypeIHetero.pdf")
par(cex=1.8)
Y=diag(exp(1:10))%*%Y
res=flip(Y,tail=1)
p.t.test=round(apply(Y,2,function(y) t.test(y,alternative="greater")$p.value),4)
plot.ecdf(p.t.test,col="black",cex=.3,main="Heteroscedastic Errors",ylab="Empirical Type I error",xlab="Sign. level (alpha)")
plot.ecdf(p.value(res),col="red",add=TRUE,cex=.3)
legend(.4,.3,legend=c("permutation","t-test"),lwd=5,lty=1,col=c("red","black"),bty="n")
text(.7,.4,"Type I error")
dev.off()
##

#H1
pdf("simPowerHomo.pdf")
par(cex=1.8)
Y=matrix(.8+rnorm(10*1000),10,1000)
res=flip(Y,tail=1)
p.t.test=round(apply(Y,2,function(y) t.test(y,alternative="greater")$p.value),4)
plot.ecdf(p.t.test,col="black",cex=.3,main="Homoscedastic Errors",ylab="Empirical Power",xlab="Sign. level (alpha)")
plot.ecdf(p.value(res),col="red",add=TRUE,cex=.3)
legend(.2,.3,legend=c("permutation","t-test"),lwd=5,lty=1,col=c("red","black"),bty="n")
text(.3,.4,"Power")
dev.off()
##
pdf("simPowerHetero.pdf")
par(cex=1.8)
Y=diag(exp(1:10))%*%Y
res=flip(Y,tail=1)
p.t.test=round(apply(Y,2,function(y) t.test(y,alternative="greater")$p.value),4)
plot.ecdf(p.t.test,col="grey",cex=.3,main="Heteroscedastic Errors",ylab="Empirical Type I error",xlab="Sign. level (alpha)")
plot.ecdf(p.value(res),col="red",add=TRUE,cex=.3)
legend(.2,.3,legend=c("permutation","t-test (invalid)"),lwd=5,lty=1,col=c("red","grey"),bty="n")
text(.3,.4,"Power")
dev.off()
