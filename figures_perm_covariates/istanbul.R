setwd("/Users/aldo/Documents/Talks/Istanbul2012/code")
rm(list=ls())

load("sim1.Rdata")

#plot(sim[1,])

sw<-sim[,1:200]
ms<-sim[,201:400]

#boxplot(sw)
#plot(cex.lab=1.5, xlab="# false hypotheses", ylab="lower bound" ,lwd=5, apply(ms,2,median), type="s", col="blue", ylim=c(0,100))
#
#abline(a=0,b=1)

#boxplot(outline=FALSE, border="blue",ms,  pars = list(boxwex =.5, staplewex = 0, outwex = 0))
boxplot(outline=FALSE, border="red",sw, pars = list(boxwex =.5, staplewex = 0, outwex = 0) , xlab="# false hypotheses", ylab="lower bound")
abline(a=0,b=1)
lines(apply(sw,2,median), type="s", lwd=5, col="red")
lines(apply(ms,2,median), type="s", lwd=5, col="blue")
legend("topleft", lty=c(1,1),lwd=c(5,5),col=c("red","blue"), legend=c("stepwise", "no stepwise"), bty="n" )


#============ parametric + lbsimes
#library(ALL)
#data(ALL)
#pheno=pData(ALL)
#pheno$X=factor(rep("B",dim(pheno)[1]),levels=c("B","T"))
#pheno$X[grep("T",pheno$BT)]="T"
#pheno$age[is.na(pheno$age)]=mean(pheno$age,na.rm=TRUE)
#pheno$sex[is.na(pheno$sex)]="M" #the mode of the distribuition
#Y=data.frame(t(exprs(ALL)))
#print(dim(Y))
#ppara=sapply(colnames(Y),function(i)summary(lm(Y[,i]~X+age+sex,data=pheno))$coefficients["XT","Pr(>|t|)"]  )
#lbsimes<-curveSimes(ppara, plot=FALSE)
#save(lbsimes,file="lbsimes.Rdata")
#============

load("resHowmanyALL.Rdata")
load("lbsimes.Rdata")

m<-object$m
maxlb<-min(which(object$lowerbound==max(object$lowerbound)))
maxm<-m
maxm<-maxlb
maxm<-5000
#============numbers
plot(cex.lab=1.5, col="blue",lwd=5,1:maxm,lbsimes[1:maxm], type="s", xlim=c(1,maxm), ylim=c(1,maxm), xlab="# of rejected hypotheses", ylab = "lower bound")
lines(col="red",lwd=5,object$lowerbound[1:maxm])
lines(lwd=1,1:maxm,1:maxm,type="s")
#legend("top", lty=c(1,1), lwd=c(5,5,),col=c("blue","red"), legend=c("Simes", "Rotations"), bty="n" )

#============proportions
plot(cex.lab=1.5, col="blue",lwd=5, 1:m,lbsimes[1:m]/(1:m), type="s", xlim=c(1,m), ylim=c(0,1), xlab="# of rejected hypotheses", ylab = "proportion of correct rejections")
lines(col="red",lwd=5,1:m,(object$lowerbound[1:m])/(1:m), type="s")
#abline(h=object$lowerbound[m]/m, col="red")
legend("topright", lty=c(1,1),lwd=c(5,5),col=c("red","blue"), legend=c("Rotations", "Simes"), bty="n" )


library("flip")


set.seed(1)
library("copula")
b<-10000
alpha<-.1
g<-rcopula(normalCopula(0.9, dim = 2), b)
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




####################################
####### strati
rm(list=ls())
set.seed(2)
n<-24
z<-rep(c(0,1),c(n/2,n/2))
y<-z*4+rnorm(n)*1.3
group<-factor(z)
group[10:15]<-group[15:10]

#PLOT1
pdf("strata1.pdf")
par(cex=1.5)
plot(z+rnorm(24)/10,y,pch=21,lwd=1,cex=2, col=1,bg=ifelse(group==0,"black","red"),  
     yaxt="n", xlab="", 
     ylab="", frame=F, xaxt="n", xlim=c(-3,2), ylim=c(-5.5,7.5), cex.lab=2)
axis(1, at=c(0,1),labels=c("F","M"),lwd=0.5)
axis(2, labels=F,lwd=0.5)
points(-1,mean(y[group==0]), cex=3,  pch=13, col="black")
points(-1,mean(y[group==1]), cex=3,  pch=13, col="red")
abline(a=0,b=4, lwd=2)
segments(x0=-1,x1=-1, y0=mean(y[group==0]), y1=mean(y[group==1]), lwd=4, col="blue")
text(-.5, (mean(y[group==1])+mean(y[group==0]))/2, expression(hat(beta)), col="blue", cex=3)
mtext(side=2, "y", cex=3, line=2)
mtext(side=1, "               z", cex=3, line=2)
dev.off()

#PLOT2
y2<-y-(group=="1")*2
pdf("strata2.pdf")
par(cex=1.5)
plot(z+rnorm(24)/10,y2,pch=21,lwd=1,cex=2, col=1, bg=ifelse(group==0,"black","red"),  yaxt="n", xlab="", ylab="", frame=F, xaxt="n", xlim=c(-3,2), ylim=c(-6.5,6.5), cex.lab=2)
axis(1, at=c(0,1),labels=c("F","M"),lwd=0.5)
axis(2, labels=F,lwd=0.5)
points(-1,mean(y2[group==0]), cex=3,  pch=13, col="black")
points(-1,mean(y2[group==1]), cex=3,  pch=13, col="red")
abline(a=0,b=4, lwd=2)
abline(a=0-2,b=4, lwd=2,col="red")
segments(x0=-1,x1=-1, y0=mean(y2[group==0]), y1=mean(y2[group==1]), lwd=4, col="blue")
text(-.5, (mean(y[group==1])+mean(y[group==0]))/2, expression(hat(beta)), col="blue", cex=3)
mtext(side=2, "y", cex=3, line=2)
mtext(side=1, "              z", cex=3, line=2)
dev.off()

######## var continue
rm(list=ls())
set.seed(123)

n<-20
z<-seq(0,6,length=n)
y<-z+rnorm(n)
group<-factor((z>median(z))*1)
group[((n/2)-(n/4)):((n/2)+(n/4))]<-sample(group[((n/2)-(n/4)):((n/2)+(n/4))])


plot(z,y, pch=21,lwd=1,cex=1.5, col=1, bg=ifelse(group==0,"grey","red"), asp=1, yaxt="n", xlab="", ylab="", frame=F, xaxt="n", xlim=c(0,6), ylim=c(-1.5,6.5), cex.lab=2)
axis(1, labels=F,lwd=0.5)
axis(2, labels=F,lwd=0.5)
points(-2,mean(y[group==0]), cex=3,  pch=13, col="black")
points(-2,mean(y[group==1]), cex=3,  pch=13, col="red")
abline(a=0,b=1, lwd=2)
segments(x0=-2,x1=-2, y0=mean(y[group==0]), y1=mean(y[group==1]), lwd=4, col="blue")
text(-1.5, (mean(y[group==1])+mean(y[group==0]))/2, expression(hat(beta)), col="blue", cex=3)
mtext(side=2, "y", cex=3, line=2)
mtext(side=1, "z", cex=3, line=2)

y2<-y-(group=="1")*3
plot(z,y2, cex=3, col=group, asp=1, yaxt="n", xlab="", ylab="", frame=F, xaxt="n", xlim=c(0,6), ylim=c(-1.5,6.5), pch=19, cex.lab=2)
axis(1, labels=F,lwd=0.5)
axis(2, labels=F,lwd=0.5)
points(-2,mean(y2[group==0]), cex=3, col="black", pch=13)
points(-2,mean(y2[group==1]), cex=3, col="red", pch=13)
abline(a=0,b=1, lwd=3)
abline(a=-3,b=1, col="red", lwd=3)
segments(x0=-2,x1=-2, y0=mean(y2[group==0]), y1=mean(y2[group==1]), lwd=3, col="blue")
text(-1.5, (mean(y2[group==1])+mean(y2[group==0]))/2, expression(hat(beta)), col="blue", cex=3)
mtext(side=2, "y", cex=3, line=2)
mtext(side=1, "z", cex=3, line=2)

n<-10
Z<-cbind(rnorm(n),rnorm(n))
H<-Z%*%solve(t(Z)%*%Z)%*%t(Z)
I_H<-diag(n)-H

Q<-eigen(I_H)$vectors[,round(eigen(I_H)$values,6)!=0]
round(t(Q)%*%Q,4)
round(I_H-Q%*%t(Q),4)

solve(Q)

dec<-qr(I_H)
solqr.Q(dec)
round(t(qr.Q(dec))%*%qr.Q(dec),6)
round(I_H - qr.Q(dec)%*%t(qr.Q(dec)),6 )


Q<-qr(I_H)$qr[,round(qr(I_H)$qraux,6)!=0]]
round(I_H-Q%*%t(Q),4)

yadj<-I_H%*%y
y2adj<-I_H%*%y2
xadj<-I_H%*%(group=="0")*1

plot(xadj,yadj, cex=3, asp=.5, col=4, yaxt="n", xlab="", ylab="", frame=F, xaxt="n",  pch=19, cex.lab=2)
axis(1, labels=F,lwd=0.5)
axis(2, labels=F,lwd=0.5)

abline(a=0,b=lm(yadj~0+xadj)$coefficient, lwd=3, col=4)
abline(h=0)
segments(x0=1,x1=1,y0=0,y1=-0.1762391, col=4, lwd=3)
text(1.25, 0, expression(hat(beta)), col="blue", cex=3)
mtext(side=2, expression(P[z^L] ~~ y), cex=3, line=1)
mtext(side=1, expression(P[z^L] ~~ x), cex=3, line=3)

plot(xadj,y2adj, cex=3, asp=.5, col=4, yaxt="n", xlab="", ylab="", frame=F, xaxt="n",  pch=19, cex.lab=2)
axis(1, labels=F,lwd=0.5)
axis(2, labels=F,lwd=0.5)

abline(a=0,b=lm(y2adj~0+xadj)$coefficient, lwd=3, col=4)
abline(h=0)
segments(x0=.75,x1=.75,y0=0,y1=2.823761*.75, col=4, lwd=3)
text(1.25, 1, expression(hat(beta)), col="blue", cex=3)
mtext(side=2, expression(P[z^L] ~~ y), cex=3, line=1)
mtext(side=1, expression(P[z^L] ~~ x), cex=3, line=3)



group<-(group=="1")*1
fit<-lm(y~0+z)
yadj<-y-fit$coefficients["z"]*z 

#plot(z,yadj, cex=2, col=as.factor(group), asp=1, xlab="z", ylab=expression(paste("y ",-hat(gamma), " z")), frame=F, xaxt="n",  xlim=c(0,6), ylim=c(-3,4), pch=19, cex.lab=2)
plot(z,yadj, cex=2, col=as.factor(group), asp=1, xlab="z", ylab="", frame=F, xaxt="n", yaxt="n",  xlim=c(0,6), ylim=c(-3,4), pch=19, cex.lab=2)
axis(1, labels=F,lwd=0.5)
axis(2, labels=F,lwd=0.5)
abline(h=mean(yadj))
points(-2,mean(yadj[group==0]), cex=2.5, col="black", pch=13)
points(-2,mean(yadj[group==1]), cex=2.5, col="red", pch=13)
segments(x0=-2,x1=-2, y0=mean(yadj[group==0]), y1=mean(yadj[group==1]), lwd=2, col="blue")
text(-1.5, (mean(yadj[group==1])+mean(yadj[group==0]))/2, expression(hat(beta)), col="blue", cex=2)
mtext(side=2, expression(paste("y ",-hat(gamma), " z")), cex=2, line=1.5)



y<-rnorm(2)
angle<-runif(1,0,2*pi)
ystar<-c(y[1]*cos(angle) - y[2]*sin(angle), y[1]*sin(angle) + y[2]*cos(angle))
lines(ystar[1],ystar[2], type="p")

fit<-lm(y~z)
Z-cbind(z)
H<-Z%*%solve(t(Z)%*%Z)%*%t(Z)
I_H<-diag(n)-H
Q<-eigen(I_H)$vectors[,round(eigen(I_H)$values,3)!=0]
ytrasf<-t(Q)%*%fit$residuals
xtrasf<-t(Q)%*%group
summary(lm(fit$residuals~I_H%*%group))
summary(lm(ytrasf~ xtrasf))


n<-12
p<-10
library(MASS)
rho=.95
Sig<-matrix(rho,p,p)+diag(rep(1-rho,p))
y<-mvrnorm(n,Sigma=Sig, mu=rep(0,p))
x<-cbind(1,runif(n))
hatB<-solve(t(x)%*%x)%*%t(x)%*%y
P<-diag(n)-x%*%solve(t(x)%*%x)%*%t(x)
hatSigma<- n^(-1)* t(y)%*%P%*%y


Z<-cbind(1,z)
H<-Z%*%solve(t(Z)%*%Z)%*%t(Z)
I_H<-diag(n)-H

ytrasf<-I_H%*%y
xtrasf<-I_H%*%(as.numeric(group)-1)
plot(xtrasf,ytrasf)
abline(a=0, b=lm(ytrasf~0+xtrasf)$coefficient)
