# formula from Berger & Sellke (1987)

nullProb = function(p,n){
  t=qt(1-p/2, df=Inf)
  1-(1 + (1 + n)^(-0.5)*exp(t^2/(2*(1+1/n))))^(-1)
}

y = matrix(data=NA, nrow=100, ncol=4)
n = 1:100

y[,1] = nullProb(p=0.10, n)
y[,2] = nullProb(p=0.05, n)
y[,3] = nullProb(p=0.01, n)
y[,4] = nullProb(p=0.001, n)

par(cex.main=1.5, mar=c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
plot(n,rep(-10,length(n)), type="p", ylab="", xlab="", cex=1.5,
     ylim = c(0,1), xlim=c(1,100), lwd=2, pch=5, axes=F, main="")
axis(1)
mtext("Sample size", side=1, line=3, cex=1.3, font=2)
axis(2)
par(las=0)
mtext("Posterior prob. of H1", side=2, line=3.5, cex=1.3, font=2)

lines(x=n, y=y[,1], lwd=1.5, lty=1) # p=0.10
lines(x=n, y=y[,2], lwd=1.5, lty=2) # p=0.05
lines(x=n, y=y[,3], lwd=1.5, lty=3) # p=0.01
lines(x=n, y=y[,4], lwd=1.5, lty=4)

text(80, y[80,1]-0.07, "p=0.10")
text(80, y[80,2]-0.07, "p=0.05")
text(80, y[80,3]-0.07, "p=0.01")
text(80, y[80,4]-0.07, "p=0.001")

