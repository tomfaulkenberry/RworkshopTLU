#Disable scientific notation (1.05e10)
options(scipen=999)

#Set number of simulations
nSims = 50000 #number of simulated experiments

M = 109
n =  50 #set sample size
SD = 15 #SD of the simulated data

p = numeric(nSims) #set up empty variable to store all simulated p-values
bars = 100
#Run simulation
for(i in 1:nSims){ #for each simulated experiment
  x = rnorm(n = n, mean = M, sd = SD) #Simulate data with specified mean, standard deviation, and sample size
  z = t.test(x, mu=100) #perform the t-test against mu (set to value you want to test against)
  p[i] = z$p.value #get the p-value and store it
  cat(sprintf("Simulation %d of %d -- p=%f\n",i,nSims,p[i]))
  }

numSig = length(p[p<0.05])
propSig = 100*numSig/nSims

#Plot figure
op = par(mar = c(5,7,4,4)) #change white-space around graph
hist(p, breaks=bars, xlab="p-values", ylab="number of experiments\n", axes=FALSE,
     main=paste("Proportion of significant results = ",propSig,"%"),
     col="grey", xlim=c(0,0.05),  ylim=c(0, nSims/10))
axis(side=1, at=seq(0,0.05, 0.01), labels=seq(0,0.05,0.01))
#axis(side=1, at=seq(0,1,0.1), labels=seq(0,1,0.1))
axis(side=2, las=2)
abline(h=nSims/bars, col="red", lty=2)
abline(v=0.05,lty=2,col="red")
