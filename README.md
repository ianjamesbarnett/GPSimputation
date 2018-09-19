## Summary of contents

Here we provide the code and instructions necessary to reproduce the results of the manuscript titled "Inferring Mobility Measures from GPS Traces with Missing Data" by Ian Barnett and J.P. Onnela. In addition, this repository contains the R package "GPSmobility" as well as a tutorial for its use.


## Figure 1. Theoretical unobserved trajectories and their surrogates.

![image](/Figures/SemiCircleTrajectoriesExampleFig2.png)

<details><summary>The code that produces this figure.</summary>
<p>
  
```
par(mfrow=c(3,2))
par(mai=c(0,0,0,0))

theta_0=(pi/2)
N = 50
d=1
mumat = matrix(0,nrow=N,ncol=2)
for(t in 2:N){
  mumat[t,1]=mumat[t-1,1]+d*cos(theta_0-2*theta_0*(t-2)/(N-2))
  mumat[t,2]=mumat[t-1,2]+d*sin(theta_0-2*theta_0*(t-2)/(N-2))
}
set.seed(130414)
G=matrix(0,nrow=N,ncol=2)
H=matrix(0,nrow=N,ncol=2)
for(t in 2:N){
  G[t,1]=G[t-1,1]+rnorm(1,mean=d*cos(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
  G[t,2]=G[t-1,2]+rnorm(1,mean=d*sin(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
  H[t,1]=H[t-1,1]+rnorm(1,mean=d*cos(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
  H[t,2]=H[t-1,2]+rnorm(1,mean=d*sin(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
}
H2=matrix(0,nrow=N,ncol=2)
F2=matrix(0,nrow=N,ncol=2)
for(t in 2:N){
  H2[t,]=H[t,]+(G[N,]-H[N,])*t/N
  F2[t,]=G[N,]*t/N
}
xrang=range(c(G[,1],H2[,1],mumat[,1]))
yrang=range(c(G[,2],H2[,2],mumat[,2]))
plot(mumat,type="l",lty=2,xlim=xrang,ylim=c(yrang[1],yrang[2]+(yrang[2]-yrang[1])/4),asp=1,bty="o",yaxt="n",xaxt="n")
lines(G,col="Red")
lines(H2,col="Green")
lines(F2,col="Purple")
#text(xrang[1],yrang[2],bquote(paste("T=",.(N),"; ",theta[0],"=",pi/2)),pos=4)
legend("topright",legend=bquote(paste("n=",.(N),"; ",theta[0],"=",pi/2)),bty="n")
legend("topleft",legend="A",bty="n")


N = 800
d=1
mumat = matrix(0,nrow=N,ncol=2)
for(t in 2:N){
  mumat[t,1]=mumat[t-1,1]+d*cos(theta_0-2*theta_0*(t-2)/(N-2))
  mumat[t,2]=mumat[t-1,2]+d*sin(theta_0-2*theta_0*(t-2)/(N-2))
}
set.seed(130418)
G=matrix(0,nrow=N,ncol=2)
H=matrix(0,nrow=N,ncol=2)
for(t in 2:N){
  G[t,1]=G[t-1,1]+rnorm(1,mean=d*cos(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
  G[t,2]=G[t-1,2]+rnorm(1,mean=d*sin(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
  H[t,1]=H[t-1,1]+rnorm(1,mean=d*cos(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
  H[t,2]=H[t-1,2]+rnorm(1,mean=d*sin(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
}
H2=matrix(0,nrow=N,ncol=2)
F2=matrix(0,nrow=N,ncol=2)
for(t in 2:N){
  H2[t,]=H[t,]+(G[N,]-H[N,])*t/N
  F2[t,]=G[N,]*t/N
}
xrang=range(c(G[,1],H2[,1],mumat[,1]))
yrang=range(c(G[,2],H2[,2],mumat[,2]))
plot(mumat,type="l",lty=2,xlim=xrang,ylim=c(yrang[1],yrang[2]+(yrang[2]-yrang[1])/4),asp=1,bty="o",yaxt="n",xaxt="n")
lines(G,col="Red")
lines(H2,col="Green")
lines(F2,col="Purple")
#text(xrang[1],yrang[2],bquote(paste("T=",.(N),"; ",theta[0],"=",pi/2)),pos=4)
legend("topright",legend=bquote(paste("n=",.(N),"; ",theta[0],"=",pi/2)),bty="n")
legend("topleft",legend="B",bty="n")



theta_0=(pi/18)
N = 50
d=1
mumat = matrix(0,nrow=N,ncol=2)
for(t in 2:N){
  mumat[t,1]=mumat[t-1,1]+d*cos(theta_0-2*theta_0*(t-2)/(N-2))
  mumat[t,2]=mumat[t-1,2]+d*sin(theta_0-2*theta_0*(t-2)/(N-2))
}
set.seed(14124209)
G=matrix(0,nrow=N,ncol=2)
H=matrix(0,nrow=N,ncol=2)
for(t in 2:N){
  G[t,1]=G[t-1,1]+rnorm(1,mean=d*cos(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
  G[t,2]=G[t-1,2]+rnorm(1,mean=d*sin(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
  H[t,1]=H[t-1,1]+rnorm(1,mean=d*cos(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
  H[t,2]=H[t-1,2]+rnorm(1,mean=d*sin(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
}
H2=matrix(0,nrow=N,ncol=2)
F2=matrix(0,nrow=N,ncol=2)
for(t in 2:N){
  H2[t,]=H[t,]+(G[N,]-H[N,])*t/N
  F2[t,]=G[N,]*t/N
}
xrang=range(c(G[,1],H2[,1],mumat[,1]))
yrang=range(c(G[,2],H2[,2],mumat[,2]))
plot(mumat,type="l",lty=2,xlim=xrang,ylim=yrang,asp=1,bty="o",yaxt="n",xaxt="n")
lines(G,col="Red")
lines(H2,col="Green")
lines(F2,col="Purple")
#text(xrang[1],yrang[2],bquote(paste("T=",.(N),"; ",theta[0],"=",pi/18)),pos=4)
legend("topright",legend=bquote(paste("n=",.(N),"; ",theta[0],"=",pi/18)),bty="n")
legend("topleft",legend="C",bty="n")

N = 800
d=1
mumat = matrix(0,nrow=N,ncol=2)
for(t in 2:N){
  mumat[t,1]=mumat[t-1,1]+d*cos(theta_0-2*theta_0*(t-2)/(N-2))
  mumat[t,2]=mumat[t-1,2]+d*sin(theta_0-2*theta_0*(t-2)/(N-2))
}
set.seed(130421)
G=matrix(0,nrow=N,ncol=2)
H=matrix(0,nrow=N,ncol=2)
for(t in 2:N){
  G[t,1]=G[t-1,1]+rnorm(1,mean=d*cos(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
  G[t,2]=G[t-1,2]+rnorm(1,mean=d*sin(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
  H[t,1]=H[t-1,1]+rnorm(1,mean=d*cos(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
  H[t,2]=H[t-1,2]+rnorm(1,mean=d*sin(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
}
H2=matrix(0,nrow=N,ncol=2)
F2=matrix(0,nrow=N,ncol=2)
for(t in 2:N){
  H2[t,]=H[t,]+(G[N,]-H[N,])*t/N
  F2[t,]=G[N,]*t/N
}
xrang=range(c(G[,1],H2[,1],mumat[,1]))
yrang=range(c(G[,2],H2[,2],mumat[,2]))
plot(mumat,type="l",lty=2,xlim=xrang,ylim=yrang,asp=1,bty="o",yaxt="n",xaxt="n")
lines(G,col="Red")
lines(H2,col="Green")
lines(F2,col="Purple")
#text(xrang[1],yrang[2],bquote(paste("T=",.(N),"; ",theta[0],"=",pi/18)),pos=4)
legend("topright",legend=bquote(paste("n=",.(N),"; ",theta[0],"=",pi/18)),bty="n")
legend("topleft",legend="D",bty="n")




theta_0=0
N = 50
d=1
mumat = matrix(0,nrow=N,ncol=2)
for(t in 2:N){
  mumat[t,1]=mumat[t-1,1]+d*cos(theta_0-2*theta_0*(t-2)/(N-2))
  mumat[t,2]=mumat[t-1,2]+d*sin(theta_0-2*theta_0*(t-2)/(N-2))
}
set.seed(14124211)
G=matrix(0,nrow=N,ncol=2)
H=matrix(0,nrow=N,ncol=2)
for(t in 2:N){
  G[t,1]=G[t-1,1]+rnorm(1,mean=d*cos(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
  G[t,2]=G[t-1,2]+rnorm(1,mean=d*sin(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
  H[t,1]=H[t-1,1]+rnorm(1,mean=d*cos(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
  H[t,2]=H[t-1,2]+rnorm(1,mean=d*sin(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
}
H2=matrix(0,nrow=N,ncol=2)
F2=matrix(0,nrow=N,ncol=2)
for(t in 2:N){
  H2[t,]=H[t,]+(G[N,]-H[N,])*t/N
  F2[t,]=G[N,]*t/N
}
xrang=range(c(G[,1],H2[,1],mumat[,1]))
yrang=range(c(G[,2],H2[,2],mumat[,2]))
plot(mumat,type="l",lty=2,xlim=xrang,ylim=yrang,asp=1,bty="o",yaxt="n",xaxt="n")
lines(G,col="Red")
lines(H2,col="Green")
lines(F2,col="Purple")
#text(xrang[1],yrang[2],bquote(paste("T=",.(N),"; ",theta[0],"=",pi/2)),pos=4)
legend("topright",legend=bquote(paste("n=",.(N),"; ",theta[0],"=",0)),bty="n")
legend("topleft",legend="E",bty="n")


N = 800
d=1
mumat = matrix(0,nrow=N,ncol=2)
for(t in 2:N){
  mumat[t,1]=mumat[t-1,1]+d*cos(theta_0-2*theta_0*(t-2)/(N-2))
  mumat[t,2]=mumat[t-1,2]+d*sin(theta_0-2*theta_0*(t-2)/(N-2))
}
set.seed(130423)
G=matrix(0,nrow=N,ncol=2)
H=matrix(0,nrow=N,ncol=2)
for(t in 2:N){
  G[t,1]=G[t-1,1]+rnorm(1,mean=d*cos(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
  G[t,2]=G[t-1,2]+rnorm(1,mean=d*sin(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
  H[t,1]=H[t-1,1]+rnorm(1,mean=d*cos(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
  H[t,2]=H[t-1,2]+rnorm(1,mean=d*sin(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
}
H2=matrix(0,nrow=N,ncol=2)
F2=matrix(0,nrow=N,ncol=2)
for(t in 2:N){
  H2[t,]=H[t,]+(G[N,]-H[N,])*t/N
  F2[t,]=G[N,]*t/N
}
xrang=range(c(G[,1],H2[,1],mumat[,1]))
yrang=range(c(G[,2],H2[,2],mumat[,2]))
plot(mumat,type="l",lty=2,xlim=xrang,ylim=yrang,asp=1,bty="o",yaxt="n",xaxt="n")
lines(G,col="Red")
lines(H2,col="Green")
lines(F2,col="Purple")
#text(xrang[1],yrang[2],bquote(paste("T=",.(N),"; ",theta[0],"=",pi/2)),pos=4)
legend("topright",legend=bquote(paste("n=",.(N),"; ",theta[0],"=",0)),bty="n")
legend("topleft",legend="F",bty="n")
legend("bottomright",c("Truth","Simulation","LI",expression(mu)),bty="n",lty=c(1,1,1,2),col=c("Red","Green","Purple","Black"))
```

</p>
</details>

## Figure 2. Expected average gap between imputed trajectories and the true unobserved trajectory.

![image](/Figures/AnalyticSquaredGapFigure.png)



<details><summary>The code that produces this figure.</summary>
<p>
  
```
N_v=seq(100,500,100)
nreps=2000
maxgap_v=rep(0,length(N_v))
maxgapH_v=rep(0,length(N_v))
for(b in 1:length(N_v)){
  ProgressBar(length(N_v),b)
  N=N_v[b]  
  mux=1
  muy=1
  sigmax=1
  sigmay=1
  cumsum=0
  cumsumH=0
  for(ii in 1:nreps){
    G=matrix(NA,nrow=N,ncol=2)
    G[1,1] = rnorm(1,mux,sigmax)
    G[1,2] = rnorm(1,muy,sigmay)
    H=matrix(NA,nrow=N,ncol=2)
    H[1,1] = rnorm(1,mux,sigmax)
    H[1,2] = rnorm(1,muy,sigmay)  
    for(i in 2:N){
      G[i,1] = G[(i-1),1]+rnorm(1,mux,sigmax)
      G[i,2] = G[(i-1),2]+rnorm(1,muy,sigmay)
      H[i,1] = H[(i-1),1]+rnorm(1,mux,sigmax)
      H[i,2] = H[(i-1),2]+rnorm(1,muy,sigmay)
    }
    gap=rep(0,N)
    gapH=rep(0,N)
    
    for(i in 1:N){
      gap[i]=(G[i,1]-G[N,1]*i/N)^2+(G[i,2]-G[N,2]*i/N)^2
      gapH[i]= (G[i,1]-(G[N,1]*i/N + H[i,1]*(N-i)/N))^2 + (G[i,2]-(G[N,2]*i/N + H[i,2]*(N-i)/N))^2
    }    
    cumsum=cumsum+max(gap)
    cumsumH=cumsumH+gapH[ceiling(N/3)]
  }
  maxgap_v[b]=cumsum/nreps
  maxgapH_v[b]=cumsumH/nreps
}


theta_0=(pi/2)
N = 50
d=1
mumat = matrix(0,nrow=N,ncol=2)
for(t in 2:N){
  mumat[t,1]=mumat[t-1,1]+d*cos(theta_0-2*theta_0*(t-2)/(N-2))
  mumat[t,2]=mumat[t-1,2]+d*sin(theta_0-2*theta_0*(t-2)/(N-2))
}
plot(mumat,type="l")

theta_0_v=c(0,(pi/2)/9,pi/3,(pi/2))
lsout = list()
for(jj in 1:length(theta_0_v)){
  theta_0=theta_0_v[jj]
  d=1
  N_v = seq(50,1000,50)
  nreps=1000
  gapH_v=rep(0,length(N_v))
  gap_v=rep(0,length(N_v))
  for(ii in 1:length(N_v)){
    ProgressBar(length(N_v),ii)
    N=N_v[ii]  
    cumsum=0
    cumsumH=0
    for(iii in 1:nreps){
      G=matrix(0,nrow=N,ncol=2)
      H=matrix(0,nrow=N,ncol=2)
      for(t in 2:N){
        G[t,1]=G[t-1,1]+rnorm(1,mean=d*cos(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
        G[t,2]=G[t-1,2]+rnorm(1,mean=d*sin(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
        H[t,1]=H[t-1,1]+rnorm(1,mean=d*cos(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
        H[t,2]=H[t-1,2]+rnorm(1,mean=d*sin(theta_0-2*theta_0*(t-2)/(N-2)),sd=1)
      }
      gap=rep(0,N)
      gapH=rep(0,N)
      for(i in 1:N){
        gap[i]=(G[i,1]-(G[N,1]*i/N+G[1,1]*(N-i)/N))^2+(G[i,2]-(G[N,2]*i/N+G[1,2]*(N-i)/N))^2
        gapH[i]= (G[i,1]-(H[i,1]+(G[N,1]-H[N,1])*i/N))^2 + (G[i,2]-(H[i,2]+(G[N,2]-H[N,2])*i/N))^2
      }    
      cumsumH=cumsumH+mean(gapH)/nreps
      cumsum=cumsum+mean(gap)/nreps
    }
    gapH_v[ii]=cumsumH
    gap_v[ii]=cumsum
  }
  lsout[[jj]]=cbind(gap_v,gapH_v)
}

par(mai=c(.5,.5,.1,.1),mgp=c(1.5,.5,0))
plot(NA,xlim=c(1,N_v[length(N_v)]),ylim=log10(range(lsout)),xlab="n",ylab=expression(paste("Average Squared Gap (",log[10]," scale)",sep="")),cex.lab=.8,cex.axis=.8)
lines(N_v,log10(lsout[[1]][,1]),lty=1,col="Red")
lines(N_v,log10(lsout[[2]][,1]),lty=1,col="Blue")
lines(N_v,log10(lsout[[3]][,1]),lty=1,col="Purple")
lines(N_v,log10(lsout[[4]][,1]),lty=1,col="ForestGreen")
lines(N_v,log10(lsout[[1]][,2]),lty=2,col="Black")
legend("topleft",c(expression(paste("LI: ",theta[0],"=",pi/2,sep="")),expression(paste("LI: ",theta[0],"=",pi/3,sep="")),expression(paste("LI: ",theta[0],"=",pi/18,sep="")),expression(paste("LI: ",theta[0],"=",0,sep="")),"Simulation"),lty=c(1,1,1,1,2),col=c("ForestGreen","Purple","Blue","Red","Black"),bty='n',cex=.6)

```

</p>
</details>
