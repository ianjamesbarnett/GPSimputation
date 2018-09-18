
## Figure 1. Theoretical unobserved trajectories and their surrogates.

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

dev.off()






##### widget for website
setwd("C:/Users/Ian/Documents/Work/JP/DP_GPS/Figures")
png("WebsiteWidgetSemiCircleTrajectory.png",width=180,height=140,units="px",res=800)
par(mfrow=c(1,1))
par(mai=c(0,0,0,0))

theta_0=(pi/2)
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
plot(mumat,type="l",lwd=.2,lty=2,xlim=xrang,ylim=yrang,asp=1,bty="n",yaxt="n",xaxt="n")
lines(G,col="Red",lwd=.3)
lines(H2,col="Green",lwd=.3)
lines(F2,col="Purple",lwd=.3)
