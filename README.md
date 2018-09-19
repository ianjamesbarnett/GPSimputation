# Summary of contents

Here we provide the code and instructions necessary to reproduce the results of the manuscript titled "Inferring Mobility Measures from GPS Traces with Missing Data" by Ian Barnett and J.P. Onnela. In addition, this repository contains the R package "GPSmobility" as well as a tutorial for its use.

## GeoLife GPS data analysis

The GeoLife project collected GPS data from 182 individuals. [Data download](https://www.microsoft.com/en-us/download/details.aspx?id=52367&from=https%3A%2F%2Fresearch.microsoft.com%2Fen-us%2Fdownloads%2Fb16d359d-d164-469e-9fd4-daa38f2b2e13%2F). In order to replicate the analysis of the GeoLife dataset (Table 2), the *GPS_preprocessing.R* and *gps_survey_communication_dailyfeatures.R* scripts need to be sourced first.

<details><summary>GeoLife analyses to produce Table 2.</summary>
<p>

```
########################
### USER INPUT REQUIRED
########################

### Modify the following string to point to gps_survey_communication_dailyfeatures.R 
daily_features_filelocation = ""
### Modify the following string to point to GPS_preprocessing.R
gps_preprocessing_filelocation = ""
### Modify the following string to point to the Geolife dataset file directory
homedir=""

########################
### END OF USER INPUT
########################

source(gps_preprocessing_filelocation)
source(daily_features_filelocation)

GPS_preprocessing = function(patient_name,
                             homedir,
                             ACCURACY_LIM=51, ### meters GPS accuracy
                             ITRVL=10, ### seconds (data concatenation)
                             tz="", ### time zone of data, defaults to current time zone
                             CENTERRAD=200, ### meters radius from significant locations considered
                             minpausedur=300,
                             minpausedist=60,
                             rad_fp=NULL,
                             wid_fp=NULL
){
  outdir = file.path(homedir,"Preprocessed_data")
  if(!file.exists(outdir)){
    dir.create(outdir)
  }  
  outdir = file.path(paste(homedir,"Preprocessed_data",sep="/"),patient_name)
  if(!file.exists(outdir)){
    dir.create(outdir)
  }  
  outfilename =paste(outdir,paste("gps_preprocessed_",patient_name,".Rdata",sep=""),sep="/")
  if(file.exists(outfilename)){
    cat("GPS already preprocessed.\n")
    return(NULL)
  }
  filelist <- list.files(path=paste(homedir,"Data",patient_name,"Trajectory",sep="/"),pattern = "\\.csv$",full.names=T)
  if(length(filelist)==0){return(NULL)}
  mobmatmiss=GPS2MobMat(filelist,itrvl=ITRVL,accuracylim=ACCURACY_LIM,r=rad_fp,w=wid_fp)
  #mobmat = GuessPause(mobmatmiss,mindur=minpausedur,r=minpausedist)
  mobmat=mobmatmiss
  obj=InitializeParams(mobmat)
  qOKmsg=MobmatQualityOK(mobmat,obj)
  if(qOKmsg!=""){
    cat(qOKmsg,"\n")
    return(NULL)
  }
  save(file=outfilename,mobmat,mobmatmiss,obj,tz,CENTERRAD,ITRVL)  
}

InduceMissingness = function(submat,neventsmiss){
  IDkeep=which((1:nrow(submat))%%(neventsmiss+1)==1)
  outmat = matrix(NA,nrow=2*length(IDkeep)-1,ncol=7)
  colnames(outmat)=colnames(submat)
  outmat[which((1:nrow(outmat))%%2==1),]=submat[IDkeep,]
  for(i in 1:(length(IDkeep)-1)){
    outmat[2*i,]=c(4,NA,NA,outmat[2*i-1,7],NA,NA,outmat[2*i+1,4])
  }
  return(outmat)
}

GetDistanceMetrics = function(submat){
  outvec = rep(NA,8)
  outvec[1]=DistanceTravelled(submat)
  outvec[2]=RadiusOfGyration(submat,ITRVL=10)
  outvec[3]=MaxDiam(submat)
  outvec[4]=AvgFlightLen(submat)
  outvec[5]=StdFlightLen(submat)
  outvec[6]=AvgFlightDur(submat)
  outvec[7]=StdFlightDur(submat)
  outvec[8]=ProbPause(submat)
  names(outvec) = c("Dist","RoG","MaxDiam","AvgFlightLen","StdFlightLen","AvgFlightDur","StdFlightDur","ProbPause")
  return(outvec)
}

IDs = list.dirs(paste(homedir,"Data",sep="/"),recursive=F,full.names=F)
simnum=1

cat("\nProcessing GPS data for",length(IDs),"subjects:\n\n")
for(i in 1:length(IDs)){
  cat(paste("Processing ID for GPS mobility: ",IDs[i]," (",i,"/",length(IDs),")\n",sep=""))
  try1=try(GPS_preprocessing(IDs[i],homedir),silent=TRUE)
  if(class(try1) == "try-error"){
    next
  }
}

ls_out=list()
minlen = 30 # minutes
neventsmiss=4
nperperson = 10
counter=1
ninds = 181

for(iii in 1:ninds){
  rdataloc = paste(homedir,"/Preprocessed_data/",IDs[iii],"/gps_preprocessed_",IDs[iii],".Rdata",sep="")
  cat("Patient: ",IDs[iii],"\n")
  if(!file.exists(rdataloc)){
    next
  }
  load(rdataloc)
  countperperson=0
  ecount=0
  scount=0
  startover=T
  for(i in 1:nrow(mobmat)){
    if(mobmat[i,1]>=3){
      if(scount > minlen*60 && ecount > 20){
        countperperson=countperperson+1
        cat(counter,"\n")
        submat=mobmat[IDstart:(i-1),]
        submatmiss = InduceMissingness(submat,neventsmiss)
        submatimp_LI=SimulateMobilityGaps(submatmiss,obj,wtype="LI")
        submatimp_TL1=SimulateMobilityGaps(submatmiss,obj,wtype="TL",spread_pars=c(1,1))
        submatimp_TL10=SimulateMobilityGaps(submatmiss,obj,wtype="TL",spread_pars=c(1,10))
        submatimp_TL20=SimulateMobilityGaps(submatmiss,obj,wtype="TL",spread_pars=c(1,20))
        submatimp_GL1=SimulateMobilityGaps(submatmiss,obj,wtype="GL",spread_pars=c(1,1))
        submatimp_GL10=SimulateMobilityGaps(submatmiss,obj,wtype="GL",spread_pars=c(1,10))
        submatimp_GL20=SimulateMobilityGaps(submatmiss,obj,wtype="GL",spread_pars=c(1,20))
        submatimp_GLR1=SimulateMobilityGaps(submatmiss,obj,wtype="GLR",spread_pars=c(1,1))
        submatimp_GLR10=SimulateMobilityGaps(submatmiss,obj,wtype="GLR",spread_pars=c(1,10))
        submatimp_GLR20=SimulateMobilityGaps(submatmiss,obj,wtype="GLR",spread_pars=c(1,20))
        ls_out[[counter]]=cbind(GetDistanceMetrics(submat),
                                GetDistanceMetrics(submatimp_LI),
                                GetDistanceMetrics(submatimp_TL1),
                                GetDistanceMetrics(submatimp_TL10),
                                GetDistanceMetrics(submatimp_TL20),
                                GetDistanceMetrics(submatimp_GL1),
                                GetDistanceMetrics(submatimp_GL10),
                                GetDistanceMetrics(submatimp_GL20),
                                GetDistanceMetrics(submatimp_GLR1),
                                GetDistanceMetrics(submatimp_GLR10),
                                GetDistanceMetrics(submatimp_GLR20))
        colnames(ls_out[[1]])=c("Truth","LI","TL1","TL10","TL20","GL1","GL10","GL20","GLR1","GLR10","GLR20")
        counter=counter+1
      }
      if(countperperson >= nperperson){
        break
      }
      startover=T
      ecount=0
      scount=0
    }else{
      if(startover){
        IDstart = i
        startover=F
      }
      scount = scount + mobmat[i,7]-mobmat[i,4]
      ecount = ecount + 1
    }
  }
}
save(file=paste(homedir,"/ComparisonTable1.Rdata",sep=""),ls_out)

errmat = matrix(0,nrow=nrow(ls_out[[1]]),ncol=ncol(ls_out[[1]]))
sumcount=0
for(i in 1:length(ls_out)){
  curmat=ls_out[[i]]
  if(length(which(curmat[,1]==0))>0){next}
  for(j in 1:nrow(ls_out[[i]])){
    errmat[j,]=errmat[j,]+100*(curmat[j,]-curmat[j,1])/(curmat[j,1])
    sumcount=sumcount+1
  }
}
errmat=errmat/sumcount
errmat = rbind(errmat,colMeans(abs(errmat)))
colnames(errmat) = colnames(ls_out[[1]])
rownames(errmat) = c(rownames(ls_out[[1]]),"Avg. Error")

cat("Average error (%)\n");colMeans(abs(errmat))


library(xtable)
xtable(errmat)
```
</p>
</details>


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

## Figure 3: A person’s daily trajectories over the course of a week.

This figure is a composite image that is used to demonstrate GPS trajectories. These mobility trace images are generated as part of the GPSmobility R package that preprocessing raw GPS data. Please see the tutorial for instructions on how to generate these kinds of figures, where demo data is also provided.
