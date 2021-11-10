setwd("C:/Users/BANZH/Dropbox/low flow project 8 week plan/Figures/")
library(ncdf4)
library(fields)
library(RColorBrewer)
ori=nc_open('ori.detrend.2000-01.nc')
a1d=nc_open('a1d.detrend.2000-01.nc')
sub=nc_open('a-b.nc')
Sf=nc_open('Sf.detrend.2000-01.nc')
Sf1=ncvar_get(Sf,'Sf')
nc_close(Sf)
ocrainc=ncvar_get(ori,'Q_rainc')
ocrainw=ncvar_get(ori,'q_rainw')
ocsnow=ncvar_get(ori,'Q_snow')
oqw=ncvar_get(ori,'Q_summer')
ofsnow=ncvar_get(ori,'F_snow')
ofrainw=ncvar_get(ori,'F_rainw')
ofrainc=ncvar_get(ori,'F_rainc')
afsnow=ncvar_get(a1d,'F_snow')
afrainw=ncvar_get(a1d,'F_rainw')
afrainc=ncvar_get(a1d,'F_rainc')
acrainc=ncvar_get(a1d,'Q_rainc')
acrainw=ncvar_get(a1d,'q_rainw')
acsnow=ncvar_get(a1d,'Q_snow')
aqw=ncvar_get(a1d,'Q_summer')
cgfsnow=ncvar_get(sub,'F_snow')
cgfrainw=ncvar_get(sub,'F_rainw')
cgfrainc=ncvar_get(sub,'F_rainc')
lon=ncvar_get(ori,'lon')
lat=ncvar_get(ori,'lat')
nc_close(ori)
nc_close(a1d)
nc_close(sub)
##Plot Fig1
colors=brewer.pal(8,'YlGnBu')
par(mfrow=c(1,3),mgp=c(2,1,0),tck=-0.02,mar=c(4,4,3,3))
image.plot(lon,lat,ofrainc,cex.axis=1.5,
           cex.lab=1.5,zlim=c(0,1),
           col=colors,main='F_rainc',cex.main=1.5,
           horizontal = TRUE,
           legend.line=-10)
image.plot(lon,lat,ofrainw,cex.axis=1.5,
           cex.lab=1.5,zlim=c(0,1),
           col=colors,main='F_rainw',cex.main=1.5,
           horizontal = TRUE,
           legend.line=-10)
image.plot(lon,lat,ofsnow,cex.axis=1.5,
           cex.lab=1.5,zlim=c(0,1),
           col=colors,main='F_snow',cex.main=1.5,
           horizontal = TRUE,
           legend.line=-10)
##Plot Fig2
colors=brewer.pal(10,'RdBu')
par(mfrow=c(1,3),mgp=c(2,1,0),tck=-0.02,mar=c(4,4,3,3))
#range of cgfrainc (-0.12~0.12)
image.plot(lon,lat,cgfrainc,cex.axis=1.5,
           cex.lab=1.5,zlim=c(-0.12,0.12),
           col=colors,main='F_rainc',cex.main=1.5,
           horizontal = TRUE,
           legend.line=-10)
cgfrainw[which(cgfrainw>0.1)]=0.12 #(-0.88,0.343)
image.plot(lon,lat,cgfrainw,cex.axis=1.5,
           cex.lab=1.5,zlim=c(-0.12,0.12),
           col=colors,main='F_rainw',cex.main=1.5,
           horizontal = TRUE,
           legend.line=-10)
cgfsnow[which(abs(cgfsnow)>0.1)]=-0.12  ##(-0.53~0.24)
image.plot(lon,lat,cgfsnow,cex.axis=1.5,
           cex.lab=1.5,zlim=c(-0.12,0.12),
           col=colors,main='F_snow',cex.main=1.5,
           horizontal = TRUE,
           legend.line=-10)
##Fig 3. absolute change
ind=which(!is.na(ocrainc)&(abs(ocrainc)<10000)&(oqw>20))
len=length(ind)
name=c(rep(c(rep("C_rainc",len),rep("C_rainw",len),rep("C_snow",len)),2))
value=c(ocrainc[ind],ocrainw[ind],ocsnow[ind],acrainc[ind],
        acrainw[ind],acsnow[ind])
scen=c(rep("solid",len*3),rep("dotdash",len*3))
Sfval=c(rep(Sf1[ind],6))
df1=as.data.frame(cbind(value,name,scen,Sfval),stringsAsFactors = FALSE)
df1$value=as.numeric(df1$value)
df1$Sfval=as.numeric(df1$Sfval)
library(ggplot2)
df1$name <- factor(df1$name,levels=c("C_rainc","C_rainw","C_snow"))
ggplot(df1[,],aes(x=Sfval,y=value,col=name))+
  geom_smooth(method="loess",span=0.5,se=TRUE,aes(fill=name,linetype=scen))+
  scale_linetype_manual(labels=c("baseline","annual1d"),values=c("solid","dotdash"))+
  xlab("Snowfall fraction")+
  ylab("Contribution of different components")
##Fig 3. absolute change
ind=which(!is.na(ocrainc)&(abs(ocrainc)<10000)&(oqw>20))
len=length(ind)
name=c(rep(c(rep("C_rainc",len),rep("C_rainw",len),rep("C_snow",len)),2))
value=c(ocrainc[ind],ocrainw[ind],ocsnow[ind],acrainc[ind],
        acrainw[ind],acsnow[ind])
qw=c(rep(oqw[ind],6))
scen=c(rep("solid",len*3),rep("dotdash",len*3))
Sfval=c(rep(Sf1[ind],6))
df1=as.data.frame(cbind(value,name,scen,Sfval,qw),stringsAsFactors = FALSE)
df1$value=as.numeric(df1$value)
df1$Sfval=as.numeric(df1$Sfval)
df1$qw=as.numeric(df1$qw)
library(ggplot2)
df1$name <- factor(df1$name,levels=c("C_rainc","C_rainw","C_snow"))
ggplot(df1[,],aes(x=qw,y=value,col=name))+
  geom_smooth(method="loess",span=0.5,se=TRUE,aes(fill=name,linetype=scen))+
  scale_linetype_manual(labels=c("baseline","annual1d"),values=c("solid","dotdash"))+
  xlab("Baseline summer streamflow (mm)")+
  ylab("Contribution of different components")

##Fig 3. absolute change no oqw preset
ind=which(!is.na(ocrainc)&(abs(ocrainc)<10000))
len=length(ind)
name=c(rep(c(rep("C_rainc",len),rep("C_rainw",len),rep("C_snow",len)),2))
value=c(ocrainc[ind],ocrainw[ind],ocsnow[ind],acrainc[ind],
        acrainw[ind],acsnow[ind])
qw=c(rep(oqw[ind],6))
scen=c(rep("solid",len*3),rep("dotdash",len*3))
Sfval=c(rep(Sf1[ind],6))
df1=as.data.frame(cbind(value,name,scen,Sfval,qw),stringsAsFactors = FALSE)
df1$value=as.numeric(df1$value)
df1$Sfval=as.numeric(df1$Sfval)
df1$qw=as.numeric(df1$qw)
library(ggplot2)
df1$name <- factor(df1$name,levels=c("C_rainc","C_rainw","C_snow"))
ggplot(df1[,],aes(x=qw,y=value,col=name))+
  geom_smooth(method="loess",span=0.5,se=FALSE,aes(fill=name,linetype=scen))+
  scale_linetype_manual(labels=c("baseline","annual1d"),values=c("solid","dotdash"))+
  xlab("Baseline summer streamflow (mm)")+
  ylab("Contribution of different components")



plot(x=oqw[ind],y=ofrainc[ind],ylim=c(0,1))
plot(x=Sf1[ind],y=ofrainw[ind],ylim=c(0,1))
plot(x=oqw[ind],y=ofsnow[ind],ylim=c(0,1))
plot(x=oqw[ind],y=afsnow[ind],ylim=c(0,1))
plot(x=oqw[ind],y=afrainc[ind],ylim=c(0,1))
plot(x=Sf1[ind],y=ocrainc[ind],ylim=c(0,200))
plot(x=Sf1[ind],y=ocrainw[ind],ylim=c(0,200))
plot(x=Sf1[ind],y=acrainc[ind],ylim=c(0,200))
plot(x=Sf1[ind],y=ocsnow[ind],ylim=c(0,200))
# ##Fig1 real amount (not impressive nor straightforward)
# par(mfrow=c(1,4),mgp=c(2,1,0),tck=-0.02,mar=c(4,4,3,3))
# image.plot(lon,lat,oqw,cex.axis=1.5,
#            cex.lab=1.5,zlim=c(0,2000),
#            col=colors,main='Summer Q (mm)',cex.main=1.5,
#            horizontal = TRUE,
#            legend.line=-10)
# image.plot(lon,lat,ocsnow,cex.axis=1.5,
#            cex.lab=1.5,zlim=c(0,2000),
#            col=colors,main='Snowmelt Contribution (mm)',cex.main=1.5,
#            horizontal = TRUE,
#            legend.line=-10)
# image.plot(lon,lat,ocrainc,cex.axis=1.5,
#            cex.lab=1.5,zlim=c(0,500),
#            col=colors,main='Cool season rainfall Contribution (mm)',cex.main=1.5,
#            horizontal = TRUE,
#            legend.line=-10)
# image.plot(lon,lat,ocrainw,cex.axis=1.5,
#            cex.lab=1.5,zlim=c(0,200),
#            col=colors,main='Warm season rainfall Contribution (mm)',cex.main=1.5,
#            horizontal = TRUE,
#            legend.line=-10)

##Fig2 Real amount change (not impressive nor straightforward)
# par(mfrow=c(1,4),mgp=c(2,1,0),tck=-0.02,mar=c(4,4,3,3))
# image.plot(lon,lat,aqw-oqw,cex.axis=1.5,
#            cex.lab=1.5,zlim=c(-200,200),
#            col=colors,main='Summer Q change (mm)',cex.main=1.5,
#            horizontal = TRUE,
#            legend.line=-10)
# image.plot(lon,lat,acsnow-ocsnow,cex.axis=1.5,
#            cex.lab=1.5,zlim=c(-200,200),
#            col=colors,main='Snowmelt contribution change (mm)',cex.main=1.5,
#            horizontal = TRUE,
#            legend.line=-10)
# image.plot(lon,lat,acrainc-ocrainc,cex.axis=1.5,
#            cex.lab=1.5,zlim=c(-10,10),
#            col=colors,main='Cool season rainfall contribution change (mm)',cex.main=1.5,
#            horizontal = TRUE,
#            legend.line=-10)
# image.plot(lon,lat,acrainw-ocrainw,cex.axis=1.5,
#            cex.lab=1.5,zlim=c(-50,50),
#            col=colors,main='Warm season rainfall contribution change (mm)',cex.main=1.5,
#            horizontal = TRUE,
#            legend.line=-10)

