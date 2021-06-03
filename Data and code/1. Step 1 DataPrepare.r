##################################################################################################
#All the remote sensing data used in this study were generated from platform of Google Earth Engine
#For more information, please refer to the following link: https://code.earthengine.google.com/61ac81fb353740554e4e3537a1b4d392 
#
#The following code is a R code
##################################################################################################

# Change the following working directory to your own directory #linanqiao is my son. 
setwd('C:/Users/linanqiao/Documents/GitHub/Tibet-Water-Energy-SEM/Data and code')
library(raster)
library(rgdal)

#Boundary of the Tibet Plateau
bound=readOGR('TPBoundary_3000.shp')
plot(bound)

# Extract the grassland of study area from the MODIS LULC product (2000-2019)
    LULC=raster('MODIS_LULC_2000-2019.tif')
    plot(LULC)
    #only analysis the grass part. 10 is for grass in the LULC map；
    LULC[LULC<10]=NA; LULC[LULC>10]=NA
    LULC[LULC==10]=1
    
    GRASS=LULC
    
    plot(GRASS)
    #writeRaster(GRASS,'GRASS.tif',format='GTiff',overwrite=TRUE)

#Data of Fig. 1a: Changes in VCI for the grassland during 1982 to 2020
    VCItrend=raster('VciChangeRate1982-2020.tif')
    VCItrend=VCItrend*GRASS  #only analysis the grassland
    hist(VCItrend,main="VCI changing rate per year")
    plot(VCItrend)
    summary(VCItrend)
    plot(bound,add=T)
    
                #View areas with significant change (p<0.05)
                VCIp=raster('VciChangeSig1982-2020.tif')
                VCIp[VCIp>0.05]=NA #Remove areas with Non-significant change (p>0.05)
                VCIp=VCIp*GRASS #only analysis the grassland
                plot(VCIp)
                plot(bound,add=T,lwd=1.5)
    writeRaster(VCItrend,'GrassVCItrend.tiff',formate="GTIFF",overwrite=T)
    writeRaster(VCIp,'GrassVCIp.tiff',formate="GTIFF",overwrite=T)

# Data of Fig. 1b: Changes in vegetation cover state for the grassland during 1982 to 2020, for LAI 
    LAItrend=raster('LAIChangeRate1982-2020.tif')*0.001 #scale factor
    LAItrend=LAItrend*GRASS  #only grass part
    plot(LAItrend)
                #Get the data range
                hist(LAItrend)
                #Eliminate outliers to solve the problem of image display
                LAItrend[LAItrend<(-0.1)]=NA
                LAItrend[LAItrend>0.1]=NA
                hist(LAItrend)
                plot(LAItrend)
                plot(bound,add=T)
                
                hist(LAItrend,main="LAI changing rate per year")
                
                #View areas with significant change (p<0.05)
                LAIp=raster('LAIChangeSig1982-2020.tif')
                LAIp[LAIp>0.05]=NA #Remove areas with Non-significant change (p>0.05)
                LAIp=LAIp*GRASS #only analysis the grassland
                plot(LAIp)
                
                plot(bound,add=T,lwd=1.5)
    writeRaster(LAItrend,'GrassLAItrend.tiff',formate="GTIFF",overwrite=T)
    writeRaster(LAIp,'GrassLAIp.tiff',formate="GTIFF",overwrite=T)
                

#############################################################################################
# Prepare all the variables used in the SEM (Structural Equation Modelling)
    # The 2000-2020 average values of all the vegetation and environmental variables

    # load and have a look at these images   
    par(mfrow=c(2,1))
LAI=raster('LAI2000-2020Mean.tif')*0.001
plot(LAI)

VCI=raster('VCI2000-2020Mean.tif')
plot(VCI)

      TrendSig=raster('VciChangeSig1982-2020.tif')*GRASS     #145 groups
          plot(TrendSig)
          TrendSig[TrendSig>0.001]=NA
          TrendSig[TrendSig==0.001]=NA
          TrendSig[TrendSig<0.001]=1
      plot(TrendSig)

      
      TrendSig=raster('VciChangeSig1982-2020.tif')*GRASS    #151 groups
      plot(TrendSig)
      TrendSig[TrendSig>0.05]=NA
      TrendSig[TrendSig==0.05]=NA
      TrendSig[TrendSig<0.05]=1
      plot(TrendSig)
 
PreSumSum=raster('PreSum2000-2020Mean.tif')  # m,total precipitation (growing season sums)
plot(PreSumSum)
hist(PreSumSum)

PreWinSum=raster('PreWin2000-2020Mean.tif')  # m,total precipitation (non-growing season sums)
plot(PreWinSum)
hist(PreWinSum)

SnowCoverSumMean=raster('SnowCoverSum2000-2020Mean.tif')  # %, the fraction (0-1) of the cell / grid-box occupied by snow (growing season average values).
plot(SnowCoverSumMean)
hist(SnowCoverSumMean)

SnowCoverWinMean=raster('SnowCoverWin2000-2020Mean.tif')  # %, the fraction (0-1) of the cell / grid-box occupied by snow (non-growing season average values).
plot(SnowCoverWinMean)

SnowDepthSumMean=raster('SnowDepthSum2000-2020Mean.tif')  # m, the snow thickness on the ground (growing season average values).
plot(SnowDepthSumMean)
hist(SnowDepthSumMean)
SnowDepthWinMean=raster('SnowDepthWin2000-2020Mean.tif')  # m, the snow thickness on the ground (non-growing season average values).
plot(SnowDepthWinMean)


SnowWaterSumMean=raster('SnowWaterSum2000-2020Mean.tif') # m of water equivalent, growing season average values;
plot(SnowWaterSumMean)
hist(SnowWaterSumMean)
SnowWaterWinMean=raster('SnowWaterWin2000-2020Mean.tif') # m of water equivalent, non-growing season average values;
plot(SnowWaterWinMean)
hist(SnowWaterWinMean)


par(mfrow=c(2,1))
SoilEvapSumSum=raster('SoilEvapSum2000-2020Mean.tif')*0.01*8 # mm, aggravated from 8-day records (mm d-1), scale factor=0.01
plot(SoilEvapSumSum)
SoilEvapWinSum=raster('SoilEvapWin2000-2020Mean.tif')*0.01*8 # mm, aggravated from 8-day records (mm d-1), scale factor=0.01
plot(SoilEvapWinSum)



PlantTransSumSum=raster('PlantTransSum2000-2020Mean.tif')*0.01*8 # mm, aggravated from 8-day records (mm d-1), scale factor=0.01
plot(PlantTransSumSum)
PlantTransWinSum=raster('PlantTransWin2000-2020Mean.tif')*0.01*8 # mm, aggravated from 8-day records (mm d-1), scale factor=0.01
plot(PlantTransWinSum)



SoilTemSumMean=raster('SoilTemSum2000-2020Mean.tif')-273.15 #℃
plot(SoilTemSumMean)
SoilTemWinMean=raster('SoilTemWin2000-2020Mean.tif')-273.15 #℃
plot(SoilTemWinMean)


TemSumMean=raster('TemSum2000-2020Mean.tif')-273.15  #℃
plot(TemSumMean)
TemWinMean=raster('TemWin2000-2020Mean.tif')-273.15  #℃
plot(TemWinMean)


AmSumMean=raster('AmSum2000-2020Mean.tif')-273.15  #The humidity of the air, measured by the dewpoint temperature at about 2 m high in the unit of ℃；
plot(AmSumMean)
AmWinMean=raster('AmWin2000-2020Mean.tif')-273.15  #The humidity of the air, measured by the dewpoint temperature at about 2 m high in the unit of ℃；
plot(AmWinMean)

SoilWaterSumMean=raster('SoilWaterSum2000-2020Mean.tif')  # m3/m3, Volume of water in the surface soil layer (0 - 7 cm). 
plot(SoilWaterSumMean)
hist(SoilWaterSumMean)
SoilWaterWinMean=raster('SoilWaterWin2000-2020Mean.tif')  # m3/m3, Volume of water in the surface soil layer (0 - 7 cm). 
plot(SoilWaterWinMean)
hist(SoilWaterWinMean)


# Merge all variables into a table named 'data'   
        vci=as.data.frame(VCI)
        colnames(vci)='VCI'
        
        LAI=as.data.frame(LAI)
        colnames(LAI)='LAI'
        
        TrendSig=as.data.frame(TrendSig)
        colnames(TrendSig)='TrendSig'
        
        
        PreSum=as.data.frame(PreSumSum)
        SnowCoverSum=as.data.frame(SnowCoverSumMean)
        SnowDepthSum=as.data.frame(SnowDepthSumMean)
        SnowWaterSum=as.data.frame(SnowWaterSumMean)
        SoilEvapSum=as.data.frame(SoilEvapSumSum)
        PlantTransSum=as.data.frame(PlantTransSumSum)
        TemSum=as.data.frame(TemSumMean)
        SoilTemSum=as.data.frame(SoilTemSumMean)
        AmSum=as.data.frame(AmSumMean)
        SoilWaterSum=as.data.frame(SoilWaterSumMean)
        
        PreWin=as.data.frame(PreWinSum)
        SnowCoverWin=as.data.frame(SnowCoverWinMean)
        SnowDepthWin=as.data.frame(SnowDepthWinMean)
        SnowWaterWin=as.data.frame(SnowWaterWinMean)
        SoilEvapWin=as.data.frame(SoilEvapWinSum)
        PlantTransWin=as.data.frame(PlantTransWinSum)
        TemWin=as.data.frame(TemWinMean)
        SoilTemWin=as.data.frame(SoilTemWinMean)
        AmWin=as.data.frame(AmWinMean)
        SoilWaterWin=as.data.frame(SoilWaterWinMean)

data=cbind(vci,LAI,#Vegetation Cover
           TrendSig,#flag
           PreSum,SnowCoverSum,SnowDepthSum,SnowWaterSum,# Wet
           PreWin,SnowCoverWin,SnowDepthWin,SnowWaterWin,# Wet
           SoilEvapSum,PlantTransSum, # Drought
           SoilEvapWin,PlantTransWin, # Drought           
           TemSum,SoilTemSum,#Warming
           TemWin,SoilTemWin,#Warming           
           AmSum,SoilWaterSum,AmWin,SoilWaterWin)#Water availability

summary(data)
data=na.omit(data) # Only areas with significant change were analyzed. data=data[data$TrendSig==1,]  

str(data)   # 16362 obs. of  23 variables
head(data)

  # Variable aggregates in each VCI interval with a VCI step of 0.005
            k=seq(min(data$VCI),max(data$VCI),0.005)  #0.005 is the VCI step
            k   # 145 groups  151 groups
            set.seed(1234)
            
            d=rep(NA,23)
            f=rep(NA,23)
            head(data)
            
            
            for (j in 1:length(k)){
             b=rep(NA,46)
             dim(b)=c(2,23)
                for (i in 1:length(data[,1])){
                    if (data[i,1]>k[j] && data[i,1]<k[j+1])  
                    {a=data[i,]
                    colnames(b)=colnames(a)
                    b=rbind(b,a)
                    }
                }
            
                if (nrow(b)>2){
                    
                c=apply(b,2,mean,na.rm=T)#change mean to sd can get the std of the values
                e=apply(b,2,sd,na.rm=T)#change mean to sd can get the std of the values
                d=rbind(d,c)
                f=rbind(f,e)
                }
                if(nrow(b)==2){
                    c=rep(NA,23)
                    e=rep(NA,23)
                    d=rbind(d,c)
                    f=rbind(f,e)
                    }
            }
            
            
            Veg_Env_Mean=d[-1,]
            Veg_Env_Std=f[-1,]
            head(Veg_Env_Mean)
            str(Veg_Env_Mean)
            rownames(Veg_Env_Mean)=1:144
            rownames(Veg_Env_Std)=1:144
            #This is the data prepared for the SEM model
            #and for Fig. 2.
            write.table(Veg_Env_Mean,"Veg_Env_Mean.csv",sep=",")    
            write.table(Veg_Env_Std,"Veg_Env_Std.csv",sep=",")
            
            
# Plot Figure 2
            
           plot(Veg_Env_Mean$TemSum2000.2020Mean,Veg_Env_Mean$VCI,type=1,ylim=c(0.1,0.9))
           upper=Veg_Env_Mean$VCI+Veg_Env_Std$VCI
           lower=Veg_Env_Mean$VCI-Veg_Env_Std$VCI
           par(new=T)
           plot(Veg_Env_Mean$TemSum2000.2020Mean,Veg_Env_Mean$VCI)


# The following code generated the data for Fig. 5
data=cbind(vci,LAI,  #Vegetation Cover
           TrendSig, #flag
           TemSum,
           PreSum,SnowWaterSum,SoilEvapSum,PlantTransSum)#Water availability

data=na.omit(data)

data=data[data$TrendSig==1,]

data=data[,-3]
str(data)
head(data)

k=seq(min(data$VCI),max(data$VCI),0.005)

k#145个值
set.seed(1234)
#subdata=data[sample(row.names(data), 5000),]
dd=rep(NA,330)
head(data)


for (j in 1:length(k)){
    b=rep(NA,14)
    dim(b)=c(2,7)
    for (i in 1:length(data[,1])){
        if (data[i,1]>k[j] && data[i,1]<k[j+1])  
        {a=data[i,]
        colnames(b)=colnames(a)
        b=rbind(b,a)
        }
    }
    
    if (nrow(b)>2){
        
        e=b[-1,]
        e=e[-1,]
        f=c(e[,7],rep(NA,330-length(e[,7]))) 
# Change the 1 in e[,1] to 2,3,4,5,6,7, we can get the VCI to LAI, TemSumMean, PreSumMean, SnowWaterSumMean, SoilEvapMean & PlantTranMean
        dd=cbind(dd,f)
     }
    if(nrow(b)==2){
        f=rep(NA,330)
        dd=cbind(dd,f)
    }
}

#Corresponding to the e[,1] in line 266
#VCISum=dd
#head(VCISum)
#v=apply(VCISum,2,mean,na.rm=T)
#write.table(v,'VCI-表头.csv',sep=",")


#Corresponding to the e[,3] in line 266
#TemSum=dd
#colnames(TemSum)=v
#b=boxplot(TemSum,col="bisque")
#write.table(b$stats,'VCI-TemSum.csv',sep=",")  #For Fig. 2

#Corresponding to the e[,4] in line 266
#PreSum=dd
#colnames(PreSum)=v
#b=boxplot(PreSum,ylim=c(0,0.05),col="bisque")
#write.table(b$stats,'VCI-PreSum.csv',sep=",") #For Fig. 5a

#Corresponding to the e[,5] in line 266
#SnowWaterSum=dd
#colnames(SnowWaterSum)=v
#b=boxplot(SnowWaterSum,ylim=c(0,0.04),col="bisque")
#write.table(b$stats,'VCI-SnowWaterSum.csv',sep=",")  #For Fig. 5b

#Corresponding to the e[,6] in line 266
#SoilEvapSum=dd
#colnames(SoilEvapSum)=v
#b=boxplot(SoilEvapSum,col="bisque")
#write.table(b$stats,'VCI-SoilEvapSum.csv',sep=",") #For Fig. 5c

#Corresponding to the e[,7] in line 266
PlantTransSum=dd
colnames(PlantTransSum)=v
b=boxplot(PlantTransSum,col="bisque")
write.table(b$stats,'VCI-PlantTransSum.csv',sep=",")  #For Fig. 5d



