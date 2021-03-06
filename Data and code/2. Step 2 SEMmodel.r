#This is a R code.
set.seed(1234)

#Set the working directory
setwd('C:/Users/linanqiao/Documents/GitHub/Tibet-Water-Energy-SEM/Data and code')
remove(list=ls())

# load library
library(lavaan)
library(blavaan) # Fitting structural equation modelslibrary(lavaan.survey)
library(AICcmodavg)
#source("lavaan.modavg.R")  

library(brms)
library(piecewiseSEM)

library(rstan)
library(rstantools)
library("future") #set proceesor work mode
plan(multiprocess) #set proceesor work mode

library(corrplot)


data=read.table('Veg_Env_Mean.csv',sep=",",header=T)
head(data)
str(data)

#Draw figures to remove outliers
        pairs(data[,1:10])
        # Remove the first and last 3 records from the 144 obs, which are obvious outliers.
        data=data[-(1:3),];str(data)
        data=data[-(139:141),];str(data)
        
        #Check outliers again 
        plot(data[,6])
        # Remove 9 more outliers from the remaining 138 obs. detected by the summer average snow depth during 2000 to 2020
        data[data[,6]>0.06,]=NA
        data=na.omit(data)
        
        #Check outlier again 
        pairs(data[,1:10])
        # It seems no more outliers now.

        
#Check the temperature response curves of vegetation cover (VCI) in Tibet Plateau
        plot(data$VCI,data$TemSum2000.2020Mean)
        

        # When 0.42 < VCI < 0.55, vegetation cover is negatively related to temperature.
        # Here we got the decoupling region of vegetation to temperature variation
        abline(v=0.422,col="red",lty=2)
        abline(v=0.55,col="red",lty=2)

        
# For the area with vegetation negatively related to growing season temperature (0.42 < VCI < 0.55),
        # we then create a SEM model to analysis the mechanism of that.

# Prepare the sem.dat         
# Rename and create variables
        dat=data[data$VCI<0.55 & data$VCI>0.422,] 
        # Variables for the latent variable "Vegetation Cover"
        sem.dat<-  with(dat,data.frame(VCI))
        sem.dat$LAI<-  with(dat,LAI)

        # Variables for the latent variable "Wet"
        sem.dat$PreSum <- with(dat, PreSum2000.2020Mean*100)#From m to cm
        sem.dat$SnowCoverSum <- with(dat, SnowCoverSum2000.2020Mean/100) #From % to 0.01
        sem.dat$SnowDepthSum <- with(dat, SnowDepthSum2000.2020Mean*100) # From m to cm
        sem.dat$SnowWaterSum <- with(dat, SnowWaterSum2000.2020Mean*100) # From m to cm
        
        sem.dat$PreWin <- with(dat, PreWin2000.2020Mean*100)#From m to cm
        sem.dat$SnowCoverWin <- with(dat, SnowCoverWin2000.2020Mean/100) #From % to 0.01
        sem.dat$SnowDepthWin <- with(dat, SnowDepthWin2000.2020Mean*100) # m to cm
        sem.dat$SnowWaterWin <- with(dat, SnowWaterWin2000.2020Mean*100) # From m to cm
        
        #  Variables for the latent variable "Drought"
        sem.dat$SoilEvapSum<- with(dat, SoilEvapSum2000.2020Mean/8)#cm
        sem.dat$PlantTransSum<- with(dat, PlantTransSum2000.2020Mean/8)#cm
        
        sem.dat$SoilEvapWin<- with(dat, SoilEvapWin2000.2020Mean/8)#cm
        sem.dat$PlantTransWin<- with(dat, PlantTransWin2000.2020Mean/8)#cm
  
        # Variables for the latent variable "Warming"
        sem.dat$TemSum <-  with(dat,TemSum2000.2020Mean)
        sem.dat$TemSoilSum <-  with(dat,SoilTemSum2000.2020Mean)
        
        sem.dat$TemWin <-  with(dat,TemWin2000.2020Mean)
        sem.dat$TemSoilWin <-  with(dat,SoilTemWin2000.2020Mean)
        
        # Variables for the latent variable "Water avalibality"
        sem.dat$AmWin <-  with(dat,AmWin2000.2020Mean) #
        sem.dat$AmSum <-  with(dat,AmSum2000.2020Mean) #
        sem.dat$SoilWaterWin <-  with(dat,SoilWaterWin2000.2020Mean)
        sem.dat$SoilWaterSum <-  with(dat,SoilWaterSum2000.2020Mean) 



        
        pairs(sem.dat)
        str(sem.dat) 


# Look at relationships between each variable
print(cor(sem.dat))
corr=cor(sem.dat)
corrplot(corr = corr,type="upper",tl.pos="tp")
corrplot(corr = corr,add=TRUE, type="lower", method="number", col="black",diag=FALSE,tl.pos="n", cl.pos="n")



# # # # Setp 1: Creat the latent variable 'Wet' in the meta model(Fig. 3)
              
              wet.dat= sem.dat[,3:10]
              names(wet.dat)
              
              print(cor(wet.dat))    #Delete three variables that are less relevant. (SnowWaterSum, SnowDepthSum, and PreWin)
              
              wet<-'
                  wet =~ PreSum + SnowCoverSum  + SnowCoverWin + SnowDepthWin + SnowWaterWin ## + SnowWaterSum + SnowDepthSum + PreWin  
                  
                  SnowDepthWin ~~ SnowWaterWin
                  '
              
              wet.fit=sem(wet,data=wet.dat,check.gradient = FALSE)
              varTable(wet.fit)
              print(wet.fit)
              summary(wet.fit, rsq=T, standardized=T)
              fitmeasures(wet.fit,c('cfi','rmsea','srmr','gfi','AIC','BIC'))
              
              subset(modindices(wet.fit),mi>3.0)
              
              wet= 1.000*wet.dat$PreSum +
                   0.301*wet.dat$SnowCoverSum +
                   0.997*wet.dat$SnowCoverWin +
                   42.555*wet.dat$SnowDepthWin +
                   7.330*wet.dat$SnowWaterWin
              
              sem.dat$wet=wet
              

              
# # # # Setp 2: Creat the latent variable 'Drought' in the meta model(Fig. 3)
            drought.dat= sem.dat[,11:14]
            names(drought.dat)
                              
            print(cor(drought.dat))    
            
            drought<-'
                drought=~ SoilEvapSum+ PlantTransSum + SoilEvapWin +PlantTransWin
                
                SoilEvapSum ~~ PlantTransWin
                '
            drought.fit=cfa(drought,data=drought.dat,check.gradient=FALSE)
            print(drought.fit)
            summary(drought.fit, rsq=T, standardized=T)
            
            fitmeasures(drought.fit,c('cfi','rmsea','srmr','gfi','AIC','BIC'))
            
            subset(modindices(drought.fit),mi>3.0)
            
            drought=1.000*drought.dat$SoilEvapSum -
                    3.306*drought.dat$PlantTransSum -
                    0.631*drought.dat$SoilEvapWin -
                    0.073*drought.dat$PlantTransWin
            sem.dat$drought=drought
            
            
# # # # Setp 3:  Creat the latent variable 'Warming' in the meta model(Fig. 3)
          warming.dat= sem.dat[,15:18]
          names(warming.dat)
          
          print(cor(warming.dat))   
          
          warming<-'
              warming=~TemSum + TemWin+ TemSoilSum +TemSoilWin
              
              TemSum ~~ TemSoilSum
              #TemWin ~~ TemSoilSum
              '
          warming.fit=cfa(warming,data=warming.dat,check.gradient=FALSE)
          print(warming.fit)
          summary(warming.fit,rsq=T, standardized=T)
          fitmeasures(warming.fit,c('cfi','rmsea','srmr','gfi','AIC','BIC'))
          
          subset(modindices(warming.fit),mi>3.8)
          
          warming=1.000*warming.dat$TemSum +
                  1.466*warming.dat$TemWin +
                  1.106*warming.dat$TemSoilSum +
                  0.474*warming.dat$TemSoilWin
          sem.dat$warming=warming
          

# # # # Setp 4: Create the latent variable 'Water availability' in the meta model(Fig. 3)
          waterAvailability.dat= sem.dat[,19:22]
          head(waterAvailability.dat)
          pairs(waterAvailability.dat)
          
          print(cor(waterAvailability.dat))   
          
          
          waterAvailability<-
            'waterAvailability =~ AmWin+AmSum+SoilWaterWin+SoilWaterSum
            
             SoilWaterWin ~~ SoilWaterSum
            '
          waterAvailability.fit=cfa(waterAvailability,data=waterAvailability.dat)#,check.gradient=FALSE)
          varTable(waterAvailability.fit)
          print(waterAvailability.fit)
          summary(waterAvailability.fit,rsq=T, standardized=T)
          fitmeasures(waterAvailability.fit,c('cfi','rmsea','srmr','gfi','AIC','BIC'))
          
          subset(modindices(waterAvailability.fit),mi>3.8)
          
          waterAva=1.000*waterAvailability.dat$AmWin +
                   0.954*waterAvailability.dat$AmSum -
                   0.006*waterAvailability.dat$SoilWaterWin -
                   0.006*waterAvailability.dat$SoilWaterSum
          sem.dat$waterAva=waterAva


# # # #Setp 5: Check the vegetation cover indexes VCI and LAI (Fig. 3)
          vegetation.dat= sem.dat[,1:2]
          names(vegetation.dat)
          print(cor(vegetation.dat))    
          
          #Because VCI and LAI are highly correlated, we only used VCI to represent vegetation cover in the final SEM model.

# # # # Setp 6: Develop the structural equation mode using the latent variables wet, drought, warming, waterAva and vegetation cover (VCI).
# # # # The meta model was shown in Fig. 3.
names(sem.dat)
# wet, drought, warming, waterAva
#The meta model:
mod1 <- '
    VCI ~ warming + waterAva
    waterAva~ drought  + wet + warming
    drought~ wet + warming
    '       

mod1.fit <- sem(mod1, data=sem.dat,check.gradient = FALSE) 
varTable(mod1.fit)
#sem.dat$drought=sem.dat$drought/100  #add a scale factor to balance the variables
#sem.dat$VCI=sem.dat$VCI*10
#mod1.fit <- sem(mod1, data=sem.dat,check.gradient = FALSE) 

print(mod1.fit)
summary(mod1.fit, rsq=T, standardized=T)
fitmeasures(mod1.fit,c('cfi','rmsea','srmr','gfi','AIC','BIC'))

mi=modindices(mod1.fit);print(mi[mi$mi>3.0,])

# P-value (Chi-square) <0.05,so  adjust the meta model for the first time: add the linkage "VCI  ~~  drought"
mod2 <- '
    VCI ~ warming + waterAva
    waterAva~ drought  + wet + warming
    drought~ wet + warming
    
    VCI ~~  drought
    '                    

mod2.fit <- sem(mod2, data=sem.dat,check.gradient = FALSE) #estimator="MLM",
print(mod2.fit)
summary(mod2.fit, rsq=T, standardized=T)
fitmeasures(mod2.fit,c('cfi','rmsea','srmr','gfi','AIC','BIC'))

mi=modindices(mod2.fit);print(mi[mi$mi>3.0,])
standardizedSolution(mod2.fit)


#Update the SEM: use Bayesian method to evaluate the model
mod2.b.fit <- bsem(mod2, data=sem.dat,check.gradient = FALSE,
                    #auto.var=TRUE,auto.fix.first=TRUE, auto.cov.lv.x=TRUE,
                    #bcontrol = list(method='rjparallel'),
                    burnin = 5000,sample = 20000,n.chains = 3) 
print(mod2.b.fit)
summary(mod2.b.fit)#, rsq=T, standardized=T)
fitmeasures(mod2.b.fit)#,c('cfi','rmsea','srmr','gfi','AIC','BIC'))

mi=modindices(mod2.b.fit);print(mi[mi$mi>3.0,])
standardizedSolution(mod2.b.fit)
plot(mod2.b.fit)


