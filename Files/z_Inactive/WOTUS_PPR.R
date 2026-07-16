#Relations between Number of ducks at the flyway scale and number of duck hunting trips (days afield)
# Code developed by Wayne Thogmartin - USGS: UMESC

#Notes from discussions with Brady Mattsson, Jim Dubovsky, Jim Devries, Jay Diffendorfer, and Darius Semmens

#Objective: Relate number of number of ducks at flyway scale to number of duck hunting days
#Duck hunter days afield in 'WF corrected days afield > duckDaysAfield', by state and flyway, years 1999-2019 
#Duck abundance in 'Total duck BPOP > Total Duck BPOP', years 1955-2019
#simple Proposed Model 1: duck hunter days afield ~ duck abundance
#most complex Proposed Model 2: duck hunters ~ duck hunter days afield + duck harvest + duck abundance + 
#                        spatial effects ( [state] + flyway(MF, CF) ) + 
#                        temporal effects ( [year] + year ) 
# spatial and temporal effects possibly related to wetness on the prairies, 
#                                   i.e., May Palmer drought severity index,
#                                   i.e., index of PPR wetland availability
#not evaluated: Proposed Model 3: duck hunters/duck hunter days afield related to duck harvest/duck hunters


# Package install and data load ------------------------------------

x = c("tidyverse", "ggplot2", "readxl", "lme4", "sjPlot", "DHARMa", "MuMIn",
      "performance", "car", "stringi", "mgcv", "yarrr", "plotrix", 
      "scales", "ggpubr", "dplyr", "rebird", "beepr")   
lapply(x, library, character.only=TRUE)
rm(x)
# If you don't know if you've installed the packages for some of these 
# libraries, run this:
# install.packages(x)


setwd("C:/Users/wthogmartin/OneDrive - DOI/wet_migration/Pintail/WOTUS");getwd()

#run WOTUSdataUSPPRprepare.R if needing to update/correct data preparation
WOTUSdataUSPPR <- read.csv("WOTUSdataUSPPR.csv", header=TRUE)
WOTUSdata2<-WOTUSdataUSPPR[7:122,]

# Modeling Duck Hunter Days Afield----------------------------------------------------------------
#Models  global 〖Days Afield〗_t=α_0+α_1 〖×USBPOP〗_t+α_2×Flyway_t+α_3×Year_t+α_4 〖×Flyway_t×Year〗_t+ α_5 〖×Method_t×USBPOP〗_t+α_6 〖×Method_t×Year〗_t+ α_7 〖×Flyway_t×USBPOP〗_t+ α_8 〖×Flyway_t×Method〗_t+ α_9 〖×Year_t×USBPOP〗_t+ α_9×Method_t×Flyway_t×USBPOP_t+ε_t
Model1 <- lm(DaysAfield ~ USPPR, data=WOTUSdata2)
Model2 <- lm(DaysAfield ~ USPPR + Flyway, data=WOTUSdata2)
Model3 <- lm(DaysAfield ~ USPPR + Flyway + Year, data=WOTUSdata2)
Model4 <- lm(DaysAfield ~ USPPR + Flyway + Year*Flyway, data=WOTUSdata2)
Model5 <- lm(DaysAfield ~ USPPR*Flyway + Year*Flyway, data=WOTUSdata2)
Model6 <- lmer(DaysAfield ~ USPPR + Flyway + Year*Flyway + (1|Year), data=WOTUSdata2)
Model7 <- lmer(DaysAfield ~ USPPR*Flyway + Year*Flyway + (1|Year), data=WOTUSdata2)
Model8 <- lm(DaysAfield ~ USPPR*method + Year*method + Year*Flyway, data=WOTUSdata2) #this is model 464 in dredgeDHD
Model9 <- lm(DaysAfield ~ USPPR*method + Year*method*Flyway, data=WOTUSdata2)
Model8.scale <- lm(DaysAfield ~ scale(USPPR)*method + scale(Year)*method + scale(Year)*Flyway, data=WOTUSdata2)
Model9.scale <- lm(DaysAfield ~ scale(USPPR)*method + scale(Year)*method*Flyway, data=WOTUSdata2)
Model10 <- lmer(DaysAfield ~ USPPR*method + Year*Flyway*method + (1|Year), data=WOTUSdata2)
Model11 <- mgcv::gam(DaysAfield ~ USPPR*method + s(Year, bs="cr", by=method) + Year*Flyway*method, data=WOTUSdata2)
Model8.spline <- gam(DaysAfield ~ USPPR*method + s(Year, bs="cr", by=method) + Year*Flyway, data=WOTUSdata2)
Model8.0int <- lm(DaysAfield ~ 0 + USPPR*method + Year*method + Year*Flyway, data=WOTUSdata2)
globalDHD <- lm(DaysAfield ~ USPPR + Year + Flyway + method + USPPR*Year + USPPR*Flyway + USPPR*method + Flyway*Year + Flyway*method + Year*method + USPPR*Year*Flyway + Year*method*Flyway,
                na.action = "na.fail", data=WOTUSdata2)
dredgeDHD <- MuMIn::dredge(globalDHD) #run to ensure important interactions were not errantly excluded
subset(dredgeDHD, delta < 4)
par(mar = c(3,5,6,4))
plot(dredgeDHD, labAsExpr = TRUE)  #more complex models than Model8 were lower in AICc

#Diagnostics
#Diagnostic plots
plot(Model1) 
plot(Model2) 
plot(Model3) #wonky leverage plot 
plot(Model4) #wonky leverage plot
plot(Model5) #wonky leverage plot
plot(Model6) #wonky leverage plot
plot(Model7) 
plot(Model8)
plot(Model9) 
plot(Model10) 
plot(Model11) 
plot(globalDHD)

#Diagnostics DHARMa
Model1_simres <- DHARMa::simulateResiduals(Model1); plot(Model1_simres) #KS, dispersion, outlier all n.s.
Model2_simres <- DHARMa::simulateResiduals(Model2); plot(Model2_simres) #KS, dispersion, outlier all n.s.
Model3_simres <- DHARMa::simulateResiduals(Model3); plot(Model3_simres) #KS, dispersion, outlier all n.s.; severe shape to residuals
Model4_simres <- DHARMa::simulateResiduals(Model4); plot(Model4_simres) #KS, dispersion, outlier all n.s.; severe shape to residuals
Model5_simres <- DHARMa::simulateResiduals(Model5); plot(Model5_simres) #KS, dispersion, outlier all n.s.; severe shape to residuals
Model6_simres <- DHARMa::simulateResiduals(Model6); plot(Model6_simres) #KS, dispersion, outlier all n.s.; severe shape to residuals
Model7_simres <- DHARMa::simulateResiduals(Model7); plot(Model7_simres) #KS, dispersion, outlier all n.s.; severe shape to residuals
Model8_simres <- DHARMa::simulateResiduals(Model8); plot(Model8_simres) #KS, dispersion, outlier all n.s.; best looking
Model9_simres <- DHARMa::simulateResiduals(Model9); plot(Model9_simres) #KS, dispersion, outlier all n.s.; best looking
Model10_simres <- DHARMa::simulateResiduals(Model10); plot(Model10_simres) #KS, dispersion, outlier all n.s.; best looking
Model11_simres <- DHARMa::simulateResiduals(Model11); plot(Model11_simres) #KS, dispersion, outlier all n.s.
Model8.0int_simres <- DHARMa::simulateResiduals(Model8.0int); plot(Model8.0int_simres)
globalDHD_simres <- DHARMa::simulateResiduals(globalDHD); plot(globalDHD_simres)

#Influence
car::influencePlot(Model1)  
car::influencePlot(Model2)
car::influencePlot(Model3)  
car::influencePlot(Model4)
car::influencePlot(Model5)  
car::influencePlot(Model6)
car::influencePlot(Model7)  
car::influencePlot(Model8)
car::influencePlot(Model9)  
car::influencePlot(Model10)
#car::influencePlot(Model11)  #not estimable
car::influencePlot(globalDHD)

#Summary
summary(Model1) 
summary(Model2) 
summary(Model3)  
summary(Model4)
summary(Model5) 
summary(Model6) 
summary(Model7) 
summary(Model8)
summary(Model9) 
summary(Model10) 
summary(Model11) 
summary(globalDHD)

summary(Model8.scale)
confint(Model8.scale)

tab_model(Model8, digits=8)
tab_model(Model8.scale,
          CSS = list(
            css.depvarhead = 'color: red;',
            css.centeralign = 'text-align: left;', 
            css.firsttablecol = 'font-weight: bold;', 
            css.summary = 'color: blue;'
          ))
tab_model(Model8, Model9, file = "Table1.html")  #unscaled, whereas below continuous covariates are scaled
tab_model(Model8.scale, Model9.scale   
          ,
          CSS = list(
            css.depvarhead = 'color: red;',
            css.centeralign = 'text-align: left;', 
            css.firsttablecol = 'font-weight: bold;', 
            css.summary = 'color: blue;'
          ))    #, file = "table2.html"

tab_model(Model8, Model8.0int   
          ,
          CSS = list(
            css.depvarhead = 'color: red;',
            css.centeralign = 'text-align: left;', 
            css.firsttablecol = 'font-weight: bold;', 
            css.summary = 'color: blue;'
          ), file = "BestModelTable.html")    #, file = "table2.html"

sjPlot::tab_model(Model8, Model8.scale   
          ,
          CSS = list(
            css.depvarhead = 'color: red;',
            css.centeralign = 'text-align: left;', 
            css.firsttablecol = 'font-weight: bold;', 
            css.summary = 'color: blue;'
          )) 

#variable importance
caret::varImp(Model8, scale = FALSE)
caret::varImp(Model9, scale = FALSE)
caret::varImp(Model8, scale = TRUE)
caret::varImp(Model9, scale = TRUE)


#R2
performance::r2(Model1)
performance::r2(Model2)
performance::r2(Model3)
performance::r2(Model4)
performance::r2(Model5)
performance::r2(Model6)
performance::r2(Model7)
performance::r2(Model8)#; yhat::effect.size(Model8)
performance::r2(Model9)
performance::r2(Model10)
performance::r2(Model11)
performance::r2(Model8.0int)
performance::r2(globalDHD)


#AIC
AIC(Model1, Model2, Model3, Model4, Model5, Model6, Model7, 
    Model8, Model9, Model10, Model11, Model8.0int, globalDHD)
DHDmodout <- MuMIn::model.sel(Model1, Model2, Model3, Model4, Model5, Model6, Model7, 
                 Model8, Model9, Model10, Model11, globalDHD)
DHDmod.table<-as.data.frame(DHDmodout)
DHDmod.table
DHDmod.tab <- subset(DHDmod.table, select = -c(15,16) )
DHDmod.tab[,c(1:2,4,7:9,13,16:18)]<- round(DHDmod.tab[,c(1:2,4,7:9,13,16:18)],4)
DHDmod.tab[,19]<- round(DHDmod.tab[,19],3)
write.csv(DHDmod.tab,"DHD selection table.csv", row.names = T)
sw(DHDmodout)

AIC(Model8,Model8.0int)

#Best Model
summary(Model8)
theme_set(theme_sjplot())
colorscheme = yarrr::piratepal(palette = "google")

sjPlot::plot_model(Model8, type = "pred", terms = "USPPR", 
                   title = " ", 
                   axis.title = c("Total Duck Abundance","Duck Hunter Days Afield")) + 
  scale_x_continuous(limits = c(0, 16000000), labels = scales::comma, oob=scales::squish )

plot_model(Model8, type = "pred", terms = "Flyway", show.data = TRUE, jitter = 0.1)
ggeffects::ggpredict(Model8, terms = "Flyway")
ggeffects::ggpredict(Model8, terms = "Flyway", condition = c( "Year" = 2016))

plot_model(Model8, type = "pred", terms = c("USPPR", "Flyway", "Year [1970, 1990, 2010]"),
           line.size=1.15, colors = c("firebrick", "darkblue"), 
           title = "", axis.title = c("Total Duck Abundance (Million)","Duck Hunter Days Afield")) +
    theme(strip.text.x = element_text(size=12),
        strip.background = element_rect(colour="black", fill="white"),
        axis.text.x = element_text(face="bold")) +
    scale_x_continuous(labels = label_number(scale = 1e-6))

plot_model(Model8, type = "pred", terms = c("USPPR", "Year [1973, 1990, 2006]", "Flyway"), colors = "Dark2", title = "", axis.title = c("Total Duck Abundance","Duck Hunter Days Afield"))
plot_model(Model8, type = "pred", terms = c("Year", "Flyway"))
ggeffects::ggpredict(Model8,  
                     terms = c("USPPR", "Flyway", "Year [1973, 1990, 2006]", "method"), 
                     show.legend = FALSE,  
                     colors = c("firebrick", "darkblue"))
plot_model(Model8, type = "pred",  
           terms = c("USPPR", "Flyway", "Year [1973, 1990, 2006]", "method"), 
           show.legend = FALSE,  
           colors = c("firebrick", "darkblue"))
# ,
# title = "", 
# axis.title = c("Total Duck Abundance","Duck Hunter Days Afield"))

ggeffects::ggpredict(Model8, terms = c("Flyway", "Year [1970, 1980, 1990, 2000, 2010]", "method [1]")) #predicts for a constant method over time


#Sniff test
ggeffects::ggpredict(Model8, terms = c("Flyway", "method [1]"), condition = c( "Year" = 2016))
#Compared to p. 62 of USFWS (2016), which indicates
hdy <- 8962000
htr <- 1189000
htrhdy <- hdy/htr; htrhdy #7.537426; which is higher than both predictions



#if wetland availability should change, what happens to duck numbers?
#duck pairs ~ wetland availability
#wetland availability (ha)
wetlossx <- c(0,
               -58633,
               -161151,
               -297556,
               -368423)
#duck pairs
dpairsy <- c(0,
             -146622,
             -401688,
             -717306,
             -887696)
dpairswetlossdata <- data.frame(cbind(wetlossx, dpairsy))
plot(wetlossx, dpairsy, xlab="Hectares lost", ylab="Duck pairs lost")
dpairswetloss <- lm(dpairsy ~ 0 + wetlossx, data=dpairswetlossdata); dpairswetloss
abline(a=0, b=dpairswetloss$coefficients[1])

#relative to Baseline
basewetland <- 3062725
baseduckpairs <- 5682822

wetloss <- basewetland-abs(wetlossx)
duckpairloss <- baseduckpairs-abs(dpairsy)

wetlossscen <- data.frame(cbind(wetloss, duckpairloss))
wetlossscen$duckloss <- wetlossscen$duckpairloss*2 #turn duck pairs into ducks lost


#consequence of wetland loss and subsequent duck population loss on duck hunter days afield
WOTUSdataUSPPR[with(WOTUSdataUSPPR, Flyway == "MF" & Year == "2016"), ]

lossMF <- data.frame(Year = rep(2016, 5), USPPR = wetlossscen[,3], Flyway = rep("MF", 5), method = rep(1, 5) )
lossDHDMF <- predict.lm(Model8, lossMF, interval = "prediction")
lossCF <- data.frame(Year = rep(2016, 5), USPPR = wetlossscen[,3], Flyway = rep("CF", 5), method = rep(1, 5) )
lossDHDCF <- predict.lm(Model8, lossCF, interval = "prediction")


lossDHD <- data.frame(rbind(lossDHDMF, lossDHDCF))
lossDHD$Flyway <- c("MF", "MF", "MF", "MF", "MF", "CF", "CF", "CF", "CF", "CF")
row.names(lossDHD) <- 1:10
#predicted duck hunter days afield in 2016, by Flyway
ggeffects::ggpredict(Model8, terms = "Flyway", condition = c( "Year" = 2016))
dhd2016effect <- ggeffects::ggpredict(Model8, terms = "Flyway", condition = c( "Year" = 2016))
str(dhd2016effect)
dhd2016MF <- data.frame(fit=dhd2016effect$predicted[2], lwr=dhd2016effect$conf.low[2], upr=dhd2016effect$conf.high[2], Flyway="MF")
dhd2016CF <- data.frame(fit=dhd2016effect$predicted[1], lwr=dhd2016effect$conf.low[1], upr=dhd2016effect$conf.high[1], Flyway="CF")
dhd2016 <- rbind(dhd2016MF, dhd2016CF)
  

#combine current and loss scenario duck hunter days
fulldhd2016 <- data.frame(rbind(dhd2016, lossDHD))
fulldhd2016$Scenario <- c("Base", "Base", "Baseline", "Low", "Medium-low", "Medium-high", "High", "Baseline", "Low", "Medium-low", "Medium-high", "High")
fulldhd2016$ScenNum <- c(1, 7, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12)

#plot duck hunter days for baseline (2016) and three wetland loss scenarios

#from prediction from model
plotrix::plotCI(fulldhd2016$ScenNum, fulldhd2016$fit, ui=fulldhd2016$upr, li=fulldhd2016$lwr, ylim=c(0,10), pt.bg=par("bg"),pch=21, ylab="Duck Hunter Days Afield", xlab="Scenario")

#from model
options(scipen = 1)
plot_model(Model8, type = "pred", axis.lim=c(0,10),  
           terms = c("USPPR", "Flyway", "Year [2016]", "method [1]"), 
           #show.legend = FALSE,  
           colors = c("firebrick", "darkblue")) + 
           #colors = c(rgb(214, 235, 194, alpha = 255, maxColorValue = 255), rgb(153, 214, 245, alpha = 255, maxColorValue = 255))) +
           #colors = c("#d6ebc2", "#99d6f5")) +
  geom_vline(xintercept = wetlossscen$duckloss, lty=c(5, 4,4,4,4)) +  
  labs(title = "", x = "Prairie Pothole Region Duck Population Size", 
       y = "Duck Hunter Days Afield") + 
  scale_x_continuous(limits = c(9000000, 12000000), labels = scales::comma) + 
  annotate("text", x = wetlossscen$duckloss, y = 8, 
           label = c("Baseline", "Low", "Medium-low", "Medium-high", "High"))  +
  geom_vline(xintercept = 9179063, lty=1) +  #see line 315, WOTUSdataUSPPR[with(WOTUSdataUSPPR, Flyway == "MF" & Year == "2016"), ]
  annotate("text", x = 9179063, y = 2.5, label = c("2016 Duck")) +
  annotate("text", x = 9179063, y = 2.1, label = c("Population Estimate"))

Model8$coefficients[2] #0.0000002008994
Model8$coefficients[2]*1775392 #loss of 0.357 days afield for biggest expected loss in birds
tab_model(Model8, digits=8)  #very flat response, 
lmSupport::modelEffectSizes(Model8)
min(WOTUSdataUSPPR$USPPR) #2.3 million ducks
max(WOTUSdataUSPPR$USPPR) #15.6 million ducks



#Economics of waterfowl harvest
#how many duck hunter days afield for 2016?
CFdhd <- ggeffects::ggpredict(Model8, terms = "Flyway [CF]", condition = c( "Year" = 2016))
MFdhd <- ggeffects::ggpredict(Model8, terms = "Flyway [MF]", condition = c( "Year" = 2016))
CFdhd$predicted; CFdhd$conf.low; CFdhd$conf.high
MFdhd$predicted; MFdhd$conf.low; MFdhd$conf.high


##
#Expenditures: $546 was expenditures per duck hunter
#Duck Hunter Days Afield in USFWS Recreation Survey (2016): 7.54 days spent afield
expdhd <- 546/7.537426409
#thus, duck hunters spent $72.44 per day

#total expenditures is per day expenditure times total days afield
expdhd*CFdhd$predicted; expdhd*CFdhd$conf.low; expdhd*CFdhd$conf.high
expdhd*MFdhd$predicted; expdhd*MFdhd$conf.low; expdhd*MFdhd$conf.high #does it make sense that because MF hunters hunt more their cost per day is less?
# CFdhd$predicted/MFdhd$predicted  
exp2016MF <- cbind(expdhd*MFdhd$predicted, expdhd*MFdhd$conf.low, expdhd*MFdhd$conf.high)
exp2016CF <- cbind(expdhd*CFdhd$predicted, expdhd*CFdhd$conf.low, expdhd*CFdhd$conf.high)
exp2016 <- data.frame(rbind(exp2016MF, exp2016CF))
colnames(exp2016) <- c("fit", "lwr", "upr")
exp2016$Flyway <- c("MF", "CF")

#now calculate for loss scenarios
fitlossexp <- expdhd*lossDHD$fit
lwrlossexp <- expdhd*lossDHD$lwr
uprlossexp <- expdhd*lossDHD$upr
lossexp <- data.frame(cbind(fitlossexp, lwrlossexp, uprlossexp))
colnames(lossexp) <- c("fit", "lwr", "upr")
lossexp$Flyway <- c("MF", "MF", "MF", "MF", "MF", "CF", "CF", "CF", "CF", "CF")

#combine current and loss scenarios
fullexp2016 <- data.frame(rbind(exp2016, lossexp))
fullexp2016$Scenario <- c("Base", "Base", "Baseline", "Low", "Medium-Low", "Medium-High", "High", "Baseline", "Low", "Medium-Low", "Medium-High", "High")
fullexp2016$ScenNum <- c(1, 7, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12)

#plot expenses for baseline (2016) and 5 wetland loss scenarios
plotrix::plotCI(fullexp2016$ScenNum, fullexp2016$fit, ui=fullexp2016$upr, li=fullexp2016$lwr, ylim=c(0,800), pt.bg=par("bg"),pch=21, ylab="Hunting-related Expenses ($)", xlab="Scenario")



#Consumer Surplus: $58.66 for CF, $42.26 for MF, per day
csCF = 58.66; csMF = 42.26
csCF/csMF  #CF is 38.8% higher than MF

#total expenditures is per day expenditure times total days afield
csCF*CFdhd$predicted; csCF*CFdhd$conf.low; csCF*CFdhd$conf.high
csMF*MFdhd$predicted; csMF*MFdhd$conf.low; csMF*MFdhd$conf.high #does it make sense that because MF hunters hunt more their cost per day is less?
# CFdhd$predicted/MFdhd$predicted  
cs2016MF <- cbind(csMF*MFdhd$predicted, csMF*MFdhd$conf.low, csMF*MFdhd$conf.high)
cs2016CF <- cbind(csCF*CFdhd$predicted, csCF*CFdhd$conf.low, csCF*CFdhd$conf.high)
cs2016 <- data.frame(rbind(cs2016MF, cs2016CF))
colnames(cs2016) <- c("fit", "lwr", "upr")
cs2016$Flyway <- c("MF", "CF")

#now calculate for loss scenarios
fitlosscsMF <- csMF*lossDHD$fit[1:5]
lwrlosscsMF <- csMF*lossDHD$lwr[1:5]
uprlosscsMF <- csMF*lossDHD$upr[1:5]
fitlosscsCF <- csCF*lossDHD$fit[6:10]
lwrlosscsCF <- csCF*lossDHD$lwr[6:10]
uprlosscsCF <- csCF*lossDHD$upr[6:10]
losscsMF <- data.frame(cbind(fitlosscsMF, lwrlosscsMF, uprlosscsMF))
colnames(losscsMF) <- c("fit", "lwr", "upr")
losscsCF <- data.frame(cbind(fitlosscsCF, lwrlosscsCF, uprlosscsCF))
colnames(losscsCF) <- c("fit", "lwr", "upr")
losscs <- data.frame(rbind(losscsMF, losscsCF))
losscs$Flyway <- c("MF", "MF", "MF", "MF", "MF", "CF", "CF", "CF", "CF", "CF")

#combine current and loss scenarios
fullcs2016 <- data.frame(rbind(cs2016, losscs))
fullcs2016$Scenario <- c("Base", "Base", "Baseline", "Low", "Medium-Low", "Medium-High", "High", "Baseline", "Low", "Medium-Low", "Medium-High", "High")
fullcs2016$ScenNum <- c(1, 7, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12)

#plot expenses for baseline (2016) and three wetland loss scenarios
plotrix::plotCI(fullcs2016$ScenNum, fullcs2016$fit, ui=fullcs2016$upr, li=fullcs2016$lwr, ylim=c(0,700), pt.bg=par("bg"),pch=21, ylab="Consumer Surplus ($)", xlab="Scenario")




# ####gives a rough time series of EX and CS, assuming 2016 $ and  per day EX and CS hold for other years
WOTUSdataUSPPR
WOTUSdataUSPPR$CS    <- ifelse(WOTUSdataUSPPR$Flyway == "CF", WOTUSdataUSPPR$DaysAfield*csCF, WOTUSdataUSPPR$DaysAfield*csMF)
WOTUSdataUSPPR$EX    <- ifelse(WOTUSdataUSPPR$Flyway == "CF", WOTUSdataUSPPR$DaysAfield*expdhd, WOTUSdataUSPPR$DaysAfield*expdhd)  #where did 70.84 and 56.41 come from?
WOTUSdataUSPPR$CS    <- ifelse(WOTUSdataUSPPR$Year > 1998, WOTUSdataUSPPR$CS + Model9$coefficients[3], WOTUSdataUSPPR$CS)
WOTUSdataUSPPR$EX    <- ifelse(WOTUSdataUSPPR$Year > 1998, WOTUSdataUSPPR$EX + Model9$coefficients[3], WOTUSdataUSPPR$EX)


require(ggplot2)
WOTUSdataUSPPR %>% 
  drop_na() %>% 
  ggplot(aes(Year, CS, colour=Flyway)) + 
  geom_line() +
  geom_point() + 
  labs(y="$", x="Year") + 
  ylim(250,600) + xlim(1960,2020)

WOTUSdataUSPPR %>% 
  drop_na() %>% 
  ggplot(aes(Year, EX, colour=Flyway)) + 
  geom_line() +
  geom_point() + 
  labs(y="$", x="Year") + 
  ylim(250,800) + xlim(1960,2020)

longer_data <- WOTUSdataUSPPR %>% drop_na() %>% 
  pivot_longer(CS:EX, names_to = "Expenses", values_to = "Dollars")
print(longer_data)

# longer_data %>%
#   ggplot(aes(Year, Dollars, colour=Flyway, group=Expenses)) + 
#   geom_line() +
#   geom_point() + 
#   labs(y="$", x="Year") + 
#   ylim(250,550) + xlim(1960,2020)

longer_data %>%
  ggplot(aes(Year, Dollars, colour=Flyway, shape=Expenses, group=interaction(Expenses, Flyway))) + 
  geom_line(size=0.8) +
  geom_point(size=3) + 
  labs(y="$", x="Year") + 
  ylim(250,800) + xlim(1960,2020) + scale_color_manual(values=c("#E69F00", "#56B4E9"))


#translate to total number of hunters...
ahunt2016MF <- WOTUSdataUSPPR$ActiveHunters[WOTUSdataUSPPR$Year == 2016 & WOTUSdataUSPPR$Flyway == "MF"]
ahunt2016CF <- WOTUSdataUSPPR$ActiveHunters[WOTUSdataUSPPR$Year == 2016 & WOTUSdataUSPPR$Flyway == "CF"]


fullexp2016$totalfit <- ifelse(fullexp2016$Flyway == "CF", fullexp2016$fit*ahunt2016CF,fullexp2016$fit*ahunt2016MF)
fullcs2016$totalfit <- ifelse(fullcs2016$Flyway == "CF", fullcs2016$fit*ahunt2016CF,fullcs2016$fit*ahunt2016MF)
fullexp2016$totallwr <- ifelse(fullexp2016$Flyway == "CF", fullexp2016$lwr*ahunt2016CF,fullexp2016$lwr*ahunt2016MF)
fullcs2016$totallwr <- ifelse(fullcs2016$Flyway == "CF", fullcs2016$lwr*ahunt2016CF,fullcs2016$lwr*ahunt2016MF)
fullexp2016$totalupr <- ifelse(fullexp2016$Flyway == "CF", fullexp2016$upr*ahunt2016CF,fullexp2016$upr*ahunt2016MF)
fullcs2016$totalupr <- ifelse(fullcs2016$Flyway == "CF", fullcs2016$upr*ahunt2016CF,fullcs2016$upr*ahunt2016MF)

#MF exp losses                                                                                           
totalMFexpbaseloss <- fullexp2016$totalfit[3]-fullexp2016$totalfit[3] 
totalMFexplowloss <- fullexp2016$totalfit[3]-fullexp2016$totalfit[4] #low
totalMFexpmedloloss <- fullexp2016$totalfit[3]-fullexp2016$totalfit[5]
totalMFexpmedhiloss <- fullexp2016$totalfit[3]-fullexp2016$totalfit[6]
totalMFexphighloss <- fullexp2016$totalfit[3]-fullexp2016$totalfit[7] #high

#CF exp losses
totalCFexpbaseloss <- fullexp2016$totalfit[8]-fullexp2016$totalfit[8] 
totalCFexplowloss <- fullexp2016$totalfit[8]-fullexp2016$totalfit[9] #low
totalCFexpmedloloss <- fullexp2016$totalfit[8]-fullexp2016$totalfit[10]
totalCFexpmedhiloss <- fullexp2016$totalfit[8]-fullexp2016$totalfit[11]
totalCFexphighloss <- fullexp2016$totalfit[8]-fullexp2016$totalfit[12] #high

#MF exp losses, lwr
totallwrMFexpbaseloss <- fullexp2016$totallwr[3]-fullexp2016$totallwr[3] 
totallwrMFexplowloss <- fullexp2016$totallwr[3]-fullexp2016$totallwr[4] #low
totallwrMFexpmedloloss <- fullexp2016$totallwr[3]-fullexp2016$totallwr[5]
totallwrMFexpmedhiloss <- fullexp2016$totallwr[3]-fullexp2016$totallwr[6]
totallwrMFexphighloss <- fullexp2016$totallwr[3]-fullexp2016$totallwr[7] #high

#CF exp losses, lwr
totallwrCFexpbaseloss <- fullexp2016$totallwr[8]-fullexp2016$totallwr[8] 
totallwrCFexplowloss <- fullexp2016$totallwr[8]-fullexp2016$totallwr[9] #low
totallwrCFexpmedloloss <- fullexp2016$totallwr[8]-fullexp2016$totallwr[10]
totallwrCFexpmedhiloss <- fullexp2016$totallwr[8]-fullexp2016$totallwr[11]
totallwrCFexphighloss <- fullexp2016$totallwr[8]-fullexp2016$totallwr[12] #high

#MF exp losses, upr
totaluprMFexpbaseloss <- fullexp2016$totalupr[3]-fullexp2016$totalupr[3] 
totaluprMFexplowloss <- fullexp2016$totalupr[3]-fullexp2016$totalupr[4] #low
totaluprMFexpmedloloss <- fullexp2016$totalupr[3]-fullexp2016$totalupr[5]
totaluprMFexpmedhiloss <- fullexp2016$totalupr[3]-fullexp2016$totalupr[6]
totaluprMFexphighloss <- fullexp2016$totalupr[3]-fullexp2016$totalupr[7] #high

#CF exp losses, upr
totaluprCFexpbaseloss <- fullexp2016$totalupr[8]-fullexp2016$totalupr[8] 
totaluprCFexplowloss <- fullexp2016$totalupr[8]-fullexp2016$totalupr[9] #low
totaluprCFexpmedloloss <- fullexp2016$totalupr[8]-fullexp2016$totalupr[10]
totaluprCFexpmedhiloss <- fullexp2016$totalupr[8]-fullexp2016$totalupr[11]
totaluprCFexphighloss <- fullexp2016$totalupr[8]-fullexp2016$totalupr[12] #high

totalexplossDHD <- data.frame(cbind(
  c(totalMFexpbaseloss,totalMFexplowloss,totalMFexpmedloloss,totalMFexpmedhiloss,totalMFexphighloss, 
    totalCFexpbaseloss, totalCFexplowloss, totalCFexpmedloloss, totalCFexpmedhiloss, totalCFexphighloss),
  c(totallwrMFexpbaseloss, totallwrMFexplowloss, totallwrMFexpmedloloss, totallwrMFexpmedhiloss, totallwrMFexphighloss,
    totallwrCFexpbaseloss, totallwrCFexplowloss, totallwrCFexpmedloloss, totallwrCFexpmedhiloss, totallwrCFexphighloss),
  c(totaluprMFexpbaseloss, totaluprMFexplowloss, totaluprMFexpmedloloss, totaluprMFexpmedhiloss, totaluprMFexphighloss, 
    totaluprCFexpbaseloss, totaluprCFexplowloss, totaluprCFexpmedloloss, totaluprCFexpmedhiloss, totaluprCFexphighloss)))

colnames(totalexplossDHD) <- c("fit", "lwr", "upr")
totalexplossDHD$Flyway <- c("MF", "MF", "MF", "MF", "MF", "CF", "CF", "CF", "CF", "CF")
totalexplossDHD$Scenario <- c("Baseline", "Low", "Medium-Low", "Medium-High", "High", "Baseline", "Low", "Medium-Low", "Medium-High", "High")
sum(totalexplossDHD[5,1],totalexplossDHD[10,1])


#cs
#MF cs losses                                                                                           
totalMFcsbaseloss <- fullcs2016$totalfit[3]-fullcs2016$totalfit[3] 
totalMFcslowloss <- fullcs2016$totalfit[3]-fullcs2016$totalfit[4] #low
totalMFcsmedloloss <- fullcs2016$totalfit[3]-fullcs2016$totalfit[5]
totalMFcsmedhiloss <- fullcs2016$totalfit[3]-fullcs2016$totalfit[6]
totalMFcshighloss <- fullcs2016$totalfit[3]-fullcs2016$totalfit[7] #high

#CF cs losses
totalCFcsbaseloss <- fullcs2016$totalfit[8]-fullcs2016$totalfit[8] 
totalCFcslowloss <- fullcs2016$totalfit[8]-fullcs2016$totalfit[9] #low
totalCFcsmedloloss <- fullcs2016$totalfit[8]-fullcs2016$totalfit[10]
totalCFcsmedhiloss <- fullcs2016$totalfit[8]-fullcs2016$totalfit[11]
totalCFcshighloss <- fullcs2016$totalfit[8]-fullcs2016$totalfit[12] #high

#MF cs losses, lwr
totallwrMFcsbaseloss <- fullcs2016$totallwr[3]-fullcs2016$totallwr[3] 
totallwrMFcslowloss <- fullcs2016$totallwr[3]-fullcs2016$totallwr[4] #low
totallwrMFcsmedloloss <- fullcs2016$totallwr[3]-fullcs2016$totallwr[5]
totallwrMFcsmedhiloss <- fullcs2016$totallwr[3]-fullcs2016$totallwr[6]
totallwrMFcshighloss <- fullcs2016$totallwr[3]-fullcs2016$totallwr[7] #high

#CF cs losses, lwr
totallwrCFcsbaseloss <- fullcs2016$totallwr[8]-fullcs2016$totallwr[8] 
totallwrCFcslowloss <- fullcs2016$totallwr[8]-fullcs2016$totallwr[9] #low
totallwrCFcsmedloloss <- fullcs2016$totallwr[8]-fullcs2016$totallwr[10]
totallwrCFcsmedhiloss <- fullcs2016$totallwr[8]-fullcs2016$totallwr[11]
totallwrCFcshighloss <- fullcs2016$totallwr[8]-fullcs2016$totallwr[12] #high

#MF cs losses, upr
totaluprMFcsbaseloss <- fullcs2016$totalupr[3]-fullcs2016$totalupr[3] 
totaluprMFcslowloss <- fullcs2016$totalupr[3]-fullcs2016$totalupr[4] #low
totaluprMFcsmedloloss <- fullcs2016$totalupr[3]-fullcs2016$totalupr[5]
totaluprMFcsmedhiloss <- fullcs2016$totalupr[3]-fullcs2016$totalupr[6]
totaluprMFcshighloss <- fullcs2016$totalupr[3]-fullcs2016$totalupr[7] #high

#CF cs losses, upr
totaluprCFcsbaseloss <- fullcs2016$totalupr[8]-fullcs2016$totalupr[8] 
totaluprCFcslowloss <- fullcs2016$totalupr[8]-fullcs2016$totalupr[9] #low
totaluprCFcsmedloloss <- fullcs2016$totalupr[8]-fullcs2016$totalupr[10]
totaluprCFcsmedhiloss <- fullcs2016$totalupr[8]-fullcs2016$totalupr[11]
totaluprCFcshighloss <- fullcs2016$totalupr[8]-fullcs2016$totalupr[12] #high

totalcslossDHD <- data.frame(cbind(
  c(totalMFcsbaseloss,totalMFcslowloss,totalMFcsmedloloss,totalMFcsmedhiloss,totalMFcshighloss, 
    totalCFcsbaseloss, totalCFcslowloss, totalCFcsmedloloss, totalCFcsmedhiloss, totalCFcshighloss),
  c(totallwrMFcsbaseloss, totallwrMFcslowloss, totallwrMFcsmedloloss, totallwrMFcsmedhiloss, totallwrMFcshighloss,
    totallwrCFcsbaseloss, totallwrCFcslowloss, totallwrCFcsmedloloss, totallwrCFcsmedhiloss, totallwrCFcshighloss),
  c(totaluprMFcsbaseloss, totaluprMFcslowloss, totaluprMFcsmedloloss, totaluprMFcsmedhiloss, totaluprMFcshighloss, 
    totaluprCFcsbaseloss, totaluprCFcslowloss, totaluprCFcsmedloloss, totaluprCFcsmedhiloss, totaluprCFcshighloss)))

colnames(totalcslossDHD) <- c("fit", "lwr", "upr")
totalcslossDHD$Flyway <- c("MF", "MF", "MF", "MF", "MF", "CF", "CF", "CF", "CF", "CF")
totalcslossDHD$Scenario <- c("Baseline", "Low", "Medium-Low", "Medium-High", "High", "Baseline", "Low", "Medium-Low", "Medium-High", "High")
sum(totalcslossDHD[5,1],totalcslossDHD[10,1])



# Modeling Duck Hunter ----------------------------------------------------------------

MASS::truehist(WOTUSdataUSPPR$ActiveHunters)
MASS::truehist(log(WOTUSdataUSPPR$ActiveHunters), col="lightgreen", xlab="log(Active Number of Hunters)")

#Models
ModelA <- lm(log(ActiveHunters) ~ USPPR, data=WOTUSdata2)
ModelB <- lm(log(ActiveHunters) ~ USPPR + Flyway, data=WOTUSdata2)
ModelC <- lm(log(ActiveHunters) ~ USPPR + Flyway + Year, data=WOTUSdata2)
#ModelD <- lm(log(ActiveHunters) ~ USPPR + Flyway + Year*Flyway, data=WOTUSdata2)
ModelE <- lm(log(ActiveHunters) ~ USPPR*Flyway + Year*Flyway, data=WOTUSdata2)
ModelF <- lmer(log(ActiveHunters) ~ USPPR + Flyway + Year*Flyway + (1|Year), data=WOTUSdata2)
ModelG <- lmer(log(ActiveHunters) ~ USPPR*Flyway + Year*Flyway + (1|Year), data=WOTUSdata2)
#ModelH <- lm(log(ActiveHunters) ~ USPPR + Year + Year*Flyway, data=WOTUSdata2)
ModelI <- lm(log(ActiveHunters) ~ USPPR + Year*Flyway, data=WOTUSdata2)
ModelH.scale <- lm(log(ActiveHunters) ~ scale(USPPR) + scale(Year) + scale(Year)*Flyway, data=WOTUSdata2)
ModelI.scale <- lm(log(ActiveHunters) ~ scale(USPPR) + scale(Year)*Flyway, data=WOTUSdata2)
#ModelJ <- lmer(log(ActiveHunters) ~ USPPR + Year*Flyway + (1|Year), data=WOTUSdata2)
Model40 <- lm(log(ActiveHunters) ~ Year*Flyway + USPPR*Year, data=WOTUSdata2) #exploration; Best model from dredge, but not as good as ModelK
Model40s <- mgcv::gam(log(ActiveHunters) ~ s(Year, bs="cr") + Year*Flyway + USPPR*Year, data=WOTUSdata2) #exploration; Best model from dredge, but with spline on year, not as good as ModelK
ModelK <- mgcv::gam(log(ActiveHunters) ~ USPPR + s(Year, bs="cr") + Year*Flyway, data=WOTUSdata2)
ModelK.scale <- gam(log(ActiveHunters) ~ scale(USPPR) + s(scale(Year), bs="cr") + scale(Year)*Flyway, data=WOTUSdata2)
globalDH <- lm(log(ActiveHunters) ~ USPPR + Year + Flyway + USPPR*Year + USPPR*Flyway + Flyway*Year + USPPR*Year*Flyway,
                na.action = "na.fail", data=WOTUSdata2)
dredgeDH <- MuMIn::dredge(globalDH) #run to ensure important interactions were not errantly excluded
subset(dredgeDH, delta < 4)
par(mar = c(3,5,6,4))
plot(dredgeDH, labAsExpr = TRUE)

#because getting prediction intervals from GAMs is damn near impossible
library(splines)
ModelK.spline <- glm(log(ActiveHunters) ~ USPPR + ns(Year, 4) + Year*Flyway, data=WOTUSdata2)

#Diagnostics

check_distribution(ModelK)
plot(check_distribution(ModelK))
check_heteroscedasticity(ModelK)
check_distribution(ModelK.spline)
plot(check_distribution(ModelK.spline))
check_heteroscedasticity(ModelK.spline)

#Diagnostic plots
plot(ModelA) #QQ plot not like we'd want it
plot(ModelB) #QQ plot not like we'd want it 
plot(ModelC) #QQ plot and scale-location not like we'd want it 
plot(ModelD) #"
plot(ModelE) #"
plot(ModelF)
plot(ModelG) 
plot(ModelH) #QQ plot and scale-location not like we'd want it
plot(ModelI) 
plot(ModelJ) 
plot(ModelK) 
#plot(ModelK.spline)


#Diagnostics DHARMa
ModelA_simres <- DHARMa::simulateResiduals(ModelA); plot(ModelA_simres) #KS, dispersion, outlier all n.s.; residuals look good
ModelB_simres <- DHARMa::simulateResiduals(ModelB); plot(ModelB_simres) #KS, dispersion, outlier all n.s.; residual deviations significant
ModelC_simres <- DHARMa::simulateResiduals(ModelC); plot(ModelC_simres) #severe shape to residuals, probably relates to Year
ModelD_simres <- DHARMa::simulateResiduals(ModelD); plot(ModelD_simres) #severe shape to residuals
ModelE_simres <- DHARMa::simulateResiduals(ModelE); plot(ModelE_simres) #severe shape to residuals
ModelF_simres <- DHARMa::simulateResiduals(ModelF); plot(ModelF_simres) #severe shape to residuals
ModelG_simres <- DHARMa::simulateResiduals(ModelG); plot(ModelG_simres) #severe shape to residuals
ModelH_simres <- DHARMa::simulateResiduals(ModelH); plot(ModelH_simres) 
ModelI_simres <- DHARMa::simulateResiduals(ModelI); plot(ModelI_simres)
ModelJ_simres <- DHARMa::simulateResiduals(ModelJ); plot(ModelJ_simres) 
ModelK_simres <- DHARMa::simulateResiduals(ModelK); plot(ModelK_simres) #KS, dispersion, outlier all n.s.; residual deviations occurring
#ModelKspline_simres <- DHARMa::simulateResiduals(ModelK.spline); plot(ModelKspline_simres) #KS, dispersion; residual deviations occurring

#ModelK


#Influence
car::influencePlot(ModelA)  
car::influencePlot(ModelB)
car::influencePlot(ModelC)  
car::influencePlot(ModelD)
car::influencePlot(ModelE)  
car::influencePlot(ModelF)
car::influencePlot(ModelG)  
car::influencePlot(ModelH)
car::influencePlot(ModelI)  
car::influencePlot(ModelJ)


#Summary
summary(ModelA) 
summary(ModelB) 
summary(ModelC)  
summary(ModelD)
summary(ModelE) 
summary(ModelF) 
summary(ModelG) 
summary(ModelH)
summary(ModelI) 
summary(ModelJ) 
summary(ModelK)   #best model
summary(ModelK.scale)
#summary(ModelK.spline)


tab_model(ModelK, digits=8)
tab_model(ModelK,ModelK.scale,
          CSS = list(
            css.depvarhead = 'color: red;',
            css.centeralign = 'text-align: left;', 
            css.firsttablecol = 'font-weight: bold;', 
            css.summary = 'color: blue;'
          ), file = "BestDHModelTable.html")



#R2
performance::r2(ModelA)
performance::r2(ModelB)
performance::r2(ModelC)
performance::r2(ModelD)
performance::r2(ModelE)
performance::r2(ModelF)
performance::r2(ModelG)
performance::r2(ModelH)#; yhat::effect.size(Model8)
performance::r2(ModelI)
performance::r2(ModelJ)
performance::r2(ModelK)
#performance::r2(ModelK.spline) ##!!!
performance::r2(ModelK.scale)


#AIC
AIC(ModelA, ModelB, ModelC, ModelD, ModelE, ModelF, ModelG, 
    ModelH, ModelI, ModelJ, ModelK, ModelK.spline)
DHmodout <- MuMIn::model.sel(ModelA, ModelB, ModelC, ModelD, ModelE, ModelF, ModelG, 
                 ModelH, ModelI, ModelJ, ModelK, ModelK.spline)
DHmod.table<-as.data.frame(DHmodout)
DHmod.table
DHmod.tab <- subset(DHmod.table, select = -c(9,10) )
DHmod.tab[,c(1:2,4,10:12)]<- round(DHmod.tab[,c(1:2,4,10:12)],4)
DHmod.tab[,13]<- round(DHmod.tab[,13],3)
write.csv(DHmod.tab,"DH selection table.csv", row.names = T)
importance(DHmodout)

#Best Model
summary(ModelK)
theme_set(theme_sjplot())
colorscheme = yarrr::piratepal(palette = "google")

plot_model(ModelK, type = "pred", terms = "USPPR", title = " ", axis.title = c("Total Duck Abundance","Duck Hunters")) + 
  scale_x_continuous(limits = c(0, 16000000), labels = scales::comma, oob=scales::squish ) +
  scale_y_continuous(limits = c(0, 400000), labels = scales::comma, oob=scales::squish )

plot_model(ModelK.spline, type = "pred", terms = "USPPR", title = " ", axis.title = c("Total Duck Abundance","Duck Hunters")) + 
  scale_x_continuous(limits = c(0, 16000000), labels = scales::comma, oob=scales::squish ) +
  scale_y_continuous(limits = c(0, 400000), labels = scales::comma, oob=scales::squish )
  
  
plot_model(ModelK, type = "pred", terms = "Flyway", show.data = TRUE, jitter = 0.1)
ggeffects::ggpredict(ModelK, terms = "Flyway")
ggeffects::ggpredict(ModelK, terms = "Flyway", condition = c( "Year" = 2016))

plot_model(ModelK, type = "pred", terms = c("USPPR", "Flyway", "Year [1973, 1990, 2006]"), colors = c("firebrick", "darkblue"), title = "", axis.title = c("Total Duck Abundance","Duck Hunter Days Afield"))

plot_model(ModelK, type = "pred", terms = c("USPPR", "Flyway", "Year [1970, 1990, 2010]"),
           line.size=1.15, colors = c("firebrick", "darkblue"), 
           title = "", axis.title = c("Total Duck Abundance (Million)","Number of Active Duck Hunters (thousands)")) +
  theme(strip.text.x = element_text(size=12),
        strip.background = element_rect(colour="black", fill="white"),
        axis.text.x = element_text(face="bold")) +
  scale_y_continuous(labels = label_number(scale = 1e-3)) +
  scale_x_continuous(labels = label_number(scale = 1e-6))






plot_model(ModelK, type = "pred", terms = c("USPPR", "Year [1973, 1990, 2006]", "Flyway"), colors = "Dark2", title = "", axis.title = c("Total Duck Abundance","Duck Hunter Days Afield"))
plot_model(ModelK, type = "pred", terms = c("Year", "Flyway"))
ggeffects::ggpredict(ModelK,  
                     terms = c("USPPR", "Flyway", "Year [1973, 1990, 2006]"), 
                     show.legend = FALSE,  
                     colors = c("firebrick", "darkblue"))
plot_model(ModelK, type = "pred",  
           terms = c("USPPR", "Flyway", "Year [1973, 1990, 2006]"), 
           show.legend = FALSE,  
           colors = c("firebrick", "darkblue"))

ggeffects::ggpredict(ModelK, terms = c("Flyway", "Year [1970, 1980, 1990, 2000, 2010]"))


###Done above for duck hunter days, above
#if wetland availability should change, what happens to duck numbers?
#duck pairs ~ wetland availability
#wetland availability (ha)
# wetlossx <- c(0,
#               -58633,
#               -161151,
#               -297556,
#               -368423)
# #duck pairs
# dpairsy <- c(0,
#              -146622,
#              -401688,
#              -717306,
#              -887696)
# dpairswetlossdata <- data.frame(cbind(wetlossx, dpairsy))
# plot(wetlossx, dpairsy, xlab="Hectares lost", ylab="Duck pairs lost")
# dpairswetloss <- lm(dpairsy ~ 0 + wetlossx, data=dpairswetlossdata); dpairswetloss
# abline(a=0, b=dpairswetloss$coefficients[1])
# 
# #relative to Baseline
# basewetland <- 3062725
# baseduckpairs <- 5682822
# 
# wetloss <- basewetland-abs(wetlossx)
# duckpairloss <- baseduckpairs-abs(dpairsy)
# 
# wetlossscen <- data.frame(cbind(wetloss, duckpairloss))
# wetlossscen$duckloss <- wetlossscen$duckpairloss*2 #turn duck pairs into ducks lost


#consequence of wetland loss and subsequent duck population loss on duck hunters
WOTUSdataUSPPR[with(WOTUSdataUSPPR, Flyway == "MF" & Year == "2016"), ]

lossMFhunter <- data.frame(Year = rep(2016, 5), USPPR = wetlossscen[,3], Flyway = rep("MF", 5))
lossDHMF <- predict.lm(ModelK.spline, newdata=lossMFhunter, interval = "prediction")

lossCFhunter <- data.frame(Year = rep(2016, 5), USPPR = wetlossscen[,3], Flyway = rep("CF", 5))
lossDHCF <- predict.lm(ModelK.spline, lossCFhunter, interval = "prediction")

#####CAN'T FIGURE OUT HOW TO FIT PREDICTION INTERVAL TO GAM; see ggpredict
lossDH <- data.frame(rbind(lossDHMF, lossDHCF))
colnames(lossDH) <- c("logfit", "loglwr", "logupr")
lossDH$Flyway <- c("MF", "MF", "MF", "MF", "MF", "CF", "CF", "CF", "CF", "CF")
row.names(lossDH) <- 1:10
lossDH$fit <- exp(lossDH$logfit)
lossDH$lwr <- exp(lossDH$loglwr)
lossDH$upr <- exp(lossDH$logupr)

#re-order
lossDH <- subset(lossDH, select=c(fit, lwr, upr, Flyway, logfit, loglwr, logupr))

#predicted duck hunters in 2016, by Flyway
dh2016effect <- ggeffects::ggpredict(ModelK, terms = "Flyway", condition = c( "Year" = 2016))
dh2016MF <- data.frame(fit=dh2016effect$predicted[2], lwr=dh2016effect$conf.low[2], upr=dh2016effect$conf.high[2], Flyway="MF")
dh2016CF <- data.frame(fit=dh2016effect$predicted[1], lwr=dh2016effect$conf.low[1], upr=dh2016effect$conf.high[1], Flyway="CF")
dh2016 <- rbind(dh2016MF, dh2016CF)

#combine current and loss scenario duck hunter days
fulldh2016 <- data.frame(rbind(dh2016, lossDH[,1:4]))
fulldh2016$Scenario <- c("Base", "Base", "Baseline", "Low", "Medium-low", "Medium-high", "High", "Baseline", "Low", "Medium-low", "Medium-high", "High")
fulldh2016$ScenNum <- c(1, 7, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12)


(fulldh2016[3,1]-fulldh2016[7,1])+
(fulldh2016[8,1]-fulldh2016[12,1])

#plot duck hunter days for baseline (2016) and three wetland loss scenarios

#from prediction from model
plotrix::plotCI(fulldh2016$ScenNum, fulldh2016$fit, ui=fulldh2016$upr, li=fulldh2016$lwr, ylim=c(0,600000), pt.bg=par("bg"),pch=21, ylab="Duck Hunters", xlab="Scenario")

#from model
options(scipen = 1)
plot_model(ModelK, type = "pred", axis.lim=c(0,600000),  
           terms = c("USPPR", "Flyway", "Year [2016]"), 
           #show.legend = FALSE,  
           colors = c("firebrick", "darkblue")) + 
  #colors = c(rgb(214, 235, 194, alpha = 255, maxColorValue = 255), rgb(153, 214, 245, alpha = 255, maxColorValue = 255))) +
  #colors = c("#d6ebc2", "#99d6f5")) +
  geom_vline(xintercept = wetlossscen$duckloss, lty=c(5, 4,4,4,4)) +  
  labs(title = "", x = "Prairie Pothole Region Duck Population Size", 
       y = "Duck Hunters") + 
  scale_x_continuous(limits = c(9000000, 12000000), labels = scales::comma) + 
  annotate("text", x = wetlossscen$duckloss, y = 8, 
           label = c("Baseline", "Low", "Medium-low", "Medium-high", "High"))  +
  geom_vline(xintercept = 9179063, lty=1) +  #see line 315, WOTUSdataUSPPR[with(WOTUSdataUSPPR, Flyway == "MF" & Year == "2016"), ]
  annotate("text", x = 9179063, y = 50000, label = c("2016 Duck")) +
  annotate("text", x = 9179063, y = 22000, label = c("Population Estimate"))

tab_model(ModelK, digits=16)  #very flat response, 
min(WOTUSdataUSPPR$USPPR) #2.3 million ducks
max(WOTUSdataUSPPR$USPPR) #15.6 million ducks




#where will that wetland and duck loss be felt?
#total ducks in fall flight ~ duck pairs + surviving ducklings
#total ducks in fall flight by flyway ~ banding results (67.92% in MF, 22.45% in CF)
#if we assume 67.92% of 1775392 would have gone to the MF and 22.45% to the CF, then 

dpairswetavaildata$duckslost <- dpairswetavaildata$dpairsy*2 #turn duck pairs into ducks lost


#Economics of waterfowl harvest

#Expenditures: $546 was expenditures per duck hunter
#Duck Hunter Days Afield in USFWS Recreation Survey (2016): 7.54 days spent afield
expdh <- 546/7.537426409
#thus, duck hunters spent $72.44 per day

#Consumer Surplus: $58.66 for CF, $42.26 for MF
csCF = 58.66; csMF = 42.26
csCF/csMF  #CF is 38.8% higher than MF


#total expenditures is per day expenditure times active hunter times number of days afield
fulldh2016$totalexp <- ifelse(fulldh2016$Flyway == "CF", fulldh2016$fit*expdh*CFdhd$predicted,fulldh2016$fit*expdh*MFdhd$predicted)
fulldh2016$lwrexp <- ifelse(fulldh2016$Flyway == "CF", fulldh2016$lwr*expdh*CFdhd$predicted,fulldh2016$lwr*expdh*MFdhd$predicted)
fulldh2016$uprexp <- ifelse(fulldh2016$Flyway == "CF", fulldh2016$upr*expdh*CFdhd$predicted,fulldh2016$upr*expdh*MFdhd$predicted)
fulldh2016$totalcs <- ifelse(fulldh2016$Flyway == "CF", fulldh2016$fit*csCF*CFdhd$predicted,fulldh2016$fit*csMF*MFdhd$predicted)
fulldh2016$lwrcs <- ifelse(fulldh2016$Flyway == "CF", fulldh2016$lwr*csCF*CFdhd$predicted,fulldh2016$lwr*csMF*MFdhd$predicted)
fulldh2016$uprcs <- ifelse(fulldh2016$Flyway == "CF", fulldh2016$upr*csCF*CFdhd$predicted,fulldh2016$upr*csMF*MFdhd$predicted)



#plot expenses for baseline (2016) and 5 wetland loss scenarios
plotrix::plotCI(fulldh2016$ScenNum, fulldh2016$totalexp, ui=fulldh2016$uprexp, li=fulldh2016$lwrexp, 
                ylim=c(0,380000000), pt.bg=par("bg"),pch=21, 
                ylab="Hunting-related Expenses ($)", 
                xlab="Scenario")

#plot consumer surplus for baseline (2016) and 5 wetland loss scenarios
plotrix::plotCI(fulldh2016$ScenNum, fulldh2016$totalcs, ui=fulldh2016$uprcs, li=fulldh2016$lwrcs, 
                ylim=c(0,350000000), pt.bg=par("bg"),pch=21, 
                ylab="Hunting-related Expenses ($)", 
                xlab="Scenario")



#MF exp losses                                                                                           
totaldhMFexpbaseloss <- fulldh2016$totalexp[3]-fulldh2016$totalexp[3] 
totaldhMFexplowloss <- fulldh2016$totalexp[3]-fulldh2016$totalexp[4] #low
totaldhMFexpmedloloss <- fulldh2016$totalexp[3]-fulldh2016$totalexp[5]
totaldhMFexpmedhiloss <- fulldh2016$totalexp[3]-fulldh2016$totalexp[6]
totaldhMFexphighloss <- fulldh2016$totalexp[3]-fulldh2016$totalexp[7] #high

#CF exp losses
totaldhCFexpbaseloss <- fulldh2016$totalexp[8]-fulldh2016$totalexp[8] 
totaldhCFexplowloss <- fulldh2016$totalexp[8]-fulldh2016$totalexp[9] #low
totaldhCFexpmedloloss <- fulldh2016$totalexp[8]-fulldh2016$totalexp[10]
totaldhCFexpmedhiloss <- fulldh2016$totalexp[8]-fulldh2016$totalexp[11]
totaldhCFexphighloss <- fulldh2016$totalexp[8]-fulldh2016$totalexp[12] #high

#MF losses, lwr
totallwrMFexpbaseloss <- fulldh2016$lwrexp[3]-fulldh2016$lwrexp[3] 
totallwrMFexplowloss <- fulldh2016$lwrexp[3]-fulldh2016$lwrexp[4] #low
totallwrMFexpmedloloss <- fulldh2016$lwrexp[3]-fulldh2016$lwrexp[5]
totallwrMFexpmedhiloss <- fulldh2016$lwrexp[3]-fulldh2016$lwrexp[6]
totallwrMFexphighloss <- fulldh2016$lwrexp[3]-fulldh2016$lwrexp[7] #high

#CF losses, lwr
totallwrCFexpbaseloss <- fulldh2016$lwrexp[8]-fulldh2016$lwrexp[8] 
totallwrCFexplowloss <- fulldh2016$lwrexp[8]-fulldh2016$lwrexp[9] #low
totallwrCFexpmedloloss <- fulldh2016$lwrexp[8]-fulldh2016$lwrexp[10]
totallwrCFexpmedhiloss <- fulldh2016$lwrexp[8]-fulldh2016$lwrexp[11]
totallwrCFexphighloss <- fulldh2016$lwrexp[8]-fulldh2016$lwrexp[12] #high

#MF losses, upr
totaluprMFexpbaseloss <- fulldh2016$uprexp[3]-fulldh2016$uprexp[3] 
totaluprMFexplowloss <- fulldh2016$uprexp[3]-fulldh2016$uprexp[4] #low
totaluprMFexpmedloloss <- fulldh2016$uprexp[3]-fulldh2016$uprexp[5]
totaluprMFexpmedhiloss <- fulldh2016$uprexp[3]-fulldh2016$uprexp[6]
totaluprMFexphighloss <- fulldh2016$uprexp[3]-fulldh2016$uprexp[7] #high

#CF losses, upr
totaluprCFexpbaseloss <- fulldh2016$uprexp[8]-fulldh2016$uprexp[8] 
totaluprCFexplowloss <- fulldh2016$uprexp[8]-fulldh2016$uprexp[9] #low
totaluprCFexpmedloloss <- fulldh2016$uprexp[8]-fulldh2016$uprexp[10]
totaluprCFexpmedhiloss <- fulldh2016$uprexp[8]-fulldh2016$uprexp[11]
totaluprCFexphighloss <- fulldh2016$uprexp[8]-fulldh2016$uprexp[12] #high



totalexplossDH <- data.frame(cbind(
  c(totaldhMFexpbaseloss, totaldhMFexplowloss, totaldhMFexpmedloloss, totaldhMFexpmedhiloss, totaldhMFexphighloss,
    totaldhCFexpbaseloss, totaldhCFexplowloss, totaldhCFexpmedloloss, totaldhCFexpmedhiloss, totaldhCFexphighloss),
  c(totallwrMFexpbaseloss, totallwrMFexplowloss, totallwrMFexpmedloloss, totallwrMFexpmedhiloss, totallwrMFexphighloss, 
    totallwrCFexpbaseloss, totallwrCFexplowloss, totallwrCFexpmedloloss, totallwrCFexpmedhiloss, totallwrCFexphighloss),
  c(totaluprMFexpbaseloss, totaluprMFexplowloss, totaluprMFexpmedloloss, totaluprMFexpmedhiloss, totaluprMFexphighloss, 
    totaluprCFexpbaseloss, totaluprCFexplowloss, totaluprCFexpmedloloss, totaluprCFexpmedhiloss, totaluprCFexphighloss) ))

colnames(totalexplossDH) <- c("fit", "lwr", "upr")
totalexplossDH$Flyway <- c("MF", "MF", "MF", "MF", "MF", "CF", "CF", "CF", "CF", "CF")
totalexplossDH$Scenario <- c("Baseline", "Low", "Medium-Low", "Medium-High", "High", "Baseline", "Low", "Medium-Low", "Medium-High", "High")
sum(totalexplossDH[5,1]+totalexplossDH[10,1])

##
#MF cs losses                                                                                           
totaldhMFcsbaseloss <- fulldh2016$totalcs[3]-fulldh2016$totalcs[3] 
totaldhMFcslowloss <- fulldh2016$totalcs[3]-fulldh2016$totalcs[4] #low
totaldhMFcsmedloloss <- fulldh2016$totalcs[3]-fulldh2016$totalcs[5]
totaldhMFcsmedhiloss <- fulldh2016$totalcs[3]-fulldh2016$totalcs[6]
totaldhMFcshighloss <- fulldh2016$totalcs[3]-fulldh2016$totalcs[7] #high

#CF cs losses
totaldhCFcsbaseloss <- fulldh2016$totalcs[8]-fulldh2016$totalcs[8] 
totaldhCFcslowloss <- fulldh2016$totalcs[8]-fulldh2016$totalcs[9] #low
totaldhCFcsmedloloss <- fulldh2016$totalcs[8]-fulldh2016$totalcs[10]
totaldhCFcsmedhiloss <- fulldh2016$totalcs[8]-fulldh2016$totalcs[11]
totaldhCFcshighloss <- fulldh2016$totalcs[8]-fulldh2016$totalcs[12] #high

#MF losses, lwr
totallwrMFcsbaseloss <- fulldh2016$lwrcs[3]-fulldh2016$lwrcs[3] 
totallwrMFcslowloss <- fulldh2016$lwrcs[3]-fulldh2016$lwrcs[4] #low
totallwrMFcsmedloloss <- fulldh2016$lwrcs[3]-fulldh2016$lwrcs[5]
totallwrMFcsmedhiloss <- fulldh2016$lwrcs[3]-fulldh2016$lwrcs[6]
totallwrMFcshighloss <- fulldh2016$lwrcs[3]-fulldh2016$lwrcs[7] #high

#CF losses, lwr
totallwrCFcsbaseloss <- fulldh2016$lwrcs[8]-fulldh2016$lwrcs[8] 
totallwrCFcslowloss <- fulldh2016$lwrcs[8]-fulldh2016$lwrcs[9] #low
totallwrCFcsmedloloss <- fulldh2016$lwrcs[8]-fulldh2016$lwrcs[10]
totallwrCFcsmedhiloss <- fulldh2016$lwrcs[8]-fulldh2016$lwrcs[11]
totallwrCFcshighloss <- fulldh2016$lwrcs[8]-fulldh2016$lwrcs[12] #high

#MF losses, upr
totaluprMFcsbaseloss <- fulldh2016$uprcs[3]-fulldh2016$uprcs[3] 
totaluprMFcslowloss <- fulldh2016$uprcs[3]-fulldh2016$uprcs[4] #low
totaluprMFcsmedloloss <- fulldh2016$uprcs[3]-fulldh2016$uprcs[5]
totaluprMFcsmedhiloss <- fulldh2016$uprcs[3]-fulldh2016$uprcs[6]
totaluprMFcshighloss <- fulldh2016$uprcs[3]-fulldh2016$uprcs[7] #high

#CF losses, upr
totaluprCFcsbaseloss <- fulldh2016$uprcs[8]-fulldh2016$uprcs[8] 
totaluprCFcslowloss <- fulldh2016$uprcs[8]-fulldh2016$uprcs[9] #low
totaluprCFcsmedloloss <- fulldh2016$uprcs[8]-fulldh2016$uprcs[10]
totaluprCFcsmedhiloss <- fulldh2016$uprcs[8]-fulldh2016$uprcs[11]
totaluprCFcshighloss <- fulldh2016$uprcs[8]-fulldh2016$uprcs[12] #high



totalcslossDH <- data.frame(cbind(
  c(totaldhMFcsbaseloss, totaldhMFcslowloss, totaldhMFcsmedloloss, totaldhMFcsmedhiloss, totaldhMFcshighloss,
    totaldhCFcsbaseloss, totaldhCFcslowloss, totaldhCFcsmedloloss, totaldhCFcsmedhiloss, totaldhCFcshighloss),
  c(totallwrMFcsbaseloss, totallwrMFcslowloss, totallwrMFcsmedloloss, totallwrMFcsmedhiloss, totallwrMFcshighloss, 
    totallwrCFcsbaseloss, totallwrCFcslowloss, totallwrCFcsmedloloss, totallwrCFcsmedhiloss, totallwrCFcshighloss),
  c(totaluprMFcsbaseloss, totaluprMFcslowloss, totaluprMFcsmedloloss, totaluprMFcsmedhiloss, totaluprMFcshighloss, 
    totaluprCFcsbaseloss, totaluprCFcslowloss, totaluprCFcsmedloloss, totaluprCFcsmedhiloss, totaluprCFcshighloss) ))

colnames(totalcslossDH) <- c("fit", "lwr", "upr")
totalcslossDH$Flyway <- c("MF", "MF", "MF", "MF", "MF", "CF", "CF", "CF", "CF", "CF")
totalcslossDH$Scenario <- c("Baseline", "Low", "Medium-Low", "Medium-High", "High", "Baseline", "Low", "Medium-Low", "Medium-High", "High")
sum(totalcslossDH[5,1]+totalcslossDH[10,1])



#number of hunters...
ahunt2016MF <- WOTUSdataUSPPR$ActiveHunters[WOTUSdataUSPPR$Year == 2016 & WOTUSdataUSPPR$Flyway == "MF"]
ahunt2016CF <- WOTUSdataUSPPR$ActiveHunters[WOTUSdataUSPPR$Year == 2016 & WOTUSdataUSPPR$Flyway == "CF"]

fract2016MF <- ahunt2016MF/(ahunt2016MF+ahunt2016CF)
fract2016CF <- ahunt2016CF/(ahunt2016MF+ahunt2016CF)


#combine
totalexplossDHD
totalcslossDHD
totalexplossDH
totalcslossDH


#total loss under scenario of largest wetland loss
sum(totalexplossDHD[5,1],totalexplossDHD[10,1],   #total loss in expenses to reduction in duck hunter days
    totalcslossDHD[5,1],totalcslossDHD[10,1],   #total loss in consumer surplus to reduction in duck hunter days
    totalexplossDH[5,1],totalexplossDH[10,1],  #total loss in expenses to reduction in hunters
    totalcslossDH[5,1],totalcslossDH[10,1])  #total loss in consumer surplus to reduction in hunters

totalexplossDHD$ScenNum <- as.numeric(seq(1:10))
# plotrix::plotCI(x =  totalexplossDHD$ScenNum, y = totalexplossDHD$fit,
#                 ui = totalexplossDHD$upr,    li = totalexplossDHD$lwr, 
#                 err="y", pch=21, pt.bg=par("bg"), gap=TRUE, ylim=c(0, 4000000),
#                 ylab="Expenses ($) Loss, Duck Hunter Days Afield", xlab="Scenario")



EXDHD <- totalexplossDHD %>%
  arrange(fit) %>%
  mutate(Scenario = factor(Scenario, levels=c(
    "Baseline", "Low", "Medium-Low", 
    "Medium-High", "High"))) %>%
  ggplot(aes(x=Scenario, y=fit, group=Flyway, shape=Flyway, color=Flyway)) +
  geom_line() + theme_minimal() +
  scale_color_manual(values=c("firebrick", "darkblue"),
                     guide="none") + 
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=Flyway), 
              linetype=c("solid", "solid", "solid", "solid", "solid", 
                         "dashed", "dashed", "dashed", "dashed", "dashed")) +
  scale_fill_manual(values=alpha(c("firebrick", "darkblue"), 0.2),
                    guide="none") +
  geom_point()  +
  scale_y_continuous(name="Lost Expenditures ($),\nDuck Hunter Days Afield", 
                    labels = unit_format(unit = "M", scale = 1e-6), limits=c(0,12000000))


CSDHD <- totalcslossDHD %>%
  arrange(fit) %>%
  mutate(Scenario = factor(Scenario, levels=c(
    "Baseline", "Low", "Medium-Low", 
    "Medium-High", "High"))) %>%
  ggplot(aes(x=Scenario, y=fit, group=Flyway, shape=Flyway, color=Flyway)) +
  geom_line() + theme_minimal() +
  scale_color_manual(values=c("firebrick", "darkblue"),
                     guide="none") + 
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=Flyway), 
              linetype=c("solid", "solid", "solid", "solid", "solid", 
                         "dashed", "dashed", "dashed", "dashed", "dashed")) +
  scale_fill_manual(values=alpha(c("firebrick", "darkblue"), 0.2),
                    guide="none") +
  geom_point()  +
  scale_y_continuous(name="Lost Consumer Surplus ($),\nDuck Hunter Days Afield", 
                     labels = unit_format(unit = "M", scale = 1e-6), limits=c(0,12000000))

EXDH <- totalexplossDH %>%
  arrange(fit) %>%
  mutate(Scenario = factor(Scenario, levels=c(
    "Baseline", "Low", "Medium-Low", 
    "Medium-High", "High"))) %>%
  ggplot(aes(x=Scenario, y=fit, group=Flyway, shape=Flyway, color=Flyway)) +
  geom_line() + theme_minimal() +
  scale_color_manual(values=c("firebrick", "darkblue"),
                     guide="none") + 
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=Flyway), 
              linetype=c("solid", "solid", "solid", "solid", "solid", 
                         "dashed", "dashed", "dashed", "dashed", "dashed")) +
  scale_fill_manual(values=alpha(c("firebrick", "darkblue"), 0.2),
                    guide="none") +
  geom_point()  +
  scale_y_continuous(name="Lost Expenditures ($),\nActive Duck Hunters", 
                     labels = unit_format(unit = "M", scale = 1e-6), limits=c(0,12000000))


CSDH <- totalcslossDH %>%
  arrange(fit) %>%
  mutate(Scenario = factor(Scenario, levels=c(
    "Baseline", "Low", "Medium-Low", 
    "Medium-High", "High"))) %>%
  ggplot(aes(x=Scenario, y=fit, group=Flyway, shape=Flyway, color=Flyway)) +
  geom_line() + theme_minimal() +
  scale_color_manual(values=c("firebrick", "darkblue"),
                     guide="none") + 
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=Flyway), 
              linetype=c("solid", "solid", "solid", "solid", "solid", 
                         "dashed", "dashed", "dashed", "dashed", "dashed")) +
  scale_fill_manual(values=alpha(c("firebrick", "darkblue"), 0.2),
                    guide="none") +
  geom_point()  + 
  scale_y_continuous(name="Lost Consumer Surplus ($),\nActive Duck Hunters", 
                     labels = unit_format(unit = "M", scale = 1e-6), limits=c(0,12000000))



ggarrange(EXDHD, CSDHD, EXDH, CSDH, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2,
          common.legend = TRUE, legend = "bottom")


#combine expenses
#calculate SE from CIs by SE = (upper limit – lower limit) / 3.92

totalexplossDHD$SE <- abs((totalexplossDHD$upr-totalexplossDHD$lwr) / 3.92)
totalcslossDHD$SE <- abs((totalcslossDHD$upr-totalcslossDHD$lwr) / 3.92)
totalexplossDH$SE <- abs((totalexplossDH$upr-totalexplossDH$lwr) / 3.92)
totalcslossDH$SE <- abs((totalcslossDH$upr-totalcslossDH$lwr) / 3.92)

totalloss <- as.data.frame((totalexplossDHD$fit + totalcslossDHD$fit + totalexplossDH$fit + totalcslossDH$fit))
colnames(totalloss) <- c("fit")

totalloss$SE <- (totalexplossDHD$SE + totalcslossDHD$SE + totalexplossDH$SE + totalcslossDH$SE)/(4^2)
totalloss$lwr <- totalloss$fit - 1.96*totalloss$SE
totalloss$upr <- totalloss$fit + 1.96*totalloss$SE

totalloss$Flyway <- c("MF", "MF", "MF", "MF", "MF", "CF", "CF", "CF", "CF", "CF")
totalloss$Scenario <- c("Baseline", "Low", "Medium-Low", "Medium-High", "High", "Baseline", "Low", "Medium-Low", "Medium-High", "High")
totalloss$WetlandLoss <- c(0, 58633, 161151, 297556, 368423, 0, 58633, 161151, 297556, 368423) #hectares
 
##old x-axis
totalcost <- totalloss %>%
  arrange(fit) %>%
  mutate(Scenario = factor(Scenario, levels=c(
    "Baseline", "Low", "Medium-Low", 
    "Medium-High", "High"))) %>%
  ggplot(aes(x=Scenario, y=fit, group=Flyway, shape=Flyway, color=Flyway)) +
  geom_line() + theme_minimal() +
  scale_color_manual(values=c("firebrick", "darkblue"),
                     guide="none") + 
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=Flyway), 
              linetype=c("solid", "solid", "solid", "solid", "solid", 
                         "dashed", "dashed", "dashed", "dashed", "dashed")) +
  scale_fill_manual(values=alpha(c("firebrick", "darkblue"), 0.2),
                    guide="none") +
  geom_point()  + 
  scale_y_continuous(name="Total Lost Economic Value ($)", 
                     labels = unit_format(unit = "M", scale = 1e-6), limits=c(0,22500000))

#x-axis in terms of wetland loss amount
totalcost <- totalloss %>%
  arrange(fit) %>%
  ggplot(aes(x=WetlandLoss, y=fit, group=Flyway, shape=Flyway, color=Flyway)) +
  geom_line() + theme_minimal() +
  scale_color_manual(values=c("firebrick", "darkblue"),
                     guide="none") + 
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=Flyway), 
              linetype=c("solid", "solid", "solid", "solid", "solid", 
                         "dashed", "dashed", "dashed", "dashed", "dashed")) +
  scale_fill_manual(values=alpha(c("firebrick", "darkblue"), 0.2),
                    guide="none") +
  geom_point()  + 
  scale_x_continuous(name="Wetland Loss (ha, thousands)", 
                     labels = unit_format(unit = "", scale = 1e-3), limits=c(0,400000)) + #unit="T"
  scale_y_continuous(name="Total Lost Economic Value ($)", 
                     labels = unit_format(unit = "M", scale = 1e-6), limits=c(0,22500000))





#########Viewing ##########
#from https://drive.google.com/file/d/1z4HnrCFfgVh3VOFcch7_PlL0eIimEPSO/view?usp=sharing, specifically the viewing trips & WTP 2019-09-03-ANOVA-by flyway.pdf
#Tables 2, 3, 4, 5

rownum <- seq(1:4)
abund <- c(1, 1, 2, 2)
respondents <- c(34, 12, 34, 12)
vflyway <- as.factor(c(2, 3, 2, 3))
flyname <- c("Central", "Mississippi", "Central", "Mississippi")


CFtrips <- c(2L, 2L, 2L, 1L, 3L, 2L, 2L, 1L, 2L, 2L, 4L, 1L, 2L, 2L, 2L, 2L, 2L, 5L, 3L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 3L, 3L, 2L, 2L, 2L, 2L)
MFtrips <- c(1L, 2L, 3L, 2L, 2L, 2L, 3L, 1L, 2L, 3L, 7L, 6L)

base::mean(MFtrips); base::mean(CFtrips); stats::median(MFtrips); stats::median(CFtrips)
gmeanMFtrips <- exp(mean(log(MFtrips)))
gmeanCFtrips <- exp(mean(log(CFtrips)))
gsdMFtrips <- exp(sd(log(MFtrips)))
gsdCFtrips <- exp(sd(log(CFtrips)))

MFdoubletrips <- c(1L, 3L, 9L, 9L, 3L, 6L, 9L, 7L, 9L, 6L, 370L, 262L)
CFdoubletrips <-  c(9L,3L, 12L,1L,6L,3L,3L,1L,6L,6L,254L,1L,6L,
                    9L,9L,6L,6L,15L,12L,6L,6L,9L,3L,4L,
                    6L,9L,3L,1L,6L,6L,12L,9L,9L,3L) 
base::mean(MFdoubletrips); base::mean(CFdoubletrips); stats::median(MFdoubletrips); stats::median(CFdoubletrips)
gmeanMFdoubletrips <- exp(mean(log(MFdoubletrips)))
gmeanCFdoubletrips <- exp(mean(log(CFdoubletrips)))
gsdMFdoubletrips <- exp(sd(log(MFdoubletrips)))
gsdCFdoubletrips <- exp(sd(log(CFdoubletrips)))


tripnum <- c(gmeanCFtrips, gmeanMFtrips, gmeanCFdoubletrips, gmeanMFdoubletrips)
tripnumSD <- c(gsdCFtrips, gsdMFtrips, gsdCFdoubletrips, gsdMFdoubletrips)
triplow <- tripnum - 1.96*tripnum/sqrt(respondents)
triphigh <- tripnum + 1.96*tripnum/sqrt(respondents)


CFexp <- c(0.5, 2, 7, 5, 5, 5, 2.5, 10, 15, 25, 15, 17, 20, 25, 25, 25, 30, 33, 45, 10, 60, 65, 60, 50, 75, 58, 80, 110, 65, 200, 210, 348, 325, 380)    #0.1 was 0, but gmean cannot address 0s
MFexp <- c(20, 45, 35, 95, 68, 120, 180, 200, 290, 295, 350, 340)
base::mean(CFexp); base::mean(MFexp); stats::median(CFexp); stats::median(MFexp)
gmeanCFexp <- exp(mean(log(CFexp)))
gmeanMFexp <- exp(mean(log(MFexp)))
gsdCFexp <- exp(sd(log(CFexp)))
gsdMFexp <- exp(sd(log(MFexp)))

CFdoubleexp <- c(0.5, 2, 7, 10, 10, 15, 22.5, 25, 25, 25, 25, 27, 30, 35, 40, 45, 50, 53, 55, 60, 60, 65, 70, 80, 95, 108, 110, 115, 135, 220, 230, 358, 375, 430)  #0.1 was 0, but gmean cannot address 0s
MFdoubleexp <- c(20, 45, 85, 105, 138, 150, 200, 300, 340, 345, 380, 440)
base::mean(CFdoubleexp); base::mean(MFdoubleexp)
gmeanCFdoubleexp <- exp(mean(log(CFdoubleexp)))
gmeanMFdoubleexp <- exp(mean(log(MFdoubleexp)))
gsdCFdoubleexp <- exp(sd(log(CFdoubleexp)))
gsdMFdoubleexp <- exp(sd(log(MFdoubleexp)))



viewexp <- c(gmeanCFexp, gmeanMFexp, gmeanCFdoubleexp, gmeanMFdoubleexp)
viewexpSD <- c(gsdCFexp, gsdMFexp, gsdCFdoubleexp, gsdMFdoubleexp)
viewlow <- viewexp - 1.96*viewexp/sqrt(respondents)
viewhigh <- viewexp + 1.96*viewexp/sqrt(respondents)


view.dat <- data.table::data.table(rownum, vflyway, flyname, respondents, abund, 
                             tripnum, tripnumSD, triplow, triphigh, 
                             viewexp, viewexpSD, viewlow, viewhigh, 
                             stringsAsFactors = FALSE)
sjPlot::tab_df(view.dat, file="viewingdata.doc")
plotrix::plotCI(x=view.dat$rownum, y=view.dat$tripnum, ui=view.dat$triphigh, li=view.dat$triplow, 
                ylim=c(0,20), pt.bg=par("bg"), pch=21, xaxt="none",
                ylab="Viewing-related Expenses ($)", 
                xlab=" ")
axis(1, seq(0,4,1))

view.dat$group <- with(view.dat, interaction(view.dat$vflyway,  view.dat$abund))
view.dat$group <- factor(as.numeric(view.dat$group) - 1)
view.dat$scen <- c("Central-Current", "Mississippi-Current", "Central-Double", "Mississippi-Double")

numplot <- ggplot(data=view.dat) +
  geom_line(aes(x=as.factor(abund), y=tripnum, 
                group=flyname, color=flyname)) +     
  geom_pointrange(aes(x=as.factor(abund), y=tripnum, ymin=triplow, ymax=triphigh, 
                      group=flyname, color=flyname), 
                  position = position_dodge(width = 1), size=0.75)  +
  facet_grid(~flyname)+
  scale_color_manual(values=c("firebrick", "darkblue")) +
  xlab("Abundance") + ylab("Annual Number\nof Viewing Trips") + labs(color = "Flyway") +
  theme_minimal()

expplot <- ggplot(data=view.dat) +
  geom_line(aes(x=as.factor(abund), y=viewexp, 
                group=flyname, color=flyname)) +     
  geom_pointrange(aes(x=as.factor(abund), y=viewexp, ymin=viewlow, ymax=viewhigh, 
                      group=flyname, color=flyname), 
                  position = position_dodge(width = 1), size=0.75)  +
  facet_grid(~flyname)+
  scale_color_manual(values=c("firebrick", "darkblue")) +
  xlab("Abundance") + ylab("Viewing-related Expenditures\n(2014$), per Trip") + labs(color = "Flyway") +
  theme_minimal()

ggpubr::ggarrange(numplot, expplot, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)

######scrape table from viewing report

library("pdftools")
download.file("https://www.fws.gov/southeast/pdf/report/birding-in-the-united-states-a-demographic-and-economic-analysis.pdf", destfile = "birding.pdf", mode = "wb")
table <- pdftools::pdf_text("birding.pdf") %>% strsplit(split = "\n")
cat(table[[11]]) 

#there's only 49 states, which one is missing?!
birders <- tibble::tribble(
                       ~State, ~Total, ~Residents, ~Nonresidents, ~SmallSample,       ~Flyway,
                    "Alabama",    607,       0.94,            NA,           NA, "Mississippi",
                     "Alaska",    512,       0.31,          0.69,           0L,     "Pacific",
                    "Arizona",   1110,       0.82,          0.18,           1L,     "Pacific",
                   "Arkansas",    539,       0.98,            NA,           NA, "Mississippi",
                 "California",   4864,       0.94,          0.06,           0L,     "Pacific",
                   "Colorado",   1188,       0.85,          0.15,           0L,     "Central",
                "Connecticut",    873,       0.93,          0.07,           1L,    "Atlantic",
                   "Delaware",    171,        0.8,            NA,           NA,    "Atlantic",
                    "Florida",   2966,       0.75,          0.25,           0L,    "Atlantic",
                    "Georgia",   1903,       0.87,          0.13,           1L,    "Atlantic",
                     "Hawaii",    254,       0.27,          0.73,           1L,     "Pacific",
                      "Idaho",    419,       0.81,          0.19,           1L,     "Pacific",
                   "Illinois",   1811,        0.9,           0.1,           1L, "Mississippi",
                    "Indiana",   1175,       0.99,            NA,           NA, "Mississippi",
                       "Iowa",    531,       0.89,            NA,           NA, "Mississippi",
                     "Kansas",    476,       0.95,            NA,           NA,     "Central",
                   "Kentucky",    827,        0.9,           0.1,           1L, "Mississippi",
                  "Louisiana",    712,       0.71,            NA,           NA, "Mississippi",
                      "Maine",    689,       0.38,          0.63,           0L,    "Atlantic",
                   "Maryland",    934,       0.84,          0.16,           1L,    "Atlantic",
              "Massachusetts",   1238,       0.75,          0.25,           0L,    "Atlantic",
                   "Michigan",   2015,       0.93,          0.07,           1L, "Mississippi",
                  "Minnesota",   1112,       0.93,          0.07,           1L, "Mississippi",
                "Mississippi",    456,       0.87,            NA,           NA, "Mississippi",
                   "Missouri",   1110,       0.92,          0.08,           1L, "Mississippi",
                    "Montana",    291,        0.6,           0.4,           1L,     "Central",
                   "Nebraska",    273,       0.89,            NA,           NA,     "Central",
                     "Nevada",    447,       0.72,          0.28,           1L,     "Pacific",
              "New Hampshire",    527,       0.55,          0.45,           1L,    "Atlantic",
                 "New Jersey",   1195,       0.87,          0.13,           1L,    "Atlantic",
                 "New Mexico",    415,       0.78,          0.22,           1L,     "Central",
                   "New York",   3272,       0.93,          0.07,           0L,    "Atlantic",
             "North Carolina",   1854,       0.84,          0.16,           0L,    "Atlantic",
                       "Ohio",   1583,       0.97,            NA,           NA, "Mississippi",
                   "Oklahoma",    773,       0.97,            NA,           NA,     "Central",
                     "Oregon",    892,       0.79,          0.21,           1L,     "Pacific",
               "Pennsylvania",   2699,       0.89,          0.11,           0L,    "Atlantic",
               "Rhode Island",    201,        0.8,           0.2,           0L,    "Atlantic",
             "South Carolina",    536,       0.72,          0.28,           1L,    "Atlantic",
               "South Dakota",    235,       0.64,          0.36,           0L,     "Central",
                  "Tennessee",   1382,       0.82,          0.18,           0L, "Mississippi",
                      "Texas",   2238,       0.95,          0.05,           1L,     "Central",
                       "Utah",    410,       0.69,          0.31,           0L,     "Pacific",
                    "Vermont",    292,       0.69,          0.31,           1L,    "Atlantic",
                   "Virginia",   1425,       0.81,          0.19,           1L,    "Atlantic",
                 "Washington",   1516,       0.83,          0.17,           1L,     "Pacific",
              "West Virginia",    547,       0.88,            NA,           NA,    "Atlantic",
                  "Wisconsin",   1678,       0.89,          0.11,           1L, "Mississippi",
                    "Wyoming",    417,       0.31,          0.69,           0L,     "Central"
             )

summary(birders)

birdersflyway <- birders %>%
  group_by(Flyway) %>%
  summarize(n(), sumtotal = sum(Total))

Centralbirders <- birdersflyway[2,3]
Mississippibirders <- birdersflyway[3,3]

#Not All Birders, not All Hunters, decrement birders by fraction of observers/observations sighting waterfowl in eBird 

library("rebird")
#https://www.birds.cornell.edu/clementschecklist/download for source of eBird code names
species_code('Anas platyrhynchos')  #mallar3  #MALL      
species_code('Mareca americana')    #amewig   #AMWI
species_code('Spatula discors')     #buwtea  #no longer Anas discors   #BWTE
species_code('Aythya valisineria')  #canvas   #CANV
species_code('Mareca strepera')     #gadwal   #GADW
species_code('Aythya marila')       #gresca   #SCAU, greater   Aythya affinis for lesser scaup
species_code('Anas crecca carolinensis')  #agwteal   #not crecca   #GWTE
species_code('Anas acuta')          #norpin   #NOPI
species_code('Spatula clypeata')    #norsho   #NOSH
species_code('Aythya americana')    #redhea   #REDH
species_code('Aythya collaris')     #rinduc   #RNDU
species_code('Oxyura jamaicensis')  #rudduc   #RUDU

#eBird key : f1tvvdh7b55c
Sys.setenv(EBIRD_KEY = "f1tvvdh7b55c"); Sys.getenv()

#example
#sum(ebirdregion(loc = 'US-AL', species = 'mallar3')[,7], na.rm=TRUE) #this is only recent
#ebirdsubregionlist("subnational1","US")
#ebirdregion(loc = 'US-AL')

days <- seq(from=as.Date('2014-01-01'), to=as.Date("2014-12-31"),by='days' )

# #eBirdCFStates <- c("US-MT", "US-ND", "US-SD", "US-WY", "US-NE", "US-KS", "US-CO", 
#                     "US-NM", "US-OK", "US-TX")
# #eBirdMFStates <- c("US-MN", "US-WI", "US-MI", "US-OH", "US-IN", "US-IL", "US-IA", 
#                     "US-MO", "US-KY", "US-TN", "US-AR", "US-LA", "US-AL", "US-MS")
length(eBirdCFStates); length(eBirdMFStates)
eBirdstates <- ebirdsubregionlist("subnational1","US")[,1]
# eBirdCFlist <- list()
# eBirdMFlist <- list()

eBirdTNlist <- list()
for ( i in seq_along(days) ){
  eBirdTNobs <- ebirdhistorical(loc="US-TN", date=days[i])
  eBirdTNlist[[length(eBirdTNlist) + 1]] <- eBirdTNobs
}
beep()

#save.image("C:/Users/wthogmartin/OneDrive - DOI/wet_migration/Pintail/WOTUS/birding data.RData")
load("C:/Users/wthogmartin/OneDrive - DOI/wet_migration/Pintail/WOTUS/birding data.RData")

#eBirdMTlist[3][[1]][["speciesCode"]] #WTF eBird? Why the y00475? American Coot!
eWY.df <- do.call(plyr::rbind.fill, eBirdWYlist) #WY has different number of columns for some reason
eWY.sum <- aggregate(eWY.df$howMany, by=list(Category=eWY.df$speciesCode), FUN=sum, na.rm=TRUE)

eCF.df <- rbind(eMT.sum, eND.sum, eSD.sum, eWY.sum, eNE.sum, eKS.sum, eCO.sum, eOK.sum, eNM.sum, eTX.sum)
eCF.sum <- aggregate(eCF.df$x, by=list(Category=eCF.df$Category), FUN=sum, na.rm=TRUE)

eMF.df <- rbind(eMN.sum, eWI.sum, eMI.sum, eIA.sum, eIL.sum, eIN.sum, eOH.sum, eMO.sum, eTN.sum, eKY.sum, eAR.sum, eAL.sum, eLA.sum, eMS.sum)
eMF.sum <- aggregate(eMF.df$x, by=list(Category=eMF.df$Category), FUN=sum, na.rm=TRUE)

CF.waterfowln <- sum(eCF.sum[which(eCF.sum$Category=='mallar3' | eCF.sum$Category=='amewig' |
                  eCF.sum$Category=='buwtea' | eCF.sum$Category=='canvas' |
                  eCF.sum$Category=='gadwal' | eCF.sum$Category=='gresca' |
                  eCF.sum$Category=='agwteal' | eCF.sum$Category=='norpin' |
                  eCF.sum$Category=='norsho' | eCF.sum$Category=='redhea' |
                  eCF.sum$Category=='rinduc' | eCF.sum$Category=='rudduc'
                  ), 2])
CF.totalbirdn <- sum(eCF.sum[,2])

CFwaterfowlprop <- CF.waterfowln/CF.totalbirdn

CF.mallardn <- sum(eCF.sum[which(eCF.sum$Category=='mallar3'), 2])
CF.pintailn <- sum(eCF.sum[which(eCF.sum$Category=='norpin'), 2])
CFnorpinprop <- CF.pintailn/CF.totalbirdn
CF.waterfowln/CF.pintailn

MF.waterfowln <- sum(eMF.sum[which(eMF.sum$Category=='mallar3' | eMF.sum$Category=='amewig' |
                  eMF.sum$Category=='buwtea' | eMF.sum$Category=='canvas' |
                  eMF.sum$Category=='gadwal' | eMF.sum$Category=='gresca' |
                  eMF.sum$Category=='agwteal' | eMF.sum$Category=='norpin' |
                  eMF.sum$Category=='norsho' | eMF.sum$Category=='redhea' |
                  eMF.sum$Category=='rinduc' | eMF.sum$Category=='rudduc'
                  ), 2])
MF.totalbirdn <- sum(eMF.sum[,2])


MF.mallardn <- sum(eMF.sum[which(eMF.sum$Category=='mallar3'), 2])
MF.pintailn <- sum(eMF.sum[which(eMF.sum$Category=='norpin'), 2])

MFwaterfowlprop <- MF.waterfowln/MF.totalbirdn
MFnorpinprop <- MF.pintailn/MF.totalbirdn
MF.waterfowln/MF.pintailn

#number of birders*DU number of trips per birder*DU expenses per trip*eBird waterfowl proportion

CFview1exp <- Centralbirders*1000*as.data.frame(view.dat[1,6])*as.data.frame(view.dat[1,10])*CFwaterfowlprop   #*0.902
MFview1exp <- Mississippibirders*1000*as.data.frame(view.dat[2,6])*as.data.frame(view.dat[2,10])*MFwaterfowlprop   #*0.906
CFview2exp <- Centralbirders*1000*as.data.frame(view.dat[3,6])*as.data.frame(view.dat[3,10])*CFwaterfowlprop   #*0.902
MFview2exp <- Mississippibirders*1000*as.data.frame(view.dat[4,6])*as.data.frame(view.dat[4,10])*MFwaterfowlprop   #*0.906

#with birder decrement, i.e., the committed waterfowler hunter-birder
CFview1expB <- Centralbirders*1000*as.data.frame(view.dat[1,6])*as.data.frame(view.dat[1,10])*CFwaterfowlprop*0.105
MFview1expB <- Mississippibirders*1000*as.data.frame(view.dat[2,6])*as.data.frame(view.dat[2,10])*MFwaterfowlprop*0.094
CFview2expB <- Centralbirders*1000*as.data.frame(view.dat[3,6])*as.data.frame(view.dat[3,10])*CFwaterfowlprop*0.105
MFview2expB <- Mississippibirders*1000*as.data.frame(view.dat[4,6])*as.data.frame(view.dat[4,10])*MFwaterfowlprop*0.094

#with birder decrement, i.e., the committed waterfowler hunter-birder
CFview1expB250 <- Centralbirders*1000*250*CFwaterfowlprop*0.105
MFview1expB250 <- Mississippibirders*1000*250*MFwaterfowlprop*0.094
CFview2expB250 <- Centralbirders*1000*250*CFwaterfowlprop*0.105
MFview2expB250 <- Mississippibirders*1000*250*MFwaterfowlprop*0.094

Mississippibirders/Centralbirders  #2.464
MFwaterfowlprop/CFwaterfowlprop #2.001

#donations distribution
CFamountdon <- c(0, 125, 575, 1750, 3750, 7500, 10000)
CFpercentdon <- c(0.667, 0.300, 0.024, 0.006, 0.002, 0, 0.001)
plot(CFamountdon~CFpercentdon)
library(fitdistrplus)
add_nls <- nls(CFamountdon ~ a*exp(r*CFpercentdon), 
               start = list(a = 0.5, r = -0.2))
coef(add_nls)
CFdon <- data.frame(CFpercentdon, CFpercenta)
fo1 <- y ~ 1 / (1 + x^c)
fm1 <- nls(fo1, data, start = list(c = 1))
Plot(data, fm1, "1 parameter")

exp.f <- fitdistr(cyber.data$event.count, "exp")

#calculate just for pintails, to see how well it coheres with previous publication, Mattsson et al.
CFpintail1exp <- Centralbirders*1000*as.data.frame(view.dat[1,6])*as.data.frame(view.dat[1,10])*CFnorpinprop
MFpintail1exp <- Mississippibirders*1000*as.data.frame(view.dat[2,6])*as.data.frame(view.dat[2,10])*MFnorpinprop
CFpintail2exp <- Centralbirders*1000*as.data.frame(view.dat[3,6])*as.data.frame(view.dat[3,10])*CFnorpinprop
MFpintail2exp <- Mississippibirders*1000*as.data.frame(view.dat[4,6])*as.data.frame(view.dat[4,10])*MFnorpinprop

view.dat$birders <- c(Centralbirders, Mississippibirders, Centralbirders, Mississippibirders)
view.dat$ebirdwfprop <- c(CFwaterfowlprop, MFwaterfowlprop, CFwaterfowlprop, MFwaterfowlprop)
view.dat$totviewexp <- c(CFview1exp, MFview1exp, CFview2exp, MFview2exp)

sjPlot::tab_df(view.dat[,c(3,5,16:18)], file="viewingdata1.doc")

#number of birders*USFWS number of trips per birder[238036000/17818000 = 13.3593]*USFWS expenses per trip[14868424740/238036000 = 62.46]*eBird waterfowl proportion
#https://www.fws.gov/southeast/pdf/report/birding-in-the-united-states-a-demographic-and-economic-analysis.pdf Table 7 (page 10, units should be in thousands!!!!)
CFview1USFWSexp <- Centralbirders*1000*13.3593*62.46*CFwaterfowlprop
MFview1USFWSexp <- Mississippibirders*1000*13.3593*62.46*MFwaterfowlprop
#what values to use for number of trips and expenses per trip when doubling?
CFview2USFWSexp <- Centralbirders*1000*VALUE*VALUE HERE*CFwaterfowlprop
MFview2USFWSexp <- Mississippibirders*1000*VALUE*VALUE HERE*MFwaterfowlprop




#multiplier of 0.07381776 applied, but why?
#Are Wildlife Recreationists Conservationists? The Journal of Wildlife Management 79(3):446–457; 2015; DOI: 10.1002/jwmg.855
#number of hunter-birdwatchers = 64, number of hunters = 290, number of birdwatchers = 513; 64/(64+290+513) = 0.07381776
PropHunterBirder <- 64/(64+290) #proportion of hunters who are birders too


#WOTUSdata2[,4] is number of active hunters*number of trips per waterfowler*expenses per trip*eBird waterfowl proportion*Proportion of hunters who are birders

CFview1hexp <- WOTUSdata2[107,4]*view.dat[1,6]*view.dat[1,10]*CFwaterfowlprop*PropHunterBirder
MFview1hexp <- WOTUSdata2[108,4]*view.dat[2,6]*view.dat[2,10]*MFwaterfowlprop*PropHunterBirder
CFview2hexp <- WOTUSdata2[107,4]*view.dat[3,6]*view.dat[3,10]*CFwaterfowlprop*PropHunterBirder
MFview2hexp <- WOTUSdata2[108,4]*view.dat[4,6]*view.dat[4,10]*MFwaterfowlprop*PropHunterBirder


#calculate slope
MFriseexp <- MFview2exp-MFview1exp
CFriseexp <- CFview2exp-CFview1exp
run <- (2*49152200)-(1*49152200)  #49152.2 is the number of waterfowl (i.e., the BPOP) in 2014
MFslope <- MFriseexp/run   #
CFslope <- CFriseexp/run   #

wetlossscen

CFwfowlprop <- CF.waterfowln/(CF.waterfowln+MF.waterfowln)
MFwfowlprop <- MF.waterfowln/(CF.waterfowln+MF.waterfowln)



#0.8 is 80% of waterfowl in PPR go to Central and Mississippi Flyways; what is the correct percent?
totalCFviewlosslow <- (wetlossscen$duckloss[1]-wetlossscen$duckloss[2])*CFslope*CFwfowlprop*0.8 #low 
totalCFviewlossmedlo <- (wetlossscen$duckloss[1]-wetlossscen$duckloss[3])*CFslope*CFwfowlprop*0.8 #medium-low
totalCFviewlossmedhi <- (wetlossscen$duckloss[1]-wetlossscen$duckloss[4])*CFslope*CFwfowlprop*0.8#medium-high
totalCFviewlosshigh <- (wetlossscen$duckloss[1]-wetlossscen$duckloss[5])*CFslope*CFwfowlprop*0.8#high

totalMFviewlosslow <- (wetlossscen$duckloss[1]-wetlossscen$duckloss[2])*MFslope*MFwfowlprop*0.8 #low 
totalMFviewlossmedlo <- (wetlossscen$duckloss[1]-wetlossscen$duckloss[3])*MFslope*MFwfowlprop*0.8 #medium-low
totalMFviewlossmedhi <- (wetlossscen$duckloss[1]-wetlossscen$duckloss[4])*MFslope*MFwfowlprop*0.8#medium-high
totalMFviewlosshigh <- (wetlossscen$duckloss[1]-wetlossscen$duckloss[5])*MFslope*MFwfowlprop*0.8#high

# trips_dbl_birds <- (data.table::data.table(
#   trips_dbl_birds = c(1L,1L,1L,1L,1L,1L,1L,1L,
#                       1L,1L,1L,1L,1L,3L,3L,3L,3L,3L,3L,3L,3L,3L,3L,
#                       3L,3L,3L,3L,3L,3L,3L,3L,3L,3L,3L,3L,3L,3L,
#                       3L,3L,3L,3L,3L,3L,3L,3L,3L,3L,3L,3L,4L,4L,6L,
#                       6L,6L,6L,6L,6L,6L,6L,6L,6L,6L,6L,6L,6L,6L,
#                       6L,6L,6L,6L,6L,6L,6L,6L,6L,6L,6L,6L,6L,6L,6L,
#                       6L,6L,6L,6L,6L,7L,9L,9L,9L,9L,9L,9L,9L,9L,
#                       9L,9L,9L,9L,9L,9L,9L,9L,9L,9L,9L,9L,9L,9L,12L,
#                       12L,12L,12L,12L,12L,12L,12L,12L,12L,12L,12L,
#                       15L,15L,15L,17L,18L,18L,20L,20L,23L,29L,34L,35L,
#                       40L,49L,168L,171L,177L,254L,262L,354L,362L,370L)
# )
# )
# 
# MASS::truehist(t(do.call(rbind.data.frame, trips_dbl_birds)), xlab="Trips at Double Abundance")



##########mapping ####

library(urbnmapr)
library(sf)
library(raster)
library(maps)
library(sp)
library(rgdal) #deprecated soon

us_states <- map_data("state")
head(us_states)

us_states$State <- tools::toTitleCase(us_states$region)
birderstate <- us_states %>% 
  left_join(birders, by=c("State"="State")) 
head(birderstate)
st_crs(birderstate) #No coordinate reference system


ggplot(birderstate, mapping = aes(x=long, y=lat, group = group, fill = Total)) +
  geom_polygon(color = "#ffffff", size = 0.25) +
  scale_fill_viridis_c(option = "H") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  xlab("Longitude") + ylab("Latitude") + labs(fill = "Total Birders")
#North Dakota is missing! Are there no birders in North Dakota?

#add Central to Flyway for North Dakota
birderstate[birderstate$State == "North Dakota",12] <- "Central"

ggplot(birderstate, mapping = aes(x=long, y=lat, group = group, fill = Flyway)) +
  geom_polygon(color = "white", size = 0.25) + 
  scale_fill_manual(values=c("gray90","firebrick", "darkblue", "gray90"),na.translate = F) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Flyway") + xlab("Longitude") + ylab("Latitude") + theme_minimal()


#add PPR shapefile, https://www.sciencebase.gov/catalog/item/54aeaef2e4b0cdd4a5caedf1
#PPRshape <- rgdal::readOGR(dsn = file.path("C:/Users/wthogmartin/OneDrive - DOI/wet_migration/Pintail/WOTUS/gmannppr/gmannppr", "gmannppr.shp"), stringsAsFactors = F) #deprecated
PPRshape <- sf::st_read("C:/Users/wthogmartin/OneDrive - DOI/wet_migration/Pintail/WOTUS/gmannppr/gmannppr/gmannppr.shp")

summary(PPRshape)

crs(PPRshape)
crs(birderstate)


states_sf <- get_urbn_map("states", sf = TRUE)

states_sf %>% 
  ggplot(aes()) +
  geom_sf(fill = "grey", color = "#ffffff")

birderstate2 <- states_sf %>% 
  left_join(birders, by=c("state_name"="State")) 
head(birderstate2)
st_crs(birderstate2) #EPSG 2163
birderstate2 <- st_as_sf(birderstate2)


#add Central to Flyway for North Dakota
birderstate2[birderstate2$state_name == "North Dakota",8] <- "Central"

# Transform to projection
PPRshape <- st_as_sf(PPRshape)
PPRshape2 <- st_transform(PPRshape, st_crs(birderstate2))


ggplot(birderstate2, aes(fill=Flyway)) +
  geom_sf(color = "#ffffff")+ 
  scale_fill_manual(values=c("gray90","firebrick", "darkblue", "gray90"), na.translate = F) +
  labs(fill = "Flyway") + xlab("Longitude") + ylab("Latitude") + theme_minimal() + 
  geom_sf(data = PPRshape2, colour = "orange", lty=2, lwd=1.2, fill = NA)

birderstate_48 <- birderstate2 %>%
    filter(!(state_name %in% c("Alaska", "Hawaii")))
uspprshp <- st_intersection(PPRshape2, birderstate_48)

ggplot(birderstate_48, aes(fill=Flyway)) +
  geom_sf(color = "#ffffff") +
  scale_fill_manual(values=c("gray90","firebrick", "darkblue", "gray90"), na.translate = F) +
  labs(fill = "Flyway") + xlab("Longitude") + ylab("Latitude") + theme_minimal() + 
  geom_sf(data = PPRshape2, colour = "orange", lty=2, lwd=1.2, fill = NA) +
  geom_sf(data = uspprshp, color="darkgray", lty=1, lwd=1.2, fill = "darkgray")  +
  geom_sf(color = "#ffffff", fill=NA)  

#add flyway boundaries
Flywayshape <- sf::st_read("C:/Users/wthogmartin/OneDrive - DOI/wet_migration/Pintail/WaterfowlFlyways/WaterfowlFlyways.shp")
geomcoords <- geom(as(Flywayshape,"Spatial")) #Alaska is >50 latitude
sf_use_s2(FALSE)
box <- c(xmin= -179.2311, ymin = 24.39631, xmax = 179.8597, ymax = 50)
Flywaysub <- st_crop(Flywayshape, xmin=-180, xmax=180, ymin=-90, ymax=50)
plot(Flywaysub$geometry)


ggplot(birderstate_48, aes(fill=Flyway)) +
  geom_sf(color = "#ffffff") +
  scale_fill_manual(values=c("gray90","firebrick", "darkblue", "gray90"), na.translate = F) +
  labs(fill = "Flyway") + xlab("Longitude") + ylab("Latitude") + theme_minimal()  +
  geom_sf(data = uspprshp, color="darkgray", lty=1, lwd=1.2, fill = "darkgray")  +
  geom_sf(data = PPRshape2, colour = "orange", lty=2, lwd=1.2, fill = NA) +  
  geom_sf(color = "#ffffff", fill=NA) +
  geom_sf(data=Flywaysub$geometry, color = "black", lty=4, lwd=1, fill=NA)




