###Testing Type III ANOVA

#######MUST DO FOR For type III tests to be correct, must change the way R codes factors (rcompanion.org)
options(contrasts = c("contr.sum", "contr.poly")) 

#use the car package
library(car)

#here I am running tests to determine whether my elevation or site had an effect on my response variable
#I am running plant as a random effect as it is irrelevatn and I am interested in endophytes

alp <- X20190716_seedelv_2_
mvn(alp[,4:10])#Everything except bacterian richness is non normal

#Here we running a MANOVA comparing all the responses 

y=cbind(alp$`Plant div`,alp$morphct,alp$morphrich,alp$`fungi rich`,alp$`Bacter rich`,alp$`ITS Div`,alp$`Bac DIV`)
fit.alp= manova(y ~ alp$elv+alp$plant)
summary(fit.alp, test="Wilks") #this did not work bc of a residual rank difference so we googled and use tol test
summary(fit.alp,tol=0) # here we saw a signifigant difference for the p value for alp$plant for 0.01638

#Type I SS  ANOVA model for ***** all responses*****
alpmorphrich <- lm(morphrich ~ plant + elv, data = alp)
anova(alpmorphrich)

alpmorphct <- lm(morphct ~ plant + elv, data = alp)
anova(alpmorphct)

alpfungirich <- lm(alp$`fungi rich` ~ plant + elv, data = alp)
anova(alpfungirich)

alpbact <- lm(alp$`Bacter rich` ~ plant + elv, data = alp)
anova(alpbact)

alpitsdiv <- lm(alp$`ITS Div` ~ plant + elv, data = alp)
anova(alpitsdiv)

alpbacdiv <- lm(alp$`Bac DIV` ~ plant + elv, data = alp)
anova(alpbacdiv)

#Lets do a b- test to see which specific elevation/ response to test.
attach(alp)
car1<- subset(alp, plant1 == "1")
des2<-subset(alp, plant1 =="2")
eri3<-subset(alp, plant1 =="3")
fes4<-subset(alp, plant1 =="4")
geu5<-subset(alp, plant1 =="5")
kob6<-subset(alp, plant1 =="6")
luz7<-subset(alp, plant1 =="7")
oxy8<-subset(alp, plant1 =="8")
sil9<-subset(alp, plant1 =="9")
tri10<-subset(alp, plant1 =="10")

#categorical plant values
alp$elv1<-as.numeric(alp$elv1)
alp$elv1<-as.factor(alp$elv1)

#bonferroni by CD

require("DescTools")
attach(car1)
mrcar1<- aov(morphrich~elv1)
PostHocTest(mrcar1, method = "bonferroni")

mc1<- aov(morphct~plant1)
PostHocTest(mccd, method = "bonferroni")

pd1<- aov(Plantdiv~plant1)
PostHocTest(pccd, method = "bonferroni")

fr1<- aov(fungirich~plant1)
PostHocTest(frcd, method = "bonferroni")

br1<- aov(Bacterrich~plant1)
PostHocTest(brcd, method = "bonferroni")

its1<- aov(ITSDiv~plant1)
PostHocTest(itsdcd, method = "bonferroni")

bacd1<- aov(BacDIV~plant1)
PostHocTest(bacdcd, method = "bonferroni")




####### linear mixed effect model
#use the lme4 package
library(lme4)

mod1<- lmer(alp$morphrich~ elv + (1|plant), data=alp)
#for a p value
anova(mod1, test="Chisq")
#Compute all the single terms in the scope argument that can be added to or dropped from the model, fit those models and compute a table of the changes in fit.
drop1(mod1, test="Chisq")
modbac<- lmer(alp$`Bacter rich`~ elv + (1|plant), data=alp)
lmer(alp$`Bacter rich`~ elv + (1|plant), data=alp)
#for a p value
anova(modbac, test="Chisq")
#Compute all the single terms in the scope argument that can be added to or dropped from the model, fit those models and compute a table of the changes in fit.
drop1(modbac, test="Chisq")
modfungir<- lmer(alp$`fungi rich`~ elv + (1|plant), data=alp)
lmer(alp$`fungi rich`~ elv + (1|plant), data=alp)
#for a p value
anova(modfungir, test="Chisq")
#Compute all the single terms in the scope argument that can be added to or dropped from the model, fit those models and compute a table of the changes in fit.
drop1(modfungir, test="Chisq")
modmorpct<-lmer(alp$morphct~ elv + (1|plant), data=alp) 
lmer(alp$morphct~ elv + (1|plant), data=alp)
#for a p value
anova(modmorpct, test="Chisq")
#Compute all the single terms in the scope argument that can be added to or dropped from the model, fit those models and compute a table of the changes in fit.
drop1(modmorpct, test="Chisq")

modITS<- lmer(alp$`ITS Div`~ elv + (1|plant), data=alp)
#for a p value
anova(modITS, test="Chisq")
#Compute all the single terms in the scope argument that can be added to or dropped from the model, fit those models and compute a table of the changes in fit.
drop1(modITS, test="Chisq")

modbacdiv<- lmer(alp$`Bac DIV`~ elv + (1|plant), data=alp)
lmer(alp$`Bac DIV`~ elv + (1|plant), data=alp)
#for a p value
anova(modbacdiv, test="Chisq")
#Compute all the single terms in the scope argument that can be added to or dropped from the model, fit those models and compute a table of the changes in fit.
drop1(modbacdiv, test="Chisq")

boxplot(alp$`fungi rich`~elv, data = alp)
boxplot(alp$morphct~elv, data = alp)
boxplot(alp$morphrich~elv, data = alp)
boxplot(alp$`Bacter rich`~elv, data = alp)
boxplot(alp$`ITS Div`~elv, data = alp)
boxplot(alp$`Bac DIV`~elv, data = alp)

