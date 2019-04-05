load("clean_epistasis.rda")

#make new column with total vein length
wings$supervein <- wings$L2s + wings$L3s + wings$L4s  + wings$L5s

#subset so that gene2 equals wild-type. Only want to look at double mutants for now. 
gene1only <- subset(wings, gene2 == "wt")

#Look at data
#want to see if differences between mutant in sam and wt in ore is due to variance other than the effect of background and the effect of genotype. Overall, does genotype behave differently in each background. 
library(ggplot2)
ggplot(data = gene1only, aes(x=gene1, y=supervein)) + geom_boxplot(aes(fill=background))

library(lme4)
#look at data to see if linear looks like it should reasonable fit 
hist(wings$supervein)
qqnorm(wings$supervein)

#mixed linear model using glmmTMB package. Singular fit when using lmer. 
library(glmmTMB)
lmm1_sv <- glmmTMB(supervein ~ background*gene1*sex + (1|rep), data = gene1only)
summary(lmm1_sv)
#genes have effect which is what we would expect. Interesting that sex does as overall wing size has been scaled for, and even more interesting that interaction between background and sex has effect (meaning effect of sex behaves differently in background). 
#don't see much of effect from just background and genotype tho.  

#check model 
library(DHARMa)
supervein_lmm1 <- simulateResiduals(lmm1_sv)
plot(supervein_lmm1)
#there is deviation for expected distrubution 
#simulation outliers are highlighted as red stars. data points that are outside the range of simulated values. 
#lines should be straight but some deviations are normal

#inference
Anova(lmm1_sv)
#strong support there is sign effect of background and gene1 on supervein (This is predicted). What we care about is there looks to be significant effect of interaction of gene1 and background and background and sex. 

#effects
(eff <- allEffects(lmm1_sv))
plot(eff)


#make model for each vein 

lmm1_L2s <- lmer(L2s ~ background*gene1*sex + (1|rep), data = gene1only)
summary(lmm1_L2s)
plot(lmm1_L2s)
Anova(lmm1_L2s)
#doesnt work when I add sex. Ask Katie. 
#emmeans_test <- emmeans(lmm1_L2s,"sex" ,"gene1", "background")
#plot(emmeans_test, comparisons = TRUE)  

lmm1_L3s <- lmer(L3s ~ background*gene1*sex + (1|rep), data = gene1only)
summary(lmm1_L3s)
plot(lmm1_L2s)
Anova(lmm1_L2s)


lmm1_L4s <- glmmTMB(L4s ~ background*gene1*sex + (1|rep),
                  data = gene1only)
summary(lmm1_L4s)
L4_lmm1 <- simulateResiduals(lmm1_sv)
plot(L4_lmm1)

lmm1_L5s <- lmer(L5s ~ background*gene1*sex + (1|rep), data = gene1only)
summary(lmm1_L5s)
plot(lmm1_L5s)
anova(lmm1_L5s)
