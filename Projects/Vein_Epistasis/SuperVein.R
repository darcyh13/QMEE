load("clean_epistasis.rda")

#make new column with total vein length
wings$supervein <- wings$L2s + wings$L3s + wings$L4s + wings$L5s

gene1only <- subset(wings, gene2 == "wt")

#want to see if these visual differences between background is significant due to interaction of genotype and background - this shows each genotype, which we don't care about.
ggplot(data = gene1only, aes(x=gene1, y=supervein)) + geom_boxplot(aes(fill=background))

library(lme4)
#look at data to see if linear looks like it should reasonable fit 
hist(wings$supervein)
qqnorm(wings$supervein)

#singular fit 

lmm1_sv <- lmer(supervein ~ background*gene1*sex + (1|rep), data = gene1only)

#make model for each vein 

lmm1_L2s <- lmer(L2s ~ background*gene1*sex + (1|rep), data = gene1only)

lmm1_L3s <- lmer(L3s ~ background*gene1*sex + (1|rep), data = gene1only)

#singular fit
#lmm1_L4s <- lm(L4s ~ background*gene1*sex + (1|rep), data = gene1only)
#fit random effect as fixed effect 
#get the level coefficients in the sum to zero form, and then compute the standard deviation of the coefficients
lmm1_L4s <- lm(L4s ~ background*gene1*sex + rep, data = gene1only)
summary(lmm1_L4s)

lmm1_L5s <- lmer(L5s ~ background*gene1*sex + (1|rep), data = gene1only)



