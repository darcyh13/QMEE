load("clean_epistasis.rda")

#make new column with total vein length
wings$supervein <- wings$L2s + wings$L3s + wings$L4s + wings$L5s


gene1only <- subset(wings, gene2 == "wt")

library(lme4)
#look at data to see if linear looks like a reasonable fit 
hist(wings$supervein)
qqnorm(wings$supervein)

#singular fit 
lmm1_sv <- lmer(supervein ~ background*gene1 + (1|rep), data = gene1only)

#fit linear for now
lm1_sv <- lm(supervein ~ background*gene1, data = gene1only)

plot(lm1_sv)
summary(lm1_sv)

Anova(lm1_sv)

