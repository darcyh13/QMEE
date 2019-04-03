load("clean_epistasis.rda")

#make new column with total vein length
wings$supervein <- wings$L2s + wings$L3s + wings$L4s + wings$L5s

gene1only <- subset(wings, gene2 == "wt")

#want to see if these visual differences between background is significant due to interaction of genotype and background - this shows each genotype, which we don't care about.
ggplot(data = gene1only, aes(x=gene1, y=supervein)) + geom_boxplot(aes(fill=background))

#is there way to show total for each background 
#add superveins for each genotype? 

mutants <- gene1only %>%
  filter (gene1 != "wt")

mutants$gene1 <- "mutant"

wt <- gene1only %>%
  filter( gene1 == "wt")

df <- rbind(mutants, wt)

#shows gene 1 mutants grouped together - relationship we care about! 
ggplot(data = df, aes(x=gene1, y=supervein)) + geom_boxplot(aes(fill=background))


library(lme4)
#look at data to see if linear looks like it should reasonable fit 
hist(wings$supervein)
qqnorm(wings$supervein)


#singular fit 
lmm1_sv <- lmer(supervein ~ background*gene1 + (1|rep), data = gene1only)

#fit linear for now
lm1_sv <- lm(supervein ~ background*gene1, data = gene1only)

plot(lm1_sv)
summary(lm1_sv)

Anova(lm1_sv)

library(dotwhisker)
dwplot(lm1_sv)
