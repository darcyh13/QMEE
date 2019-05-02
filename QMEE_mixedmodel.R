##March 13th 

library("tidyverse")

## BMB: should put all of this cleanup/rearranging stuff in a separate file,
##  save the results as .rda or .rds, read it in here -- don't need to
##  keep copying code ...
Genetic_Background_Interactions <- read_csv("GeneticBackgroundVeinMutantInteractionsFinal.csv")

#Change second gene to say WT if it does not have a second mutation -> get rid of NA in this column 
Genetic_Background_Interactions$gene2[is.na(Genetic_Background_Interactions$gene2)] <- "wt"

#only want to deal with two genes for now, not three
Genetic_Background_Interactions <- mutate(Genetic_Background_Interactions,
                                          genotype=interaction(gene1, rhomboid))

#change relevant columns to factors 

columns_to_factors <- c("background", "gene1", "gene2", "rhomboid", "sex", "individual", "genotype")
Genetic_Background_Interactions[,columns_to_factors] <- lapply(Genetic_Background_Interactions[,columns_to_factors], as.factor)

#omit remaining NAs 
Genetic_Background_Interactions <- na.omit(Genetic_Background_Interactions)

#install.packages("Matrix")
library(lme4)

#shows that wing length was measured in each genotype and each cross was performed in four replicates. Therefore replicate number may be a possible random effect.  
q0 <- (ggplot(Genetic_Background_Interactions, aes(genotype, WL))
       +facet_wrap(~rep)
       + geom_point())  

## BMB: rep makes sense as a grouping variable *if* there are multiple samples per *replicate*

print(q0+geom_line())

lmm1<- lmer(WL ~ genotype + background + sex + (1 | rep), Genetic_Background_Interactions)
## BMB: this doesn't really make sense -- assumes that rep=1 reflects a
## consistent thing across backgrounds and sexes.  Makes much more sense to
## treat genotypes and backgrounds as REs ...

#influence of genotype and background and sex on WL with random intercepts for each replicate -> each rep is assigned different intercept value and this mixed model should estimate these intercepts 

#maximal model <- lmer(WL ~ genotype + background + sex + (1 | rep), (1| individual) Genetic_Background_Interactions)
#I did not include individual number as females were numbered 1-10 for each geneotype in each replicate and males were numbered 1-10 for each genotype in each replicate. So individual is nested in replicate but I don't know how to handle when sex also influences individual numbering. In addition it seems to simply be an ID (not my data) and that there would not be an effect to ID.  

summary(lmm1)
#sd shows how much variability in the wing length there is due to replicate. 
#residual show variability that’s not accounted for -> factors that effect wing length that are included in dataset
#Can compare effects of fixed effects. Different genotypes and sex have effects on wing length. Looks like sex has the strongest effect. 
#correlation of fixed effects is not about correlation of variables but correlation of of regression coefficients 

plot(lmm1)
#residuals look to be around 0 so I think this looks good. 
## BMB: you should probably worry about the big outlier here
## (with resid approx -3.5, fitted value about 7.5)

#install.packages("DHARMa")
DHARMa::simulateResiduals(lmm1, plot=TRUE)
#plot looks good does not look underdispersed or overdispersed. 
#I think plots look good. 

## BMB: yes.  (Outlier doesn't make much difference.)

#I think model looks good but I want to tell if random effect is significant. compare models with and without. 
#looks like there are cautions when testing to see if random effect is significant but I am not sure what these are. 
lm1<- lm(WL ~ genotype + background + sex, Genetic_Background_Interactions)

anova(lmm1,lm1)

## BMB: *why* do you want to know whether the RE is significant? What
## is the significance going to tell you about the biology?

#models look very similar. Therefore, replicate as a random effect is not very strong. Even if it is not strong I don't know ( at least yet) of cons for including it. More information makes model make more accurate estimates. At some point models may not converge if you have too many variable but I did not run into this problem, at that point it would make sense to remove variables that do not have strong effect.

## BMB: the main reason not to include it here is that it doesn't make sense (see comments above); but I agree that in general it's a good idea *not* to omit random effects that do make sense from the model, as long as they're not causing problems
