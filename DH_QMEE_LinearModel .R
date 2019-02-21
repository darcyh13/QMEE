
library("tidyverse")

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

#subset Ore 
Genetic_Ore_Interactions <-Genetic_Background_Interactions %>% 
  filter(background == "Ore")


#hypothesis (1) Does genotype effect L3 in ORE 

Genetic_Ore_Interactions$genotype <- relevel(Genetic_Ore_Interactions$genotype, "wt.rho")
lm_geno_L3 <- lm(L3 ~ genotype + sex + genotype:sex , data= Genetic_Ore_Interactions)


summary(lm_geno_L3)
#RSE model is 0.3486 so you can say that you can predict L3 vein length on an individual with accuracy of 0.3486 with this model 
#median is around 0 - this shold be the case of residuals 
#intercept ~5 
#also gives estimated slope 
#for all of the observed variation for L3 in dataset this model can account for 46.77% of variation


#this lets me see if genotype as a whole and sex together have influence rather than looking at each genotype and sex
drop1(lm_geno_L3, test = "F")

#residuals vs fitted - these are residuals for your response vs the fitted values for your response - can let you see if there is patterns of non-linearity in data. In this case red line is pretty much straight, so it looks linear. 
#normal Q-Q plot - standardized residuals versus theoretical residuals -> what should be expected if they were normally distributed - residuals follow line well 
#Scale-Location - the square root of standardized residuals against fitted values -this plot shows if residuals are spread equally along the ranges of predictors - red line is horizontal so it looks good 
#residuals vs leverage plot - how much potential influence can each point have - cook's distance says how far out are these distances - plot helps us to find influential cases if points occur outside cook's distance 
par(mfrow = c(2,2))
plot(lm_geno_L3)

#linear models looks to be a good fit 

#this makes boxplot of L3 and genotype and a boxplot of L3 and sex - you can see both may have effects on L3
par(mfrow=c(2,1))
plot(L3 ~ genotype + sex + genotype:sex, data = Genetic_Background_Interactions)
abline(lm_geno_L3)

#install.packages("dotwhisker")
library(dotwhisker)
dwplot(lm_geno_L3)
#used to visualize regression model 
#you can see possible effects of genotypes and genotypes with sex 

#emmeans lets me see effects genotype has on L3 when accounting for sex
#Aos.rho has largest vein 3 length
#shows that genotype can have influence on L3 length 
library(emmeans)
emmeans_test <- emmeans(lm_geno_L3, "genotype")
plot(emmeans_test, comparisons = TRUE)  


#emmeans plot agrees with my hypothesis that genotypes influence length of vein 3 in Ore

#how to check for unaccounted for variation 
#shows what happens when data is permuted 
#complete independence of observations? 
acf(resid(lm_geno_L3))




