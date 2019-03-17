##March 13th 

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

#install.packages("Matrix")
library(lme4)

#shows that wing length was measured in each genotype and each cross was preformed in four replicates. Therefore replicate number may be a possible random effect.  
q0 <- (ggplot(Genetic_Background_Interactions, aes(genotype, WL))
       +facet_wrap(~rep)
       + geom_point())  
      
print(q0+geom_line())

lmm1<- lmer(WL ~ genotype + background + sex + (1 | rep), Genetic_Background_Interactions)

summary(lmm1)

plot(lmm1)

anova(lmm1)

#install.packages("DHARMa")
DHARMa::simulateResiduals(lmm1, plot=TRUE)


