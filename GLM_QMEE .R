
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


#make a generalized linear model for data 

obs <- Genetic_Background_Interactions$L2

hist(obs, col="black")

qqnorm(obs)

plot(density(obs),main="Density estimate of data")

curve(dgamma(x, scale=1.5, shape=2),from=0, to=15, main="Gamma
distribution")

#looks like gamma might be a good fit

Genetic_Ore_Interactions$genotype <- relevel(Genetic_Ore_Interactions$genotype, "wt.rho")
glm_L2 <- glm(L2 ~ genotype + sex + genotype:sex , data= Genetic_Ore_Interactions,  family = Gamma(link = "log") )

plot(glm_L2)
#plots look OK - fit seems reasonable 

summary(glm_L2)
#looks like some genotypes effects L2 negatively while some impact postitively 
#Bs.rho genotype effects L2 signficantly negatively compared to wt.rho genotype- does it act as an enhancer? 
#S.rho genotype effects L2 signficantly negatively compared to wt.rho genotype- does it act as an enhancer? 
#Aos.rho genotype effects L2 signficantly positively compared to wt.rho genotype - does aos suppress rho? 
#if flies are male it effects L2 negatively 

#value of 8.7438 on 474 degrees of freedom. Including sex and genotype reduced deviance to 4.6330 on 463 degrees of freedom. 

dwplot(glm_L2)
#you can again see that sex and genotype Aos.rho influence L2 

acf(residuals(glm_L2))

#install.packages("ResourceSelection")
library(ResourceSelection)

hoslem.test(Genetic_Ore_Interactions$L2, fitted(glm_L2))

#Model looks to fit because there is no significant difference between the model and the observed data - p-value of 1




