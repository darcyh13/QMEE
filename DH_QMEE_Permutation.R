#hypothesis (1) - is there differences between genotypes for L3 vein length. I want to look at a specific interesting relationship 
#hypothesis (2) - is there differences between backgrounds across all genotypes for L3 vein length 
#using same tests - brute force permutation, which is efficent but clear to understand and t-test which seems to be a good fit here as I want to compare two means. A t-test is a good way to determine whether there is a significant difference between the means of two groups.
#does it make sense to use others tests for these questions? 

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

#seperate out backgrounds - want to observe individually 
Genetic_Ore_Interactions <-Genetic_Background_Interactions %>% 
  filter(background == "Ore") %>%
  select(L3, genotype) 


Genetic_Sam_Interactions <-Genetic_Background_Interactions %>% 
  filter(background == "Sam") %>%
  select(L3, genotype)


#I want to explore to see if there are real differences between genotypes in each background - does this code suffice? 

install.packages("rcompanion")
library(rcompanion)

Pairwise_ORE <- pairwisePermutationTest(L3 ~ genotype,
                        data = Genetic_Ore_Interactions,
                        method="fdr")
Pairwise_ORE

#difference between aos.rho and wt.rho in ORE
#not many differences between double mutants and single mutation in rho

Pairwise_SAM <-pairwisePermutationTest(L3 ~ genotype,
                        data = Genetic_Sam_Interactions,
                        method="fdr")
Pairwise_SAM 

#differences between aos.rho and wt.rho, Bs.rho and wt.rho, Egfr.rho and wt.rho, S.rho and wt.rho, and Spi.rho and wt.rho in SAM.
#this tells me there is more differences between double mutants and single mutant rho in SAM than in ORE
#I want to take a closer look at one of these relationships - Egfr.rho and wt.rho in each background



#want to look at interactions in both backgrounds 
(ggplot(Genetic_Sam_Interactions,aes(genotype, L3))
  + geom_boxplot()
  + stat_sum(colour="darkgray",alpha=0.5)
  + scale_size(breaks=1:2, range=c(3,6))
  +coord_flip()
)

(ggplot(Genetic_Ore_Interactions,aes(genotype, L3))
  + geom_boxplot()
  + stat_sum(colour="darkgray",alpha=0.5)
  + scale_size(breaks=1:2, range=c(3,6))
  +coord_flip()
)

#confirming Egfr.rho in L3 may be a good one to start with - want to do this with only one gene(that looks interesting) right now 
#Egfr.rho has different effect on L3 than just wt.rho - may enhance rho in SAM but not ORE but is this signifcant?

#Ore background Egfr.rho and wt.rho
set.seed(13) ## for reproducibility
nsim <- 1000
res <- numeric(nsim) ## set aside space for results
for (i in 1:nsim) {
  ## standard approach: scramble response value
  perm <- sample(nrow(Genetic_Ore_Interactions))
  bdat <- transform(Genetic_Ore_Interactions, L3=L3[perm])
  ## compute & store difference in means; store the value
  res[i] <- mean(bdat[bdat$genotype=="Egfr.rho","L3"])-
    mean(bdat[bdat$genotype=="wt.rho","L3"])
}

Egfr_L3_ORE <- Genetic_Ore_Interactions[Genetic_Ore_Interactions$genotype=="Egfr.rho","L3"]
Egfr_L3_ORE <- mean(Egfr_L3_ORE$L3)
wt_L3_ORE <- Genetic_Ore_Interactions[Genetic_Ore_Interactions$genotype=="wt.rho","L3"]
wt_L3_ORE <- mean(wt_L3_ORE$L3)
obs <- Egfr_L3_ORE- wt_L3_ORE
## append the observed value to the list of results
res <- c(res,obs)

hist(res,col="gray",las=1,main="")
abline(v=obs,col="red")


2*mean(res>=obs)  
mean(abs(res)>=abs(obs))

#t-test can only use two levels 

Genetic_Ore_Interactions2 <-Genetic_Ore_Interactions %>% 
  filter( genotype  %in% c("wt.rho", "Egfr.rho"))
  
t.test(L3~genotype,data=Genetic_Ore_Interactions2,var.equal=TRUE)

#not a difference in dataset between wt.rho and Egfr.rho in ORE

#SAM background Egfr.rho and wt.rho
set.seed(13) ## for reproducibility
nsim <- 1000
res <- numeric(nsim) ## set aside space for results
for (i in 1:nsim) {
  ## standard approach: scramble response value
  perm <- sample(nrow(Genetic_Sam_Interactions))
  bdat <- transform(Genetic_Sam_Interactions, L3=L3[perm])
  ## compute & store difference in means; store the value
  res[i] <- mean(bdat[bdat$genotype=="Egfr.rho","L3"])-
    mean(bdat[bdat$genotype=="wt.rho","L3"])
}

Egfr_L3_SAM <- Genetic_Sam_Interactions[Genetic_Sam_Interactions$genotype=="Egfr.rho","L3"]
Egfr_L3_SAM <- mean(Egfr_L3_SAM$L3)
wt_L3_SAM <- Genetic_Sam_Interactions[Genetic_Sam_Interactions$genotype=="wt.rho","L3"]
wt_L3_SAM <- mean(wt_L3_SAM$L3)
obs <- wt_L3_SAM - Egfr_L3_SAM
## append the observed value to the list of results
res <- c(res,obs)

hist(res,col="gray",las=1,main="")
abline(v=obs,col="red")


2*mean(res>=obs)  
mean(abs(res)>=abs(obs))

Genetic_Sam_Interactions2 <-Genetic_Sam_Interactions %>% 
  filter( genotype  %in% c("wt.rho", "Egfr.rho"))

t.test(L3~genotype,data=Genetic_Sam_Interactions2,var.equal=TRUE)


#Confirming again that there is real differences in the L3 vein length between Egfr.rho and wt.rho genotypes in SAM but not ORE. 
#Egfr may act as enhancer of rho in SAM but does not have same interaction in ORE

#I now want to see if there is background effects for L3 between genotypes 


(ggplot(Genetic_Background_Interactions,aes(background, L3))
  + geom_boxplot()
  + stat_sum(colour="darkgray",alpha=0.5)
  + scale_size(breaks=1:2, range=c(3,6))
  +coord_flip()
)

set.seed(13) ## for reproducibility
nsim <- 1000
res <- numeric(nsim) ## set aside space for results
for (i in 1:nsim) {
  ## standard approach: scramble response value
  perm <- sample(nrow(Genetic_Background_Interactions))
  bdat <- transform(Genetic_Background_Interactions, L3=L3[perm])
  ## compute & store difference in means; store the value
  res[i] <- mean(bdat[bdat$background=="Sam","L3"])-
    mean(bdat[bdat$background=="Ore","L3"])
}

Sam_L3 <- Genetic_Background_Interactions[Genetic_Background_Interactions$background=="Sam","L3"]
Sam_L3 <- mean(Sam_L3$L3)
Ore_L3 <- Genetic_Background_Interactions[Genetic_Background_Interactions$background=="Ore","L3"]
Ore_L3 <- mean(Ore_L3$L3)
obs <- Ore_L3 - Sam_L3
## append the observed value to the list of results
res <- c(res,obs)

hist(res,col="gray",las=1,main="")
abline(v=obs,col="red")


2*mean(res>=obs)  
mean(abs(res)>=abs(obs))


t.test_backgrounds <-t.test(L3~background,data=Genetic_Background_Interactions,var.equal=TRUE)
t.test_backgrounds

#there is real differences between backgrounds for L3 length in dataset 
#I want to eventually be able too see how interaction changes between backgrounds - for example is Egfr acting as an enhancer to rho or are differences due to starting points in each background 
#I feel comfortable concluding that there is differences  in L3 length between Egfr.rho and wt.rho genotypes in SAM but not ORE background
#I also feel comfortable concluding that there is differences between SAM and ORE for L3 length when all genotypes are included 