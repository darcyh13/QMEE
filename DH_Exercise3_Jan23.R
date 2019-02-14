library("tidyverse")

Genetic_Background_Interactions <- read_csv("GeneticBackgroundVeinMutantInteractionsFinal.csv")

#Change second gene to say WT if it does not have a second mutation -> get rid of NA in this column 
Genetic_Background_Interactions$gene2[is.na(Genetic_Background_Interactions$gene2)] <- "wt"

#only want to deal with two genes for now, not three
Genetic_Background_Interactions <- mutate(Genetic_Background_Interactions,
                                          genotype=interaction(gene1, rhomboid))

#change relevant columns to factors 
columns_to_factors <- c(1:4,6:7,13)
Genetic_Background_Interactions[,columns_to_factors] <- lapply(Genetic_Background_Interactions[,columns_to_factors], as.factor)

#omit remaining NAs 
Genetic_Background_Interactions <- na.omit(Genetic_Background_Interactions)

#should backgrounds be seperated out -for now yes
Genetic_Ore_Interactions <-Genetic_Background_Interactions %>% 
filter(background == "Ore") %>%
  select(L3, genotype) 


Genetic_Sam_Interactions <-Genetic_Background_Interactions %>% 
  filter(background == "Sam") %>%
  select(L3, genotype)


(ggplot(Genetic_Sam_Interactions,aes(genotype, L3))
  + geom_boxplot()
  + stat_sum(colour="darkgray",alpha=0.5)
  + scale_size(breaks=1:2, range=c(3,6))
)


(ggplot(Genetic_Ore_Interactions,aes(genotype, L3))
  + geom_boxplot()
  + stat_sum(colour="darkgray",alpha=0.5)
  + scale_size(breaks=1:2, range=c(3,6))
)



#Egfr.rho in L3 may be a good one to start with - want to do this with only one gene(that looks interesting) right now 
#will see test gene and then background and gene 


Ore_Egfr <- Genetic_Ore_Interactions %>%
  filter(genotype %in% c("wt.rho", "Egfr.rho"))

Sam_Egfr <- Genetic_Sam_Interactions %>%
  filter(genotype %in% c("wt.rho", "Egfr.rho"))




set.seed(13) ## for reproducibility
nsim <- 1000
res <- numeric(nsim) ## set aside space for results
for (i in 1:nsim) {
  ## standard approach: scramble response value
  perm <- sample(nrow(Ore_Egfr))
  bdat <- transform(Ore_Egfr, L3=L3[perm])
  ## compute & store difference in means; store the value
  res[i] <- mean(bdat[bdat$genotype=="Egfr.rho","L3"])-
    mean(bdat[bdat$genotype=="wt.rho","L3"])
}

Egfr_L3 <- Ore_Egfr[Ore_Egfr$genotype=="Egfr.rho","L3"]
Egfr_L3 <- mean(Egfr_L3$L3)
wt_L3 <- Ore_Egfr[Ore_Egfr$genotype=="wt.rho","L3"]
wt_L3 <- mean(wt_L3$L3)
obs <- Egfr_L3- wt_L3
## append the observed value to the list of results
res <- c(res,obs)

hist(res,col="gray",las=1,main="")
abline(v=obs,col="red")


2*mean(res>=obs)  
mean(abs(res)>=abs(obs))


set.seed(13) ## for reproducibility
nsim <- 1000
res <- numeric(nsim) ## set aside space for results
for (i in 1:nsim) {
  ## standard approach: scramble response value
  perm <- sample(nrow(Sam_Egfr))
  bdat <- transform(Sam_Egfr, L3=L3[perm])
  ## compute & store difference in means; store the value
  res[i] <- mean(bdat[bdat$genotype=="Egfr.rho","L3"])-
    mean(bdat[bdat$genotype=="wt.rho","L3"])
}

Sam_L3 <- Sam_Egfr[Sam_Egfr$genotype=="Egfr.rho","L3"]
Sam_L3 <- mean(Sam_L3$L3)
wt_L3 <- Sam_Egfr[Sam_Egfr$genotype=="wt.rho","L3"]
wt_L3 <- mean(wt_L3$L3)
obs <- Sam_L3- wt_L3
## append the observed value to the list of results
res <- c(res,obs)

hist(res,col="gray",las=1,main="")
abline(v=obs,col="red")

2*mean(res<=obs)  
mean(abs(res)<=abs(obs))


#there is significant differences between genotypes in SAM but not ORE. 

(tt <- t.test(L3~genotype,data=Ore_Egfr,var.equal=TRUE))

(tt <- t.test(L3~genotype,data=Sam_Egfr,var.equal=TRUE))

#there is significant differences between genotypes in SAM but not ORE. 

#now I want to see if theres is a difference between in 



