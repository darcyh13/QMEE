
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

#need this function
named_list <- function (...) 
{
  L <- list(...)
  snm <- sapply(substitute(list(...)), deparse)[-1]
  if (is.null(nm <- names(L))) 
    nm <- snm
  if (any(nonames <- nm == "")) 
    nm[nonames] <- snm[nonames]
  setNames(L, nm)
}

#model L3 (length of vein 3) with effects of genotpe and sex 


#data need to be in list 
#genotype need to converted to numeric 
OREdat2<-with(Genetic_Ore_Interactions,
              named_list(N=nrow(Genetic_Ore_Interactions),
                         ngenotype=length(levels(genotype)),
                         genotype=as.numeric(genotype),
                         sex=as.numeric(sex),
                         L3))


genomodel3 <- function() {
  for (i in 1:N) {
    ## Poisson model
    logmean[i] <- b_genotype[genotype[i]] ## predicted log(counts)
    sexeff[i] <- b_sex*(sex[i]-1) 
    pred[i] <- exp(logmean[i] + sexeff[i])       ## predicted counts
    L3[i] ~ dnorm(pred[i], 0.001)
  }
  ## define priors in a loop
  for (i in 1:ngenotype) {
    b_genotype[i] ~ dnorm(0,0.001)

  }
  
  b_sex ~ dnorm(0,0.001)
}

j3 <- jags(data=OREdat2,
           inits=NULL,n.iter = 50000, n.thin = 100,
           parameters=c("b_genotype", "b_sex"),
           model.file=genomodel3, n.chains = 4)

tidy(j3,conf.int=TRUE, conf.method="quantile")

bb2 <- j3$BUGSoutput  ## extract the "BUGS output" component
mm2 <- as.mcmc.bugs(bb2)  ## convert it to an "mcmc" object that coda can handle

plot(j3)

xyplot(mm2) #I think this looks ok - doesn't look like burnin needs to be adjusted

densityplot(mm2) #This doesn't look perfect but I think it looks OK 

print(dwplot(j3))


#Disscuss prior assumptions 
#set priors to b_sex ~ dnorm(0,0.001) and  b_genotype[i] ~ dnorm(0,0.001)
#assuming a normal distrubution 

lm_geno_L3 <- lm(L3 ~ genotype + sex , data= Genetic_Ore_Interactions)
plot(lm_geno_L3)

#scanned literature and could not find research that would allow me to make sensible priors. Therefore, I chose priors that would have minimal impact. With little knowledge about previous work I think these are safe priors. Better to be safe than assume wrong.  
#plots from linear model look like a good fit and although bayesian approach is a powerful appraoch I don't think I used it in a powerful sense. For example, priors were not specific but rather safe choices. To my understanding, a bayesian model with uninformative priors will not differ drastically (if at all) from maximum likelihood because data is not being strongly influenced by prior.I think this approach is a better fit when you have reason for strong priors . 


