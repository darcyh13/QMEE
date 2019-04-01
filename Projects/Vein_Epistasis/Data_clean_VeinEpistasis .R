
library("tidyverse")
Vein_epistasis <- read_csv("GeneticBackgroundVeinMutantInteractionsFinal.csv")

#Change second gene to say wt if it does not have a second mutation -> get rid of NA in this column 
Vein_epistasis$gene2[is.na(Vein_epistasis$gene2)] <- "wt"

#change relevant columns to factors 
columns_to_factors <- c("background", "gene1", "gene2", "rhomboid", "sex", "individual")
Vein_epistasis[,columns_to_factors] <- lapply(Vein_epistasis[,columns_to_factors], as.factor)

#omit NAs - indicates dead flies 
Vein_epistasis <- na.omit(Vein_epistasis)

#make list of what occurs with what and how many times (Checks for duplicates)
print(Vein_epistasis
      %>% group_by(background, individual,rep,gene1, gene2, L2, L3, L4, L5,WL, sex)
      %>% summarize(count = n())
      %>% filter(count>1)
)

#looks like no duplicates

#want to make a new column with genotype 
Vein_epistasis <- mutate(Vein_epistasis,
                                          genotype=interaction(gene1, gene2,rhomboid))

#checking for any obvious outliers

#hist
Vein_epistasis %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

#might be outliers in L5 and WL 

#boxplot for L5
(ggplot(Vein_epistasis,aes(genotype, L5))
  + geom_boxplot()
  + stat_sum(colour="darkgray",alpha=0.5)
  + scale_size(breaks=1:2, range=c(3,6))
)

#remove outlier 
Vein_epistasis<- subset(Vein_epistasis, Vein_epistasis[11]<5)

#double check outlier has been removed 

(ggplot(Vein_epistasis,aes(genotype, L5))
  + geom_boxplot()
  + stat_sum(colour="darkgray",alpha=0.5)
  + scale_size(breaks=1:2, range=c(3,6))
)

#boxplot for WL

(ggplot(Vein_epistasis,aes(genotype, WL))
  + geom_boxplot()
  + stat_sum(colour="darkgray",alpha=0.5)
  + scale_size(breaks=1:2, range=c(3,6))
)

#remove outlier 

Vein_epistasis<- subset(Vein_epistasis, Vein_epistasis[12]>4)

#double check outlier has been removed 

(ggplot(Vein_epistasis,aes(genotype, WL))
  + geom_boxplot()
  + stat_sum(colour="darkgray",alpha=0.5)
  + scale_size(breaks=1:2, range=c(3,6))
)


#For some reason all the lenghts were created as factors and not numberic variables. 

Veins_numeric <- c("L2", "L3", "L4", "L5", "WL")
Vein_epistasis[,Veins_numeric] <- lapply(Vein_epistasis[,Veins_numeric], as.numeric)


Vein_epistasis_scale <- (Vein_epistasis
  %>% mutate(L2s = L2/WL)
  %>% mutate(L3s = L3/WL)
  %>% mutate(L4s = L4/WL)
  %>% mutate(L5s = L5/WL)
)

wings <- Vein_epistasis_scale

#We can add additional code here to filter for specific genes when we want to 

 
write.csv(Vein_epistasis, file ="Vein_epistasis_clean.csv")
save(wings, 
     file = "Projects/Vein_Epistasis/clean_epistasis.rda")

