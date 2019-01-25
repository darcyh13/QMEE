library("tidyverse")
setwd("~/Data")
Genetic_Background_Interactions <- read_csv("GeneticBackgroundVeinMutantInteractionsFinal.csv")

#Change second gene to say WT if it does not have a second mutation -> get rid of NA in this column 
Genetic_Background_Interactions$gene2[is.na(Genetic_Background_Interactions$gene2)] <- "wt"

#change relevant columns to factors 
columns_to_factors <- c(1:3,6:7)
Genetic_Background_Interactions[,columns_to_factors] <- lapply(Genetic_Background_Interactions[,columns_to_factors], as.factor)

#omit remaining NAs 
Genetic_Background_Interactions <- na.omit(Genetic_Background_Interactions)

#should backgrounds be seperated out?
Genetic_Ore_Interactions <-Genetic_Background_Interactions %>% 
filter(background == "Ore")

Genetic_Sam_Interactions <-Genetic_Background_Interactions %>% 
  filter(background == "Sam")

#want to see if gene combination has effect - dont know how to group by Gene1 and Gene2 - combine columns?

Genetic_Background_Interactions$genotype <- paste(Genetic_Background_Interactions$gene1,Genetic_Background_Interactions$gene2)
Genetic_Background_Interactions

#make boxplot with both genes for L2
#make multiple for each vein and wing length - facet wrap

Genetic_Background_Interactions2 <- gather(Genetic_Background_Interactions, key="measure", value="value", c("L2", "L3","L4", "L5","WL"))

WL_Genotype <- (ggplot(Genetic_Background_Interactions2,aes(x=genotype,y=value,
                                                           colour=background))
                + geom_boxplot(outlier.colour=NULL)  ## set outlier points to same colour as boxes
                + labs(y="L2 Vein Length")
                +theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
                +facet_wrap(~ measure, scales="free")
                +ggtitle("Genetic Background Interactions in Vein Mutations")
                +theme(plot.title = element_text(size=10, face="bold", 
                                                 margin = margin(10, 0, 10, 0))) +
                  theme(plot.background = element_rect(fill = 'grey'))
                
)

print(WL_Genotype)

#lets take closer look at relationship identified from boxplot 
#bar graph with Egfr_Bs, Egfr_wt, Bs_wt, wt
#should be able to figure out way to facet_wrap with all veins and wing length

#lets look at L4 - looked interesting in boxplot 

Egfr_Bs_values %>% 
  group_by(genotype, background) %>% 
  summarise(Average_L4_Length = mean(L4)) %>% 
  ungroup() %>% 
  
  
  ggplot(mapping = aes(x=genotype, y=Average_L4_Length, fill = genotype)) +
  geom_col() +
  scale_y_continuous(name = "Average L4 Length") +
  scale_x_discrete("Genotype") +
  facet_grid(.~background) +
  ggtitle('Genetic Background Interactions in Vein Mutations') +
  theme(plot.background = element_rect(fill = 'grey'))


#something interesting in Sam background - doesn't appear to be additive - suppressor effect - but is this signifcant? 




