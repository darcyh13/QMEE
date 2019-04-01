library(tidyverse)

##I am reading in the not yet cleaned version. There are some n.a. values in this data set
#Be aware of this in case of error messages. 
#It looks like there are 45 cases where all measurements are missing? Darcy thinks these might be dead flies? 
#there is also one wing missing the L5 measurement and one missing a wing length 

wings <- read_csv("GeneticBackgroundVeinMutantInteractionsFinal.csv")

#For some reason all the lenghts were created as factors and not numberic variables. 
wings$L2 <- as.numeric(wings$L2)
wings$L3 <- as.numeric(wings$L3)
wings$L4 <- as.numeric(wings$L4)
wings$L5 <- as.numeric(wings$L5)
wings$WL <- as.numeric(wings$WL)

#We are going to rescale the vein length relitive to wing length 
#This is making the assumption that the changes in vein length we are interested in are independent of wing size 
#We want to do this step because we do not want to detect allometric changes in vein length.

#I think this error out is because its trying to read the na vals. Need to remove these before I can rescale.. 

swings <- (wings 
          %>% mutate(sL2 = "L2"/"WL")
)
