library(tidyverse)

#Loading in data, created earlier 
load("clean_epistasis.rda")


#I want to do a PC analysis on the vein data to ask the question:
#is it a good idea to consider each vein seperatley? Can we consider them all together as one 'supervein' 
#If there is not a lot of varriation within PCs, we can conclude that all four veins respond in the same way to a genetic pertubation
#Using the scaled values because allometric effects have been controled for. 
#Decided to leave the scaling set as false (defult) because I had already scaled all the vein lengths to the length of the wing.
#I did also run these tests with the resacling step in the PC decomposition and the same pattrens were observed.

PCs <- prcomp(wings[,14:17], scale. = F)

#Going to plot the PCs to get an idea of where the inflection point is. 
#Part of what we want to know is if the varriation in any two veins is in the same direction (if this is true, we would expect to see one PC explaining much less of the varriance)
#The above senario might be the case as PC4 (and possibly PC3) account for less of the varrance than PC1 and PC2. 
#By a visual check alone, you might say these are after the inflection point. However, more invesitigation is needed to say this with confidence. 
plot(PCs)

#I want to see how well the PCs can seperate the diffrent veins/treatments 
#First, I will add the PCs the the data frame 
wings_combined <- data.frame(wings, PCs$x[,1:4]) 


#This shows that the background seperates out pretty well, but sex doesn't seem to matter much (this makes sense to me, we rescaled for size already)
#However, there is a lot more varriance (mostly captured in PC1), in the ORE background as compared to the SAM. 
#This is supported by previous work that demonstrated that the ORE background was more senesitive than SAM to genetic pretubations in *scalloped*, at least in relation to size (Chari and Dworkin 2013).
#However, this clear seperation of background effects was unexpected. I showed it to Ian and he also did not have a good explanation for what was going on. 

ggplot(wings_combined, aes(x = PC1, y = PC2, 
                           col = background, shape = sex)) +
  geom_point()



#Going to look at genotype and background insted
#This is a lot less clean, the most clean seperation is between the background and not the genotype. 
ggplot(wings_combined, aes(x = PC1, y = PC2, col = genotype, 
                           shape = background)) +
                           geom_point()

#Now I will compare the other PCs
#The SAM background still seems to cluster. 
#This indicates that at least in the SAM background, all the vein lengths behave the same in response to genetic pertubations. 
ggplot(wings_combined, aes(x = PC1, y = PC3, col = genotype, shape = background)) +
  geom_point()
ggplot(wings_combined, aes(x = PC1, y = PC4, col = genotype, shape = background)) +
  geom_point()
ggplot(wings_combined, aes(x = PC2, y = PC3, col = genotype, shape = background)) +
  geom_point()
ggplot(wings_combined, aes(x = PC2, y = PC4, col = genotype, shape = background)) +
  geom_point()
#This is all overlaping. Not suprising since this is PC3 and PC4, capturing very little varriation. 
ggplot(wings_combined, aes(x = PC3, y = PC4, col = genotype, shape = background)) +
  geom_point()

#For this test, I resacled to overall wing length before running the PCA. Therefore, we do not expect varriantion in wing length to be captured in any one of the PCs
#However, we know that there is an allometric relationship between size and shape in the fly wing. 
#Because SAM wings are just bigger than ORE wings, I want to ask if this seperation we observe is just due to the allometric effect of SAM wings being larger than ORE
ggplot(wings_combined, aes(x = WL, y = PC1, col = background, shape = background)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = lm)
ggplot(wings_combined, aes(x = WL, y = PC2, col = background, shape = background)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = lm)
ggplot(wings_combined, aes(x = WL, y = PC3, col = background, shape = background)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = lm)
ggplot(wings_combined, aes(x = WL, y = PC4, col = background, shape = background)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = lm)
