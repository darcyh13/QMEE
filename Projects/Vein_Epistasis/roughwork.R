library(lme4)
library(car)
library(effects)
library(emmeans)

#After meeting with Ian, I decided to take another crack at this. 
#He reminded me that what we are interested in is the diffrences between the backgrounds not the specific genetic interactions. 

#going to subset for a single gene and then work up from there. 
#Goin gto look at Aos because thats easier to subset. 
with(wings, table(gene1, gene2))

only1 <- subset(wings, gene1 == "Aos")
onlywt <- subset(wings, gene1 == "wt")
only1 <- rbind(only1, onlywt)

#linear model 
#Its not a great fit (of course) but it works for now 
mod1 <- lm(L2s ~ gene1*background, data = only1)
summary(mod1)
plot(mod1)

#can I do this for all the gene 1s?
#this seems to work ok. 
gene1only <- subset(wings, gene2 == "wt")
mod2 <-  lm(L2s ~ gene1*background, data = gene1only)
summary(mod2)
plot(mod2)

#Lets move this into a mixed model because that is the easier way to fit more effeects. 

mod3 <- lmer(L2s ~ background*gene1 + (1|sex) + (1|rep), data = gene1only)
summary(mod3)
VarCorr(mod3)
plot(mod3)
Anova(mod3)


##What if I take out sex, shouldn't matter much because of rescaling. 
#This does look more shitty. 
#but this anova makes sense. I think this is the right model (or at least close to it?)
mod4 <- lmer(L2s ~ background*gene1 + (1|rep), data = gene1only)
summary(mod4)
VarCorr(mod4)
plot(mod4)
#I think this is the right test. 
Anova(mod4)
plot(allEffects(mod4))

#This is sort of what I want. Except I want everything relitive to wt?
mod4eff <- emmeans(mod4, specs = c("background", "gene1"))
plot(mod4eff)



#What if I fit gene1 as a random effect?
#This is not right because there is no interaction term 
mod5 <- lmer(L2s ~ (background|gene1) +(1|rep), data = gene1only)
summary(mod5)

#This gives a sigular fit. That makes a lot of sense because alot of the varriance in background is created by these effects 
mod6 <- lmer(L2s ~ background*gene1 + (1+ background |sex) + (1+ background|rep), data = gene1only)

#Took out sex becasue I don't know how much I need to consider it here 
#It is still a sigular fit. 
mod7 <- lmer(L2s ~ background*gene1 + (1+ background|rep), data = gene1only)

