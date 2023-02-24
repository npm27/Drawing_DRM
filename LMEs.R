##set up
library(readxl)
library(lme4)
library(car)
library(ez)
library(emmeans)
library(lmerTest)
library(bayestestR)

#turn off scientific notation
options(scipen = 999)

#read in the Ex 1 data
dat1 = read_excel("Data/Raw.xlsx", sheet = "EX1")

#read in the EX 2 data
dat2 = read_excel("Data/Raw.xlsx", sheet = "EX2")

##Cleaning
#take summaries of the data -- Let's see what we're working with here
summary(dat1)
summary(dat2)

table(dat1$Item)
table(dat1$`Item Type`)

#fix item type
colnames(dat1)[6] = "Item_Type"
colnames(dat2)[7] = "Item_Type"

#fiX Subject ID
colnames(dat1)[1] = "Sub.ID"
colnames(dat2)[1] = "Sub.ID"

#fix group column
colnames(dat1)[2] = "Group"
colnames(dat2)[2] = "Group"

table(dat2$Group)

#fix relational
dat2$Group[dat2$Group == "relational"] = "Relational"

#do the n's line up?
length(unique(dat1$Sub.ID))
table(dat1$Group) / 40

##Descriptives
tapply(dat1$Response, list(dat1$Item_Type, dat1$Group), mean, na.rm = T)
tapply(dat2$Response, list(dat2$Item_Type, dat2$Group), mean, na.rm = T)

tapply(dat1$Response, dat1$Group, mean)

##I might need to subset -- first just looking at correct recognition of list items
list_items = subset(dat1,
                    dat1$Item_Type == 1)

####EX 1 Models####
#okay, first things first, can I replicate the ANOVA?
ezANOVA(list_items,
        dv = Response,
        between = Group,
        wid = Sub.ID,
        type = 3) #Anova comes out significant! Not the same F though...

tapply(list_items$Response, list_items$Group, mean)

##Re-level group factor so everything is relative to the control
list_items$Group = factor(list_items$Group,
                          levels = c("Read", "BlackDraw", "ColorDraw"))

##Ex 1
#correct recognition of list items
#Build final model
model1 = glmer(Response ~ Group
               + (1|Sub.ID), data = list_items, family = "binomial")

model1.int = glmer(Response ~ 1 + (1|Sub.ID), data = list_items, family = "binomial")

#compare models
bayestestR::bayesfactor_models(model1, denominator = model1.int)

#final model output
summary(model1)
Anova(model1, type = "III")

anova(model1, model1.final)

out1 = emmeans(model1, list(pairwise ~ Group), adjust = "none")

out1$`emmeans of Group`
out1$`pairwise differences of Group`

##false recognition
cl = subset(dat1, dat1$Item_Type == 2)

#recreate the ANOVA
cl$Group = factor(cl$Group,
            levels = c("Read", "BlackDraw", "ColorDraw"))

##Just CLs
ezANOVA(cl,
        dv = Response,
        between = Group,
        wid = Sub.ID,
        type = 3)

tapply(cl$Response, cl$Group, mean)

model4.int = glmer(Response ~ 1 + (1|Sub.ID), data = cl, family = "binomial")
summary(model4.int) #bic 592

model4 = glmer(Response ~ Group
               + (1|Sub.ID), data = cl, family = "binomial") 

summary(model4) #bic 583
anova(model4.int, model4)

Anova(model4, type = "III")

out4 = emmeans(model4, list(pairwise ~ Group), adjust = "none")

out4$`emmeans of Group`
out4$`pairwise differences of Group`

#compare models
bayestestR::bayesfactor_models(model4, denominator = model4.int)

####Ex 2 models####
tapply(dat2$Response, list(dat2$Group, dat2$Item_Type), mean, na.rm = T)

list_items2 = subset(dat2,
                     dat2$Item_Type == 1)

cl2 = subset(dat2,
             dat2$Item_Type == 2)

##reorder factors
cl2$Group = factor(cl2$Group,
                  levels = c("Relational", "Item-Specific"))

list_items2$Group = factor(list_items2$Group,
                           levels = c("Relational", "Item-Specific"))

##Correct recognition
#get means
tapply(list_items2$Response, list_items2$Group, mean, na.rm = T)

summary(list_items2)

list_items2 = na.omit(list_items2)

#Re-run the ANOVA (well it was a t-test in the paper but still...)
ezANOVA(list_items2,
        dv = Response,
        between = Group,
        wid = Sub.ID,
        type = 3)

#linear model time!
model5.int = glmer(Response ~ 1 + (1|Sub.ID), data = list_items2, family = "binomial")

model5 = glmer(Response ~ Group
               + (1|Sub.ID), data = list_items2, family = "binomial")

#compare models
bayestestR::bayesfactor_models(model5, denominator = model5.int)

#final model output
summary(model5)
anova(model5.int, model5)

Anova(model5, type = "III")
emmeans(model5, list(pairwise ~ Group), adjust = "none")

#False recognition
ezANOVA(cl2,
        dv = Response,
        between = Group,
        wid = Sub.ID,
        type = 3)

tapply(cl2$Response, cl2$Group, mean)

#linear model time!
model6.int = glmer(Response ~ 1 + (1|Sub.ID), data = cl2, family = "binomial")

model6 = glmer(Response ~ Group
               + (1|Sub.ID), data = cl2, family = "binomial")

#compare models
bayestestR::bayesfactor_models(model6, denominator = model6.int)

#final model output
summary(model6)
anova(model6.int, model6)

Anova(model6, type = "III")
emmeans(model6, list(pairwise ~ Group), adjust = "none")

#compute BF
BayesFactor::generalTestBF(Response ~ Group, data = cl2, whichRandom = "Sub.ID")
