####Run R1's suggested analyses####
####Set up####
##libraries
library(ez)
library(psychReport)
library(reshape)

##scientific notation
options(scipen = 999)

####Experiment 1####
mixed = read.csv("EX1/mixed.csv")
pure = read.csv("Ex1/pure.csv")

##okay, ANOVA looking only the related pairs...
mixed.r = subset(mixed,
                 mixed$Direction == "F")
pure.r = subset(pure,
                pure$Direction == "F")

pure.r = pure.r[ , -c(7:8)]

pure.r$List_Type = rep("Pure")

##now combine the related pairs
all.r = rbind(mixed.r, pure.r)

#2 (JOL vs. read only) x 2 (mixed vs. pure) 
#3 (JOL vs. freq. vs read) x 2(mixed vs. pure)

model = ezANOVA(all.r,
                between = .(Encoding, List_Type),
                wid = id,
                dv = Scored,
                type = 3,
                detailed = T)

model$ANOVA$MSE = model$ANOVA$SSd/model$ANOVA$DFd
model$ANOVA$MSE

model #F = 17; interaction < 1, p = .48

psychReport::aovEffectSize(model, effectSize = "pes") #.15

#sig main effect of encoding group
tapply(all.r$Scored, all.r$Encoding, mean)
tapply(all.r$Scored, list(all.r$Encoding, all.r$List_Type), mean)

#t-tests
all.r2 = cast(all.r[ , c(1:4,6:7,5)], id ~ Encoding, mean)

#jol vs freq
temp = t.test(all.r2$JOL, all.r2$Frequency, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp #p = .46

#jol vs read
temp = t.test(all.r2$JOL, all.r2$READ, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp #sig! p < .001

#freq vs read
temp = t.test(all.r2$Frequency, all.r2$READ, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp #sig! p < .001

##Run the one-way ANOVA
ezANOVA(pure.r,
        dv = Scored,
        wid = id,
        between = Encoding,
        type = 3,
        detailed = T) #Comes out Sig, as expected.

##So, the exact same pattern occurs for related pairs in both experiments -- making JOLs/freqs boosts recall realtive to a read only condition.

####Experiment 2####
mixed = read.csv("EX2/mixed.csv")
pure = read.csv("Ex2/pure.csv")

##okay, ANOVA looking only the related pairs...
mixed.r = subset(mixed,
                 mixed$Direction == "B")
pure.r = subset(pure,
                pure$Direction == "B")

pure.r = pure.r[ , -c(7:8)]

pure.r$List_Type = rep("Pure")

##now combine the related pairs
all.r = rbind(mixed.r, pure.r)

#2 (JOL vs. read only) x 2 (mixed vs. pure) 
#3 (JOL vs. freq. vs read) x 2(mixed vs. pure)
model = ezANOVA(all.r,
                between = .(Encoding, List_Type),
                wid = id,
                dv = Scored,
                type = 3,
                detailed = T)

model$ANOVA$MSE = model$ANOVA$SSd/model$ANOVA$DFd
model$ANOVA$MSE

model #looks like the same pattern as Ex 1 #F = 9.36; int < 1, p = .90

psychReport::aovEffectSize(model, effectSize = "pes") #.07

#sig main effect of encoding group
tapply(all.r$Scored, all.r$Encoding, mean)
tapply(all.r$Scored, list(all.r$Encoding, all.r$List_Type), mean)

#t-tests
all.r2 = cast(all.r[ , c(1:4,6:7,5)], id ~ Encoding, mean)

#jol vs freq
temp = t.test(all.r2$JOL, all.r2$Frequency, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp #p = .54

#jol vs read
temp = t.test(all.r2$JOL, all.r2$READ, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp #sig! p < .001

#freq vs read
temp = t.test(all.r2$Frequency, all.r2$READ, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp #sig! p < .001

#Yep, the same pattern w/ backward pairs

##Run the one-way ANOVA
model = ezANOVA(pure.r,
        dv = Scored,
        wid = id,
        between = Encoding,
        type = 3,
        detailed = T) #Comes out Sig, again (p = .02)

psychReport::aovEffectSize(model, effectSize = "pes")

####Experiment 3####
mixed = read.csv("EX3/mixed.csv")
pure = read.csv("Ex3/pure.csv")

##okay, ANOVA looking only the related pairs... I'm convinced this will blow up
mixed.r = subset(mixed,
                 mixed$Direction == "S")
pure.r = subset(pure,
                pure$Direction == "S")

pure.r = pure.r[ , -c(7:8)]

pure.r$List_Type = rep("Pure")

##now combine the related pairs
all.r = rbind(mixed.r, pure.r)

#2 (JOL vs. read only) x 2 (mixed vs. pure) 
#3 (JOL vs. freq. vs read) x 2(mixed vs. pure)
model = ezANOVA(all.r,
                between = .(Encoding, List_Type),
                wid = id,
                dv = Scored,
                type = 3,
                detailed = T)

model$ANOVA$MSE = model$ANOVA$SSd/model$ANOVA$DFd
model$ANOVA$MSE

model #looks like the same pattern as Ex 1; int < 1, p = .54

#sig main effect of encoding group
tapply(all.r$Scored, all.r$Encoding, mean)
tapply(all.r$Scored, all.r$List_Type, mean)
tapply(all.r$Scored, list(all.r$Encoding, all.r$List_Type), mean)

#t-tests
all.r2 = cast(all.r[ , c(1:4,6:7,5)], id ~ Encoding, mean)

#jol vs freq
temp = t.test(all.r2$JOL, all.r2$Frequency, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp #p = .42

#jol vs read
temp = t.test(all.r2$JOL, all.r2$READ, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp #sig! p < .001

#freq vs read
temp = t.test(all.r2$Frequency, all.r2$READ, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp #sig! p < .001

#And the pattern occurs w/ symmetrical pairs
##Run the one-way ANOVA
ezANOVA(pure.r,
        dv = Scored,
        wid = id,
        between = Encoding,
        type = 3,
        detailed = T) #and again
