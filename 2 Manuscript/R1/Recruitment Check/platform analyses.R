####Did Recall rates differ based on recruitment platform?####
##load libraries
library(ez)
library(reshape)

options(scipen = 999)

####Experiment 1 -- F vs U####
##load ex1 data
mixed = read.csv("EX1/mixed.csv")
pure = read.csv("Ex1/pure.csv")

##Mixed lists
#Did recall rates differ by recruitment platform?
model = ezANOVA(mixed,
        dv = Scored,
        wid = id,
        between = .(Platform, Encoding),
        within = Direction,
        type = 3,
        detailed = T)

model$ANOVA$MSE = model$ANOVA$SSd/model$ANOVA$DFd
model$ANOVA$MSE

model

length(unique(mixed$id))

#no effect of platform, no interactions w/ platform

##pure lists
model = ezANOVA(pure,
        dv = Scored,
        wid = id,
        between = .(Platform, Encoding, Direction),
        type = 3,
        detailed = T)

model$ANOVA$MSE = model$ANOVA$SSd/model$ANOVA$DFd
model$ANOVA$MSE

model

#again, no effects of recruitment platform

####Experiment 2 -- B vs U####
##load ex2 data
mixed = read.csv("EX2/mixed.csv")
pure = read.csv("Ex2/pure.csv")

##Mixed lists
#Did recall rates differ by recruitment platform?
model = ezANOVA(mixed,
        dv = Scored,
        wid = id,
        between = .(Platform, Encoding),
        within = Direction,
        type = 3,
        detailed = T)

model$ANOVA$MSE = model$ANOVA$SSd/model$ANOVA$DFd
model$ANOVA$MSE

model

#no interactions w/ platform. Main effect of platform was marginal
tapply(mixed$Scored, mixed$Platform, mean, na.rm = T) #Overall recall was a bit higher for Prolific participants
tapply(mixed$Scored, list(mixed$Platform, mixed$Direction), mean, na.rm = T) #occurred equally for both pair types

##pure lists
model = ezANOVA(pure,
        dv = Scored,
        wid = id,
        between = .(Platform, Encoding, Direction),
        type = 3,
        detailed = T)

model$ANOVA$MSE = model$ANOVA$SSd/model$ANOVA$DFd
model$ANOVA$MSE

model

##No effect or interactions w/ platform
tapply(pure$Scored, pure$Platform, mean, na.rm = T)

####Experiment 3 -- S vs U####
##load the ex3 data
mixed = read.csv("Ex3/mixed.csv")
pure = read.csv("Ex3/pure.csv")

##Mixed lists
#Did recall rates differ by recruitment platform?
model = ezANOVA(mixed,
        dv = Scored,
        wid = id,
        between = .(Platform, Encoding),
        within = Direction,
        type = 3,
        detailed = T)

model$ANOVA$MSE = model$ANOVA$SSd/model$ANOVA$DFd
model$ANOVA$MSE

model

#nope!

tapply(mixed$Scored, mixed$Platform, mean, na.rm = T)

##Pure lists
model = ezANOVA(pure,
        dv = Scored,
        wid = id,
        between = .(Platform, Encoding, Direction),
        type = 3,
        detailed = T)

model$ANOVA$MSE = model$ANOVA$SSd/model$ANOVA$DFd
model$ANOVA$MSE

model

psychReport::aovEffectSize(model, effectSize = "pes")

tapply(pure$Scored, pure$Platform, mean, na.rm = T)

#Platform by encoding interaction. Everything else ns
tapply(pure$Scored, list(pure$Platform, pure$Encoding), mean, na.rm = T)
tapply(pure$Scored, list(pure$Platform, pure$Encoding), mean, na.rm = T)
#JOL prolific participants did worse for some reason

##do the t-tests
#Wrangle the data into the right shape!
sub1 = subset(pure, pure$Encoding == "JOL")

sub2 = cast(sub1[ , c(1:4, 6:8, 5)], id ~ Platform, mean)

#run the test [UPDATE THIS CODE!]
temp = t.test(sub2$Prolific, sub2$USM, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.9

#get d
mean(sub2$USM, na.rm = T); mean(sub2$Prolific, na.rm = T)
sd(sub2$USM, na.rm = T); sd(sub2$Prolific, na.rm = T)

##get t's for other comparisons
#Freq
sub3 = subset(pure, pure$Encoding == "Frequency")

sub4 = cast(sub3[ , c(1:4, 6:8, 5)], id ~ Platform, mean)

temp = t.test(sub4$Prolific, sub4$USM, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp

##Pbic here


#Read
sub5 = subset(pure, pure$Encoding == "READ")

sub6 = cast(sub5[ , c(1:4, 6:8, 5)], id ~ Platform, mean)

temp = t.test(sub6$Prolific, sub6$USM, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp

##Pbic here
pbic1 = sub4[ , c(1,2)]
pbic2 = sub4[ , c(1,3)]

pbic1$source = rep("prolific")
pbic2$source = rep("USM")

colnames(pbic1)[2] = "Score"
colnames(pbic2)[2] = "Score"

pbic1 = na.omit(pbic1)
pbic2 = na.omit(pbic2)

pbic3 = rbind(pbic1, pbic2)

model = ezANOVA(pbic3,
                dv = Score,
                wid = id,
                between = (source),
                type = 3,
                detailed = T)

model$ANOVA$MSE = model$ANOVA$SSd/model$ANOVA$DFd
model$ANOVA$MSE

model

##how many JOL prolific participants were there?
table(pure$Platform) / 80 #51 total pure prolific participants
table(list(pure$Platform, pure$Encoding)) / 80 #24 pure JOL Prolific peeps

pure_jol_prolific = subset(pure,
                           pure$Platform == "Prolific" & pure$Encoding == "JOL")

#Check for weirdos
weirdos = cast(pure_jol_prolific[ , c(1:4, 6:8, 5)], id ~ Direction, mean)

##Okay, this is because almost all of them were in the Unrelated condition
tapply(pure_jol_prolific$Scored, pure_jol_prolific$Direction, mean, na.rm = T)
