###Set up####
source("Ex 4 Data Cleaning.R")

##Read in the Pure U data
F3 = read.csv("Pure Unrelated/FREQ_PURE_U.csv")
J3 = read.csv("Pure Unrelated/JOL_PURE_U.csv")
R3 = read.csv("Pure Unrelated/Study_Pure_U.csv")

####Get n's####
##1 = Mixed
##2 = Pure S
##3 = Pure U

#Frequency
length(unique(F1$id)) #33 #Mixed
length(unique(F2$id)) #36 #Pure S
length(unique(F3$id)) #37 #Pure U

#JOL
length(unique(J1$id)) #35 #Mixed
length(unique(J2$id)) #33 #Pure S
length(unique(J3$id)) #37 #Pure U

#Study
length(unique(R1$id)) #35 #Mixed
length(unique(R2$id)) #35 #Pure S
length(unique(R3$id)) #35 #Pure U

#Fix colnames
colnames(F3)[3] = "Freq"

####Arrange data for Models####
##Combine on Encoding strategy
FREQ2 = rbind(F1, F2, F3)
JOL2 = rbind(J1, J2, J3)
Study2 = rbind(R1, R2, R3)

####Make ANOVA DATASETS####
Final = rbind(FREQ2[ , -3], JOL2[ , -3], Study2)

mixed = subset(Final,
               Final$List_Type == "Mixed")

Pure_U = subset(Final,
                Final$List_Type == "Pure U")
Pure_B = subset(Final,
                Final$List_Type == "Pure B")

Pure_U$List_Type2 = rep("Pure U")
Pure_B$List_Type2 = rep("Pure B")

##Make pure data
pure = rbind(Pure_B, Pure_U)

##Get means
tapply(mixed$Scored, list(mixed$Encoding, mixed$Direction), mean)
tapply(pure$Scored, list(pure$Encoding, pure$Direction), mean)

####ANOVAS####
##Mixed lists
model5 = ezANOVA(mixed,
                 wid = id,
                 between = Encoding,
                 within = Direction,
                 dv = Scored,
                 return_aov = T,
                 type = 3,
                 detailed = T)
model5

model5$ANOVA$MSE = model5$ANOVA$SSd/model5$ANOVA$DFd
model5$ANOVA$MSE

aovEffectSize(model5, effectSize = "pes")

length(unique(mixed$id))

##Pure lists
model6 = ezANOVA(pure,
                 wid = id,
                 between = .(Encoding, Direction),
                 dv = Scored,
                 return_aov = T,
                 type = 3,
                 detailed = T)

model6

length(unique(pure$id))

model6$ANOVA$MSE = model6$ANOVA$SSd/model6$ANOVA$DFd
model6$ANOVA$MSE

aovEffectSize(model6, effectSize = "pes")

####Post-hocs####
##Let's do the mixed lists first
mixed = mixed[ , c(1, 2, 3, 4, 6, 5)]

#main effect of direction
tapply(mixed$Scored, mixed$Direction, mean) #This will be sig based on ANOVA

#Don't need to run t-test here because only two comparisons

#main effect of encoding #Non-sig, but breaking it down anyways
tapply(mixed$Scored, mixed$Encoding, mean)

mixed.encoding = cast(mixed, id ~ Encoding, mean)

##JOL vs FREQ
temp = t.test(mixed.encoding$Frequency, mixed.encoding$JOL, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-sig

##FREQ VS READ
temp = t.test(mixed.encoding$Frequency, mixed.encoding$READ, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-sig, p = .13

#JOL vs READ
temp = t.test(mixed.encoding$JOL, mixed.encoding$READ, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #non-sig, p = .28

#Nothing differs

##Interaction was sig, let's break it down
tapply(mixed$Scored, list(mixed$Encoding, mixed$Direction), mean)

#setup
mixed.jol = subset(mixed, mixed$Encoding == "JOL")
mixed.freq = subset(mixed, mixed$Encoding == "Frequency")
mixed.read = subset(mixed, mixed$Encoding == "READ")

mixed.jol2 = cast(mixed.jol, id ~ Direction, mean)
mixed.freq2 = cast(mixed.freq, id ~ Direction, mean)
mixed.read2 = cast(mixed.read, id ~ Direction, mean)

#jol descriptives
apply(mixed.jol2, 2, mean, na.rm = T)

x = apply(mixed.jol2, 2, sd, na.rm = T)
x / sqrt(length(unique(mixed.jol2$id))) * 1.96

#freq descriptives
apply(mixed.freq2, 2, mean, na.rm = T)

x = apply(mixed.freq2, 2, sd, na.rm = T)
x / sqrt(length(unique(mixed.freq2$id))) * 1.96

#read descriptives
apply(mixed.read2, 2, mean, na.rm = T)

x = apply(mixed.read2, 2, sd, na.rm = T)
x / sqrt(length(unique(mixed.read2$id))) * 1.96

#run the t-tests
#Symmetrical pairs
#JOL vs FREQ
temp = t.test(mixed.jol2$S, mixed.freq2$S, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-sig, p =.61

##JOL vs READ
temp = t.test(mixed.jol2$S, mixed.read2$S, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #sig, p = .008

##FREQ vs READ
temp = t.test(mixed.freq2$S, mixed.read2$S, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Significant! p =.004

##Symmetrical pairs: Freq and Read significantly differ, JOLs and read sig

#unrelated PAIRS
#JOL VS FREQ
temp = t.test(mixed.jol2$U, mixed.freq2$U, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-sig

#JOL VS READ
temp = t.test(mixed.jol2$U, mixed.read2$U, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #non-sig

##pbic
pbic1 = mixed.jol2[ , c(1, 3)]
pbic1$task = rep("jol")

pbic2 = mixed.read2[ , c(1, 3)]
pbic2$task = rep("read")

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        dv = U,
        wid = id,
        between = task,
        detailed = T)

##FREQ VS READ
temp = t.test(mixed.freq2$U, mixed.read2$U, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-sig

##For mixed lists, Recall of unrelated pairs doesn't differ as a function of encoding group

####Now for the pure lists####
pure = pure[ , c(1, 2, 3, 4, 6, 5)]

#main effect of direction
tapply(pure$Scored, pure$Direction, mean) #This will be sig based on ANOVA

#Don't need to run t-test here because only two comparisons

#main effect of encoding
tapply(pure$Scored, pure$Encoding, mean)

pure.encoding = cast(pure, id ~ Encoding, mean)

##FREQ VS JOL
temp = t.test(pure.encoding$Frequency, pure.encoding$JOL, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-Sig

##pbic
pbic1 = pure.encoding[ , c(1, 2)]
pbic1 = na.omit(pbic1)

pbic1$task = rep("freq")

colnames(pbic1)[2] = "score"

pbic2 = pure.encoding[ , c(1, 3)]
pbic2 = na.omit(pbic2)

pbic2$task = rep("jol")

colnames(pbic2)[2] = "score"

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        dv = score,
        wid = id,
        between = task,
        detailed = T)

##FREQ VS READ
temp = t.test(pure.encoding$Frequency, pure.encoding$READ, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #significant!, p = .01

##JOL VS READ
temp = t.test(pure.encoding$JOL, pure.encoding$READ, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-sig

##interaction
tapply(pure$Scored, list(pure$Encoding, pure$Direction), mean)

#setup
pure.jol = subset(pure, pure$Encoding == "JOL")
pure.freq = subset(pure, pure$Encoding == "Frequency")
pure.read = subset(pure, pure$Encoding == "READ")

pure.jol2 = cast(pure.jol, id ~ Direction, mean)
pure.freq2 = cast(pure.freq, id ~ Direction, mean)
pure.read2 = cast(pure.read, id ~ Direction, mean)

#jol descriptives
apply(pure.jol2, 2, mean, na.rm = T)

x = apply(pure.jol2, 2, sd, na.rm = T)
x / sqrt(length(unique(pure.jol2$id))) * 1.96

#freq descriptives
apply(pure.freq2, 2, mean, na.rm = T)

x = apply(pure.freq2, 2, sd, na.rm = T)
x / sqrt(length(unique(pure.freq2$id))) * 1.96

#read descriptives
apply(pure.read2, 2, mean, na.rm = T)

x = apply(pure.read2, 2, sd, na.rm = T)
x / sqrt(length(unique(pure.read2$id))) * 1.96

#run the t-tests
#Forward pairs
#jol vs freq
temp = t.test(pure.jol2$S, pure.freq2$S, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-sig, p = .23

pbic1 = pure.jol2[ , c(1, 2)]
pbic1 = na.omit(pbic1)
pbic1$task = rep("jol")

pbic2 = pure.freq2[ , c(1, 2)]
pbic2 = na.omit(pbic2)
pbic2$task = rep("freq")

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        dv = S,
        wid = id,
        between = task,
        detailed = T)

#jol vs read
temp = t.test(pure.jol2$S, pure.read2$S, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #significant!

#freq vs read
temp = t.test(pure.freq2$S, pure.read2$S, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #significant!

##Correct pattern emerges!

#unrelated
#JOL vs FREQ
temp = t.test(pure.jol2$U, pure.freq2$U, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-sig, p = .09

#JOL vs READ
temp = t.test(pure.jol2$U, pure.read2$U, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #non-sig

#freq vs read
temp = t.test(pure.freq2$U, pure.read2$U, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-sig

##No difference between unrelated.
tapply(mixed$Response.RT, list(mixed$Encoding, mixed$Direction), mean, na.rm = T)
tapply(pure$Response.RT, list(pure$Encoding, pure$Direction), mean, na.rm = T)
