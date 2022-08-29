####Did Recall rates differ based on recruitment platform?####
##load libraries
library(ez)
library(reshape)

####Experiment 1 -- F vs U####
##load ex1 data
mixed = read.csv("EX1/mixed.csv")
pure = read.csv("Ex1/pure.csv")

##Mixed lists
#Did recall rates differ by recruitment platform?
ezANOVA(mixed,
        dv = Scored,
        wid = id,
        between = .(Platform, Encoding),
        within = Direction,
        type = 3,
        detailed = T)

#no effect of platform, no interactions w/ platform

##pure lists
ezANOVA(pure,
        dv = Scored,
        wid = id,
        between = .(Platform, Encoding, Direction),
        type = 3,
        detailed = T)

#again, no effects of recruitment platform

####Experiment 2 -- B vs U####
##load ex2 data
mixed = read.csv("EX2/mixed.csv")
pure = read.csv("Ex2/pure.csv")

##Mixed lists
#Did recall rates differ by recruitment platform?
ezANOVA(mixed,
        dv = Scored,
        wid = id,
        between = .(Platform, Encoding),
        within = Direction,
        type = 3,
        detailed = T)

#no interactions w/ platform. Main effect of platform was marginal
tapply(mixed$Scored, mixed$Platform, mean, na.rm = T) #Overall recall was a bit higher for Prolific participants
tapply(mixed$Scored, list(mixed$Platform, mixed$Direction), mean, na.rm = T) #occurred equally for both pair types

##pure lists
ezANOVA(pure,
        dv = Scored,
        wid = id,
        between = .(Platform, Encoding, Direction),
        type = 3,
        detailed = T)

##No effect or interactions w/ platform

####Experiment 3 -- S vs U####
##load the ex3 data
mixed = read.csv("Ex3/mixed.csv")
pure = read.csv("Ex3/pure.csv")

##Mixed lists
#Did recall rates differ by recruitment platform?
ezANOVA(mixed,
        dv = Scored,
        wid = id,
        between = .(Platform, Encoding),
        within = Direction,
        type = 3,
        detailed = T)

#nope!

##Pure lists
ezANOVA(pure,
        dv = Scored,
        wid = id,
        between = .(Platform, Encoding, Direction),
        type = 3,
        detailed = T)

#Platform by encoding interaction. Everything else ns
tapply(pure$Scored, list(pure$Platform, pure$Encoding), mean, na.rm = T)

#JOL prolific participants did worse for some reason

##how many JOL prolific participants were there?
table(pure$Platform) / 80 #51 total pure prolific participants
table(list(pure$Platform, pure$Encoding)) / 80 #24 pure JOL Prolifics

pure_jol_prolific = subset(pure,
                           pure$Platform == "Prolific" & pure$Encoding == "JOL")

#Check for weirdos
weirdos = cast(pure_jol_prolific[ , c(1:4, 6:8, 5)], id ~ Direction, mean)

##Okay, this is because almost all of them were in the Unrelated condition
tapply(pure_jol_prolific$Scored, pure_jol_prolific$Direction, mean, na.rm = T)
