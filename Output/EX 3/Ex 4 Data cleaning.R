####Set up
##Load in data
FREQ = read.csv("Scored/FREQ Scored.csv")
JOL = read.csv("Scored/JOL Scored.csv")
Study = read.csv("Scored/Read Scored.csv")

#remove duplicate participant
FREQ = subset(FREQ,
              FREQ$id != "w10050492_ang")
JOL = subset(JOL,
             JOL$id != "w10050492_ang")

#Remove index
FREQ = FREQ[ , -1]
JOL = JOL[ , -1]
Study = Study[ , -1]

##Get recall on the right scale
FREQ$Scored = FREQ$Scored * 100
JOL$Scored = JOL$Scored * 100
Study$Scored = Study$Scored * 100

##Settings
options(scipen = 999)

#Figure out recruitment platform
table(FREQ$id) #Prolific IDs all start w/ 6. No USM IDs start w/ 6

FREQ$Platform = substr(FREQ$id, 1, 1)
table(FREQ$Platform)

FREQ$Platform[FREQ$Platform == "5"] = "Prolific"
FREQ$Platform[FREQ$Platform == "6"] = "Prolific"
FREQ$Platform[FREQ$Platform == "1"] = "USM"
FREQ$Platform[FREQ$Platform == "2"] = "USM"
FREQ$Platform[FREQ$Platform == "9"] = "USM"
FREQ$Platform[FREQ$Platform == "W"] = "USM"
FREQ$Platform[FREQ$Platform == "w"] = "USM"

#Now JOLs
table(JOL$id)

JOL$Platform = substr(JOL$id, 1, 1)
table(JOL$Platform)

JOL$Platform[JOL$Platform == "5"] = "Prolific"
JOL$Platform[JOL$Platform == "6"] = "Prolific"
JOL$Platform[JOL$Platform == "1"] = "USM"
JOL$Platform[JOL$Platform == "9"] = "USM"
JOL$Platform[JOL$Platform == "W"] = "USM"
JOL$Platform[JOL$Platform == "w"] = "USM"

##Now do read group
table(Study$id)

Study$Platform = substr(Study$id, 1, 1)
table(Study$Platform)

Study$Platform[Study$Platform == "6"] = "Prolific"
Study$Platform[Study$Platform == "5"] = "Prolific"
Study$Platform[Study$Platform == "1"] = "USM"
Study$Platform[Study$Platform == "W"] = "USM"
Study$Platform[Study$Platform == "w"] = "USM"
Study$Platform[Study$Platform == "9"] = "USM"

##How many from each?
table(FREQ$Platform) / 80
table(JOL$Platform) / 80
table(Study$Platform) / 80

table(FREQ$Condition.Description) / 80
table(JOL$Condition.Description) / 80
table(Study$Condition.Description) / 80

##Load libraries
library(reshape)
library(ez)
library(psychReport)

####Data screening####
summary(FREQ)
summary(JOL)
summary(Study)

#Drop first columns
#FREQ = FREQ[ , -1]
#JOL = JOL[ , -1]
#Study = Study[ , -1]

#Rename columns
colnames(FREQ)[12] = "Freq"
colnames(JOL)[12] = "JOL"

colnames(FREQ)[10] = "Direction"
colnames(JOL)[10] = "Direction"
colnames(Study)[10] = "Direction"

##Fix out of range JOLs/Freqs
FREQ$Freq = as.numeric(FREQ$Freq)
FREQ$Freq[FREQ$Freq > 100] = NA

JOL$JOL = as.numeric(JOL$JOL)
JOL$JOL[JOL$JOL > 100] = NA

##Separate out the different conditions
##will have 6 total: Pure S and Mixed for each encoding strategy

##Start with FREQ
FREQ$Condition.Description = substr(FREQ$Condition.Description, start = 6, stop = length(FREQ$Condition.Description))
FREQ$Condition.Description = substr(FREQ$Condition.Description, 1, nchar(FREQ$Condition.Description) - 2)

##JOL
JOL$Condition.Description = substr(JOL$Condition.Description, start = 5, stop = length(JOL$Condition.Description))
JOL$Condition.Description = substr(JOL$Condition.Description, 1, nchar(JOL$Condition.Description) - 2)

##Study
Study$Condition.Description = substr(Study$Condition.Description, start = 6, stop = length(Study$Condition.Description))
Study$Condition.Description = substr(Study$Condition.Description, 1, nchar(Study$Condition.Description) - 2)

#Make sure things look right
table(Study$Condition.Description)
table(JOL$Condition.Description)
table(FREQ$Condition.Description)

##Fix labels
Study$Condition.Description[Study$Condition.Description == "Pure F"] = "Pure S"
JOL$Condition.Description[JOL$Condition.Description == "Pure F"] = "Pure S"
FREQ$Condition.Description[FREQ$Condition.Description == "Pure F"] = "Pure S"

####Descriptives####
##Start with Frequency
tapply(FREQ$Scored, list(FREQ$Direction, FREQ$Condition.Description), mean, na.rm = T)

##JOLs
tapply(JOL$Scored, list(JOL$Direction, JOL$Condition.Description), mean, na.rm = T)

##Study
tapply(Study$Scored, list(Study$Direction, Study$Condition.Description), mean, na.rm = T)

####Get number of participants####
length(unique(FREQ$id))
length(unique(JOL$id))
length(unique(Study$id))

####Screen for outliers####
##breakdown by condition

##FREQ
F1 = subset(FREQ,
            FREQ$Condition.Description == "Mixed")
F2 = subset(FREQ,
            FREQ$Condition.Description == "Pure S")
##JOL
J1 = subset(JOL,
            Condition.Description == "Mixed")
J2 = subset(JOL,
            Condition.Description == "Pure S")

##READ
R1 = subset(Study,
            Study$Condition.Description == "Mixed")
R2 = subset(Study,
            Study$Condition.Description == "Pure S")

#Now drop unused columns
#Frequency
F1 = F1[ , c(1, 10, 12, 15, 14, 5)]
F2 = F2[ , c(1, 10, 12, 15, 14, 5)]

#1 = Mixed
#2 = Pure S

#JOLs
J1 = J1[ , c(1, 10, 12, 15, 14, 5)]
J2 = J2[ , c(1, 10, 12, 15, 14, 5)]

#Read
R1 = R1[ , c(1, 10, 14, 13, 5)]
R2 = R2[ , c(1, 10, 14, 13, 5)]

###Now start reshaping and searching for outliers
##F1
temp = cast(F1, id ~ Direction, mean)

F1 = subset(F1,
            F1$id != "w10084560_aalw")

##Remove outlier(s)
##F2
temp = cast(F2, id ~ Direction, mean)

F2 = subset(F2,
            F1$id != "w983496_alh")
F2 = subset(F2,
            F2$id != "w10026623_kmw")
F2 = subset(F2,
            F2$id != "213606")
F2 = subset(F2,
            F2$id != "w10068412")

##J1
temp = cast(J1, id ~ Direction, mean)

J1 = subset(J1,
            J1$id != "w10051066")
J1 = subset(J1,
            J1$id != "w10047853")
J1 = subset(J1,
            J1$id != "w10016220")

##J2
temp = cast(J2, id ~ Direction, mean)

J2 = subset(J2,
            J2$id != "w10106441")
J2 = subset(J2,
            J2$id != "60ef12d2f7ff5a187d391929")

##R1
temp = cast(R1, id ~ Direction, mean)

R1 = subset(R1,
            R1$id != "w10081684")
R1 = subset(R1,
            R1$id != "w10078459")
R1 = subset(R1,
            R1$id != "5fee8cc55ce3aeca62e4f546")

##R2
temp = cast(R2, id ~ Direction, mean)

R2 = subset(R2,
            R2$id != "w10026294")
R2 = subset(R2,
            R2$id != "60ef3192738a031e10ca51be")
R2 = subset(R2,
            R2$id != "976614")
R2 = subset(R2,
            R2$id != "w10070340_ACM")
R2 = subset(R2,
            R2$id != "6129b29dc8a1278704650c55")

####Build ANOVA data!####
##Add encoding tags
#FREQ
F1$Encoding = rep("Frequency")
F2$Encoding = rep("Frequency")

#JOL
J1$Encoding = rep("JOL")
J2$Encoding = rep("JOL")

#Read
R1$Encoding = rep("READ")
R2$Encoding = rep("READ")

##Now add mixed vs pure tags
##Mixed
F1$List_Type = rep("Mixed")
J1$List_Type = rep("Mixed")
R1$List_Type = rep("Mixed")

##Pure B
F2$List_Type = rep("Pure B")
J2$List_Type = rep("Pure B")
R2$List_Type = rep("Pure B")

