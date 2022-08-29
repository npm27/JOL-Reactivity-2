####Set up
##Load in data
FREQ = read.csv("Scored Data/FREQ Scored.csv")
JOL = read.csv("Scored Data/JOL Scored.csv")
Study = read.csv("Scored Data/Study Scored.csv")

#Figure out recruitment platform
table(FREQ$id) #Prolific IDs all start w/ 5. No USM IDs start w/ 5

FREQ$Platform = substr(FREQ$id, 1, 1)
table(FREQ$Platform)

FREQ$Platform[FREQ$Platform == "5"] = "Prolific"
FREQ$Platform[FREQ$Platform == "6"] = "Prolific"
FREQ$Platform[FREQ$Platform == "1"] = "USM"
FREQ$Platform[FREQ$Platform == "2"] = "USM"
FREQ$Platform[FREQ$Platform == "3"] = "USM"
FREQ$Platform[FREQ$Platform == "4"] = "USM"
FREQ$Platform[FREQ$Platform == "9"] = "USM"
FREQ$Platform[FREQ$Platform == "W"] = "USM"
FREQ$Platform[FREQ$Platform == "w"] = "USM"

#Now JOLs
table(JOL$id) #Prolific IDs all start w/ 5 or 6. No USM IDs start w/ 5 or 6

JOL$Platform = substr(JOL$id, 1, 1)
table(JOL$Platform)

JOL$Platform[JOL$Platform == "5"] = "Prolific"
JOL$Platform[JOL$Platform == "6"] = "Prolific"
JOL$Platform[JOL$Platform == "1"] = "USM"
JOL$Platform[JOL$Platform == "2"] = "USM"
JOL$Platform[JOL$Platform == "3"] = "USM"
JOL$Platform[JOL$Platform == "9"] = "USM"
JOL$Platform[JOL$Platform == "W"] = "USM"
JOL$Platform[JOL$Platform == "w"] = "USM"
JOL$Platform[JOL$Platform == "s"] = "USM"

##Now do read group
table(Study$id) #Prolific IDs all start w/ 5. No USM IDs start w/ 5

Study$Platform = substr(Study$id, 1, 1)
table(Study$Platform)

Study$Platform[Study$Platform == "5"] = "Prolific"
Study$Platform[Study$Platform == "6"] = "Prolific"
Study$Platform[Study$Platform == "1"] = "USM"
Study$Platform[Study$Platform == "W"] = "USM"
Study$Platform[Study$Platform == "w"] = "USM"
Study$Platform[Study$Platform == "9"] = "USM"

##How many from each?
table(FREQ$Platform) / 80
table(JOL$Platform) / 80
table(Study$Platform) / 80

##how many for each list type?
table(FREQ$Condition.Description) / 80
table(JOL$Condition.Description) / 80
table(Study$Condition.Description) / 80

#REMOVE INDEX COLUMN IF NEEDED
FREQ = FREQ[ , -1]
JOL = JOL[ , -1]
Study = Study[ , -1]

##Get recall on the right scale
FREQ$Scored = FREQ$Scored * 100
JOL$Scored = JOL$Scored * 100
Study$Scored = Study$Scored * 100

##Settings
options(scipen = 999)

##Load libraries
library(reshape)
library(ez)
library(psychReport)

####Data screening####
summary(FREQ)
summary(JOL)
summary(Study)

#Fix column names
colnames(FREQ)[12] = "Frequency"
colnames(JOL)[12] = "JOL"

colnames(FREQ)[10] = "Direction"
colnames(JOL)[10] = "Direction"
colnames(Study)[10] = "Direction"

##Fix out of range JOL/Frequency Scores
table(FREQ$Frequency)

FREQ$Frequency[FREQ$Frequency == "98Z"] = NA
FREQ$Frequency[FREQ$Frequency == "8p"] = NA
FREQ$Frequency[FREQ$Frequency == "1t"] = NA
FREQ$Frequency[FREQ$Frequency == "5?"] = NA
#FREQ$Frequency[FREQ$Frequency == "5<ef>"] = NA

FREQ$Frequency = as.numeric(FREQ$Frequency)

FREQ$Frequency[FREQ$Frequency > 100] = NA

summary(JOL$JOL)
JOL$JOL = as.numeric(JOL$JOL)

JOL$JOL[JOL$JOL > 100] = NA

summary(FREQ)
summary(JOL)

##Separate out the different conditions
##will have 9 total: Pure F, Pure U, and Mixed for each encoding strategy
##Start with FREQ
FREQ$Condition.Description = substr(FREQ$Condition.Description, start = 6, stop = length(FREQ$Condition.Description))
FREQ$Condition.Description = substr(FREQ$Condition.Description, 1, nchar(FREQ$Condition.Description) - 2)

##JOL
JOL$Condition.Description = substr(JOL$Condition.Description, start = 5, stop = length(JOL$Condition.Description))
JOL$Condition.Description = substr(JOL$Condition.Description, 1, nchar(JOL$Condition.Description) - 2)

##Study
Study$Condition.Description = substr(Study$Condition.Description, start = 6, stop = length(Study$Condition.Description))
Study$Condition.Description = substr(Study$Condition.Description, 1, nchar(Study$Condition.Description) - 2)

##Accidentally coded Forward as Backward -- not sure if this a backward subject or if coding got off, so just going to remove.
JOL = subset(JOL,
             id != "w10034986")

####Get Descriptives####
##Start with Frequency
tapply(FREQ$Scored, list(FREQ$Direction, FREQ$Condition.Description), mean, na.rm = T)

##JOLs
tapply(JOL$Scored, list(JOL$Direction, JOL$Condition.Description), mean, na.rm = T)

##Study
tapply(Study$Scored, list(Study$Direction, Study$Condition.Description), mean, na.rm = T)

####Get number of participants####
length(unique(FREQ$id)) #110
length(unique(JOL$id)) #117
length(unique(Study$id)) #115

##breakdown by condition
##FREQ
F1 = subset(FREQ,
            FREQ$Condition.Description == "Mixed")
F2 = subset(FREQ,
            FREQ$Condition.Description == "Pure U")
F3 = subset(FREQ,
            FREQ$Condition.Description == "Pure F")

##JOL
J1 = subset(JOL,
            Condition.Description == "Mixed")
J2 = subset(JOL,
            Condition.Description == "Pure U")
J3 = subset(JOL,
            Condition.Description == "Pure F")

##READ
R1 = subset(Study,
            Study$Condition.Description == "Mixed")
R2 = subset(Study,
            Study$Condition.Description == "Pure U")
R3 = subset(Study,
            Study$Condition.Description == "Pure F")

####Get condition n's####
#Frequency
length(unique(F1$id)) #35 #Mixed
length(unique(F2$id)) #42 #Pure U
length(unique(F3$id)) #33 #Pure F

#JOL
length(unique(J1$id)) #38 #Mixed
length(unique(J2$id)) #48 #Pure U (This is the one for Trevor's thing)
length(unique(J3$id)) #31 #Pure F

##Write J2 to .csv for Trevor's Project
#write.csv(J2, file = "Pure_Unrelated.csv", row.names = F)
#mean(J2$JOL, na.rm = T)

#Study
length(unique(R1$id)) #38 #Mixed
length(unique(R2$id)) #46 #Pure U
length(unique(R3$id)) #32 #Pure F

####Outliers####
#First drop unused columns
#Frequency
F1 = F1[ , c(1, 10, 12, 15, 14, 5)]
F2 = F2[ , c(1, 10, 12, 15, 14, 5)]
F3 = F3[ , c(1, 10, 12, 15, 14, 5)]

#1 = Mixed
#2 = Pure U
#3 = Pure F

#JOLs
J1 = J1[ , c(1, 10, 12, 15, 14, 5)]
J2 = J2[ , c(1, 10, 12, 15, 14, 5)]
J3 = J3[ , c(1, 10, 12, 15, 14, 5)]

#Read
R1 = R1[ , c(1, 10, 14, 13, 5)]
R2 = R2[ , c(1, 10, 14, 13, 5)]
R3 = R3[ , c(1, 10, 14, 13, 5)]

##F1
temp = cast(F1, id ~ Direction, mean)

F1 = subset(F1,
            F1$id != "366039")

##F2
temp2 = cast(F2, id ~ Direction, mean)

##Remove outlier(s)
F2 = subset(F2,
            F2$id != "10068593")
F2 = subset(F2,
            F2$id != "5ec42fd9269fcc2e424cc0e1")
F2 = subset(F2,
            F2$id != "w990486_jgh")
F2 = subset(F2,
            F2$id != "w10124425")
F2 = subset(F2,
            F2$id != "w10029489")

##F3
temp = cast(F3, id ~ Direction, mean)

##Remove outlier(s)
F3 = subset(F3,
            F3$id != "w10034428")
F3 = subset(F3,
            F3$id != "w10070345")

##JOLs
##J1
temp = cast(J1, id ~ Direction, mean)

##Remove outlier(s)
J1 = subset(J1,
            J1$id != "983960")
J1 = subset(J1,
            J1$id != "w416354")

##J2
temp = cast(J2, id ~ Direction, mean)

##Remove outlier(s)
J2 = subset(J2,
            J2$id != "w10044868")
J2 = subset(J2,
            J2$id != "972730")
J2 = subset(J2,
            J2$id != "5f12900e0ad01c209b1fade9")
J2 = subset(J2,
            J2$id != "5e8b806203f2f605d5b6f531")
J2 = subset(J2,
            J2$id != "5ea47ff57c48522b29976b79")
J2 = subset(J2,
            J2$id != "5f1eadf617e7c80008b0d2ae")
J2 = subset(J2,
            J2$id != "5ee64cd142c24a0008de839b")
J2 = subset(J2,
            J2$id != "5f0c10b787a3fe533f292145")
J2 = subset(J2,
            J2$id != "w10002554")
J2 = subset(J2,
            J2$id != "5eb031acf88798083686548a")
J2 = subset(J2,
            J2$id != "w10092043_acb")
J2 = subset(J2,
            J2$id != "w10131848")
J2 = subset(J2,
            J2$id != "5c6039c8c5405100016c90ab")

##J3
temp = cast(J3, id ~ Direction, mean)

###Study
##R1
temp = cast(R1, id ~ Direction, mean)

##Remove outlier(s)
R1 = subset(R1,
            R1$id != "5e8f0692facc7d1c12b4c9a1")
R1 = subset(R1,
            R1$id != "w10013029")
R1 = subset(R1,
            R1$id != "10085738")

##R2
temp = cast(R2, id ~ Direction, mean)

##Remove outlier(s)
R2 = subset(R2,
            R2$id != "5e5137d5c20dd2166539ed00")
R2 = subset(R2,
            R2$id != "w10076838mj")
R2 = subset(R2,
            R2$id != "w975249")
R2 = subset(R2,
            R2$id != "w10005410")
R2 = subset(R2,
            R2$id != "w10132291")
R2 = subset(R2,
            R2$id != "w10055396_hed")
R2 = subset(R2,
            R2$id != "10081497")
R2 = subset(R2,
            R2$id != "5d4ad0ffef19060018a973b9")
R2 = subset(R2,
            R2$id != "6133795d493f4bf5eb6cb3de")
R2 = subset(R2,
            R2$id != "612b61421bafbebe20702843")

##R3
temp = cast(R3, id ~ Direction, mean)

##Remove outlier(s)
R3 = subset(R3,
            R3$id != "w10060940_df")
R3 = subset(R3,
            R3$id != "5f0a107604954e2bd03ddb6e")
R3 = subset(R3,
            R3$id != "95ef31f36e283b802f1363928")
R3 = subset(R3,
            R3$id != "w10054290")

####Build ANOVA data!####
##Add encoding tags
#FREQ
F1$Encoding = rep("Frequency")
F2$Encoding = rep("Frequency")
F3$Encoding = rep("Frequency")

#JOL
J1$Encoding = rep("JOL")
J2$Encoding = rep("JOL")
J3$Encoding = rep("JOL")

#Read
R1$Encoding = rep("READ")
R2$Encoding = rep("READ")
R3$Encoding = rep("READ")

##Now add mixed vs pure tags
##Mixed
F1$List_Type = rep("Mixed")
J1$List_Type = rep("Mixed")
R1$List_Type = rep("Mixed")

##Pure U
F2$List_Type = rep("Pure U")
J2$List_Type = rep("Pure U")
R2$List_Type = rep("Pure U")

##Pure F
F3$List_Type = rep("Pure F")
J3$List_Type = rep("Pure F")
R3$List_Type = rep("Pure F")
