#authors: Bruno Jerkovic, Josip Koprcina, Christos Mavrikis

#If you are running this script for the first time
#please uncomment and install the packages below

#install.packages(c("remotes","pROC","naivebayes"))
#remotes::install_github("jtextor/bayesianNetworks")
#install.packages("lavaan", repos="http://cran.at.r-project.org/")
#install.packages("dummies")
#install.packages("bnlearn")
#install.packages("dagitty")
#install.packages("Rgraphviz",dependenacies = TRUE)
#install.packages("pcalg",dependenacies = TRUE)
#if (!requireNamespace("BiocManager", quietly = TRUE))
# install.packages("BiocManager")
#BiocManager::install(c("Rgraphviz", "RBGL"))
#include used libs for assignment
#BiocManager::valid()
#library(dummies)
#library(tibble)
#library(naivebayes)
#install.packages("Hmisc")
#install.packages("Hmisc", dependencies = T)
library(corrplot)
library(lavaan)
library(bnlearn)
library(dagitty)
library(bayesianNetworks)
library(pROC)
library(BiocManager)
library(pcalg)
library(Hmisc)

obese1 <- read.csv("Dataset/ObesityDataSet_raw_and_data_sinthetic.csv")
head(obese1[1:10,])

g_cutoff <- dagitty('
dag {
bb="0,0,1,1"
Age [pos="0.050,0.037"]
CAEC [pos="0.258,0.368"]
CALC [pos="0.948,0.314"]
CH2O [pos="0.250,0.277"]
FAF [pos="0.791,0.234"]
FAVC [pos="0.394,0.364"]
FCVC [pos="0.502,0.348"]
Gender [pos="0.249,0.037"]
Height [pos="0.140,0.685"]
MTRANS   [pos="0.653,0.475"]
NCP [pos="0.381,0.523"]
NObeyesdad [pos="0.258,0.935"]
SCC [pos="0.817,0.119"]
SMOKE [pos="0.027,0.326"]
TUE [pos="0.121,0.277"]
Weight [pos="0.511,0.726"]
family_history_with_overweight [pos="0.547,0.038"]
Age -> MTRANS
Age -> TUE
Age -> Weight
CAEC -> Weight
CALC-> Weight
FAF -> NCP
FAVC -> Weight
FCVC -> Weight
Gender -> FAF
Gender -> Weight
Gender -> Height
Height -> NObeyesdad
SCC -> FAVC
Weight -> NObeyesdad
family_history_with_overweight -> CAEC
family_history_with_overweight -> FAVC
family_history_with_overweight -> Weight
}
')

g_cutoff_1 <- dagitty('
dag {
bb="0,0,1,1"
Age [pos="0.050,0.037"]
CAEC [pos="0.258,0.368"]
CALC [pos="0.948,0.314"]
CH2O [pos="0.250,0.277"]
FAF [pos="0.791,0.234"]
FAVC [pos="0.394,0.364"]
FCVC [pos="0.502,0.348"]
Gender [pos="0.249,0.037"]
Height [pos="0.140,0.685"]
MTRANS   [pos="0.653,0.475"]
NCP [pos="0.381,0.523"]
NObeyesdad [pos="0.258,0.935"]
SCC [pos="0.817,0.119"]
SMOKE [pos="0.027,0.326"]
TUE [pos="0.121,0.277"]
Weight [pos="0.511,0.726"]
family_history_with_overweight [pos="0.547,0.038"]
Age -> MTRANS
Age -> TUE
Age -> Weight
CAEC -> Weight
CALC-> Weight
FAF -> NCP
FAVC -> Weight
FCVC -> Weight
Gender -> FAF
Gender -> Weight
Gender -> Height
Height -> NObeyesdad
SCC -> FAVC
Weight -> NObeyesdad
family_history_with_overweight -> CAEC
family_history_with_overweight -> FAVC
family_history_with_overweight -> Weight
FAF -> MTRANS
}
')

### HAMMING DISTANCE PLAYGROUND ###
g_cutoff <- model2network(toString(g_cutoff,"bnlearn"))
g_cutoff_1 <- model2network(toString(g_cutoff_1,"bnlearn"))

plot(g_cutoff_1)

g_cutoff_cpdag <- cpdag(g_cutoff)
g_cutoff_1_cpdag <- cpdag(g_cutoff_1)

hamming(learned=g_cutoff_cpdag,true=g_cutoff_1_cpdag,debug = FALSE)
shd(learned=g_cutoff_cpdag, true=g_cutoff_1_cpdag, debug = FALSE)
### HAMMING DISTANCE PLAYGROUND END ###

plot(g_cutoff)

#Start of preprocessing 

#Dividing into Age groups
s <-rep("<20",nrow(obese1))
s[obese1$Age>=20 &obese1$Age<35] <- "20-34"
s[obese1$Age>=35 &obese1$Age<50] <- "35-49"
s[obese1$Age>=50] <- "50>="
obese1$Age <-as.numeric(ordered(s)) # <20 = 1 , 20-34 = 2 , 35-49 = 3 , 50>= = 4
#print column $Age

table(obese1$CALC)
#Dividing into Height groups
s <-rep("<1.60",nrow(obese1))
s[obese1$Height>=1.60 &obese1$Height<1.75] <- "1.60-1.74"
s[obese1$Height>=1.75 &obese1$Height<1.85] <- "1.75-1.84"
s[obese1$Height>=1.85] <- "1.85>="
obese1$Height <-as.numeric(ordered(s)) # <1.60 = 1 , 1.60-1.74 = 2 , 1.75-1.84 = 3 , 1.85>= = 4
#print column $Height
#obese1$Height

#Dividing into Weight groups
s <-rep("<50",nrow(obese1))
s[obese1$Weight>=50 &obese1$Weight<70] <- "50-69"
s[obese1$Weight>=70 &obese1$Weight<90] <- "70-89"
s[obese1$Weight>=90] <- "90>="
obese1$Weight <-as.numeric(ordered(s))  # <50 = 1 , 50-69 = 2 , 70-89 = 3 ,90>= = 4
#print column $Weight
#obese1$Weight

#Breakdown Weight levels as well
obese1$NObeyesdad <- as.character(obese1$NObeyesdad)
obese1$NObeyesdad[obese1$NObeyesdad %in% c("Insufficient_Weight","Normal_Weight")] <- "Normal_Weight"
obese1$NObeyesdad[obese1$NObeyesdad %in% c("Overweight_Level_I","Overweight_Level_II")] <- "Overweight"
obese1$NObeyesdad[obese1$NObeyesdad %in% c("Obesity_Type_I","Obesity_Type_II","Obesity_Type_III")] <- "obese1"
obese1$NObeyesdad <- as.numeric(ordered(obese1$NObeyesdad))#Normal_Weight = 1 Overweight = 2 obese1= 3
#print column $NObeyesdad
#obese1$NObeyesdad




#as.numeric ordered
obese1$family_history_with_overweight <- as.numeric(ordered(obese1$family_history_with_overweight,c("no","yes"))) #no = 1 &  yes = 2
obese1$FAVC                           <- as.numeric(ordered(obese1$FAVC,c("no","yes"))) #no = 1 &  yes = 2
obese1$SCC                            <- as.numeric(ordered(obese1$SCC,c("no","yes"))) #no = 1 &  yes = 2
obese1$SMOKE                          <- as.numeric(ordered(obese1$SMOKE,c("no","yes"))) #no = 1 &  yes = 2
obese1$Gender                         <- as.numeric(ordered(obese1$Gender,c("Female","Male"))) #Female = 1 &  Male = 2
obese1$CAEC                           <- as.numeric(ordered(obese1$CAEC, levels=c("no","Sometimes","Frequently","Always"))) #no = 1 Sometimes = 2 Frequently = 3 Always = 4
obese1$CALC                           <- as.numeric(ordered(obese1$CALC, levels=c("no","Sometimes","Frequently","Always"))) #no = 1 Sometimes = 2 Frequently = 3 Always = 4
obese1$MTRANS                         <- as.numeric(ordered(obese1$MTRANS, c("Walking","Bike","Public_Transportation","Motorbike","Automobile")))
obese1

### STRUCTURE LEARNING

# Blacklist
b_from <- c(replicate(16, "NObeyesdad"), c(colnames(obese1)[1:4], colnames(obese1)[6:17]), colnames(obese1)[2:17], c(colnames(obese1)[1], colnames(obese1)[3:17]))
b_to <- c(colnames(obese1)[1:16], replicate(16, 'family_history_with_overweight'), replicate(16, 'Gender'), replicate(16, "Age"))
blacklist <- data.frame(b_from, b_to)
blacklist

# Whitelist
w_from <- c('Weight', 'Height')
w_to <- c(replicate(2, 'NObeyesdad'))
whitelist <- data.frame(w_from, w_to)
whitelist

res_hc <- hc(obese1, maxp=7, blacklist = blacklist, whitelist = whitelist, score = "bic-g") #hill - climbing
res_hc <- cpdag(res)
res_hc

res_pc <- pc.stable(obese1,blacklist = blacklist, whitelist = whitelist, a=0.05) #pc
res_pc
res_pc <- cpdag(res1)

normal_res_hc <- hc(obese1)
normal_res1_pc <-pc.stable(obese1)

plot(res_hc)
plot(res_pc)
plot(g_cutoff)

#graphviz.compare(res, res1 , layout = "dot" , shape = "circle")
net <- model2network(toString(g_cutoff,"bnlearn"))

compare(net, res_hc)
all.equal(net, res_hc)
hamming(learned=net,true=res_hc,debug = FALSE)
shd(learned=net, true=res_hc, debug = FALSE)
