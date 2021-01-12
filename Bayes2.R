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
#install.packages("gridExtra")
library(gridExtra)
require(gridExtra)
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

hand_constructed_net <- dagitty('
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
Age -> CALC
Age -> CAEC
Age -> MTRANS
Age -> FAF
Age -> FAVC
Age -> FCVC
Age -> Height
Age -> SCC
Age -> SMOKE
Age -> TUE
Age -> Weight
Age -> CH2O
CAEC -> Weight
CALC-> Weight
CH2O -> Weight
FAF -> CH2O
FAF -> FCVC
FAF -> NCP
FAF -> Weight
FAVC -> Weight
FCVC -> Weight
Gender -> FAF
Gender -> SCC
Gender -> SMOKE
Gender -> TUE
Gender -> Weight
Gender -> Height
Height -> NObeyesdad
MTRANS -> Weight
NCP -> FAVC
NCP -> Weight
SCC -> CH2O
SCC -> FAF
SCC -> FAVC
SCC -> FCVC
SCC -> NCP
SCC -> Weight
SCC -> CALC
SCC -> SMOKE
SMOKE -> Weight
TUE -> Weight
Weight -> NObeyesdad
family_history_with_overweight -> CAEC
family_history_with_overweight -> FAVC
family_history_with_overweight -> FCVC
family_history_with_overweight -> NCP
family_history_with_overweight -> SCC
family_history_with_overweight -> Weight
}
')

hand_constructed_cutoff_net <- dagitty('
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

# Turn dagitty nets to bnlearn nets
hand_constructed_net <- model2network(toString(hand_constructed_net,"bnlearn"))
hand_constructed_cutoff_net <- model2network(toString(hand_constructed_cutoff_net, "bnlearn"))

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

# STRUCTURE LEARNING NETS

# Comparison function
compare_nets <- function(net1, net2) {
  par(mfrow=c(1,2))
  plot(net1, main=deparse(substitute(net1)))
  plot(net2, main=deparse(substitute(net2)))
  
  tb = compare(target=net1, current=net2)
  print("CONFUSION MATRIX:")
  print(tb)
  h = hamming(learned=net1, true=net2, debug=FALSE)
  print("HAMMING DISTANCE:")
  print(h)
  s = shd(learned=net1, true=net2, debug=FALSE)
  print("STRUCTURAL HAMMING DISTANCE:")
  print(s)
  if (all.equal(net1, net2) == TRUE) {
    print("NETWORKS HAVE THE SAME STRUCTURE")
  } else {
    print("NETWORKS DO NOT HAVE THE SAME STRUCTURE")
  }
}

# Best HC and PC
hc_net <- hc(obese1, maxp=7, blacklist = blacklist, whitelist = whitelist, score = "bic-g") #hill-climbing
hc_net <- cpdag(hc_net)

pc_net <- pc.stable(obese1,blacklist = blacklist, whitelist = whitelist, a=0.05) #pc
pc_net <- cpdag(pc_net)

# Default HC and PC
hc_net_def_params <- hc(obese1)
pc_net_def_params <-pc.stable(obese1)

# Testing hyperparameters on networks
pc_net_low_alpha <- pc.stable(obese1,blacklist = blacklist, whitelist = whitelist, a=5*10^(-12))
pc_net_high_alpha <- pc.stable(obese1,blacklist = blacklist, whitelist = whitelist, a=0.7)
hc_net_no_pen <- hc(obese1, maxp=7, blacklist = blacklist, whitelist = whitelist, score = "loglik-g", restart=100) #hill-climbing
hc_net_one_parent <- hc(obese1, maxp=1, blacklist = blacklist, whitelist = whitelist, score = "bic-g", restart=100) #hill-climbing

# PRINT RESULTS (if it is not visible enough, zoom the graph)
# Print main nets
par(mfrow=c(1,1))
plot(pc_net, main="PC Net")
plot(hc_net, main="HC Net")
plot(hand_constructed_net, main="Hand Constructed Net")
plot(hand_constructed_cutoff_net, main="Hand Constructed Cutoff Net")

# Print nets with tweaked hyperparameters
compare_nets(pc_net, hc_net)
compare_nets(pc_net, pc_net_low_alpha)
compare_nets(pc_net, pc_net_high_alpha)
compare_nets(pc_net, pc_net_def_params)

compare_nets(hc_net, pc_net)
compare_nets(hc_net, hc_net_def_params)
compare_nets(hc_net, hc_net_no_pen)
compare_nets(hc_net, hc_net_one_parent)

# Print report table
compare_nets(pc_net, hc_net)
compare_nets(hand_constructed_cutoff_net, pc_net)
compare_nets(hc_net, hand_constructed_cutoff_net)
compare_nets(hc_net, hand_constructed_net)
compare_nets(hand_constructed_net, pc_net)
compare_nets(hand_constructed_net, hand_constructed_cutoff_net)
