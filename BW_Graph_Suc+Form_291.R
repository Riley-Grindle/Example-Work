
install.packages("rstudioapi")
install.packages("data.table")
install.packages("readxl")
install.packages("ff")
install.packages("plotrix")
install.packages("ggplot2")

library(data.table)
library(readxl)
library(ff)
library(plotrix)
library(ggplot2)

setwd("C:/Users/grindr/OneDrive - The Jackson Laboratory/Documents/Survival_Hydro_Graphing")

#DATA CLEANING AND FORMATTING - removed unnecessary rows & renamed columns

raw_data <- read.csv("36063-Suc+For Study - BW_BCS_Survival (from P0).csv", 
                     header=TRUE, sep= ",", fill = TRUE)
raw_data_1 <- raw_data[-c(1:9),]
colnames(raw_data_1) <- raw_data_1[c(1),]
raw_data_2 <- raw_data_1[-c(1),]

#DATA FRAGMENTATION - Body weight and BCS data organized by Sex and Genotype

data_clean <- raw_data_2[ , -which(names(raw_data_2) %in% c("Observations", "BCS"))]
data_clean_1 <- data_clean[, -c(8,9,11,12,13,14,15,16,17,18,19,20)]

Male_Data <- data_clean_1[!( data_clean_1$Sex =="F"),]
Male_Data_clean <- Male_Data[-c(41:100),]

Female_Data <- data_clean_1[!( data_clean_1$Sex =="M"),]
Female_Data_clean <- Female_Data[-c(41:100),]

WT_Data <- data_clean_1[!( data_clean_1$Geno =="Het Hemi"),]
WT_Data_clean <- WT_Data[-c(41:100),]

HET_Data <- data_clean_1[!( data_clean_1$Geno =="WT"),]
HET_Data_clean <- HET_Data[-c(41:100),]

#FURTHER DATA SPECIFICATION - Each sex separated by Genotype

Male_WT_Data <- Male_Data_clean[!(Male_Data_clean$Geno =="Het Hemi"),]
Male_Het_Data <- Male_Data_clean[!(Male_Data_clean$Geno =="WT"),]

Female_WT_Data <- Female_Data_clean[!(Female_Data_clean$Geno =="Het Hemi"),]
Female_Het_Data <- Female_Data_clean[!(Female_Data_clean$Geno =="WT"),]

#ARM1 SEPARATION - Formate Water vs. Sucralose + Formate Water

ARM_1_Male_WT <- Male_WT_Data[!(Male_WT_Data$Arm =="2"),]
ARM_1_Male_Het <- Male_Het_Data[!(Male_Het_Data$Arm =="2"),]
ARM_1_Female_WT <- Female_WT_Data[!(Female_WT_Data$Arm =="2"),]
ARM_1_Female_Het <- Female_Het_Data[!(Female_Het_Data$Arm =="2"),]

A1_Male_WT_data <- ARM_1_Male_WT[,-c(1,2,3,4,5,6,7,8)]
A1_Male_Het_data <- ARM_1_Male_Het[,-c(1,2,3,4,5,6,7,8)]
A1_Female_WT_data <- ARM_1_Female_WT[,-c(1,2,3,4,5,6,7,8)]
A1_Female_Het_data <- ARM_1_Female_Het[,-c(1,2,3,4,5,6,7,8)]

#ARM1 Days and BW separation 

toDelete_Days <- seq(1, ncol(A1_Male_WT_data), 2)
A1_Male_WT_BW <- A1_Male_WT_data[ ,toDelete_Days ]

toDelete_Days <- seq(1, ncol(A1_Male_Het_data), 2)
A1_Male_Het_BW <- A1_Male_Het_data[ ,toDelete_Days ]

toDelete_Days <- seq(1, ncol(A1_Female_WT_data), 2)
A1_Female_WT_BW <- A1_Female_WT_data[ ,toDelete_Days ]

toDelete_Days <- seq(1, ncol(A1_Female_Het_data), 2)
A1_Female_Het_BW <- A1_Female_Het_data[ ,toDelete_Days ]

toDelete_BW <- seq(0, ncol(A1_Male_WT_data), 2)
A1_Male_WT_Days <- A1_Male_WT_data[ ,toDelete_BW ]

toDelete_BW <- seq(0, ncol(A1_Male_Het_data), 2)
A1_Male_Het_Days <- A1_Male_Het_data[ ,toDelete_BW ]

toDelete_BW <- seq(0, ncol(A1_Female_WT_data), 2)
A1_Female_WT_Days <- A1_Female_WT_data[ ,toDelete_BW ]

toDelete_BW <- seq(0, ncol(A1_Female_Het_data), 2)
A1_Female_Het_Days <- A1_Female_Het_data[ ,toDelete_BW ]

#ARM2 SEPARATION - Formate Water vs. Sucralose + Formate Water

ARM_2_Male_WT <- Male_WT_Data[!(Male_WT_Data$Arm =="1"),]
ARM_2_Male_Het <- Male_Het_Data[!(Male_Het_Data$Arm =="1"),]
ARM_2_Female_WT <- Female_WT_Data[!(Female_WT_Data$Arm =="1"),]
ARM_2_Female_Het <- Female_Het_Data[!(Female_Het_Data$Arm =="1"),]

A2_Male_WT_data <- ARM_2_Male_WT[,-c(1,2,3,4,5,6,7,8)]
A2_Male_Het_data <- ARM_2_Male_Het[,-c(1,2,3,4,5,6,7,8)]
A2_Female_WT_data <- ARM_2_Female_WT[,-c(1,2,3,4,5,6,7,8)]
A2_Female_Het_data <- ARM_2_Female_Het[,-c(1,2,3,4,5,6,7,8)]

#ARM2 Days and BW separation

toDelete_Days <- seq(1, ncol(A2_Male_WT_data), 2)
A2_Male_WT_BW <- A2_Male_WT_data[ ,toDelete_Days ]

toDelete_Days <- seq(1, ncol(A2_Male_Het_data), 2)
A2_Male_Het_BW <- A2_Male_Het_data[ ,toDelete_Days ]

toDelete_Days <- seq(1, ncol(A2_Female_WT_data), 2)
A2_Female_WT_BW <- A2_Female_WT_data[ ,toDelete_Days ]

toDelete_Days <- seq(1, ncol(A2_Female_Het_data), 2)
A2_Female_Het_BW <- A2_Female_Het_data[ ,toDelete_Days ]

toDelete_BW <- seq(0, ncol(A2_Male_WT_data), 2)
A2_Male_WT_Days <- A2_Male_WT_data[ ,toDelete_BW ]

toDelete_BW <- seq(0, ncol(A2_Male_Het_data), 2)
A2_Male_Het_Days <- A2_Male_Het_data[ ,toDelete_BW ]

toDelete_BW <- seq(0, ncol(A2_Female_WT_data), 2)
A2_Female_WT_Days <- A2_Female_WT_data[ ,toDelete_BW ]

toDelete_BW <- seq(0, ncol(A2_Female_Het_data), 2)
A2_Female_Het_Days <- A2_Female_Het_data[ ,toDelete_BW ]


# STANDARD ERROR & MEAN CALCULATION & APPENDAGE ###########################
#Armature 1 - Male WT BW

A1_SEM_1 <- 0 
for (i in 1:length(A1_Male_WT_BW)){
  A1_SEM_1[i] <- (std.error(A1_Male_WT_BW[i]))
}
A1_SEM_1 <- as.data.frame(A1_SEM_1)

A1_MEAN_1 <- 0
A1_Male_WT_BW <- sapply(A1_Male_WT_BW, as.numeric)
A1_Male_WT_BW <- as.data.frame(A1_Male_WT_BW)
for (i in 1:length(A1_Male_WT_BW)){
  A1_MEAN_1[i]  <- mean(A1_Male_WT_BW[[i]], na.rm = TRUE) 
}
A1_MEAN_1 <- as.data.frame(A1_MEAN_1)

###############################
#Armature 1 - Male HET BW

A1_SEM_2 <- 0 
for (i in 1:length(A1_Male_Het_BW)){
  A1_SEM_2[i] <- (std.error(A1_Male_Het_BW[i]))
}
A1_SEM_2 <- as.data.frame(A1_SEM_2)

A1_MEAN_2 <- 0
A1_Male_Het_BW <- sapply(A1_Male_Het_BW, as.numeric)
A1_Male_Het_BW <- as.data.frame(A1_Male_Het_BW)
for (i in 1:length(A1_Male_Het_BW)){
  A1_MEAN_2[i]  <- mean(A1_Male_Het_BW[[i]], na.rm = TRUE) 
}
A1_MEAN_2 <- as.data.frame(A1_MEAN_2)

##################################
#Armature 1 - Female WT BW

A1_SEM_3 <- 0 
for (i in 1:length(A1_Female_WT_BW)){
  A1_SEM_3[i] <- (std.error(A1_Female_WT_BW[i]))
}
A1_SEM_3 <- as.data.frame(A1_SEM_3)

A1_MEAN_3 <- 0
A1_Female_WT_BW <- sapply(A1_Female_WT_BW, as.numeric)
A1_Female_WT_BW <- as.data.frame(A1_Female_WT_BW)
for (i in 1:length(A1_Female_WT_BW)){
  A1_MEAN_3[i]  <- mean(A1_Female_WT_BW[[i]], na.rm = TRUE) 
}
A1_MEAN_3 <- as.data.frame(A1_MEAN_3)

##########################################
#Armature 1 - Female HET BW

A1_SEM_4 <- 0 
for (i in 1:length(A1_Female_Het_BW)){
  A1_SEM_4[i] <- (std.error(A1_Female_Het_BW[i]))
}
A1_SEM_4 <- as.data.frame(A1_SEM_4)

A1_MEAN_4 <- 0
A1_Female_Het_BW <- sapply(A1_Female_Het_BW, as.numeric)
A1_Female_Het_BW <- as.data.frame(A1_Female_Het_BW)
for (i in 1:length(A1_Female_Het_BW)){
  A1_MEAN_4[i]  <- mean(A1_Female_Het_BW[[i]], na.rm = TRUE) 
}
A1_MEAN_4 <- as.data.frame(A1_MEAN_4)


############### #Armature 2 - Male WT BW #####################

A2_SEM_1 <- 0 
for (i in 1:length(A2_Male_WT_BW)){
  A2_SEM_1[i] <- (std.error(A2_Male_WT_BW[i]))
}
A2_SEM_1 <- as.data.frame(A2_SEM_1)

A2_MEAN_1 <- 0
A2_Male_WT_BW <- sapply(A2_Male_WT_BW, as.numeric)
A2_Male_WT_BW <- as.data.frame(A2_Male_WT_BW)
for (i in 1:length(A2_Male_WT_BW)){
  A2_MEAN_1[i]  <- mean(A2_Male_WT_BW[[i]], na.rm = TRUE) 
}
A2_MEAN_1 <- as.data.frame(A2_MEAN_1)

###############################
#Armature 2 - Male HET BW

A2_SEM_2 <- 0 
for (i in 1:length(A2_Male_Het_BW)){
  A2_SEM_2[i] <- (std.error(A2_Male_Het_BW[i]))
}
A2_SEM_2 <- as.data.frame(A2_SEM_2)

A2_MEAN_2 <- 0
A2_Male_Het_BW <- sapply(A2_Male_Het_BW, as.numeric)
A2_Male_Het_BW <- as.data.frame(A2_Male_Het_BW)
for (i in 1:length(A2_Male_Het_BW)){
  A2_MEAN_2[i]  <- mean(A2_Male_Het_BW[[i]], na.rm = TRUE) 
}
A2_MEAN_2 <- as.data.frame(A2_MEAN_2)

##################################
#Armature 2 - Female WT BW

A2_SEM_3 <- 0 
for (i in 1:length(A2_Female_WT_BW)){
  A2_SEM_3[i] <- (std.error(A2_Female_WT_BW[i]))
}
A2_SEM_3 <- as.data.frame(A2_SEM_3)

A2_MEAN_3 <- 0
A2_Female_WT_BW <- sapply(A2_Female_WT_BW, as.numeric)
A2_Female_WT_BW <- as.data.frame(A2_Female_WT_BW)
for (i in 1:length(A2_Female_WT_BW)){
  A2_MEAN_3[i]  <- mean(A2_Female_WT_BW[[i]], na.rm = TRUE) 
}
A2_MEAN_3 <- as.data.frame(A2_MEAN_3)

##########################################
#Armature 2 - Female HET BW

A2_SEM_4 <- 0 
for (i in 1:length(A2_Female_Het_BW)){
  A2_SEM_4[i] <- (std.error(A2_Female_Het_BW[i]))
}
A2_SEM_4 <- as.data.frame(A2_SEM_4)

A2_MEAN_4 <- 0
A2_Female_Het_BW <- sapply(A2_Female_Het_BW, as.numeric)
A2_Female_Het_BW <- as.data.frame(A2_Female_Het_BW)
for (i in 1:length(A2_Female_Het_BW)){
  A2_MEAN_4[i]  <- mean(A2_Female_Het_BW[[i]], na.rm = TRUE) 
}
A2_MEAN_4 <- as.data.frame(A2_MEAN_4)

########## DAY ELAPSED AVERAGING for X axis ##################
# Armature 1 - Male WT Days

A1_Days_1 <- 0
A1_Male_WT_Days <- sapply(A1_Male_WT_Days, as.numeric)
A1_Male_WT_Days <- as.data.frame(A1_Male_WT_Days)
for (i in 1:length(A1_Male_WT_Days)){
  A1_Days_1[i]  <- mean(A1_Male_WT_Days[[i]], na.rm = TRUE) 
}
A1_Days_1 <- as.data.frame(A1_Days_1)


##############################################
# Armature 1 - Male HET Days

A1_Days_2 <- 0
A1_Male_Het_Days <- sapply(A1_Male_Het_Days, as.numeric)
A1_Male_Het_Days <- as.data.frame(A1_Male_Het_Days)
for (i in 1:length(A1_Male_Het_Days)){
  A1_Days_2[i]  <- mean(A1_Male_Het_Days[[i]], na.rm = TRUE) 
}
A1_Days_2 <- as.data.frame(A1_Days_2)


##################################################
# Armature 1 - Female WT Days

A1_Days_3 <- 0
A1_Female_WT_Days <- sapply(A1_Female_WT_Days, as.numeric)
A1_Female_WT_Days <- as.data.frame(A1_Female_WT_Days)
for (i in 1:length(A1_Female_WT_Days)){
  A1_Days_3[i]  <- mean(A1_Female_WT_Days[[i]], na.rm = TRUE) 
}
A1_Days_3 <- as.data.frame(A1_Days_3)

####################################################
# Armature 1 - Female Het Days

A1_Days_4 <- 0
A1_Female_Het_Days <- sapply(A1_Female_Het_Days, as.numeric)
A1_Female_Het_Days <- as.data.frame(A1_Female_Het_Days)
for (i in 1:length(A1_Female_Het_Days)){
  A1_Days_4[i]  <- mean(A1_Female_Het_Days[[i]], na.rm = TRUE) 
}
A1_Days_4 <- as.data.frame(A1_Days_4)

######################################################
# Armature 2 - Male WT Days

A2_Days_1 <- 0
A2_Male_WT_Days <- sapply(A2_Male_WT_Days, as.numeric)
A2_Male_WT_Days <- as.data.frame(A2_Male_WT_Days)
for (i in 1:length(A2_Male_WT_Days)){
  A2_Days_1[i]  <- mean(A2_Male_WT_Days[[i]], na.rm = TRUE) 
}
A2_Days_1 <- as.data.frame(A2_Days_1)


##############################################
# Armature 2 - Male HET Days

A2_Days_2 <- 0
A2_Male_Het_Days <- sapply(A2_Male_Het_Days, as.numeric)
A2_Male_Het_Days <- as.data.frame(A2_Male_Het_Days)
for (i in 1:length(A2_Male_Het_Days)){
  A2_Days_2[i]  <- mean(A2_Male_Het_Days[[i]], na.rm = TRUE) 
}
A2_Days_2 <- as.data.frame(A2_Days_2)


##################################################
# Armature 2 - Female WT Days

A2_Days_3 <- 0
A2_Female_WT_Days <- sapply(A2_Female_WT_Days, as.numeric)
A2_Female_WT_Days <- as.data.frame(A2_Female_WT_Days)
for (i in 1:length(A2_Female_WT_Days)){
  A2_Days_3[i]  <- mean(A2_Female_WT_Days[[i]], na.rm = TRUE) 
}
A2_Days_3 <- as.data.frame(A2_Days_3)

####################################################
# Armature 2 - Female Het Days

A2_Days_4 <- 0
A2_Female_Het_Days <- sapply(A2_Female_Het_Days, as.numeric)
A2_Female_Het_Days <- as.data.frame(A2_Female_Het_Days)
for (i in 1:length(A2_Female_Het_Days)){
  A2_Days_4[i]  <- mean(A2_Female_Het_Days[[i]], na.rm = TRUE) 
}
A2_Days_4 <- as.data.frame(A2_Days_4)

############ ARM 1 & 2 DATA TABLE CONSTRUCTION - Data Merging ###################

# binding the first two columns as it is 
# and stacking the third and fourth columns

A1_MEAN_1$numbers<-1:nrow(A1_MEAN_1)
A1_MEAN_2$numbers<-1:nrow(A1_MEAN_2)
A1_MEAN_3$numbers<-1:nrow(A1_MEAN_3)
A1_MEAN_4$numbers<-1:nrow(A1_MEAN_4)
A1_SEM_1$numbers<-1:nrow(A1_SEM_1)
A1_SEM_2$numbers<-1:nrow(A1_SEM_2)
A1_SEM_3$numbers<-1:nrow(A1_SEM_3)
A1_SEM_4$numbers<-1:nrow(A1_SEM_4)
A1_Days_1$numbers<-1:nrow(A1_Days_1)
A1_Days_2$numbers<-1:nrow(A1_Days_2)
A1_Days_3$numbers<-1:nrow(A1_Days_3)
A1_Days_4$numbers<-1:nrow(A1_Days_4)


merge_1_ARM_1 <- merge(A1_MEAN_1, A1_MEAN_2, by.x = "numbers")
merge_2_ARM_1 <- merge(merge_1_ARM_1, A1_MEAN_3, by = "numbers")
merge_3_ARM_1 <- merge(merge_2_ARM_1, A1_MEAN_4, by = "numbers")
merge_4_ARM_1 <- merge(merge_3_ARM_1, A1_SEM_1, by = "numbers")
merge_5_ARM_1 <- merge(merge_4_ARM_1, A1_SEM_2, by = "numbers")
merge_6_ARM_1 <- merge(merge_5_ARM_1, A1_SEM_3, by = "numbers")
merge_7_ARM_1 <- merge(merge_6_ARM_1, A1_SEM_4, by = "numbers")
merge_8_ARM_1 <- merge(merge_7_ARM_1, A1_Days_1, by = "numbers")
merge_9_ARM_1 <- merge(merge_8_ARM_1, A1_Days_2, by = "numbers")
merge_10_ARM_1 <- merge(merge_9_ARM_1, A1_Days_3, by = "numbers")
merge_11_ARM_1 <- merge(merge_10_ARM_1, A1_Days_4, by = "numbers")

colnames(merge_11_ARM_1) <- c("Numbers", "Male WT (n=10)", "Male Het (n=10)", "Female WT (n=10)", "Female Het (n=10)")

ARM_1_BW_Combined <- cbind(merge_11_ARM_1[1], 
                           stack(merge_11_ARM_1[2:5]), 
                           stack(merge_11_ARM_1[6:9]),
                           stack(merge_11_ARM_1[10:13]))

colnames(ARM_1_BW_Combined) <- c("Replicate #", "BW", "ID", "SEM", "SEM ID", "Days_Elapsed", "DAYS ID")

ARM_1_BW_Combined <- ARM_1_BW_Combined[,-c(5,7)]
ARM_1_BW_Combined <- ARM_1_BW_Combined[, c(1,3,2,4,5)]

##################### ARM 2 Merging ##########################

A2_MEAN_1$numbers<-1:nrow(A2_MEAN_1)
A2_MEAN_2$numbers<-1:nrow(A2_MEAN_2)
A2_MEAN_3$numbers<-1:nrow(A2_MEAN_3)
A2_MEAN_4$numbers<-1:nrow(A2_MEAN_4)
A2_SEM_1$numbers<-1:nrow(A2_SEM_1)
A2_SEM_2$numbers<-1:nrow(A2_SEM_2)
A2_SEM_3$numbers<-1:nrow(A2_SEM_3)
A2_SEM_4$numbers<-1:nrow(A2_SEM_4)
A2_Days_1$numbers<-1:nrow(A2_Days_1)
A2_Days_2$numbers<-1:nrow(A2_Days_2)
A2_Days_3$numbers<-1:nrow(A2_Days_3)
A2_Days_4$numbers<-1:nrow(A2_Days_4)


merge_1_ARM_2 <- merge(A2_MEAN_1, A2_MEAN_2, by.x = "numbers")
merge_2_ARM_2 <- merge(merge_1_ARM_2, A2_MEAN_3, by = "numbers")
merge_3_ARM_2 <- merge(merge_2_ARM_2, A2_MEAN_4, by = "numbers")
merge_4_ARM_2 <- merge(merge_3_ARM_2, A2_SEM_1, by = "numbers")
merge_5_ARM_2 <- merge(merge_4_ARM_2, A2_SEM_2, by = "numbers")
merge_6_ARM_2 <- merge(merge_5_ARM_2, A2_SEM_3, by = "numbers")
merge_7_ARM_2 <- merge(merge_6_ARM_2, A2_SEM_4, by = "numbers")
merge_8_ARM_2 <- merge(merge_7_ARM_2, A2_Days_1, by = "numbers")
merge_9_ARM_2 <- merge(merge_8_ARM_2, A2_Days_2, by = "numbers")
merge_10_ARM_2 <- merge(merge_9_ARM_2, A2_Days_3, by = "numbers")
merge_11_ARM_2 <- merge(merge_10_ARM_2, A2_Days_4, by = "numbers")

colnames(merge_11_ARM_2) <- c("Numbers", "Male WT (n=10)", "Male Het (n=10)", "Female WT (n=10)", "Female Het (n=10)")

ARM_2_BW_Combined <- cbind(merge_11_ARM_2[1], 
                           stack(merge_11_ARM_2[2:5]), 
                           stack(merge_11_ARM_2[6:9]),
                           stack(merge_11_ARM_2[10:13]))

colnames(ARM_2_BW_Combined) <- c("Replicate #", "BW", "ID", "SEM", "SEM ID", "Days_Elapsed", "DAYS ID")

ARM_2_BW_Combined <- ARM_2_BW_Combined[,-c(5,7)]
ARM_2_BW_Combined <- ARM_2_BW_Combined[, c(1,3,2,4,5)]

########### GGPLOT LINE CONNECTED SCATTER #################

ARM_1_BW_Combined <- pmax(ARM_1_BW_Combined, 0)
ARM_2_BW_Combined <- pmax(ARM_2_BW_Combined, 0)

ggplot(ARM_1_BW_Combined, aes(Days_Elapsed, BW, colour = ID)) + 
  geom_point() +
  geom_line() + 
  labs(title = "Arm-A", subtitle = "291_LUTZ_BW", y = "BW [g]", x = "Days Elapsed",) +
  guides(fill=guide_legend(title="g")) +
  geom_errorbar(aes(ymin=BW-SEM, ymax=BW+SEM), width=2,
                position=position_dodge(.9)) +
  theme_bw()



ggplot(ARM_2_BW_Combined, aes(Days_Elapsed, BW, colour = ID)) + 
  geom_point() +
  geom_line() + 
  labs(title = "Arm-B", subtitle = "291_LUTZ_BW", y = "BW [g]", x = "Days Elapsed",) +
  guides(fill=guide_legend(title="g")) +
  geom_errorbar(aes(ymin=BW-SEM, ymax=BW+SEM), width=2,
                position=position_dodge(.9)) +
  theme_bw()







