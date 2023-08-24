
install.packages("rstudioapi")
install.packages("data.table")
install.packages("readxl")
install.packages("ff")
install.packages("plotrix")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("FSA")
install.packages("dunn.test")
install.packages("geom_signif")
install.packages("plyr")
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("rstatix")
install.packages("tidyr")
install.packages("reshape2")

library(reshape2)
library(tidyr)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(plyr)
library(dunn.test)
library(FSA)
library(data.table)
library(readxl)
library(ff)
library(plotrix)
library(ggplot2)
library(dplyr)

options(scipen = 100, digits = 4)

setwd("C:/Users/grindr/OneDrive - The Jackson Laboratory/Documents/329_Liv_Plasma_Data_Analysis")

########## DATA CLEANING AND FORMATTING - removed unnecessary rows & renamed columns

raw_data <- read.csv("JAX_PEX1_G844D_329_Plasma_6wks_6-21-23.csv", 
                     header=TRUE, sep= ",", fill = TRUE)

clean_data_1 <- raw_data[-c(1:7,8,10,11,79:159),-c(10,22,28,30,31,36:48)]

specific_FA_Data <- clean_data_1[-c(55:69),]
row.names(specific_FA_Data) <- specific_FA_Data[,1]
specific_FA_Data <- specific_FA_Data[,-1]
colnames(specific_FA_Data) <- specific_FA_Data[1,]
specific_FA_Data <- specific_FA_Data[-1,]
specific_FA_Data <- specific_FA_Data[-1,]

total_FA_Data <- clean_data_1[-c(2:58,67),]
row.names(total_FA_Data) <- total_FA_Data[,1]
total_FA_Data <- total_FA_Data[,-c(1)]
colnames(total_FA_Data) <- colnames(specific_FA_Data)
total_FA_Data <- total_FA_Data[-1,]


######### DOSING SEPARATION ################

B_spec <- specific_FA_Data[,c(1:6)]
C_spec <- specific_FA_Data[,c(7:12)]
D_spec <- specific_FA_Data[,c(13:19)]
E_spec_WT <- specific_FA_Data[,c(25:29)]
E_spec_MU <- specific_FA_Data[,c(20:24)]


B_Tot <- total_FA_Data[,c(1:6)]
C_Tot <- total_FA_Data[,c(7:12)]
D_Tot <- total_FA_Data[,c(13:19)]
E_Tot_WT <- total_FA_Data[,c(25:29)]
E_Tot_MU <- total_FA_Data[,c(20:24)]




########## SEM and MEAN cALCULATION - SPECIFIC FA GROUP ################
# DOSE B

B_S_SEM <- apply(B_spec, 1, sd)
B_S_SEM <- B_S_SEM / sqrt(6)
B_S_SEM <- as.data.frame(B_S_SEM)

B_spec1 <- sapply(B_spec, as.numeric)
B_spec1 <- as.data.frame(B_spec1)
B_S_MEAN <- rowMeans(B_spec1)
B_S_MEAN <- as.data.frame(B_S_MEAN)

# DOSE C

C_S_SEM <- apply(C_spec, 1, sd)
C_S_SEM <- C_S_SEM / sqrt(6)
C_S_SEM <- as.data.frame(C_S_SEM)

C_spec1 <- sapply(C_spec, as.numeric)
C_spec1 <- as.data.frame(C_spec1)
C_S_MEAN <- rowMeans(C_spec1)
C_S_MEAN <- as.data.frame(C_S_MEAN)

# DOSE D

D_S_SEM <- apply(D_spec, 1, sd)
D_S_SEM <- D_S_SEM / sqrt(6)
D_S_SEM <- as.data.frame(D_S_SEM)

D_spec1 <- sapply(D_spec, as.numeric)
D_spec1 <- as.data.frame(D_spec1)
D_S_MEAN <- rowMeans(D_spec1)
D_S_MEAN <- as.data.frame(D_S_MEAN)

# DOSE E WT

EWT_S_SEM <- apply(E_spec_WT, 1, sd)
EWT_S_SEM <- EWT_S_SEM / sqrt(6)
EWT_S_SEM <- as.data.frame(EWT_S_SEM)

E_spec_WT1 <- sapply(E_spec_WT, as.numeric)
E_spec_WT1 <- as.data.frame(E_spec_WT1)
EWT_S_MEAN <- rowMeans(E_spec_WT1)
EWT_S_MEAN <- as.data.frame(EWT_S_MEAN)

# DOSE E MU

EMU_S_SEM <- apply(E_spec_MU, 1, sd)
EMU_S_SEM <- EMU_S_SEM / sqrt(6)
EMU_S_SEM <- as.data.frame(EMU_S_SEM)

E_spec_MU1 <- sapply(E_spec_MU, as.numeric)
E_spec_MU1 <- as.data.frame(E_spec_MU1)
EMU_S_MEAN <- rowMeans(E_spec_MU1)
EMU_S_MEAN <- as.data.frame(EMU_S_MEAN)

########## SEM and MEAN cALCULATION - Total FA GROUP ################

B_T_SEM <- apply(B_Tot, 1, sd)
B_T_SEM <- B_T_SEM / sqrt(6)
B_T_SEM <- as.data.frame(B_T_SEM)

B_Tot1 <- sapply(B_Tot, as.numeric)
B_Tot1 <- as.data.frame(B_Tot1)
B_T_MEAN <- rowMeans(B_Tot1)
B_T_MEAN <- as.data.frame(B_T_MEAN)

# DOSE C

C_T_SEM <- apply(C_Tot, 1, sd)
C_T_SEM <- C_T_SEM / sqrt(6)
C_T_SEM <- as.data.frame(C_T_SEM)

C_Tot1 <- sapply(C_Tot, as.numeric)
C_Tot1 <- as.data.frame(C_Tot1)
C_T_MEAN <- rowMeans(C_Tot1)
C_T_MEAN <- as.data.frame(C_T_MEAN)

# DOSE D

D_T_SEM <- apply(D_Tot, 1, sd)
D_T_SEM <- D_T_SEM / sqrt(6)
D_T_SEM <- as.data.frame(D_T_SEM)

D_Tot1 <- sapply(D_Tot, as.numeric)
D_Tot1 <- as.data.frame(D_Tot1)
D_T_MEAN <- rowMeans(D_Tot1)
D_T_MEAN <- as.data.frame(D_T_MEAN)

# DOSE E WT

EWT_T_SEM <- apply(E_Tot_WT, 1, sd)
EWT_T_SEM <- EWT_T_SEM / sqrt(6)
EWT_T_SEM <- as.data.frame(EWT_T_SEM)

EWT_Tot1 <- sapply(E_Tot_WT, as.numeric)
EWT_Tot1 <- as.data.frame(EWT_Tot1)
EWT_T_MEAN <- rowMeans(EWT_Tot1)
EWT_T_MEAN <- as.data.frame(EWT_T_MEAN)

# DOSE E MU

EMU_T_SEM <- apply(E_Tot_MU, 1, sd)
EMU_T_SEM <- EMU_T_SEM / sqrt(6)
EMU_T_SEM <- as.data.frame(EMU_T_SEM)

EMU_Tot1 <- sapply(E_Tot_MU, as.numeric)
EMU_Tot1 <- as.data.frame(EMU_Tot1)
EMU_T_MEAN <- rowMeans(EMU_Tot1)
EMU_T_MEAN <- as.data.frame(EMU_T_MEAN)


############ Plasma VLCFA BCFA TOTALS 6 WK ug/mL graph ############

B_T_MEAN$numbers<-1:nrow(B_T_MEAN)
C_T_MEAN$numbers<-1:nrow(C_T_MEAN)
D_T_MEAN$numbers<-1:nrow(D_T_MEAN)
EWT_T_MEAN$numbers<-1:nrow(EWT_T_MEAN)
EMU_T_MEAN$numbers<-1:nrow(EMU_T_MEAN)
B_T_SEM$numbers<-1:nrow(B_T_SEM)
C_T_SEM$numbers<-1:nrow(C_T_SEM)
D_T_SEM$numbers<-1:nrow(D_T_SEM)
EWT_T_SEM$numbers<-1:nrow(EWT_T_SEM)
EMU_T_SEM$numbers<-1:nrow(EMU_T_SEM)

merge_1_T <- merge(B_T_MEAN, C_T_MEAN, by.x = "numbers")
merge_2_T <- merge(merge_1_T, D_T_MEAN, by = "numbers")
merge_3_T <- merge(merge_2_T, EWT_T_MEAN, by = "numbers")
merge_4_T <- merge(merge_3_T, EMU_T_MEAN, by = "numbers")
merge_5_T <- merge(merge_4_T, B_T_SEM, by = "numbers")
merge_6_T <- merge(merge_5_T, C_T_SEM, by = "numbers")
merge_7_T <- merge(merge_6_T, D_T_SEM, by = "numbers")
merge_8_T <- merge(merge_7_T, EWT_T_SEM, by = "numbers")
merge_9_T <- merge(merge_8_T, EMU_T_SEM, by = "numbers")

colnames(merge_9_T) <- c("Numbers", "Pex1(G844D/G844D)- Dil 1:4 (N=6)", "Pex1(G844D/G844D)- Dil 1:10 (N=6)", "Pex1(G844D/G844D)- Dil 1:40 (N=7)", "Pex1(+/+)- Vehicle (N=6)", "Pex1(G844D/G844D)- Vehicle (N=6)",
                         "B_S_SEM",
                         "C_S_SEM",
                         "D_S_SEM",
                         "EWT_S_SEM",
                         "EMU_S_SEM"
)

merge_9_T$FA <-row.names(B_Tot)

final_data_T <- cbind(merge_9_T[1], merge_9_T[12],
                      stack(merge_9_T[2:6]), 
                      stack(merge_9_T[7:11]))

colnames(final_data_T) <- c("Numbers", "Fatty_Acid", 
                            "Mean", 
                            "Group_ID", 
                            "SEM", 
                            "ID"
)

ggplot(final_data_T, aes(fill=Group_ID, y=Mean, x=Fatty_Acid)) +
  geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=Mean-SEM, ymax=Mean+SEM, color=factor(Group_ID)), width=0.25,
                position=position_dodge(.9),) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1.1, hjust=1.1))+
  theme(axis.text.x = element_text(size = 5)) +
  labs(title = "Plasma_VLCFA_BCFA_totals_6wks_ug/ml", y = "Plasma FA [ug/ml]", x = " ") +
  guides(fill=guide_legend(title=" "))+
  guides(color = FALSE, size = FALSE)


############ PLASMA VLCFA BCFA 6 WK ug/mL graph ############

B_S_MEAN$numbers<-1:nrow(B_S_MEAN)
C_S_MEAN$numbers<-1:nrow(C_S_MEAN)
D_S_MEAN$numbers<-1:nrow(D_S_MEAN)
EWT_S_MEAN$numbers<-1:nrow(EWT_S_MEAN)
EMU_S_MEAN$numbers<-1:nrow(EMU_S_MEAN)
B_S_SEM$numbers<-1:nrow(B_S_SEM)
C_S_SEM$numbers<-1:nrow(C_S_SEM)
D_S_SEM$numbers<-1:nrow(D_S_SEM)
EWT_S_SEM$numbers<-1:nrow(EWT_S_SEM)
EMU_S_SEM$numbers<-1:nrow(EMU_S_SEM)

merge_1_S <- merge(B_S_MEAN, C_S_MEAN, by.x = "numbers")
merge_2_S <- merge(merge_1_S, D_S_MEAN, by = "numbers")
merge_3_S <- merge(merge_2_S, EWT_S_MEAN, by = "numbers")
merge_4_S <- merge(merge_3_S, EMU_S_MEAN, by = "numbers")
merge_5_S <- merge(merge_4_S, B_S_SEM, by = "numbers")
merge_6_S <- merge(merge_5_S, C_S_SEM, by = "numbers")
merge_7_S <- merge(merge_6_S, D_S_SEM, by = "numbers")
merge_8_S <- merge(merge_7_S, EWT_S_SEM, by = "numbers")
merge_9_S <- merge(merge_8_S, EMU_S_SEM, by = "numbers")

colnames(merge_9_S) <- c("Numbers", "Pex1(G844D/G844D)- Dil 1:4 (N=6)", "Pex1(G844D/G844D)- Dil 1:10 (N=6)", "Pex1(G844D/G844D)- Dil 1:40 (N=7)", "Pex1(+/+)- Vehicle (N=6)", "Pex1(G844D/G844D)- Vehicle (N=6)",
                         "B_S_SEM",
                         "C_S_SEM",
                         "D_S_SEM",
                         "EWT_S_SEM",
                         "EMU_S_SEM"
)

merge_9_S$FA <-row.names(B_spec)

final_data_S <- cbind(merge_9_S[1], merge_9_S[12],
                      stack(merge_9_S[2:6]), 
                      stack(merge_9_S[7:11]))

colnames(final_data_S) <- c("Numbers", "Fatty_Acid", 
                            "Mean", 
                            "Group_ID", 
                            "SEM", 
                            "ID"
)

ggplot(final_data_S, aes(fill=Group_ID, y=Mean, x=Fatty_Acid)) +
  geom_bar(position=position_dodge(), stat='identity') + 
  geom_errorbar(aes(ymin=Mean-SEM, ymax=Mean+SEM, color=factor(Group_ID)), width=0.25,
                position=position_dodge(.9),) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1.1, hjust=1.1))+
  theme(axis.text.x = element_text(size = 5)) +
  labs(title = "Plasma_VLCFA_BCFA_totals_6wks_ug/mL", y = "Plasma FA [ug/mL]", x = " ") +
  guides(fill=guide_legend(title=" "))+
  guides(color = FALSE, size = FALSE)


################### Plasma Totals BCFA Graphing #######################


rownames(merge_9_T) <- merge_9_T[,12]

BCFA_B <- t(B_Tot)
BCFA_B <- as.data.frame(BCFA_B)
BCFA_B <- as.data.frame(BCFA_B[,c(6)])
colnames(BCFA_B) <- c("B")

BCFA_C <- t(C_Tot)
BCFA_C <- as.data.frame(BCFA_C)
BCFA_C <- as.data.frame(BCFA_C[,c(6)])
colnames(BCFA_C) <- c("C")

BCFA_D <- t(D_Tot)
BCFA_D <- as.data.frame(BCFA_D)
BCFA_D <- as.data.frame(BCFA_D[,c(6)])
colnames(BCFA_D) <- c("D")

BCFA_EWT <- t(E_Tot_WT)
BCFA_EWT <- as.data.frame(BCFA_EWT)
BCFA_EWT <- as.data.frame(BCFA_EWT[,c(6)])
colnames(BCFA_EWT) <- c("EWT")

BCFA_EMU <- t(E_Tot_MU)
BCFA_EMU <- as.data.frame(BCFA_EMU)
BCFA_EMU <- as.data.frame(BCFA_EMU[,c(6)])
colnames(BCFA_EMU) <- c("EMU")

B_Tot_point <- B_Tot[c(6),]
rownames(B_Tot_point) <- c("B")
colnames(B_Tot_point) <- c("1", "2", "3", "4", "5", "6")
C_Tot_point <- C_Tot[c(6),]
rownames(C_Tot_point) <- c("C")
colnames(C_Tot_point) <- c("1", "2", "3", "4", "5", "6")
D_Tot_point <- D_Tot[c(6),]
rownames(D_Tot_point) <- c("D")
colnames(D_Tot_point) <- c("1", "2", "3", "4", "5", "6", "7")
EWT_Tot_point <- E_Tot_WT[c(6),]
rownames(EWT_Tot_point) <- c("EWT")
colnames(EWT_Tot_point) <- c("1", "2", "3", "4", "5")
EMU_Tot_point <- E_Tot_MU[c(6),]
rownames(EMU_Tot_point) <- c("EMU")
colnames(EMU_Tot_point) <- c("1", "2", "3", "4", "5")


merged_point <- rbind.fill(B_Tot_point,
                      C_Tot_point,
                      D_Tot_point,
                      EWT_Tot_point,
                      EMU_Tot_point)

rownames(merged_point) <- c("B", 'C', "D", "EWT", "EMU")
merged_point <- t(merged_point)
merged_point_1 <- as.data.frame(merged_point)
merged_point <- reshape2::melt(merged_point, variable.name = 'Name')
merged_point <- na.omit(merged_point)
merged_point_1 <- sapply(merged_point_1, as.numeric)
merged_point_1 <- as.data.frame(merged_point_1)



BCFA_B$numbers<-1:nrow(BCFA_B)
BCFA_C$numbers<-1:nrow(BCFA_C)
BCFA_D$numbers<-1:nrow(BCFA_D)
BCFA_EWT$numbers<-1:nrow(BCFA_EWT)
BCFA_EMU$numbers<-1:nrow(BCFA_EMU)

merge_1 <- merge(BCFA_B, BCFA_C, by = "numbers" )
merge_2 <- merge(merge_1, BCFA_D, by = "numbers" )
merge_3 <- merge(merge_2, BCFA_EWT, by = "numbers" )
merge_4 <- merge(merge_3, BCFA_EMU, by = "numbers" )
merge_4 <- merge_4[,-c(1)]

merge_4 <- sapply(merge_4, as.numeric)
merge_4 <- as.data.frame(merge_4)
stack_data <- cbind(stack(merge_4[1:5]))

kruskal.test(merge_4)

dunnTest(stack_data$values, stack_data$ind, method="hs")

stack_data %>% 
  dunn_test(values ~ ind, p.adjust.method = "bonferroni")

graph_data <- colMeans(merge_4)
graph_data <- as.data.frame(graph_data)
graph_data$names <- row.names(graph_data)
SEM <- apply(merge_4, 2, sd)
SEM <- SEM/ sqrt(6)
SEM <- as.data.frame(SEM)
graph_data$sem <- SEM$SEM
colnames(graph_data) <- c("Value", "Name", "SEM")

y_end <- max(max(merged_point_1$D, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end <- 1.10 * y_end
L <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
R <- 1.05 * (max(merged_point_1$D, na.rm = TRUE))
y_end_1 <- 1.10 * y_end
L_1 <- 1.05 * y_end
R_1 <- 1.05 * (max(merged_point_1$EWT, na.rm = TRUE))
y_end_2 <-  max(max(merged_point_1$C, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end_2 <- 1.10 * y_end_2
L_2 <- 1.05 * (max(merged_point_1$C, na.rm = TRUE))
R_2 <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
y_end_3 <- 1.10 * y_end_2
L_3 <- 1.05 * (max(merged_point_1$B, na.rm = TRUE))
R_3 <- 1.05 * y_end_2
ns_1 <- 1.05 * y_end_1
ns_2 <- 1.05 * y_end
ns_3 <- 1.05 * y_end_2
ns_4 <- 1.05 * y_end_3

graph_data$Name <- factor(graph_data$Name, levels = c("B", "C", "EMU", "D", "EWT"))
merged_point$Var2 <- factor(merged_point$Var2, levels = c("B", "C", "EMU", "D", "EWT"))

merged_point$value <- as.numeric(merged_point$value)

ggplot() + 
  scale_x_discrete(limits = c("B", "C", "EMU", "D", 'EWT'))+
  geom_bar(graph_data, mapping = aes(fill=Name,x=Name, y=Value),position = position_dodge(), stat = "identity", alpha = 0.5, color = "Black") +
  geom_errorbar(graph_data, mapping = aes(Name,ymin=Value-SEM, ymax=Value+SEM, color=Name), width=0.25,
                position=position_dodge(.9),)+
  geom_point(merged_point, mapping = aes(x = Var2, y = value, color = Var2)) +
  scale_fill_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  scale_color_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 5)) +
  labs(title = "Liver_Totals_BCFA_6wks_ug/mg", y = "Liver Branched FA [ug/mg]", x = " ") +
  guides(fill=guide_legend(title=" "))+
  guides(color = FALSE, size = FALSE)+
  geom_segment(aes(x = 5, y = R_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = y_end_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = L_1, xend = 3.15, yend = y_end_1))+
  annotate("text",x=4,y=ns_1,label="**")+
  geom_segment(aes(x = 3.15, y = L, xend = 3.15, yend = y_end))+
  geom_segment(aes(x = 3.15, y = y_end, xend = 4, yend = y_end))+
  geom_segment(aes(x = 4, y = R, xend = 4, yend = y_end))+
  annotate("text",x=3.5,y= ns_2,label="ns")+
  geom_segment(aes(x = 2, y = L_2, xend = 2, yend = y_end_2))+
  geom_segment(aes(x = 2, y = y_end_2, xend = 2.85, yend = y_end_2))+
  geom_segment(aes(x = 2.85, y = R_2, xend = 2.85, yend = y_end_2))+
  annotate("text",x=2.5,y=ns_3,label="ns")+
  geom_segment(aes(x = 1, y = L_3, xend = 1, yend = y_end_3))+
  geom_segment(aes(x = 1, y = y_end_3, xend = 2.85, yend = y_end_3))+
  geom_segment(aes(x = 2.85, y = R_3, xend = 2.85, yend = y_end_3))+
  annotate("text",x=2,y=ns_4,label="*")


################### Plasma Total Sat_VLCFA_6wks_ug/ml #######################  

BCFA_B <- t(B_Tot)
BCFA_B <- as.data.frame(BCFA_B)
BCFA_B <- as.data.frame(BCFA_B[,c(9)])
colnames(BCFA_B) <- c("B")

BCFA_C <- t(C_Tot)
BCFA_C <- as.data.frame(BCFA_C)
BCFA_C <- as.data.frame(BCFA_C[,c(9)])
colnames(BCFA_C) <- c("C")

BCFA_D <- t(D_Tot)
BCFA_D <- as.data.frame(BCFA_D)
BCFA_D <- as.data.frame(BCFA_D[,c(9)])
colnames(BCFA_D) <- c("D")

BCFA_EWT <- t(E_Tot_WT)
BCFA_EWT <- as.data.frame(BCFA_EWT)
BCFA_EWT <- as.data.frame(BCFA_EWT[,c(9)])
colnames(BCFA_EWT) <- c("EWT")

BCFA_EMU <- t(E_Tot_MU)
BCFA_EMU <- as.data.frame(BCFA_EMU)
BCFA_EMU <- as.data.frame(BCFA_EMU[,c(9)])
colnames(BCFA_EMU) <- c("EMU")

B_Tot_point <- B_Tot[c(9),]
rownames(B_Tot_point) <- c("B")
colnames(B_Tot_point) <- c("1", "2", "3", "4", "5", "6")
C_Tot_point <- C_Tot[c(9),]
rownames(C_Tot_point) <- c("C")
colnames(C_Tot_point) <- c("1", "2", "3", "4", "5", "6")
D_Tot_point <- D_Tot[c(9),]
rownames(D_Tot_point) <- c("D")
colnames(D_Tot_point) <- c("1", "2", "3", "4", "5", "6", "7")
EWT_Tot_point <- E_Tot_WT[c(9),]
rownames(EWT_Tot_point) <- c("EWT")
colnames(EWT_Tot_point) <- c("1", "2", "3", "4", "5")
EMU_Tot_point <- E_Tot_MU[c(9),]
rownames(EMU_Tot_point) <- c("EMU")
colnames(EMU_Tot_point) <- c("1", "2", "3", "4", "5")


merged_point <- rbind.fill(B_Tot_point,
                           C_Tot_point,
                           D_Tot_point,
                           EWT_Tot_point,
                           EMU_Tot_point)

rownames(merged_point) <- c("B", 'C', "D", "EWT", "EMU")
merged_point <- t(merged_point)
merged_point_1 <- as.data.frame(merged_point)
merged_point <- reshape2::melt(merged_point, variable.name = 'Name')
merged_point <- na.omit(merged_point)
merged_point_1 <- sapply(merged_point_1, as.numeric)
merged_point_1 <- as.data.frame(merged_point_1)



BCFA_B$numbers<-1:nrow(BCFA_B)
BCFA_C$numbers<-1:nrow(BCFA_C)
BCFA_D$numbers<-1:nrow(BCFA_D)
BCFA_EWT$numbers<-1:nrow(BCFA_EWT)
BCFA_EMU$numbers<-1:nrow(BCFA_EMU)

merge_1 <- merge(BCFA_B, BCFA_C, by = "numbers" )
merge_2 <- merge(merge_1, BCFA_D, by = "numbers" )
merge_3 <- merge(merge_2, BCFA_EWT, by = "numbers" )
merge_4 <- merge(merge_3, BCFA_EMU, by = "numbers" )
merge_4 <- merge_4[,-c(1)]

merge_4 <- sapply(merge_4, as.numeric)
merge_4 <- as.data.frame(merge_4)
stack_data <- cbind(stack(merge_4[1:5]))

kruskal.test(merge_4)

dunnTest(stack_data$values, stack_data$ind, method="hs")

stack_data %>% 
  dunn_test(values ~ ind, p.adjust.method = "bonferroni")

graph_data <- colMeans(merge_4)
graph_data <- as.data.frame(graph_data)
graph_data$names <- row.names(graph_data)
SEM <- apply(merge_4, 2, sd)
SEM <- SEM/ sqrt(6)
SEM <- as.data.frame(SEM)
graph_data$sem <- SEM$SEM
colnames(graph_data) <- c("Value", "Name", "SEM")

y_end <- max(max(merged_point_1$D, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end <- 1.10 * y_end
L <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
R <- 1.05 * (max(merged_point_1$D, na.rm = TRUE))
y_end_1 <- 1.10 * y_end
L_1 <- 1.05 * y_end
R_1 <- 1.05 * (max(merged_point_1$EWT, na.rm = TRUE))
y_end_2 <-  max(max(merged_point_1$C, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end_2 <- 1.10 * y_end_2
L_2 <- 1.05 * (max(merged_point_1$C, na.rm = TRUE))
R_2 <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
y_end_3 <- 1.10 * y_end_2
L_3 <- 1.05 * (max(merged_point_1$B, na.rm = TRUE))
R_3 <- 1.05 * y_end_2
ns_1 <- 1.05 * y_end_1
ns_2 <- 1.05 * y_end
ns_3 <- 1.05 * y_end_2
ns_4 <- 1.05 * y_end_3

graph_data$Name <- factor(graph_data$Name, levels = c("B", "C", "EMU", "D", "EWT"))
merged_point$Var2 <- factor(merged_point$Var2, levels = c("B", "C", "EMU", "D", "EWT"))

merged_point$value <- as.numeric(merged_point$value)

ggplot() + 
  scale_x_discrete(limits = c("B", "C", "EMU", "D", 'EWT'))+
  geom_bar(graph_data, mapping = aes(fill=Name,x=Name, y=Value),position = position_dodge(), stat = "identity", alpha = 0.5, color = "Black") +
  geom_errorbar(graph_data, mapping = aes(Name,ymin=Value-SEM, ymax=Value+SEM, color=Name), width=0.25,
                position=position_dodge(.9),)+
  geom_point(merged_point, mapping = aes(x = Var2, y = value, color = Var2)) +
  scale_fill_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  scale_color_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 5)) +
  labs(title = "Plasma Total Sat_VLCFA_6wks_ug/ml", y = "Plasma Branched FA [ug/mg]", x = " ") +
  guides(fill=guide_legend(title=" "))+
  guides(color = FALSE, size = FALSE)+
  geom_segment(aes(x = 5, y = R_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = y_end_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = L_1, xend = 3.15, yend = y_end_1))+
  annotate("text",x=4,y=ns_1,label="*")+
  geom_segment(aes(x = 3.15, y = L, xend = 3.15, yend = y_end))+
  geom_segment(aes(x = 3.15, y = y_end, xend = 4, yend = y_end))+
  geom_segment(aes(x = 4, y = R, xend = 4, yend = y_end))+
  annotate("text",x=3.5,y= ns_2,label="ns")+
  geom_segment(aes(x = 2, y = L_2, xend = 2, yend = y_end_2))+
  geom_segment(aes(x = 2, y = y_end_2, xend = 2.85, yend = y_end_2))+
  geom_segment(aes(x = 2.85, y = R_2, xend = 2.85, yend = y_end_2))+
  annotate("text",x=2.5,y=ns_3,label="ns")+
  geom_segment(aes(x = 1, y = L_3, xend = 1, yend = y_end_3))+
  geom_segment(aes(x = 1, y = y_end_3, xend = 2.85, yend = y_end_3))+
  geom_segment(aes(x = 2.85, y = R_3, xend = 2.85, yend = y_end_3))+
  annotate("text",x=2,y=ns_4,label="ns")

################### Plasma Pristanic FA_6 wks_ug/ml #######################  


BCFA_B <- t(B_spec)
BCFA_B <- as.data.frame(BCFA_B)
BCFA_B <- as.data.frame(BCFA_B[,c(48)])
colnames(BCFA_B) <- c("B")

BCFA_C <- t(C_spec)
BCFA_C <- as.data.frame(BCFA_C)
BCFA_C <- as.data.frame(BCFA_C[,c(48)])
colnames(BCFA_C) <- c("C")

BCFA_D <- t(D_spec)
BCFA_D <- as.data.frame(BCFA_D)
BCFA_D <- as.data.frame(BCFA_D[,c(48)])
colnames(BCFA_D) <- c("D")

BCFA_EWT <- t(E_spec_WT)
BCFA_EWT <- as.data.frame(BCFA_EWT)
BCFA_EWT <- as.data.frame(BCFA_EWT[,c(48)])
colnames(BCFA_EWT) <- c("EWT")

BCFA_EMU <- t(E_spec_MU)
BCFA_EMU <- as.data.frame(BCFA_EMU)
BCFA_EMU <- as.data.frame(BCFA_EMU[,c(48)])
colnames(BCFA_EMU) <- c("EMU")

B_Tot_point <- B_spec[c(48),]
rownames(B_Tot_point) <- c("B")
colnames(B_Tot_point) <- c("1", "2", "3", "4", "5", "6")
C_Tot_point <- C_spec[c(48),]
rownames(C_Tot_point) <- c("C")
colnames(C_Tot_point) <- c("1", "2", "3", "4", "5", "6")
D_Tot_point <- D_spec[c(48),]
rownames(D_Tot_point) <- c("D")
colnames(D_Tot_point) <- c("1", "2", "3", "4", "5", "6", "7")
EWT_Tot_point <- E_spec_WT[c(48),]
rownames(EWT_Tot_point) <- c("EWT")
colnames(EWT_Tot_point) <- c("1", "2", "3", "4", "5")
EMU_Tot_point <- E_spec_MU[c(48),]
rownames(EMU_Tot_point) <- c("EMU")
colnames(EMU_Tot_point) <- c("1", "2", "3", "4", "5")


merged_point <- rbind.fill(B_Tot_point,
                           C_Tot_point,
                           D_Tot_point,
                           EWT_Tot_point,
                           EMU_Tot_point)

rownames(merged_point) <- c("B", 'C', "D", "EWT", "EMU")
merged_point <- t(merged_point)
merged_point_1 <- as.data.frame(merged_point)
merged_point <- reshape2::melt(merged_point, variable.name = 'Name')
merged_point <- na.omit(merged_point)
merged_point_1 <- sapply(merged_point_1, as.numeric)
merged_point_1 <- as.data.frame(merged_point_1)



BCFA_B$numbers<-1:nrow(BCFA_B)
BCFA_C$numbers<-1:nrow(BCFA_C)
BCFA_D$numbers<-1:nrow(BCFA_D)
BCFA_EWT$numbers<-1:nrow(BCFA_EWT)
BCFA_EMU$numbers<-1:nrow(BCFA_EMU)

merge_1 <- merge(BCFA_B, BCFA_C, by = "numbers" )
merge_2 <- merge(merge_1, BCFA_D, by = "numbers" )
merge_3 <- merge(merge_2, BCFA_EWT, by = "numbers" )
merge_4 <- merge(merge_3, BCFA_EMU, by = "numbers" )
merge_4 <- merge_4[,-c(1)]

merge_4 <- sapply(merge_4, as.numeric)
merge_4 <- as.data.frame(merge_4)
stack_data <- cbind(stack(merge_4[1:5]))

kruskal.test(merge_4)

dunnTest(stack_data$values, stack_data$ind, method="hs")

stack_data %>% 
  dunn_test(values ~ ind, p.adjust.method = "bonferroni")

graph_data <- colMeans(merge_4)
graph_data <- as.data.frame(graph_data)
graph_data$names <- row.names(graph_data)
SEM <- apply(merge_4, 2, sd)
SEM <- SEM/ sqrt(6)
SEM <- as.data.frame(SEM)
graph_data$sem <- SEM$SEM
colnames(graph_data) <- c("Value", "Name", "SEM")

y_end <- max(max(merged_point_1$D, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end <- 1.10 * y_end
L <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
R <- 1.05 * (max(merged_point_1$D, na.rm = TRUE))
y_end_1 <- 1.10 * y_end
L_1 <- 1.05 * y_end
R_1 <- 1.05 * (max(merged_point_1$EWT, na.rm = TRUE))
y_end_2 <-  max(max(merged_point_1$C, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end_2 <- 1.10 * y_end_2
L_2 <- 1.05 * (max(merged_point_1$C, na.rm = TRUE))
R_2 <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
y_end_3 <- 1.10 * y_end_2
L_3 <- 1.05 * (max(merged_point_1$B, na.rm = TRUE))
R_3 <- 1.05 * y_end_2
ns_1 <- 1.05 * y_end_1
ns_2 <- 1.05 * y_end
ns_3 <- 1.05 * y_end_2
ns_4 <- 1.05 * y_end_3

graph_data$Name <- factor(graph_data$Name, levels = c("B", "C", "EMU", "D", "EWT"))
merged_point$Var2 <- factor(merged_point$Var2, levels = c("B", "C", "EMU", "D", "EWT"))

merged_point$value <- as.numeric(merged_point$value)

ggplot() + 
  scale_x_discrete(limits = c("B", "C", "EMU", "D", 'EWT'))+
  geom_bar(graph_data, mapping = aes(fill=Name,x=Name, y=Value),position = position_dodge(), stat = "identity", alpha = 0.5, color = "Black") +
  geom_errorbar(graph_data, mapping = aes(Name,ymin=Value-SEM, ymax=Value+SEM, color=Name), width=0.25,
                position=position_dodge(.9),)+
  geom_point(merged_point, mapping = aes(x = Var2, y = value, color = Var2)) +
  scale_fill_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  scale_color_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 5)) +
  labs(title = "Plasma Pristanic FA_6 wks_ug/ml", y = "Plasma FA [ug/mg]", x = " ") +
  guides(fill=guide_legend(title=" "))+
  guides(color = FALSE, size = FALSE)+
  geom_segment(aes(x = 5, y = R_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = y_end_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = L_1, xend = 3.15, yend = y_end_1))+
  annotate("text",x=4,y=ns_1,label="***")+
  geom_segment(aes(x = 3.15, y = L, xend = 3.15, yend = y_end))+
  geom_segment(aes(x = 3.15, y = y_end, xend = 4, yend = y_end))+
  geom_segment(aes(x = 4, y = R, xend = 4, yend = y_end))+
  annotate("text",x=3.5,y= ns_2,label="ns")+
  geom_segment(aes(x = 2, y = L_2, xend = 2, yend = y_end_2))+
  geom_segment(aes(x = 2, y = y_end_2, xend = 2.85, yend = y_end_2))+
  geom_segment(aes(x = 2.85, y = R_2, xend = 2.85, yend = y_end_2))+
  annotate("text",x=2.5,y=ns_3,label="ns")+
  geom_segment(aes(x = 1, y = L_3, xend = 1, yend = y_end_3))+
  geom_segment(aes(x = 1, y = y_end_3, xend = 2.85, yend = y_end_3))+
  geom_segment(aes(x = 2.85, y = R_3, xend = 2.85, yend = y_end_3))+
  annotate("text",x=2,y=ns_4,label="*")


################### Plasma Phytanic FA_6 wks_ug/ml #######################  


BCFA_B <- t(B_spec)
BCFA_B <- as.data.frame(BCFA_B)
BCFA_B <- as.data.frame(BCFA_B[,c(49)])
colnames(BCFA_B) <- c("B")

BCFA_C <- t(C_spec)
BCFA_C <- as.data.frame(BCFA_C)
BCFA_C <- as.data.frame(BCFA_C[,c(49)])
colnames(BCFA_C) <- c("C")

BCFA_D <- t(D_spec)
BCFA_D <- as.data.frame(BCFA_D)
BCFA_D <- as.data.frame(BCFA_D[,c(49)])
colnames(BCFA_D) <- c("D")

BCFA_EWT <- t(E_spec_WT)
BCFA_EWT <- as.data.frame(BCFA_EWT)
BCFA_EWT <- as.data.frame(BCFA_EWT[,c(49)])
colnames(BCFA_EWT) <- c("EWT")

BCFA_EMU <- t(E_spec_MU)
BCFA_EMU <- as.data.frame(BCFA_EMU)
BCFA_EMU <- as.data.frame(BCFA_EMU[,c(49)])
colnames(BCFA_EMU) <- c("EMU")

B_Tot_point <- B_spec[c(49),]
rownames(B_Tot_point) <- c("B")
colnames(B_Tot_point) <- c("1", "2", "3", "4", "5", "6")
C_Tot_point <- C_spec[c(49),]
rownames(C_Tot_point) <- c("C")
colnames(C_Tot_point) <- c("1", "2", "3", "4", "5", "6")
D_Tot_point <- D_spec[c(49),]
rownames(D_Tot_point) <- c("D")
colnames(D_Tot_point) <- c("1", "2", "3", "4", "5", "6", "7")
EWT_Tot_point <- E_spec_WT[c(49),]
rownames(EWT_Tot_point) <- c("EWT")
colnames(EWT_Tot_point) <- c("1", "2", "3", "4", "5")
EMU_Tot_point <- E_spec_MU[c(49),]
rownames(EMU_Tot_point) <- c("EMU")
colnames(EMU_Tot_point) <- c("1", "2", "3", "4", "5")


merged_point <- rbind.fill(B_Tot_point,
                           C_Tot_point,
                           D_Tot_point,
                           EWT_Tot_point,
                           EMU_Tot_point)

rownames(merged_point) <- c("B", 'C', "D", "EWT", "EMU")
merged_point <- t(merged_point)
merged_point_1 <- as.data.frame(merged_point)
merged_point <- reshape2::melt(merged_point, variable.name = 'Name')
merged_point <- na.omit(merged_point)
merged_point_1 <- sapply(merged_point_1, as.numeric)
merged_point_1 <- as.data.frame(merged_point_1)



BCFA_B$numbers<-1:nrow(BCFA_B)
BCFA_C$numbers<-1:nrow(BCFA_C)
BCFA_D$numbers<-1:nrow(BCFA_D)
BCFA_EWT$numbers<-1:nrow(BCFA_EWT)
BCFA_EMU$numbers<-1:nrow(BCFA_EMU)

merge_1 <- merge(BCFA_B, BCFA_C, by = "numbers" )
merge_2 <- merge(merge_1, BCFA_D, by = "numbers" )
merge_3 <- merge(merge_2, BCFA_EWT, by = "numbers" )
merge_4 <- merge(merge_3, BCFA_EMU, by = "numbers" )
merge_4 <- merge_4[,-c(1)]

merge_4 <- sapply(merge_4, as.numeric)
merge_4 <- as.data.frame(merge_4)
stack_data <- cbind(stack(merge_4[1:5]))

kruskal.test(merge_4)

dunnTest(stack_data$values, stack_data$ind, method="hs")

stack_data %>% 
  dunn_test(values ~ ind, p.adjust.method = "bonferroni")

graph_data <- colMeans(merge_4)
graph_data <- as.data.frame(graph_data)
graph_data$names <- row.names(graph_data)
SEM <- apply(merge_4, 2, sd)
SEM <- SEM/ sqrt(6)
SEM <- as.data.frame(SEM)
graph_data$sem <- SEM$SEM
colnames(graph_data) <- c("Value", "Name", "SEM")

y_end <- max(max(merged_point_1$D, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end <- 1.10 * y_end
L <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
R <- 1.05 * (max(merged_point_1$D, na.rm = TRUE))
y_end_1 <- 1.10 * y_end
L_1 <- 1.05 * y_end
R_1 <- 1.05 * (max(merged_point_1$EWT, na.rm = TRUE))
y_end_2 <-  max(max(merged_point_1$C, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end_2 <- 1.10 * y_end_2
L_2 <- 1.05 * (max(merged_point_1$C, na.rm = TRUE))
R_2 <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
y_end_3 <- 1.10 * y_end_2
L_3 <- 1.05 * (max(merged_point_1$B, na.rm = TRUE))
R_3 <- 1.05 * y_end_2
ns_1 <- 1.05 * y_end_1
ns_2 <- 1.05 * y_end
ns_3 <- 1.05 * y_end_2
ns_4 <- 1.05 * y_end_3

graph_data$Name <- factor(graph_data$Name, levels = c("B", "C", "EMU", "D", "EWT"))
merged_point$Var2 <- factor(merged_point$Var2, levels = c("B", "C", "EMU", "D", "EWT"))

merged_point$value <- as.numeric(merged_point$value)

ggplot() + 
  scale_x_discrete(limits = c("B", "C", "EMU", "D", 'EWT'))+
  geom_bar(graph_data, mapping = aes(fill=Name,x=Name, y=Value),position = position_dodge(), stat = "identity", alpha = 0.5, color = "Black") +
  geom_errorbar(graph_data, mapping = aes(Name,ymin=Value-SEM, ymax=Value+SEM, color=Name), width=0.25,
                position=position_dodge(.9),)+
  geom_point(merged_point, mapping = aes(x = Var2, y = value, color = Var2)) +
  scale_fill_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  scale_color_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 5)) +
  labs(title = "Plasma Phytanic FA_6 wks_ug/ml", y = "Plasma FA [ug/mg]", x = " ") +
  guides(fill=guide_legend(title=" "))+
  guides(color = FALSE, size = FALSE)+
  geom_segment(aes(x = 5, y = R_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = y_end_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = L_1, xend = 3.15, yend = y_end_1))+
  annotate("text",x=4,y=ns_1,label="**")+
  geom_segment(aes(x = 3.15, y = L, xend = 3.15, yend = y_end))+
  geom_segment(aes(x = 3.15, y = y_end, xend = 4, yend = y_end))+
  geom_segment(aes(x = 4, y = R, xend = 4, yend = y_end))+
  annotate("text",x=3.5,y= ns_2,label="ns")+
  geom_segment(aes(x = 2, y = L_2, xend = 2, yend = y_end_2))+
  geom_segment(aes(x = 2, y = y_end_2, xend = 2.85, yend = y_end_2))+
  geom_segment(aes(x = 2.85, y = R_2, xend = 2.85, yend = y_end_2))+
  annotate("text",x=2.5,y=ns_3,label="ns")+
  geom_segment(aes(x = 1, y = L_3, xend = 1, yend = y_end_3))+
  geom_segment(aes(x = 1, y = y_end_3, xend = 2.85, yend = y_end_3))+
  geom_segment(aes(x = 2.85, y = R_3, xend = 2.85, yend = y_end_3))+
  annotate("text",x=2,y=ns_4,label="**")

################### Plasma Sat VLCFA_C24_6 wks_ug/ml #######################  

BCFA_B <- t(B_spec)
BCFA_B <- as.data.frame(BCFA_B)
BCFA_B <- as.data.frame(BCFA_B[,c(11)])
colnames(BCFA_B) <- c("B")

BCFA_C <- t(C_spec)
BCFA_C <- as.data.frame(BCFA_C)
BCFA_C <- as.data.frame(BCFA_C[,c(11)])
colnames(BCFA_C) <- c("C")

BCFA_D <- t(D_spec)
BCFA_D <- as.data.frame(BCFA_D)
BCFA_D <- as.data.frame(BCFA_D[,c(11)])
colnames(BCFA_D) <- c("D")

BCFA_EWT <- t(E_spec_WT)
BCFA_EWT <- as.data.frame(BCFA_EWT)
BCFA_EWT <- as.data.frame(BCFA_EWT[,c(11)])
colnames(BCFA_EWT) <- c("EWT")

BCFA_EMU <- t(E_spec_MU)
BCFA_EMU <- as.data.frame(BCFA_EMU)
BCFA_EMU <- as.data.frame(BCFA_EMU[,c(11)])
colnames(BCFA_EMU) <- c("EMU")

B_Tot_point <- B_spec[c(11),]
rownames(B_Tot_point) <- c("B")
colnames(B_Tot_point) <- c("1", "2", "3", "4", "5", "6")
C_Tot_point <- C_spec[c(11),]
rownames(C_Tot_point) <- c("C")
colnames(C_Tot_point) <- c("1", "2", "3", "4", "5", "6")
D_Tot_point <- D_spec[c(11),]
rownames(D_Tot_point) <- c("D")
colnames(D_Tot_point) <- c("1", "2", "3", "4", "5", "6", "7")
EWT_Tot_point <- E_spec_WT[c(11),]
rownames(EWT_Tot_point) <- c("EWT")
colnames(EWT_Tot_point) <- c("1", "2", "3", "4", "5")
EMU_Tot_point <- E_spec_MU[c(11),]
rownames(EMU_Tot_point) <- c("EMU")
colnames(EMU_Tot_point) <- c("1", "2", "3", "4", "5")


merged_point <- rbind.fill(B_Tot_point,
                           C_Tot_point,
                           D_Tot_point,
                           EWT_Tot_point,
                           EMU_Tot_point)

rownames(merged_point) <- c("B", 'C', "D", "EWT", "EMU")
merged_point <- t(merged_point)
merged_point_1 <- as.data.frame(merged_point)
merged_point <- reshape2::melt(merged_point, variable.name = 'Name')
merged_point <- na.omit(merged_point)
merged_point_1 <- sapply(merged_point_1, as.numeric)
merged_point_1 <- as.data.frame(merged_point_1)



BCFA_B$numbers<-1:nrow(BCFA_B)
BCFA_C$numbers<-1:nrow(BCFA_C)
BCFA_D$numbers<-1:nrow(BCFA_D)
BCFA_EWT$numbers<-1:nrow(BCFA_EWT)
BCFA_EMU$numbers<-1:nrow(BCFA_EMU)

merge_1 <- merge(BCFA_B, BCFA_C, by = "numbers" )
merge_2 <- merge(merge_1, BCFA_D, by = "numbers" )
merge_3 <- merge(merge_2, BCFA_EWT, by = "numbers" )
merge_4 <- merge(merge_3, BCFA_EMU, by = "numbers" )
merge_4 <- merge_4[,-c(1)]

merge_4 <- sapply(merge_4, as.numeric)
merge_4 <- as.data.frame(merge_4)
stack_data <- cbind(stack(merge_4[1:5]))

kruskal.test(merge_4)

dunnTest(stack_data$values, stack_data$ind, method="hs")

stack_data %>% 
  dunn_test(values ~ ind, p.adjust.method = "bonferroni")

graph_data <- colMeans(merge_4)
graph_data <- as.data.frame(graph_data)
graph_data$names <- row.names(graph_data)
SEM <- apply(merge_4, 2, sd)
SEM <- SEM/ sqrt(6)
SEM <- as.data.frame(SEM)
graph_data$sem <- SEM$SEM
colnames(graph_data) <- c("Value", "Name", "SEM")

y_end <- max(max(merged_point_1$D, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end <- 1.10 * y_end
L <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
R <- 1.05 * (max(merged_point_1$D, na.rm = TRUE))
y_end_1 <- 1.10 * y_end
L_1 <- 1.05 * y_end
R_1 <- 1.05 * (max(merged_point_1$EWT, na.rm = TRUE))
y_end_2 <-  max(max(merged_point_1$C, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end_2 <- 1.10 * y_end_2
L_2 <- 1.05 * (max(merged_point_1$C, na.rm = TRUE))
R_2 <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
y_end_3 <- 1.10 * y_end_2
L_3 <- 1.05 * (max(merged_point_1$B, na.rm = TRUE))
R_3 <- 1.05 * y_end_2
ns_1 <- 1.05 * y_end_1
ns_2 <- 1.05 * y_end
ns_3 <- 1.05 * y_end_2
ns_4 <- 1.05 * y_end_3

graph_data$Name <- factor(graph_data$Name, levels = c("B", "C", "EMU", "D", "EWT"))
merged_point$Var2 <- factor(merged_point$Var2, levels = c("B", "C", "EMU", "D", "EWT"))

merged_point$value <- as.numeric(merged_point$value)

ggplot() + 
  scale_x_discrete(limits = c("B", "C", "EMU", "D", 'EWT'))+
  geom_bar(graph_data, mapping = aes(fill=Name,x=Name, y=Value),position = position_dodge(), stat = "identity", alpha = 0.5, color = "Black") +
  geom_errorbar(graph_data, mapping = aes(Name,ymin=Value-SEM, ymax=Value+SEM, color=Name), width=0.25,
                position=position_dodge(.9),)+
  geom_point(merged_point, mapping = aes(x = Var2, y = value, color = Var2)) +
  scale_fill_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  scale_color_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 5)) +
  labs(title = "Plasma Sat VLCFA_C24_6 wks_ug/ml", y = "Plasma FA [ug/mg]", x = " ") +
  guides(fill=guide_legend(title=" "))+
  guides(color = FALSE, size = FALSE)+
  geom_segment(aes(x = 5, y = R_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = y_end_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = L_1, xend = 3.15, yend = y_end_1))+
  annotate("text",x=4,y=ns_1,label="*")+
  geom_segment(aes(x = 3.15, y = L, xend = 3.15, yend = y_end))+
  geom_segment(aes(x = 3.15, y = y_end, xend = 4, yend = y_end))+
  geom_segment(aes(x = 4, y = R, xend = 4, yend = y_end))+
  annotate("text",x=3.5,y= ns_2,label="ns")+
  geom_segment(aes(x = 2, y = L_2, xend = 2, yend = y_end_2))+
  geom_segment(aes(x = 2, y = y_end_2, xend = 2.85, yend = y_end_2))+
  geom_segment(aes(x = 2.85, y = R_2, xend = 2.85, yend = y_end_2))+
  annotate("text",x=2.5,y=ns_3,label="ns")+
  geom_segment(aes(x = 1, y = L_3, xend = 1, yend = y_end_3))+
  geom_segment(aes(x = 1, y = y_end_3, xend = 2.85, yend = y_end_3))+
  geom_segment(aes(x = 2.85, y = R_3, xend = 2.85, yend = y_end_3))+
  annotate("text",x=2,y=ns_4,label="ns")

################### Plasma Sat VLCFA_C26_6 wks_ug/ml #######################  

BCFA_B <- t(B_spec)
BCFA_B <- as.data.frame(BCFA_B)
BCFA_B <- as.data.frame(BCFA_B[,c(13)])
colnames(BCFA_B) <- c("B")

BCFA_C <- t(C_spec)
BCFA_C <- as.data.frame(BCFA_C)
BCFA_C <- as.data.frame(BCFA_C[,c(13)])
colnames(BCFA_C) <- c("C")

BCFA_D <- t(D_spec)
BCFA_D <- as.data.frame(BCFA_D)
BCFA_D <- as.data.frame(BCFA_D[,c(13)])
colnames(BCFA_D) <- c("D")

BCFA_EWT <- t(E_spec_WT)
BCFA_EWT <- as.data.frame(BCFA_EWT)
BCFA_EWT <- as.data.frame(BCFA_EWT[,c(13)])
colnames(BCFA_EWT) <- c("EWT")

BCFA_EMU <- t(E_spec_MU)
BCFA_EMU <- as.data.frame(BCFA_EMU)
BCFA_EMU <- as.data.frame(BCFA_EMU[,c(13)])
colnames(BCFA_EMU) <- c("EMU")

B_Tot_point <- B_spec[c(13),]
rownames(B_Tot_point) <- c("B")
colnames(B_Tot_point) <- c("1", "2", "3", "4", "5", "6")
C_Tot_point <- C_spec[c(13),]
rownames(C_Tot_point) <- c("C")
colnames(C_Tot_point) <- c("1", "2", "3", "4", "5", "6")
D_Tot_point <- D_spec[c(13),]
rownames(D_Tot_point) <- c("D")
colnames(D_Tot_point) <- c("1", "2", "3", "4", "5", "6", "7")
EWT_Tot_point <- E_spec_WT[c(13),]
rownames(EWT_Tot_point) <- c("EWT")
colnames(EWT_Tot_point) <- c("1", "2", "3", "4", "5")
EMU_Tot_point <- E_spec_MU[c(13),]
rownames(EMU_Tot_point) <- c("EMU")
colnames(EMU_Tot_point) <- c("1", "2", "3", "4", "5")


merged_point <- rbind.fill(B_Tot_point,
                           C_Tot_point,
                           D_Tot_point,
                           EWT_Tot_point,
                           EMU_Tot_point)

rownames(merged_point) <- c("B", 'C', "D", "EWT", "EMU")
merged_point <- t(merged_point)
merged_point_1 <- as.data.frame(merged_point)
merged_point <- reshape2::melt(merged_point, variable.name = 'Name')
merged_point <- na.omit(merged_point)
merged_point_1 <- sapply(merged_point_1, as.numeric)
merged_point_1 <- as.data.frame(merged_point_1)



BCFA_B$numbers<-1:nrow(BCFA_B)
BCFA_C$numbers<-1:nrow(BCFA_C)
BCFA_D$numbers<-1:nrow(BCFA_D)
BCFA_EWT$numbers<-1:nrow(BCFA_EWT)
BCFA_EMU$numbers<-1:nrow(BCFA_EMU)

merge_1 <- merge(BCFA_B, BCFA_C, by = "numbers" )
merge_2 <- merge(merge_1, BCFA_D, by = "numbers" )
merge_3 <- merge(merge_2, BCFA_EWT, by = "numbers" )
merge_4 <- merge(merge_3, BCFA_EMU, by = "numbers" )
merge_4 <- merge_4[,-c(1)]

merge_4 <- sapply(merge_4, as.numeric)
merge_4 <- as.data.frame(merge_4)
stack_data <- cbind(stack(merge_4[1:5]))

kruskal.test(merge_4)

dunnTest(stack_data$values, stack_data$ind, method="hs")

stack_data %>% 
  dunn_test(values ~ ind, p.adjust.method = "bonferroni")

graph_data <- colMeans(merge_4)
graph_data <- as.data.frame(graph_data)
graph_data$names <- row.names(graph_data)
SEM <- apply(merge_4, 2, sd)
SEM <- SEM/ sqrt(6)
SEM <- as.data.frame(SEM)
graph_data$sem <- SEM$SEM
colnames(graph_data) <- c("Value", "Name", "SEM")

y_end <- max(max(merged_point_1$D, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end <- 1.10 * y_end
L <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
R <- 1.05 * (max(merged_point_1$D, na.rm = TRUE))
y_end_1 <- 1.10 * y_end
L_1 <- 1.05 * y_end
R_1 <- 1.05 * (max(merged_point_1$EWT, na.rm = TRUE))
y_end_2 <-  max(max(merged_point_1$C, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end_2 <- 1.10 * y_end_2
L_2 <- 1.05 * (max(merged_point_1$C, na.rm = TRUE))
R_2 <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
y_end_3 <- 1.10 * y_end_2
L_3 <- 1.05 * (max(merged_point_1$B, na.rm = TRUE))
R_3 <- 1.05 * y_end_2
ns_1 <- 1.05 * y_end_1
ns_2 <- 1.05 * y_end
ns_3 <- 1.05 * y_end_2
ns_4 <- 1.05 * y_end_3

graph_data$Name <- factor(graph_data$Name, levels = c("B", "C", "EMU", "D", "EWT"))
merged_point$Var2 <- factor(merged_point$Var2, levels = c("B", "C", "EMU", "D", "EWT"))

merged_point$value <- as.numeric(merged_point$value)

ggplot() + 
  scale_x_discrete(limits = c("B", "C", "EMU", "D", 'EWT'))+
  geom_bar(graph_data, mapping = aes(fill=Name,x=Name, y=Value),position = position_dodge(), stat = "identity", alpha = 0.5, color = "Black") +
  geom_errorbar(graph_data, mapping = aes(Name,ymin=Value-SEM, ymax=Value+SEM, color=Name), width=0.25,
                position=position_dodge(.9),)+
  geom_point(merged_point, mapping = aes(x = Var2, y = value, color = Var2)) +
  scale_fill_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  scale_color_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 5)) +
  labs(title = "Plasma Sat VLCFA_C26_6 wks_ug/ml", y = "Plasma FA [ug/mg]", x = " ") +
  guides(fill=guide_legend(title=" "))+
  guides(color = FALSE, size = FALSE)+
  geom_segment(aes(x = 5, y = R_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = y_end_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = L_1, xend = 3.15, yend = y_end_1))+
  annotate("text",x=4,y=ns_1,label="*")+
  geom_segment(aes(x = 3.15, y = L, xend = 3.15, yend = y_end))+
  geom_segment(aes(x = 3.15, y = y_end, xend = 4, yend = y_end))+
  geom_segment(aes(x = 4, y = R, xend = 4, yend = y_end))+
  annotate("text",x=3.5,y= ns_2,label="ns")+
  geom_segment(aes(x = 2, y = L_2, xend = 2, yend = y_end_2))+
  geom_segment(aes(x = 2, y = y_end_2, xend = 2.85, yend = y_end_2))+
  geom_segment(aes(x = 2.85, y = R_2, xend = 2.85, yend = y_end_2))+
  annotate("text",x=2.5,y=ns_3,label="ns")+
  geom_segment(aes(x = 1, y = L_3, xend = 1, yend = y_end_3))+
  geom_segment(aes(x = 1, y = y_end_3, xend = 2.85, yend = y_end_3))+
  geom_segment(aes(x = 2.85, y = R_3, xend = 2.85, yend = y_end_3))+
  annotate("text",x=2,y=ns_4,label="ns")

################### Plasma UnSat VLCFA_C24_2 wks_ug/ml #######################  

BCFA_B <- t(B_spec)
BCFA_B <- as.data.frame(BCFA_B)
BCFA_B <- as.data.frame(BCFA_B[,c(42)])
colnames(BCFA_B) <- c("B")

BCFA_C <- t(C_spec)
BCFA_C <- as.data.frame(BCFA_C)
BCFA_C <- as.data.frame(BCFA_C[,c(42)])
colnames(BCFA_C) <- c("C")

BCFA_D <- t(D_spec)
BCFA_D <- as.data.frame(BCFA_D)
BCFA_D <- as.data.frame(BCFA_D[,c(42)])
colnames(BCFA_D) <- c("D")

BCFA_EWT <- t(E_spec_WT)
BCFA_EWT <- as.data.frame(BCFA_EWT)
BCFA_EWT <- as.data.frame(BCFA_EWT[,c(42)])
colnames(BCFA_EWT) <- c("EWT")

BCFA_EMU <- t(E_spec_MU)
BCFA_EMU <- as.data.frame(BCFA_EMU)
BCFA_EMU <- as.data.frame(BCFA_EMU[,c(42)])
colnames(BCFA_EMU) <- c("EMU")

B_Tot_point <- B_spec[c(42),]
rownames(B_Tot_point) <- c("B")
colnames(B_Tot_point) <- c("1", "2", "3", "4", "5", "6")
C_Tot_point <- C_spec[c(42),]
rownames(C_Tot_point) <- c("C")
colnames(C_Tot_point) <- c("1", "2", "3", "4", "5", "6")
D_Tot_point <- D_spec[c(42),]
rownames(D_Tot_point) <- c("D")
colnames(D_Tot_point) <- c("1", "2", "3", "4", "5", "6", "7")
EWT_Tot_point <- E_spec_WT[c(42),]
rownames(EWT_Tot_point) <- c("EWT")
colnames(EWT_Tot_point) <- c("1", "2", "3", "4", "5")
EMU_Tot_point <- E_spec_MU[c(42),]
rownames(EMU_Tot_point) <- c("EMU")
colnames(EMU_Tot_point) <- c("1", "2", "3", "4", "5")


merged_point <- rbind.fill(B_Tot_point,
                           C_Tot_point,
                           D_Tot_point,
                           EWT_Tot_point,
                           EMU_Tot_point)

rownames(merged_point) <- c("B", 'C', "D", "EWT", "EMU")
merged_point <- t(merged_point)
merged_point_1 <- as.data.frame(merged_point)
merged_point <- reshape2::melt(merged_point, variable.name = 'Name')
merged_point <- na.omit(merged_point)
merged_point_1 <- sapply(merged_point_1, as.numeric)
merged_point_1 <- as.data.frame(merged_point_1)



BCFA_B$numbers<-1:nrow(BCFA_B)
BCFA_C$numbers<-1:nrow(BCFA_C)
BCFA_D$numbers<-1:nrow(BCFA_D)
BCFA_EWT$numbers<-1:nrow(BCFA_EWT)
BCFA_EMU$numbers<-1:nrow(BCFA_EMU)

merge_1 <- merge(BCFA_B, BCFA_C, by = "numbers" )
merge_2 <- merge(merge_1, BCFA_D, by = "numbers" )
merge_3 <- merge(merge_2, BCFA_EWT, by = "numbers" )
merge_4 <- merge(merge_3, BCFA_EMU, by = "numbers" )
merge_4 <- merge_4[,-c(1)]

merge_4 <- sapply(merge_4, as.numeric)
merge_4 <- as.data.frame(merge_4)
stack_data <- cbind(stack(merge_4[1:5]))

kruskal.test(merge_4)

dunnTest(stack_data$values, stack_data$ind, method="hs")

stack_data %>% 
  dunn_test(values ~ ind, p.adjust.method = "bonferroni")

graph_data <- colMeans(merge_4)
graph_data <- as.data.frame(graph_data)
graph_data$names <- row.names(graph_data)
SEM <- apply(merge_4, 2, sd)
SEM <- SEM/ sqrt(6)
SEM <- as.data.frame(SEM)
graph_data$sem <- SEM$SEM
colnames(graph_data) <- c("Value", "Name", "SEM")

y_end <- max(max(merged_point_1$D, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end <- 1.10 * y_end
L <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
R <- 1.05 * (max(merged_point_1$D, na.rm = TRUE))
y_end_1 <- 1.10 * y_end
L_1 <- 1.05 * y_end
R_1 <- 1.05 * (max(merged_point_1$EWT, na.rm = TRUE))
y_end_2 <-  max(max(merged_point_1$C, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end_2 <- 1.10 * y_end_2
L_2 <- 1.05 * (max(merged_point_1$C, na.rm = TRUE))
R_2 <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
y_end_3 <- 1.10 * y_end_2
L_3 <- 1.05 * (max(merged_point_1$B, na.rm = TRUE))
R_3 <- 1.05 * y_end_2
ns_1 <- 1.05 * y_end_1
ns_2 <- 1.05 * y_end
ns_3 <- 1.05 * y_end_2
ns_4 <- 1.05 * y_end_3

graph_data$Name <- factor(graph_data$Name, levels = c("B", "C", "EMU", "D", "EWT"))
merged_point$Var2 <- factor(merged_point$Var2, levels = c("B", "C", "EMU", "D", "EWT"))

merged_point$value <- as.numeric(merged_point$value)

ggplot() + 
  scale_x_discrete(limits = c("B", "C", "EMU", "D", 'EWT'))+
  geom_bar(graph_data, mapping = aes(fill=Name,x=Name, y=Value),position = position_dodge(), stat = "identity", alpha = 0.5, color = "Black") +
  geom_errorbar(graph_data, mapping = aes(Name,ymin=Value-SEM, ymax=Value+SEM, color=Name), width=0.25,
                position=position_dodge(.9),)+
  geom_point(merged_point, mapping = aes(x = Var2, y = value, color = Var2)) +
  scale_fill_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  scale_color_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 5)) +
  labs(title = "Plasma UnSat VLCFA_C24_2 wks_ug/ml", y = "Plasma FA [ug/mg]", x = " ") +
  guides(fill=guide_legend(title=" "))+
  guides(color = FALSE, size = FALSE)+
  geom_segment(aes(x = 5, y = R_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = y_end_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = L_1, xend = 3.15, yend = y_end_1))+
  annotate("text",x=4,y=ns_1,label="ns")+
  geom_segment(aes(x = 3.15, y = L, xend = 3.15, yend = y_end))+
  geom_segment(aes(x = 3.15, y = y_end, xend = 4, yend = y_end))+
  geom_segment(aes(x = 4, y = R, xend = 4, yend = y_end))+
  annotate("text",x=3.5,y= ns_2,label="ns")+
  geom_segment(aes(x = 2, y = L_2, xend = 2, yend = y_end_2))+
  geom_segment(aes(x = 2, y = y_end_2, xend = 2.85, yend = y_end_2))+
  geom_segment(aes(x = 2.85, y = R_2, xend = 2.85, yend = y_end_2))+
  annotate("text",x=2.5,y=ns_3,label="ns")+
  geom_segment(aes(x = 1, y = L_3, xend = 1, yend = y_end_3))+
  geom_segment(aes(x = 1, y = y_end_3, xend = 2.85, yend = y_end_3))+
  geom_segment(aes(x = 2.85, y = R_3, xend = 2.85, yend = y_end_3))+
  annotate("text",x=2,y=ns_4,label="ns")

################### Plasma UnSat VLCFA_C26_1 wks_ug/ml #######################  

BCFA_B <- t(B_spec)
BCFA_B <- as.data.frame(BCFA_B)
BCFA_B <- as.data.frame(BCFA_B[,c(25)])
colnames(BCFA_B) <- c("B")

BCFA_C <- t(C_spec)
BCFA_C <- as.data.frame(BCFA_C)
BCFA_C <- as.data.frame(BCFA_C[,c(25)])
colnames(BCFA_C) <- c("C")

BCFA_D <- t(D_spec)
BCFA_D <- as.data.frame(BCFA_D)
BCFA_D <- as.data.frame(BCFA_D[,c(25)])
colnames(BCFA_D) <- c("D")

BCFA_EWT <- t(E_spec_WT)
BCFA_EWT <- as.data.frame(BCFA_EWT)
BCFA_EWT <- as.data.frame(BCFA_EWT[,c(25)])
colnames(BCFA_EWT) <- c("EWT")

BCFA_EMU <- t(E_spec_MU)
BCFA_EMU <- as.data.frame(BCFA_EMU)
BCFA_EMU <- as.data.frame(BCFA_EMU[,c(25)])
colnames(BCFA_EMU) <- c("EMU")

B_Tot_point <- B_spec[c(25),]
rownames(B_Tot_point) <- c("B")
colnames(B_Tot_point) <- c("1", "2", "3", "4", "5", "6")
C_Tot_point <- C_spec[c(25),]
rownames(C_Tot_point) <- c("C")
colnames(C_Tot_point) <- c("1", "2", "3", "4", "5", "6")
D_Tot_point <- D_spec[c(25),]
rownames(D_Tot_point) <- c("D")
colnames(D_Tot_point) <- c("1", "2", "3", "4", "5", "6", "7")
EWT_Tot_point <- E_spec_WT[c(25),]
rownames(EWT_Tot_point) <- c("EWT")
colnames(EWT_Tot_point) <- c("1", "2", "3", "4", "5")
EMU_Tot_point <- E_spec_MU[c(25),]
rownames(EMU_Tot_point) <- c("EMU")
colnames(EMU_Tot_point) <- c("1", "2", "3", "4", "5")


merged_point <- rbind.fill(B_Tot_point,
                           C_Tot_point,
                           D_Tot_point,
                           EWT_Tot_point,
                           EMU_Tot_point)

rownames(merged_point) <- c("B", 'C', "D", "EWT", "EMU")
merged_point <- t(merged_point)
merged_point_1 <- as.data.frame(merged_point)
merged_point <- reshape2::melt(merged_point, variable.name = 'Name')
merged_point <- na.omit(merged_point)
merged_point_1 <- sapply(merged_point_1, as.numeric)
merged_point_1 <- as.data.frame(merged_point_1)



BCFA_B$numbers<-1:nrow(BCFA_B)
BCFA_C$numbers<-1:nrow(BCFA_C)
BCFA_D$numbers<-1:nrow(BCFA_D)
BCFA_EWT$numbers<-1:nrow(BCFA_EWT)
BCFA_EMU$numbers<-1:nrow(BCFA_EMU)

merge_1 <- merge(BCFA_B, BCFA_C, by = "numbers" )
merge_2 <- merge(merge_1, BCFA_D, by = "numbers" )
merge_3 <- merge(merge_2, BCFA_EWT, by = "numbers" )
merge_4 <- merge(merge_3, BCFA_EMU, by = "numbers" )
merge_4 <- merge_4[,-c(1)]

merge_4 <- sapply(merge_4, as.numeric)
merge_4 <- as.data.frame(merge_4)
stack_data <- cbind(stack(merge_4[1:5]))

kruskal.test(merge_4)

dunnTest(stack_data$values, stack_data$ind, method="hs")

stack_data %>% 
  dunn_test(values ~ ind, p.adjust.method = "bonferroni")

graph_data <- colMeans(merge_4)
graph_data <- as.data.frame(graph_data)
graph_data$names <- row.names(graph_data)
SEM <- apply(merge_4, 2, sd)
SEM <- SEM/ sqrt(6)
SEM <- as.data.frame(SEM)
graph_data$sem <- SEM$SEM
colnames(graph_data) <- c("Value", "Name", "SEM")

y_end <- max(max(merged_point_1$D, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end <- 1.10 * y_end
L <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
R <- 1.05 * (max(merged_point_1$D, na.rm = TRUE))
y_end_1 <- 1.10 * y_end
L_1 <- 1.05 * y_end
R_1 <- 1.05 * (max(merged_point_1$EWT, na.rm = TRUE))
y_end_2 <-  max(max(merged_point_1$C, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end_2 <- 1.10 * y_end_2
L_2 <- 1.05 * (max(merged_point_1$C, na.rm = TRUE))
R_2 <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
y_end_3 <- 1.10 * y_end_2
L_3 <- 1.05 * (max(merged_point_1$B, na.rm = TRUE))
R_3 <- 1.05 * y_end_2
ns_1 <- 1.05 * y_end_1
ns_2 <- 1.05 * y_end
ns_3 <- 1.05 * y_end_2
ns_4 <- 1.05 * y_end_3

graph_data$Name <- factor(graph_data$Name, levels = c("B", "C", "EMU", "D", "EWT"))
merged_point$Var2 <- factor(merged_point$Var2, levels = c("B", "C", "EMU", "D", "EWT"))

merged_point$value <- as.numeric(merged_point$value)

ggplot() + 
  scale_x_discrete(limits = c("B", "C", "EMU", "D", 'EWT'))+
  geom_bar(graph_data, mapping = aes(fill=Name,x=Name, y=Value),position = position_dodge(), stat = "identity", alpha = 0.5, color = "Black") +
  geom_errorbar(graph_data, mapping = aes(Name,ymin=Value-SEM, ymax=Value+SEM, color=Name), width=0.25,
                position=position_dodge(.9),)+
  geom_point(merged_point, mapping = aes(x = Var2, y = value, color = Var2)) +
  scale_fill_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  scale_color_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 5)) +
  labs(title = "Plasma UnSat VLCFA_C26_1 wks_ug/ml", y = "Plasma FA [ug/mg]", x = " ") +
  guides(fill=guide_legend(title=" "))+
  guides(color = FALSE, size = FALSE)+
  geom_segment(aes(x = 5, y = R_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = y_end_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = L_1, xend = 3.15, yend = y_end_1))+
  annotate("text",x=4,y=ns_1,label="***")+
  geom_segment(aes(x = 3.15, y = L, xend = 3.15, yend = y_end))+
  geom_segment(aes(x = 3.15, y = y_end, xend = 4, yend = y_end))+
  geom_segment(aes(x = 4, y = R, xend = 4, yend = y_end))+
  annotate("text",x=3.5,y= ns_2,label="ns")+
  geom_segment(aes(x = 2, y = L_2, xend = 2, yend = y_end_2))+
  geom_segment(aes(x = 2, y = y_end_2, xend = 2.85, yend = y_end_2))+
  geom_segment(aes(x = 2.85, y = R_2, xend = 2.85, yend = y_end_2))+
  annotate("text",x=2.5,y=ns_3,label="ns")+
  geom_segment(aes(x = 1, y = L_3, xend = 1, yend = y_end_3))+
  geom_segment(aes(x = 1, y = y_end_3, xend = 2.85, yend = y_end_3))+
  geom_segment(aes(x = 2.85, y = R_3, xend = 2.85, yend = y_end_3))+
  annotate("text",x=2,y=ns_4,label="ns")

################### Plasma Sat VLCFA_C26_2 wks_ug/ml #######################  


BCFA_B <- t(B_spec)
BCFA_B <- as.data.frame(BCFA_B)
BCFA_B <- as.data.frame(BCFA_B[,c(43)])
colnames(BCFA_B) <- c("B")

BCFA_C <- t(C_spec)
BCFA_C <- as.data.frame(BCFA_C)
BCFA_C <- as.data.frame(BCFA_C[,c(43)])
colnames(BCFA_C) <- c("C")

BCFA_D <- t(D_spec)
BCFA_D <- as.data.frame(BCFA_D)
BCFA_D <- as.data.frame(BCFA_D[,c(43)])
colnames(BCFA_D) <- c("D")

BCFA_EWT <- t(E_spec_WT)
BCFA_EWT <- as.data.frame(BCFA_EWT)
BCFA_EWT <- as.data.frame(BCFA_EWT[,c(43)])
colnames(BCFA_EWT) <- c("EWT")

BCFA_EMU <- t(E_spec_MU)
BCFA_EMU <- as.data.frame(BCFA_EMU)
BCFA_EMU <- as.data.frame(BCFA_EMU[,c(43)])
colnames(BCFA_EMU) <- c("EMU")

B_Tot_point <- B_spec[c(43),]
rownames(B_Tot_point) <- c("B")
colnames(B_Tot_point) <- c("1", "2", "3", "4", "5", "6")
C_Tot_point <- C_spec[c(43),]
rownames(C_Tot_point) <- c("C")
colnames(C_Tot_point) <- c("1", "2", "3", "4", "5", "6")
D_Tot_point <- D_spec[c(43),]
rownames(D_Tot_point) <- c("D")
colnames(D_Tot_point) <- c("1", "2", "3", "4", "5", "6", "7")
EWT_Tot_point <- E_spec_WT[c(43),]
rownames(EWT_Tot_point) <- c("EWT")
colnames(EWT_Tot_point) <- c("1", "2", "3", "4", "5")
EMU_Tot_point <- E_spec_MU[c(43),]
rownames(EMU_Tot_point) <- c("EMU")
colnames(EMU_Tot_point) <- c("1", "2", "3", "4", "5")


merged_point <- rbind.fill(B_Tot_point,
                           C_Tot_point,
                           D_Tot_point,
                           EWT_Tot_point,
                           EMU_Tot_point)

rownames(merged_point) <- c("B", 'C', "D", "EWT", "EMU")
merged_point <- t(merged_point)
merged_point_1 <- as.data.frame(merged_point)
merged_point <- reshape2::melt(merged_point, variable.name = 'Name')
merged_point <- na.omit(merged_point)
merged_point_1 <- sapply(merged_point_1, as.numeric)
merged_point_1 <- as.data.frame(merged_point_1)



BCFA_B$numbers<-1:nrow(BCFA_B)
BCFA_C$numbers<-1:nrow(BCFA_C)
BCFA_D$numbers<-1:nrow(BCFA_D)
BCFA_EWT$numbers<-1:nrow(BCFA_EWT)
BCFA_EMU$numbers<-1:nrow(BCFA_EMU)

merge_1 <- merge(BCFA_B, BCFA_C, by = "numbers" )
merge_2 <- merge(merge_1, BCFA_D, by = "numbers" )
merge_3 <- merge(merge_2, BCFA_EWT, by = "numbers" )
merge_4 <- merge(merge_3, BCFA_EMU, by = "numbers" )
merge_4 <- merge_4[,-c(1)]

merge_4 <- sapply(merge_4, as.numeric)
merge_4 <- as.data.frame(merge_4)
stack_data <- cbind(stack(merge_4[1:5]))

kruskal.test(merge_4)

dunnTest(stack_data$values, stack_data$ind, method="hs")

stack_data %>% 
  dunn_test(values ~ ind, p.adjust.method = "bonferroni")

graph_data <- colMeans(merge_4)
graph_data <- as.data.frame(graph_data)
graph_data$names <- row.names(graph_data)
SEM <- apply(merge_4, 2, sd)
SEM <- SEM/ sqrt(6)
SEM <- as.data.frame(SEM)
graph_data$sem <- SEM$SEM
colnames(graph_data) <- c("Value", "Name", "SEM")

y_end <- max(max(merged_point_1$D, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end <- 1.10 * y_end
L <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
R <- 1.05 * (max(merged_point_1$D, na.rm = TRUE))
y_end_1 <- 1.10 * y_end
L_1 <- 1.05 * y_end
R_1 <- 1.05 * (max(merged_point_1$EWT, na.rm = TRUE))
y_end_2 <-  max(max(merged_point_1$C, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end_2 <- 1.10 * y_end_2
L_2 <- 1.05 * (max(merged_point_1$C, na.rm = TRUE))
R_2 <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
y_end_3 <- 1.10 * y_end_2
L_3 <- 1.05 * (max(merged_point_1$B, na.rm = TRUE))
R_3 <- 1.05 * y_end_2
ns_1 <- 1.05 * y_end_1
ns_2 <- 1.05 * y_end
ns_3 <- 1.05 * y_end_2
ns_4 <- 1.05 * y_end_3

graph_data$Name <- factor(graph_data$Name, levels = c("B", "C", "EMU", "D", "EWT"))
merged_point$Var2 <- factor(merged_point$Var2, levels = c("B", "C", "EMU", "D", "EWT"))

merged_point$value <- as.numeric(merged_point$value)

ggplot() + 
  scale_x_discrete(limits = c("B", "C", "EMU", "D", 'EWT'))+
  geom_bar(graph_data, mapping = aes(fill=Name,x=Name, y=Value),position = position_dodge(), stat = "identity", alpha = 0.5, color = "Black") +
  geom_errorbar(graph_data, mapping = aes(Name,ymin=Value-SEM, ymax=Value+SEM, color=Name), width=0.25,
                position=position_dodge(.9),)+
  geom_point(merged_point, mapping = aes(x = Var2, y = value, color = Var2)) +
  scale_fill_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  scale_color_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 5)) +
  labs(title = "Plasma Sat VLCFA_C26_2 wks_ug/ml", y = "Plasma FA [ug/mg]", x = " ") +
  guides(fill=guide_legend(title=" "))+
  guides(color = FALSE, size = FALSE)+
  geom_segment(aes(x = 5, y = R_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = y_end_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = L_1, xend = 3.15, yend = y_end_1))+
  annotate("text",x=4,y=ns_1,label="***")+
  geom_segment(aes(x = 3.15, y = L, xend = 3.15, yend = y_end))+
  geom_segment(aes(x = 3.15, y = y_end, xend = 4, yend = y_end))+
  geom_segment(aes(x = 4, y = R, xend = 4, yend = y_end))+
  annotate("text",x=3.5,y= ns_2,label="ns")+
  geom_segment(aes(x = 2, y = L_2, xend = 2, yend = y_end_2))+
  geom_segment(aes(x = 2, y = y_end_2, xend = 2.85, yend = y_end_2))+
  geom_segment(aes(x = 2.85, y = R_2, xend = 2.85, yend = y_end_2))+
  annotate("text",x=2.5,y=ns_3,label="ns")+
  geom_segment(aes(x = 1, y = L_3, xend = 1, yend = y_end_3))+
  geom_segment(aes(x = 1, y = y_end_3, xend = 2.85, yend = y_end_3))+
  geom_segment(aes(x = 2.85, y = R_3, xend = 2.85, yend = y_end_3))+
  annotate("text",x=2,y=ns_4,label="ns")


################### Plasma Sat FA_C20_6 wks_ug/ml #######################  


BCFA_B <- t(B_spec)
BCFA_B <- as.data.frame(BCFA_B)
BCFA_B <- as.data.frame(BCFA_B[,c(8)])
colnames(BCFA_B) <- c("B")

BCFA_C <- t(C_spec)
BCFA_C <- as.data.frame(BCFA_C)
BCFA_C <- as.data.frame(BCFA_C[,c(8)])
colnames(BCFA_C) <- c("C")

BCFA_D <- t(D_spec)
BCFA_D <- as.data.frame(BCFA_D)
BCFA_D <- as.data.frame(BCFA_D[,c(8)])
colnames(BCFA_D) <- c("D")

BCFA_EWT <- t(E_spec_WT)
BCFA_EWT <- as.data.frame(BCFA_EWT)
BCFA_EWT <- as.data.frame(BCFA_EWT[,c(8)])
colnames(BCFA_EWT) <- c("EWT")

BCFA_EMU <- t(E_spec_MU)
BCFA_EMU <- as.data.frame(BCFA_EMU)
BCFA_EMU <- as.data.frame(BCFA_EMU[,c(8)])
colnames(BCFA_EMU) <- c("EMU")

B_Tot_point <- B_spec[c(8),]
rownames(B_Tot_point) <- c("B")
colnames(B_Tot_point) <- c("1", "2", "3", "4", "5", "6")
C_Tot_point <- C_spec[c(8),]
rownames(C_Tot_point) <- c("C")
colnames(C_Tot_point) <- c("1", "2", "3", "4", "5", "6")
D_Tot_point <- D_spec[c(8),]
rownames(D_Tot_point) <- c("D")
colnames(D_Tot_point) <- c("1", "2", "3", "4", "5", "6", "7")
EWT_Tot_point <- E_spec_WT[c(8),]
rownames(EWT_Tot_point) <- c("EWT")
colnames(EWT_Tot_point) <- c("1", "2", "3", "4", "5")
EMU_Tot_point <- E_spec_MU[c(8),]
rownames(EMU_Tot_point) <- c("EMU")
colnames(EMU_Tot_point) <- c("1", "2", "3", "4", "5")


merged_point <- rbind.fill(B_Tot_point,
                           C_Tot_point,
                           D_Tot_point,
                           EWT_Tot_point,
                           EMU_Tot_point)

rownames(merged_point) <- c("B", 'C', "D", "EWT", "EMU")
merged_point <- t(merged_point)
merged_point_1 <- as.data.frame(merged_point)
merged_point <- reshape2::melt(merged_point, variable.name = 'Name')
merged_point <- na.omit(merged_point)
merged_point_1 <- sapply(merged_point_1, as.numeric)
merged_point_1 <- as.data.frame(merged_point_1)



BCFA_B$numbers<-1:nrow(BCFA_B)
BCFA_C$numbers<-1:nrow(BCFA_C)
BCFA_D$numbers<-1:nrow(BCFA_D)
BCFA_EWT$numbers<-1:nrow(BCFA_EWT)
BCFA_EMU$numbers<-1:nrow(BCFA_EMU)

merge_1 <- merge(BCFA_B, BCFA_C, by = "numbers" )
merge_2 <- merge(merge_1, BCFA_D, by = "numbers" )
merge_3 <- merge(merge_2, BCFA_EWT, by = "numbers" )
merge_4 <- merge(merge_3, BCFA_EMU, by = "numbers" )
merge_4 <- merge_4[,-c(1)]

merge_4 <- sapply(merge_4, as.numeric)
merge_4 <- as.data.frame(merge_4)
stack_data <- cbind(stack(merge_4[1:5]))

kruskal.test(merge_4)

dunnTest(stack_data$values, stack_data$ind, method="hs")

stack_data %>% 
  dunn_test(values ~ ind, p.adjust.method = "bonferroni")

graph_data <- colMeans(merge_4)
graph_data <- as.data.frame(graph_data)
graph_data$names <- row.names(graph_data)
SEM <- apply(merge_4, 2, sd)
SEM <- SEM/ sqrt(6)
SEM <- as.data.frame(SEM)
graph_data$sem <- SEM$SEM
colnames(graph_data) <- c("Value", "Name", "SEM")

y_end <- max(max(merged_point_1$D, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end <- 1.10 * y_end
L <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
R <- 1.05 * (max(merged_point_1$D, na.rm = TRUE))
y_end_1 <- 1.10 * y_end
L_1 <- 1.05 * y_end
R_1 <- 1.05 * (max(merged_point_1$EWT, na.rm = TRUE))
y_end_2 <-  max(max(merged_point_1$C, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end_2 <- 1.10 * y_end_2
L_2 <- 1.05 * (max(merged_point_1$C, na.rm = TRUE))
R_2 <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
y_end_3 <- 1.10 * y_end_2
L_3 <- 1.05 * (max(merged_point_1$B, na.rm = TRUE))
R_3 <- 1.05 * y_end_2
ns_1 <- 1.05 * y_end_1
ns_2 <- 1.05 * y_end
ns_3 <- 1.05 * y_end_2
ns_4 <- 1.05 * y_end_3

graph_data$Name <- factor(graph_data$Name, levels = c("B", "C", "EMU", "D", "EWT"))
merged_point$Var2 <- factor(merged_point$Var2, levels = c("B", "C", "EMU", "D", "EWT"))

merged_point$value <- as.numeric(merged_point$value)

ggplot() + 
  scale_x_discrete(limits = c("B", "C", "EMU", "D", 'EWT'))+
  geom_bar(graph_data, mapping = aes(fill=Name,x=Name, y=Value),position = position_dodge(), stat = "identity", alpha = 0.5, color = "Black") +
  geom_errorbar(graph_data, mapping = aes(Name,ymin=Value-SEM, ymax=Value+SEM, color=Name), width=0.25,
                position=position_dodge(.9),)+
  geom_point(merged_point, mapping = aes(x = Var2, y = value, color = Var2)) +
  scale_fill_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  scale_color_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 5)) +
  labs(title = "Plasma Sat FA_C20_6 wks_ug/ml", y = "Plasma FA [ug/mg]", x = " ") +
  guides(fill=guide_legend(title=" "))+
  guides(color = FALSE, size = FALSE)+
  geom_segment(aes(x = 5, y = R_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = y_end_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = L_1, xend = 3.15, yend = y_end_1))+
  annotate("text",x=4,y=ns_1,label="ns")+
  geom_segment(aes(x = 3.15, y = L, xend = 3.15, yend = y_end))+
  geom_segment(aes(x = 3.15, y = y_end, xend = 4, yend = y_end))+
  geom_segment(aes(x = 4, y = R, xend = 4, yend = y_end))+
  annotate("text",x=3.5,y= ns_2,label="ns")+
  geom_segment(aes(x = 2, y = L_2, xend = 2, yend = y_end_2))+
  geom_segment(aes(x = 2, y = y_end_2, xend = 2.85, yend = y_end_2))+
  geom_segment(aes(x = 2.85, y = R_2, xend = 2.85, yend = y_end_2))+
  annotate("text",x=2.5,y=ns_3,label="ns")+
  geom_segment(aes(x = 1, y = L_3, xend = 1, yend = y_end_3))+
  geom_segment(aes(x = 1, y = y_end_3, xend = 2.85, yend = y_end_3))+
  geom_segment(aes(x = 2.85, y = R_3, xend = 2.85, yend = y_end_3))+
  annotate("text",x=2,y=ns_4,label="ns")



################### Plasma Mono UnSat C18-Oleic_6 wks_ug/ml #######################  

BCFA_B <- t(B_spec)
BCFA_B <- as.data.frame(BCFA_B)
BCFA_B <- as.data.frame(BCFA_B[,c(20)])
colnames(BCFA_B) <- c("B")

BCFA_C <- t(C_spec)
BCFA_C <- as.data.frame(BCFA_C)
BCFA_C <- as.data.frame(BCFA_C[,c(20)])
colnames(BCFA_C) <- c("C")

BCFA_D <- t(D_spec)
BCFA_D <- as.data.frame(BCFA_D)
BCFA_D <- as.data.frame(BCFA_D[,c(20)])
colnames(BCFA_D) <- c("D")

BCFA_EWT <- t(E_spec_WT)
BCFA_EWT <- as.data.frame(BCFA_EWT)
BCFA_EWT <- as.data.frame(BCFA_EWT[,c(20)])
colnames(BCFA_EWT) <- c("EWT")

BCFA_EMU <- t(E_spec_MU)
BCFA_EMU <- as.data.frame(BCFA_EMU)
BCFA_EMU <- as.data.frame(BCFA_EMU[,c(20)])
colnames(BCFA_EMU) <- c("EMU")

B_Tot_point <- B_spec[c(20),]
rownames(B_Tot_point) <- c("B")
colnames(B_Tot_point) <- c("1", "2", "3", "4", "5", "6")
C_Tot_point <- C_spec[c(20),]
rownames(C_Tot_point) <- c("C")
colnames(C_Tot_point) <- c("1", "2", "3", "4", "5", "6")
D_Tot_point <- D_spec[c(20),]
rownames(D_Tot_point) <- c("D")
colnames(D_Tot_point) <- c("1", "2", "3", "4", "5", "6", "7")
EWT_Tot_point <- E_spec_WT[c(20),]
rownames(EWT_Tot_point) <- c("EWT")
colnames(EWT_Tot_point) <- c("1", "2", "3", "4", "5")
EMU_Tot_point <- E_spec_MU[c(20),]
rownames(EMU_Tot_point) <- c("EMU")
colnames(EMU_Tot_point) <- c("1", "2", "3", "4", "5")


merged_point <- rbind.fill(B_Tot_point,
                           C_Tot_point,
                           D_Tot_point,
                           EWT_Tot_point,
                           EMU_Tot_point)

rownames(merged_point) <- c("B", 'C', "D", "EWT", "EMU")
merged_point <- t(merged_point)
merged_point_1 <- as.data.frame(merged_point)
merged_point <- reshape2::melt(merged_point, variable.name = 'Name')
merged_point <- na.omit(merged_point)
merged_point_1 <- sapply(merged_point_1, as.numeric)
merged_point_1 <- as.data.frame(merged_point_1)



BCFA_B$numbers<-1:nrow(BCFA_B)
BCFA_C$numbers<-1:nrow(BCFA_C)
BCFA_D$numbers<-1:nrow(BCFA_D)
BCFA_EWT$numbers<-1:nrow(BCFA_EWT)
BCFA_EMU$numbers<-1:nrow(BCFA_EMU)

merge_1 <- merge(BCFA_B, BCFA_C, by = "numbers" )
merge_2 <- merge(merge_1, BCFA_D, by = "numbers" )
merge_3 <- merge(merge_2, BCFA_EWT, by = "numbers" )
merge_4 <- merge(merge_3, BCFA_EMU, by = "numbers" )
merge_4 <- merge_4[,-c(1)]

merge_4 <- sapply(merge_4, as.numeric)
merge_4 <- as.data.frame(merge_4)
stack_data <- cbind(stack(merge_4[1:5]))

kruskal.test(merge_4)

dunnTest(stack_data$values, stack_data$ind, method="hs")

stack_data %>% 
  dunn_test(values ~ ind, p.adjust.method = "bonferroni")

graph_data <- colMeans(merge_4)
graph_data <- as.data.frame(graph_data)
graph_data$names <- row.names(graph_data)
SEM <- apply(merge_4, 2, sd)
SEM <- SEM/ sqrt(6)
SEM <- as.data.frame(SEM)
graph_data$sem <- SEM$SEM
colnames(graph_data) <- c("Value", "Name", "SEM")

y_end <- max(max(merged_point_1$D, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end <- 1.10 * y_end
L <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
R <- 1.05 * (max(merged_point_1$D, na.rm = TRUE))
y_end_1 <- 1.10 * y_end
L_1 <- 1.05 * y_end
R_1 <- 1.05 * (max(merged_point_1$EWT, na.rm = TRUE))
y_end_2 <-  max(max(merged_point_1$C, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end_2 <- 1.10 * y_end_2
L_2 <- 1.05 * (max(merged_point_1$C, na.rm = TRUE))
R_2 <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
y_end_3 <- 1.10 * y_end_2
L_3 <- 1.05 * (max(merged_point_1$B, na.rm = TRUE))
R_3 <- 1.05 * y_end_2
ns_1 <- 1.05 * y_end_1
ns_2 <- 1.05 * y_end
ns_3 <- 1.05 * y_end_2
ns_4 <- 1.05 * y_end_3

graph_data$Name <- factor(graph_data$Name, levels = c("B", "C", "EMU", "D", "EWT"))
merged_point$Var2 <- factor(merged_point$Var2, levels = c("B", "C", "EMU", "D", "EWT"))

merged_point$value <- as.numeric(merged_point$value)

ggplot() + 
  scale_x_discrete(limits = c("B", "C", "EMU", "D", 'EWT'))+
  geom_bar(graph_data, mapping = aes(fill=Name,x=Name, y=Value),position = position_dodge(), stat = "identity", alpha = 0.5, color = "Black") +
  geom_errorbar(graph_data, mapping = aes(Name,ymin=Value-SEM, ymax=Value+SEM, color=Name), width=0.25,
                position=position_dodge(.9),)+
  geom_point(merged_point, mapping = aes(x = Var2, y = value, color = Var2)) +
  scale_fill_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  scale_color_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 5)) +
  labs(title = "Plasma Mono UnSat C18-Oleic_6 wks_ug/ml", y = "Plasma FA [ug/mg]", x = " ") +
  guides(fill=guide_legend(title=" "))+
  guides(color = FALSE, size = FALSE)+
  geom_segment(aes(x = 5, y = R_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = y_end_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = L_1, xend = 3.15, yend = y_end_1))+
  annotate("text",x=4,y=ns_1,label="ns")+
  geom_segment(aes(x = 3.15, y = L, xend = 3.15, yend = y_end))+
  geom_segment(aes(x = 3.15, y = y_end, xend = 4, yend = y_end))+
  geom_segment(aes(x = 4, y = R, xend = 4, yend = y_end))+
  annotate("text",x=3.5,y= ns_2,label="ns")+
  geom_segment(aes(x = 2, y = L_2, xend = 2, yend = y_end_2))+
  geom_segment(aes(x = 2, y = y_end_2, xend = 2.85, yend = y_end_2))+
  geom_segment(aes(x = 2.85, y = R_2, xend = 2.85, yend = y_end_2))+
  annotate("text",x=2.5,y=ns_3,label="ns")+
  geom_segment(aes(x = 1, y = L_3, xend = 1, yend = y_end_3))+
  geom_segment(aes(x = 1, y = y_end_3, xend = 2.85, yend = y_end_3))+
  geom_segment(aes(x = 2.85, y = R_3, xend = 2.85, yend = y_end_3))+
  annotate("text",x=2,y=ns_4,label="ns")

################### Plasma Mono_UnSat_20:1_6 wks_ug/ml #######################  

BCFA_B <- t(B_spec)
BCFA_B <- as.data.frame(BCFA_B)
BCFA_B <- as.data.frame(BCFA_B[,c(21)])
colnames(BCFA_B) <- c("B")

BCFA_C <- t(C_spec)
BCFA_C <- as.data.frame(BCFA_C)
BCFA_C <- as.data.frame(BCFA_C[,c(21)])
colnames(BCFA_C) <- c("C")

BCFA_D <- t(D_spec)
BCFA_D <- as.data.frame(BCFA_D)
BCFA_D <- as.data.frame(BCFA_D[,c(21)])
colnames(BCFA_D) <- c("D")

BCFA_EWT <- t(E_spec_WT)
BCFA_EWT <- as.data.frame(BCFA_EWT)
BCFA_EWT <- as.data.frame(BCFA_EWT[,c(21)])
colnames(BCFA_EWT) <- c("EWT")

BCFA_EMU <- t(E_spec_MU)
BCFA_EMU <- as.data.frame(BCFA_EMU)
BCFA_EMU <- as.data.frame(BCFA_EMU[,c(21)])
colnames(BCFA_EMU) <- c("EMU")

B_Tot_point <- B_spec[c(21),]
rownames(B_Tot_point) <- c("B")
colnames(B_Tot_point) <- c("1", "2", "3", "4", "5", "6")
C_Tot_point <- C_spec[c(21),]
rownames(C_Tot_point) <- c("C")
colnames(C_Tot_point) <- c("1", "2", "3", "4", "5", "6")
D_Tot_point <- D_spec[c(21),]
rownames(D_Tot_point) <- c("D")
colnames(D_Tot_point) <- c("1", "2", "3", "4", "5", "6", "7")
EWT_Tot_point <- E_spec_WT[c(21),]
rownames(EWT_Tot_point) <- c("EWT")
colnames(EWT_Tot_point) <- c("1", "2", "3", "4", "5")
EMU_Tot_point <- E_spec_MU[c(21),]
rownames(EMU_Tot_point) <- c("EMU")
colnames(EMU_Tot_point) <- c("1", "2", "3", "4", "5")


merged_point <- rbind.fill(B_Tot_point,
                           C_Tot_point,
                           D_Tot_point,
                           EWT_Tot_point,
                           EMU_Tot_point)

rownames(merged_point) <- c("B", 'C', "D", "EWT", "EMU")
merged_point <- t(merged_point)
merged_point_1 <- as.data.frame(merged_point)
merged_point <- reshape2::melt(merged_point, variable.name = 'Name')
merged_point <- na.omit(merged_point)
merged_point_1 <- sapply(merged_point_1, as.numeric)
merged_point_1 <- as.data.frame(merged_point_1)



BCFA_B$numbers<-1:nrow(BCFA_B)
BCFA_C$numbers<-1:nrow(BCFA_C)
BCFA_D$numbers<-1:nrow(BCFA_D)
BCFA_EWT$numbers<-1:nrow(BCFA_EWT)
BCFA_EMU$numbers<-1:nrow(BCFA_EMU)

merge_1 <- merge(BCFA_B, BCFA_C, by = "numbers" )
merge_2 <- merge(merge_1, BCFA_D, by = "numbers" )
merge_3 <- merge(merge_2, BCFA_EWT, by = "numbers" )
merge_4 <- merge(merge_3, BCFA_EMU, by = "numbers" )
merge_4 <- merge_4[,-c(1)]

merge_4 <- sapply(merge_4, as.numeric)
merge_4 <- as.data.frame(merge_4)
stack_data <- cbind(stack(merge_4[1:5]))

kruskal.test(merge_4)

dunnTest(stack_data$values, stack_data$ind, method="hs")

stack_data %>% 
  dunn_test(values ~ ind, p.adjust.method = "bonferroni")

graph_data <- colMeans(merge_4)
graph_data <- as.data.frame(graph_data)
graph_data$names <- row.names(graph_data)
SEM <- apply(merge_4, 2, sd)
SEM <- SEM/ sqrt(6)
SEM <- as.data.frame(SEM)
graph_data$sem <- SEM$SEM
colnames(graph_data) <- c("Value", "Name", "SEM")

y_end <- max(max(merged_point_1$D, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end <- 1.10 * y_end
L <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
R <- 1.05 * (max(merged_point_1$D, na.rm = TRUE))
y_end_1 <- 1.10 * y_end
L_1 <- 1.05 * y_end
R_1 <- 1.05 * (max(merged_point_1$EWT, na.rm = TRUE))
y_end_2 <-  max(max(merged_point_1$C, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end_2 <- 1.10 * y_end_2
L_2 <- 1.05 * (max(merged_point_1$C, na.rm = TRUE))
R_2 <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
y_end_3 <- 1.10 * y_end_2
L_3 <- 1.05 * (max(merged_point_1$B, na.rm = TRUE))
R_3 <- 1.05 * y_end_2
ns_1 <- 1.05 * y_end_1
ns_2 <- 1.05 * y_end
ns_3 <- 1.05 * y_end_2
ns_4 <- 1.05 * y_end_3

graph_data$Name <- factor(graph_data$Name, levels = c("B", "C", "EMU", "D", "EWT"))
merged_point$Var2 <- factor(merged_point$Var2, levels = c("B", "C", "EMU", "D", "EWT"))

merged_point$value <- as.numeric(merged_point$value)

ggplot() + 
  scale_x_discrete(limits = c("B", "C", "EMU", "D", 'EWT'))+
  geom_bar(graph_data, mapping = aes(fill=Name,x=Name, y=Value),position = position_dodge(), stat = "identity", alpha = 0.5, color = "Black") +
  geom_errorbar(graph_data, mapping = aes(Name,ymin=Value-SEM, ymax=Value+SEM, color=Name), width=0.25,
                position=position_dodge(.9),)+
  geom_point(merged_point, mapping = aes(x = Var2, y = value, color = Var2)) +
  scale_fill_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  scale_color_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 5)) +
  labs(title = "Plasma Mono_UnSat_20:1_6 wks_ug/ml", y = "Plasma FA [ug/mg]", x = " ") +
  guides(fill=guide_legend(title=" "))+
  guides(color = FALSE, size = FALSE)+
  geom_segment(aes(x = 5, y = R_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = y_end_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = L_1, xend = 3.15, yend = y_end_1))+
  annotate("text",x=4,y=ns_1,label="ns")+
  geom_segment(aes(x = 3.15, y = L, xend = 3.15, yend = y_end))+
  geom_segment(aes(x = 3.15, y = y_end, xend = 4, yend = y_end))+
  geom_segment(aes(x = 4, y = R, xend = 4, yend = y_end))+
  annotate("text",x=3.5,y= ns_2,label="ns")+
  geom_segment(aes(x = 2, y = L_2, xend = 2, yend = y_end_2))+
  geom_segment(aes(x = 2, y = y_end_2, xend = 2.85, yend = y_end_2))+
  geom_segment(aes(x = 2.85, y = R_2, xend = 2.85, yend = y_end_2))+
  annotate("text",x=2.5,y=ns_3,label="ns")+
  geom_segment(aes(x = 1, y = L_3, xend = 1, yend = y_end_3))+
  geom_segment(aes(x = 1, y = y_end_3, xend = 2.85, yend = y_end_3))+
  geom_segment(aes(x = 2.85, y = R_3, xend = 2.85, yend = y_end_3))+
  annotate("text",x=2,y=ns_4,label="ns")

################### Plasma Unsat FA_C20:4-Arachidonic_6 wks_ug/ml #######################  

BCFA_B <- t(B_spec)
BCFA_B <- as.data.frame(BCFA_B)
BCFA_B <- as.data.frame(BCFA_B[,c(38)])
colnames(BCFA_B) <- c("B")

BCFA_C <- t(C_spec)
BCFA_C <- as.data.frame(BCFA_C)
BCFA_C <- as.data.frame(BCFA_C[,c(38)])
colnames(BCFA_C) <- c("C")

BCFA_D <- t(D_spec)
BCFA_D <- as.data.frame(BCFA_D)
BCFA_D <- as.data.frame(BCFA_D[,c(38)])
colnames(BCFA_D) <- c("D")

BCFA_EWT <- t(E_spec_WT)
BCFA_EWT <- as.data.frame(BCFA_EWT)
BCFA_EWT <- as.data.frame(BCFA_EWT[,c(38)])
colnames(BCFA_EWT) <- c("EWT")

BCFA_EMU <- t(E_spec_MU)
BCFA_EMU <- as.data.frame(BCFA_EMU)
BCFA_EMU <- as.data.frame(BCFA_EMU[,c(38)])
colnames(BCFA_EMU) <- c("EMU")

B_Tot_point <- B_spec[c(38),]
rownames(B_Tot_point) <- c("B")
colnames(B_Tot_point) <- c("1", "2", "3", "4", "5", "6")
C_Tot_point <- C_spec[c(38),]
rownames(C_Tot_point) <- c("C")
colnames(C_Tot_point) <- c("1", "2", "3", "4", "5", "6")
D_Tot_point <- D_spec[c(38),]
rownames(D_Tot_point) <- c("D")
colnames(D_Tot_point) <- c("1", "2", "3", "4", "5", "6", "7")
EWT_Tot_point <- E_spec_WT[c(38),]
rownames(EWT_Tot_point) <- c("EWT")
colnames(EWT_Tot_point) <- c("1", "2", "3", "4", "5")
EMU_Tot_point <- E_spec_MU[c(38),]
rownames(EMU_Tot_point) <- c("EMU")
colnames(EMU_Tot_point) <- c("1", "2", "3", "4", "5")


merged_point <- rbind.fill(B_Tot_point,
                           C_Tot_point,
                           D_Tot_point,
                           EWT_Tot_point,
                           EMU_Tot_point)

rownames(merged_point) <- c("B", 'C', "D", "EWT", "EMU")
merged_point <- t(merged_point)
merged_point_1 <- as.data.frame(merged_point)
merged_point <- reshape2::melt(merged_point, variable.name = 'Name')
merged_point <- na.omit(merged_point)
merged_point_1 <- sapply(merged_point_1, as.numeric)
merged_point_1 <- as.data.frame(merged_point_1)



BCFA_B$numbers<-1:nrow(BCFA_B)
BCFA_C$numbers<-1:nrow(BCFA_C)
BCFA_D$numbers<-1:nrow(BCFA_D)
BCFA_EWT$numbers<-1:nrow(BCFA_EWT)
BCFA_EMU$numbers<-1:nrow(BCFA_EMU)

merge_1 <- merge(BCFA_B, BCFA_C, by = "numbers" )
merge_2 <- merge(merge_1, BCFA_D, by = "numbers" )
merge_3 <- merge(merge_2, BCFA_EWT, by = "numbers" )
merge_4 <- merge(merge_3, BCFA_EMU, by = "numbers" )
merge_4 <- merge_4[,-c(1)]

merge_4 <- sapply(merge_4, as.numeric)
merge_4 <- as.data.frame(merge_4)
stack_data <- cbind(stack(merge_4[1:5]))

kruskal.test(merge_4)

dunnTest(stack_data$values, stack_data$ind, method="hs")

stack_data %>% 
  dunn_test(values ~ ind, p.adjust.method = "bonferroni")

graph_data <- colMeans(merge_4)
graph_data <- as.data.frame(graph_data)
graph_data$names <- row.names(graph_data)
SEM <- apply(merge_4, 2, sd)
SEM <- SEM/ sqrt(6)
SEM <- as.data.frame(SEM)
graph_data$sem <- SEM$SEM
colnames(graph_data) <- c("Value", "Name", "SEM")

y_end <- max(max(merged_point_1$D, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end <- 1.10 * y_end
L <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
R <- 1.05 * (max(merged_point_1$D, na.rm = TRUE))
y_end_1 <- 1.10 * y_end
L_1 <- 1.05 * y_end
R_1 <- 1.05 * (max(merged_point_1$EWT, na.rm = TRUE))
y_end_2 <-  max(max(merged_point_1$C, na.rm = TRUE),max(merged_point_1$EMU, na.rm = TRUE))
y_end_2 <- 1.10 * y_end_2
L_2 <- 1.05 * (max(merged_point_1$C, na.rm = TRUE))
R_2 <- 1.05 * (max(merged_point_1$EMU, na.rm = TRUE))
y_end_3 <- 1.10 * y_end_2
L_3 <- 1.05 * (max(merged_point_1$B, na.rm = TRUE))
R_3 <- 1.05 * y_end_2
ns_1 <- 1.05 * y_end_1
ns_2 <- 1.05 * y_end
ns_3 <- 1.05 * y_end_2
ns_4 <- 1.05 * y_end_3

graph_data$Name <- factor(graph_data$Name, levels = c("B", "C", "EMU", "D", "EWT"))
merged_point$Var2 <- factor(merged_point$Var2, levels = c("B", "C", "EMU", "D", "EWT"))

merged_point$value <- as.numeric(merged_point$value)

ggplot() + 
  scale_x_discrete(limits = c("B", "C", "EMU", "D", 'EWT'))+
  geom_bar(graph_data, mapping = aes(fill=Name,x=Name, y=Value),position = position_dodge(), stat = "identity", alpha = 0.5, color = "Black") +
  geom_errorbar(graph_data, mapping = aes(Name,ymin=Value-SEM, ymax=Value+SEM, color=Name), width=0.25,
                position=position_dodge(.9),)+
  geom_point(merged_point, mapping = aes(x = Var2, y = value, color = Var2)) +
  scale_fill_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  scale_color_manual(values = c("B" = "red", "C" = "blue", "D" = "green", "EWT" = "orange", "EMU" = "purple")) +
  theme_bw()+
  theme(axis.text.x = element_text(size = 5)) +
  labs(title = "Plasma Unsat FA_C20:4-Arachidonic_6 wks_ug/ml", y = "Plasma FA [ug/mg]", x = " ") +
  guides(fill=guide_legend(title=" "))+
  guides(color = FALSE, size = FALSE)+
  geom_segment(aes(x = 5, y = R_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = y_end_1, xend = 5, yend = y_end_1))+
  geom_segment(aes(x = 3.15, y = L_1, xend = 3.15, yend = y_end_1))+
  annotate("text",x=4,y=ns_1,label="ns")+
  geom_segment(aes(x = 3.15, y = L, xend = 3.15, yend = y_end))+
  geom_segment(aes(x = 3.15, y = y_end, xend = 4, yend = y_end))+
  geom_segment(aes(x = 4, y = R, xend = 4, yend = y_end))+
  annotate("text",x=3.5,y= ns_2,label="ns")+
  geom_segment(aes(x = 2, y = L_2, xend = 2, yend = y_end_2))+
  geom_segment(aes(x = 2, y = y_end_2, xend = 2.85, yend = y_end_2))+
  geom_segment(aes(x = 2.85, y = R_2, xend = 2.85, yend = y_end_2))+
  annotate("text",x=2.5,y=ns_3,label="ns")+
  geom_segment(aes(x = 1, y = L_3, xend = 1, yend = y_end_3))+
  geom_segment(aes(x = 1, y = y_end_3, xend = 2.85, yend = y_end_3))+
  geom_segment(aes(x = 2.85, y = R_3, xend = 2.85, yend = y_end_3))+
  annotate("text",x=2,y=ns_4,label="*")



























