library(PMCMRplus)
library(PMCMR)

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
library(hrbrthemes)
library(RColorBrewer)

options(scipen = 100, digits = 4)

setwd("C:/Users/grindr/OneDrive - The Jackson Laboratory/Documents/329_Liv_Plasma_Data_Analysis/Raw Data Files")

########## DATA CLEANING AND FORMATTING - removed unnecessary rows & renamed columns

raw_data <- read.csv("JAX_PEX1_G844D_329_Liver_16wks_6-21-23.csv", 
                     header=TRUE, sep= ",", fill = TRUE)

clean_data_1 <- raw_data[-c(1:3,6,7,60,69),-c(42:50)]

FA_Data <- clean_data_1
row.names(FA_Data) <- FA_Data[,1]
FA_Data <- FA_Data[,-1]
colnames(FA_Data) <- FA_Data[1,]
FA_Data <- FA_Data[-1,]


######### DOSING SEPARATION ################

B_spec <- FA_Data[,c(7:13)]
C_spec <- FA_Data[,c(14:16,34:36,38,39)]
D_spec <- FA_Data[,c(20:28)]
E_spec_WT <- FA_Data[,c(1:6)]
E_spec_MU <- FA_Data[,c(18,19,29:31,40)]

B_spec <- B_spec[-c(1),]
C_spec <- C_spec[-c(1),]
D_spec <- D_spec[-c(1),]
E_spec_WT <- E_spec_WT[-c(1),]
E_spec_MU <- E_spec_MU[-c(1),]

####################### Log2 Fold Changes #####################

B_Mean <- as.data.frame(rowMeans(sapply(B_spec, as.numeric)))
rownames(B_Mean) <- rownames(B_spec)
colnames(B_Mean) <- c("B")

C_Mean <- as.data.frame(rowMeans(sapply(C_spec, as.numeric)))
rownames(C_Mean) <- rownames(C_spec)
colnames(C_Mean) <- c("C")

D_Mean <- as.data.frame(rowMeans(sapply(D_spec, as.numeric)))
rownames(D_Mean) <- rownames(D_spec)
colnames(D_Mean) <- c("D")

EMU_Mean <- as.data.frame(rowMeans(sapply(E_spec_MU, as.numeric)))
rownames(EMU_Mean) <- rownames(E_spec_MU)
colnames(EMU_Mean) <- c("EMU")

EWT_Mean <- as.data.frame(rowMeans(sapply(E_spec_WT, as.numeric)))
rownames(EWT_Mean) <- rownames(E_spec_WT)
colnames(EWT_Mean) <- c("EWT")

mean_data <- cbind(B_Mean, C_Mean, D_Mean, EMU_Mean, EWT_Mean)

fc_data <- as.data.frame(log2(mean_data$B/mean_data$EWT))
fc_data_1 <- as.data.frame(log2(mean_data$C/mean_data$EWT))
fc_data_2 <- as.data.frame(log2(mean_data$D/mean_data$EWT))
fc_data_3 <- as.data.frame(log2(mean_data$EMU/mean_data$EWT))
fc_data_4 <- as.data.frame(log2(mean_data$EWT/mean_data$EWT))

fc_data <- cbind(fc_data, fc_data_1, fc_data_2, fc_data_3, fc_data_4)

colnames(fc_data) <- c("Pex1.G844D.G844D.DOSE_B", 'Pex1.G844D.G844D.DOSE_C', "Pex1.G844D.G844D.DOSE_D", "Pex1.G844D.G844D.DOSE_E", "Pex1.+/+.Veh")
rownames(fc_data) <- rownames(B_spec)
fc_data$FA <- rownames(B_spec)
fc_data <- fc_data[-c(60),c(5,4,1,2,3,6)]
my_order <- c(row.names(fc_data))
my_order <- my_order[c(60:1)]
fc_data <- cbind(fc_data[,6],stack(fc_data[1:5]))
colnames(fc_data) <- c("FA", "values", "Dose")
max(fc_data$values)



############################ GGPLOTS #####################################

png("16wk_Liver_fc_heatmap_final.png", width = 800, height = 800)
ggplot(fc_data, aes(Dose, FA, fill= values))+
  geom_tile()+
  scale_fill_gradient2(low = "#000066", mid = "white", high = "#660000")+
  theme_bw() +
  theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 12))+
  labs(title = "Liver 16 wks ug/mg Log2 Fold Change vs Pex1 +/+ Veh", x = " ", y = " ")+
  scale_x_discrete(labels = c("     Pex1(G844D) 
                              Veh                            ", "Pex1(G844D)
                              1:4                             ", "Pex1(G844D)
                              1:10                            ", "Pex1(G844D)
                              1:40                            ", "Pex1 +/+ Veh"))+
  scale_y_discrete(limits = my_order[9:60])+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5,margin = margin(b = 20), face = "bold"), 
        axis.title.y = element_text(margin = margin(r = 20)))

dev.off()

jpeg("16wk_Liver_fc_heatmap_final.jpg", width = 800, height = 800)
ggplot(fc_data, aes(Dose, FA, fill= values))+
  geom_tile()+
  scale_fill_gradient2(low = "#000066", mid = "white", high = "#660000")+
  theme_bw() +
  theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 12))+
  labs(title = "Liver 16 wks ug/mg Log2 Fold Change vs Pex1 +/+ Veh", x = " ", y = " ")+
  scale_x_discrete(labels = c("     Pex1(G844D) 
                              Veh                            ", "Pex1(G844D)
                              1:4                             ", "Pex1(G844D)
                              1:10                            ", "Pex1(G844D)
                              1:40                            ", "Pex1 +/+ Veh"))+
  scale_y_discrete(limits = my_order[9:60])+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5,margin = margin(b = 20), face = "bold"), 
        axis.title.y = element_text(margin = margin(r = 20)))

dev.off()

png("16wk_Liver_fc_heatmap_tot_final.png", width = 800, height = 800)
ggplot(fc_data, aes(Dose, FA, fill= values))+
  geom_tile()+
  scale_fill_gradient2(low = "#000066", mid = "white", high = "#660000")+
  theme_bw() +
  theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 12))+
  labs(title = "Liver 16 wks ug/mg Log2 Fold Change vs Pex1 +/+ Veh", x = " ", y = " ")+
  scale_x_discrete(labels = c("     Pex1(G844D) 
                              Veh                            ", "Pex1(G844D)
                              1:4                             ", "Pex1(G844D)
                              1:10                            ", "Pex1(G844D)
                              1:40                            ", "Pex1 +/+ Veh"))+
  scale_y_discrete(limits = my_order[1:8])+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5,margin = margin(b = 20), face = "bold"), 
        axis.title.y = element_text(margin = margin(r = 20)))

dev.off()

jpeg("16wk_Liver_fc_heatmap_tot_final.jpg", width = 800, height = 800)
ggplot(fc_data, aes(Dose, FA, fill= values))+
  geom_tile()+
  scale_fill_gradient2(low = "#000066", mid = "white", high = "#660000")+
  theme_bw() +
  theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 12))+
  labs(title = "Liver 16 wks ug/mg Log2 Fold Change vs Pex1 +/+ Veh", x = " ", y = " ")+
  scale_x_discrete(labels = c("     Pex1(G844D) 
                              Veh                            ", "Pex1(G844D)
                              1:4                             ", "Pex1(G844D)
                              1:10                            ", "Pex1(G844D)
                              1:40                            ", "Pex1 +/+ Veh"))+
  scale_y_discrete(limits = my_order[1:8])+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5,margin = margin(b = 20), face = "bold"), 
        axis.title.y = element_text(margin = margin(r = 20)))

dev.off() 
