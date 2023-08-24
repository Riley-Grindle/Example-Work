
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

################################################################################################################


xyz <- data.frame(matrix(nrow = 0, ncol = 0))

for (i in 1:((nrow(B_spec)))){
  print(i)
  B_Tot_point <- B_spec[c(i),]
  rownames(B_Tot_point) <- c("B") 
  colnames(B_Tot_point) <- c("1", "2", "3", "4", "5", "6", "7")
  C_Tot_point <- C_spec[c(i),]
  rownames(C_Tot_point) <- c("C")
  colnames(C_Tot_point) <- c("1", "2", "3", "4", "5", "6", "7", "8")
  D_Tot_point <- D_spec[c(i),]
  rownames(D_Tot_point) <- c("D")
  colnames(D_Tot_point) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
  EWT_Tot_point <- E_spec_WT[c(i),]
  rownames(EWT_Tot_point) <- c("EWT")
  colnames(EWT_Tot_point) <- c("1", "2", "3", "4", "5", "6")
  EMU_Tot_point <- E_spec_MU[c(i),]
  rownames(EMU_Tot_point) <- c("EMU")
  colnames(EMU_Tot_point) <- c("1", "2", "3", "4", "5", "6")
  
  binded_data <- rbind.fill(B_Tot_point, C_Tot_point, D_Tot_point, EMU_Tot_point, EWT_Tot_point)
  binded_data_1 <- t(binded_data)
  colnames(binded_data_1) <- c("Pex1.G844D.G844D.DOSE_B", 'Pex1.G844D.G844D.DOSE_C', "Pex1.G844D.G844D.DOSE_D", "Pex1.G844D.G844D.DOSE_E", "Pex1.WT.DOSE_E")
  binded_data_1 <- as.data.frame(binded_data_1)
  binded_data_3 <- cbind(stack(binded_data_1[1:5]))
  
  
  binded_data_1 <- binded_data_1[, c(5,1,2,3,4)]
  binded_data_3 <- cbind(stack(binded_data_1[1:5]))
  binded_data_3$values <- as.numeric(binded_data_3$values)
  p <- pairwise_t_test(binded_data_3, values ~ ind, ref.group = "Pex1.WT.DOSE_E", p.adj ='holm')
  print(p)
  x <- as.data.frame(p$p.adj)
  x <- t(x)
  x <- as.data.frame(x)
  
  binded_data_1 <- sapply(binded_data_1, as.numeric)
  binded_data_1 <- as.data.frame(binded_data_1)
  
  
  xyz <- rbind.fill(xyz, x)
}

rownames(xyz) <- rownames(B_spec)

xyz$V0 <- 1
xyz$FA = rownames(xyz)
xyz <- xyz[-c(60),c(5,4,1,2,3,6)]

my_order <- c(row.names(xyz))
my_order <- my_order[c(60:1)]

data <- cbind(xyz[,6],stack(xyz[1:5]))
colnames(data) <- c("FA", "values", "Dose")

#################### Run the Fold Change Script #######################################

fc_data_f <- fc_data

for (i in 1:nrow(fc_data_f)){
  if (data$values[i] > 0.05){
    fc_data_f$values[i] <- 101
  }
}

min(fc_data$values)
max(fc_data$values)
###################################################################################

png("16wk_liver_pval_heatmap_final.png", width = 800, height = 800)
ggplot(fc_data_f, aes(Dose, FA, fill= values))+
  geom_tile() +
  scale_fill_distiller(palette ="RdBu",
                       breaks = c(100, -9, 9, 0), 
                       limits = c(-9, 9),
                       na.value = "black",
                       direction = -1)+
  theme_bw() +
  theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 12))+
  labs(title = "Liver 16 wks Log2 Fold Change vs Pex1 +/+ Veh", x = " ", y = " ")+
  scale_x_discrete(labels = c("Pex1 (+/+) 
                              Veh                            ", "     Pex1(G844D) 
                              Veh                            ", "Pex1(G844D)
                              1:4                             ", "Pex1(G844D)
                              1:10                            ", "Pex1(G844D)
                              1:40                            "))+
  scale_y_discrete(limits = my_order[9:60])+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5,margin = margin(b = 20), face = "bold"), 
        axis.title.y = element_text(margin = margin(r =75)))

dev.off()

jpeg("16wk_liver_pval_heatmap_final.jpg", width = 800, height = 800)
ggplot(fc_data_f, aes(Dose, FA, fill= values))+
  geom_tile() +
  scale_fill_distiller(palette ="RdBu",
                       breaks = c(100, -9, 9, 0), 
                       limits = c(-9, 9),
                       na.value = "black",
                       direction = -1)+
  theme_bw() +
  theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 12))+
  labs(title = "Liver 16 wks Log2 Fold Change vs Pex1 +/+ Veh", x = " ", y = " ")+
  scale_x_discrete(labels = c("Pex1 (+/+) 
                              Veh                            ", "     Pex1(G844D) 
                              Veh                            ", "Pex1(G844D)
                              1:4                             ", "Pex1(G844D)
                              1:10                            ", "Pex1(G844D)
                              1:40                            "))+
  scale_y_discrete(limits = my_order[9:60])+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5,margin = margin(b = 20), face = "bold"), 
        axis.title.y = element_text(margin = margin(r =75)))

dev.off()

png("16wk_liver_pval_heatmap_tot_final.png", width = 800, height = 800)
ggplot(fc_data_f, aes(Dose, FA, fill= values))+
  geom_tile() +
  scale_fill_distiller(palette ="RdBu",
                       breaks = c(100, -9, 9, 0), 
                       limits = c(-9, 9),
                       na.value = "black",
                       direction = -1)+
  theme_bw() +
  theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 12))+
  labs(title = "Liver 16 wks Log2 Fold Change vs Pex1 +/+ Veh", x = " ", y = " ")+
  scale_x_discrete(labels = c("Pex1 (+/+) 
                              Veh                            ", "     Pex1(G844D) 
                              Veh                            ", "Pex1(G844D)
                              1:4                             ", "Pex1(G844D)
                              1:10                            ", "Pex1(G844D)
                              1:40                            "))+
  scale_y_discrete(limits = my_order[1:8])+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5,margin = margin(b = 20), face = "bold"), 
        axis.title.y = element_text(margin = margin(r =75)))

dev.off()

jpeg("16wk_liver_pval_heatmap_tot_final.jpg", width = 800, height = 800)
ggplot(fc_data_f, aes(Dose, FA, fill= values))+
  geom_tile() +
  scale_fill_distiller(palette ="RdBu",
                       breaks = c(100, -9, 9, 0), 
                       limits = c(-9, 9),
                       na.value = "black",
                       direction = -1)+
  theme_bw() +
  theme(axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 12))+
  labs(title = "Liver 16 wks Log2 Fold Change vs Pex1 +/+ Veh", x = " ", y = " ")+
  scale_x_discrete(labels = c("Pex1 (+/+) 
                              Veh                            ", "     Pex1(G844D) 
                              Veh                            ", "Pex1(G844D)
                              1:4                             ", "Pex1(G844D)
                              1:10                            ", "Pex1(G844D)
                              1:40                            "))+
  scale_y_discrete(limits = my_order[1:8])+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5,margin = margin(b = 20), face = "bold"), 
        axis.title.y = element_text(margin = margin(r =75)))

dev.off()
