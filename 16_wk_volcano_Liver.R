# Instructions #
# Run heatmap scripts for both fold change and P value of same data group (ex: 6 wk Liver)
# Stop scripts directly before ggplot function
# Run following script

library(ggrepel)

merge_1 <- cbind(data, fc_data)
merged_data <- merge_1[,-c(3,4)]
colnames(merged_data) <- c("FA", "p_value", "Fold_Change", "Dose")
merged_data$up_dwn <- merged_data$Fold_Change

for (i in 1:nrow(merged_data)){
  if (as.numeric(merged_data$Fold_Change[i]) < 0 && merged_data$p_value[i] <= 0.05){
    merged_data$up_dwn[i] <- "Down"
  }
  if (as.numeric(merged_data$Fold_Change[i]) > 0 && merged_data$p_value[i] <= 0.05){
    merged_data$up_dwn[i] <- "Up"
  }
  if (as.numeric( merged_data$p_value[i] > 0.05 )){
    merged_data$up_dwn[i] <- "No"
  }
}

merged_data_E = merged_data[61:120,]
merged_data_E <- rbind(merged_data_E, data.frame(FA = 'X', p_value = 100000, Fold_Change = 100000, Dose = "NA", up_dwn = "Down"))
merged_data_E <- rbind(merged_data_E, data.frame(FA = 'X', p_value = 100000, Fold_Change = 100000, Dose = "NA", up_dwn = "Down"))
merged_data_E <- rbind(merged_data_E, data.frame(FA = 'X', p_value = 100000, Fold_Change = 100000, Dose = "NA", up_dwn = "Up"))
filtered_data_d <- subset(merged_data_E, up_dwn == "Down")
filtered_data_u <- subset(merged_data_E, up_dwn == "Up")

png("16_Liver_E_vs_WT.png", width = 1000, height = 750)
ggplot(data=merged_data_E, aes(x=Fold_Change, y=-log10(p_value), col = up_dwn)) +
  geom_point(size=4) +
  geom_vline(xintercept=c(0), col="black", linetype = "dashed") +
  geom_hline(yintercept=-log10(0.05), col="black", linetype = "dashed")+
  geom_text_repel(data = filtered_data_u, aes(label = FA), vjust = -0.5, size = 3, nudge_x = 1, nudge_y = 0.1, max.overlaps = 100)+
  geom_text_repel(data = filtered_data_d, aes(label = FA), vjust = -0.5, size = 3, nudge_x = -1, nudge_y = 1, max.overlaps = 100)+
  scale_x_continuous(limits = c(-9.5, 9.5), breaks = c(-9,-5,-1,0,1,5,9), labels = c("-9","-5","-1", "0", "1", "5", "9") )+
  scale_y_continuous(limits = c(0, 25))+
  scale_color_manual(values=c("blue", "black", "red")) +
  theme_bw()+
  labs(title = "Pex1 G844d Veh vs Pex1 +/+ Veh", x = "Log2 Fold Change")+
  theme(legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5,margin = margin(b = 20), face = "bold"), 
        axis.title.y = element_text(margin = margin(r = 20)),
        axis.title.x = element_text(margin = margin(t = 20),)
  )+
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))+
  theme(legend.text = element_text(size = 14),
        legend.key.size = unit(1.5, "cm"))

dev.off()

jpeg("16_Liver_E_vs_WT.jpg", width = 1000, height = 750)
ggplot(data=merged_data_E, aes(x=Fold_Change, y=-log10(p_value), col = up_dwn)) +
  geom_point(size=4) +
  geom_vline(xintercept=c(0), col="black", linetype = "dashed") +
  geom_hline(yintercept=-log10(0.05), col="black", linetype = "dashed")+
  geom_text_repel(data = filtered_data_u, aes(label = FA), vjust = -0.5, size = 3, nudge_x = 1, nudge_y = 0.1, max.overlaps = 100)+
  geom_text_repel(data = filtered_data_d, aes(label = FA), vjust = -0.5, size = 3, nudge_x = -1, nudge_y = 1, max.overlaps = 100)+
  scale_x_continuous(limits = c(-9.5, 9.5), breaks = c(-9,-5,-1,0,1,5,9), labels = c("-9","-5","-1", "0", "1", "5", "9") )+
  scale_y_continuous(limits = c(0, 25))+
  scale_color_manual(values=c("blue", "black", "red")) +
  theme_bw()+
  labs(title = "Pex1 G844d Veh vs Pex1 +/+ Veh", x = "Log2 Fold Change")+
  theme(legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5,margin = margin(b = 20), face = "bold"), 
        axis.title.y = element_text(margin = margin(r = 20)),
        axis.title.x = element_text(margin = margin(t = 20),)
  )+
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))+
  theme(legend.text = element_text(size = 14),
        legend.key.size = unit(1.5, "cm"))

dev.off()

merged_data_D = merged_data[241:300,]
merged_data_D <- rbind(merged_data_D, data.frame(FA = 'X', p_value = 100000, Fold_Change = 100000, Dose = "NA", up_dwn = "Down"))
filtered_data_d <- subset(merged_data_D, up_dwn == "Down")
filtered_data_u <- subset(merged_data_D, up_dwn == "Up")



png("16_Liver_D_vs_WT.png", width = 1000, height = 750)
ggplot(data=merged_data_D, aes(x=Fold_Change, y=-log10(p_value), col = up_dwn)) +
  geom_point(size=4) +
  geom_vline(xintercept=c(0), col="black", linetype = "dashed") +
  geom_hline(yintercept=-log10(0.05), col="black", linetype = "dashed")+
  geom_text_repel(data = filtered_data_u, aes(label = FA), vjust = -0.5, size = 3, nudge_x = 1, nudge_y = 0.1, max.overlaps = 100)+
  geom_text_repel(data = filtered_data_d, aes(label = FA), vjust = -0.5, size = 3, nudge_x = -1, nudge_y = 1, max.overlaps = 100)+
  scale_x_continuous(limits = c(-9.5, 9.5), breaks = c(-9,-5,-1,0,1,5,9), labels = c("-9","-5","-1", "0", "1", "5", "9") )+
  scale_y_continuous(limits = c(0, 25))+
  scale_color_manual(values=c("blue", "black", "red")) +
  theme_bw()+
  labs(title = "Pex1 G844d 1:40 vs Pex1 +/+ Veh", x = "Log2 Fold Change")+
  theme(legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5,margin = margin(b = 20), face = "bold"), 
        axis.title.y = element_text(margin = margin(r = 20)),
        axis.title.x = element_text(margin = margin(t = 20))
  )+
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))+
  theme(legend.text = element_text(size = 14),
        legend.key.size = unit(1.5, "cm"))

dev.off()

jpeg("16_Liver_D_vs_WT.jpg", width = 1000, height = 750)
ggplot(data=merged_data_D, aes(x=Fold_Change, y=-log10(p_value), col = up_dwn)) +
  geom_point(size=4) +
  geom_vline(xintercept=c(0), col="black", linetype = "dashed") +
  geom_hline(yintercept=-log10(0.05), col="black", linetype = "dashed")+
  geom_text_repel(data = filtered_data_u, aes(label = FA), vjust = -0.5, size = 3, nudge_x = 1, nudge_y = 0.1, max.overlaps = 100)+
  geom_text_repel(data = filtered_data_d, aes(label = FA), vjust = -0.5, size = 3, nudge_x = -1, nudge_y = 1, max.overlaps = 100)+
  scale_x_continuous(limits = c(-9.5, 9.5), breaks = c(-9,-5,-1,0,1,5,9), labels = c("-9","-5","-1", "0", "1", "5", "9") )+
  scale_y_continuous(limits = c(0, 25))+
  scale_color_manual(values=c("blue", "black", "red")) +
  theme_bw()+
  labs(title = "Pex1 G844d 1:40 vs Pex1 +/+ Veh", x = "Log2 Fold Change")+
  theme(legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5,margin = margin(b = 20), face = "bold"), 
        axis.title.y = element_text(margin = margin(r = 20)),
        axis.title.x = element_text(margin = margin(t = 20))
  )+
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))+
  theme(legend.text = element_text(size = 14),
        legend.key.size = unit(1.5, "cm"))

dev.off()

merged_data_C = merged_data[181:240,]
merged_data_C <- rbind(merged_data_C, data.frame(FA = 'X', p_value = 100000, Fold_Change = 100000, Dose = "NA", up_dwn = "Down"))
merged_data_C <- rbind(merged_data_C, data.frame(FA = 'X', p_value = 100000, Fold_Change = 100000, Dose = "NA", up_dwn = "Up"))
filtered_data_d <- subset(merged_data_C, up_dwn == "Down")
filtered_data_u <- subset(merged_data_C, up_dwn == "Up")



png("16_Liver_C_vs_WT.png", width = 1000, height = 750)
ggplot(data=merged_data_C, aes(x=Fold_Change, y=-log10(p_value), col = up_dwn)) +
  geom_point(size=4) +
  geom_vline(xintercept=c(0), col="black", linetype = "dashed") +
  geom_hline(yintercept=-log10(0.05), col="black", linetype = "dashed")+
  geom_text_repel(data = filtered_data_u, aes(label = FA), vjust = -0.5, size = 3, nudge_x = 1, nudge_y = 0.1, max.overlaps = 100)+
  geom_text_repel(data = filtered_data_d, aes(label = FA), vjust = -0.5, size = 3, nudge_x = -1, nudge_y = 1, max.overlaps = 100)+
  scale_x_continuous(limits = c(-9.5, 9.5), breaks = c(-9,-5,-1,0,1,5,9), labels = c("-9","-5","-1", "0", "1", "5", "9") )+
  scale_y_continuous(limits = c(0, 25))+
  scale_color_manual(values=c("blue", "black", "red")) +
  theme_bw()+
  labs(title = "Pex1 G844d 1:10 vs Pex1 +/+ Veh", x = "Log2 Fold Change")+
  theme(legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5,margin = margin(b = 20), face = "bold"), 
        axis.title.y = element_text(margin = margin(r = 20)),
        axis.title.x = element_text(margin = margin(t = 20))
  )+
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))+
  theme(legend.text = element_text(size = 14),
        legend.key.size = unit(1.5, "cm"))

dev.off()

jpeg("16_Liver_C_vs_WT.jpg", width = 1000, height = 750)
ggplot(data=merged_data_C, aes(x=Fold_Change, y=-log10(p_value), col = up_dwn)) +
  geom_point(size=4) +
  geom_vline(xintercept=c(0), col="black", linetype = "dashed") +
  geom_hline(yintercept=-log10(0.05), col="black", linetype = "dashed")+
  geom_text_repel(data = filtered_data_u, aes(label = FA), vjust = -0.5, size = 3, nudge_x = 1, nudge_y = 0.1, max.overlaps = 100)+
  geom_text_repel(data = filtered_data_d, aes(label = FA), vjust = -0.5, size = 3, nudge_x = -1, nudge_y = 1, max.overlaps = 100)+
  scale_x_continuous(limits = c(-9.5, 9.5), breaks = c(-9,-5,-1,0,1,5,9), labels = c("-9","-5","-1", "0", "1", "5", "9") )+
  scale_y_continuous(limits = c(0, 25))+
  scale_color_manual(values=c("blue", "black", "red")) +
  theme_bw()+
  labs(title = "Pex1 G844d 1:10 vs Pex1 +/+ Veh", x = "Log2 Fold Change")+
  theme(legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5,margin = margin(b = 20), face = "bold"), 
        axis.title.y = element_text(margin = margin(r = 20)),
        axis.title.x = element_text(margin = margin(t = 20))
  )+
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))+
  theme(legend.text = element_text(size = 14),
        legend.key.size = unit(1.5, "cm"))

dev.off()

merged_data_B = merged_data[121:180,]
merged_data_B <- rbind(merged_data_B, data.frame(FA = 'X', p_value = 100000, Fold_Change = 100000, Dose = "NA", up_dwn = "Down"))
merged_data_B <- rbind(merged_data_B, data.frame(FA = 'X', p_value = 100000, Fold_Change = 100000, Dose = "NA", up_dwn = "Up"))
filtered_data_d <- subset(merged_data_B, up_dwn == "Down")
filtered_data_u <- subset(merged_data_B, up_dwn == "Up")



png("16_Liver_B_vs_WT.png", width = 1000, height = 750)
ggplot(data=merged_data_B, aes(x=Fold_Change, y=-log10(p_value), col = up_dwn)) +
  geom_point(size=4) +
  geom_vline(xintercept=c(0), col="black", linetype = "dashed") +
  geom_hline(yintercept=-log10(0.05), col="black", linetype = "dashed")+
  geom_text_repel(data = filtered_data_u, aes(label = FA), vjust = -0.5, size = 3, nudge_x = 1, nudge_y = 0.1, max.overlaps = 100)+
  geom_text_repel(data = filtered_data_d, aes(label = FA), vjust = -0.5, size = 3, nudge_x = -1, nudge_y = 1, max.overlaps = 100)+
  scale_x_continuous(limits = c(-9.5, 9.5), breaks = c(-9,-5,-1,0,1,5,9), labels = c("-9","-5","-1", "0", "1", "5", "9") )+
  scale_y_continuous(limits = c(0, 25))+
  scale_color_manual(values=c("blue", "black", "red")) +
  theme_bw()+
  labs(title = "Pex1 G844d 1:4 vs Pex1 +/+ Veh", x = "Log2 Fold Change")+
  theme(legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5,margin = margin(b = 20), face = "bold"), 
        axis.title.y = element_text(margin = margin(r = 20)),
        axis.title.x = element_text(margin = margin(t = 20))
  )+
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))+
  theme(legend.text = element_text(size = 14),
        legend.key.size = unit(1.5, "cm"))

dev.off()

jpeg("16_Liver_B_vs_WT.jpg", width = 1000, height = 750)
ggplot(data=merged_data_B, aes(x=Fold_Change, y=-log10(p_value), col = up_dwn)) +
  geom_point(size=4) +
  geom_vline(xintercept=c(0), col="black", linetype = "dashed") +
  geom_hline(yintercept=-log10(0.05), col="black", linetype = "dashed")+
  geom_text_repel(data = filtered_data_u, aes(label = FA), vjust = -0.5, size = 3, nudge_x = 1, nudge_y = 0.1, max.overlaps = 100)+
  geom_text_repel(data = filtered_data_d, aes(label = FA), vjust = -0.5, size = 3, nudge_x = -1, nudge_y = 1, max.overlaps = 100)+
  scale_x_continuous(limits = c(-9.5, 9.5), breaks = c(-9,-5,-1,0,1,5,9), labels = c("-9","-5","-1", "0", "1", "5", "9") )+
  scale_y_continuous(limits = c(0, 25))+
  scale_color_manual(values=c("blue", "black", "red")) +
  theme_bw()+
  labs(title = "Pex1 G844d 1:4 vs Pex1 +/+ Veh", x = "Log2 Fold Change")+
  theme(legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5,margin = margin(b = 20), face = "bold"), 
        axis.title.y = element_text(margin = margin(r = 20)),
        axis.title.x = element_text(margin = margin(t = 20))
  )+
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))+
  theme(legend.text = element_text(size = 14),
        legend.key.size = unit(1.5, "cm"))

dev.off()