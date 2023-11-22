library(lubridate)
library(ggplot2)
library(reshape2)
setwd("/home/shikhani/Documents/Bubble_data/Data_2022_08_13/")

#the next csv file was based on the  calibration curves in the word document and the output from the spreadsheet. the linear fit coefficients were spitted into columns for the ease of calculations. the delusion volumes are also provided
gc_data <- read.csv("GC_RawData_20220805.csv", sep = ";")
# calculating the co2 concetration from the GC in PPM using the calibiration curve linear equation and accounting for the sample volume 
gc_data$co2_GC_ppm <- (gc_data$co2_a*gc_data$co2_results_raw + gc_data$co2_b)/gc_data$Diluted.volume.for.GC2..ml.._injected.into.GC2


# Calculation of Dilution 

# the background is C1*V1=C2*V2 so C1= C2*(V2/V1) and the V2/V1 is the dilution factor
# the diluation factor , here DF, =(V(gas extracted from the original sample)+ V(big exetainer))/V(gas extracted from the original sample)
# so we have C1=C2*DF , where C2 is the concetration obtained from GC
gc_data$DF = (gc_data$Withdrawal.GC2.for.dilution..ml._injected.into.dilution.exetainer+ gc_data$Dilution.exetainer.volume..ml.)/(gc_data$Withdrawal.GC2.for.dilution..ml._injected.into.dilution.exetainer)
# calculating the real value of the the co2 in PPM
gc_data$co2_real = gc_data$co2_GC_ppm*gc_data$DF 
gc_data$co2_percent = gc_data$co2_real/10000

# repeat fro CH4

gc_data$ch4_GC_ppm <- (gc_data$ch4_a*gc_data$ch4_result_raw + gc_data$ch4_b)/gc_data$Diluted.volume.for.GC2..ml.._injected.into.GC2


gc_data$ch4_real = gc_data$ch4_GC_ppm*gc_data$DF 
gc_data$ch4_percent = gc_data$ch4_real/10000
# ch4 is less than 0.01% so it could be negelected, you can mention that in the text

##########
# Calculating N2 and Ar
# directly from the linear equation since the calibiration takes place against air samples and N2 100% samples
gc_data$N2_percent <- (gc_data$N2_a*gc_data$N2_result_raw+gc_data$N2_b)/gc_data$Withdrawal.for.GC1..ml.._injected.into.GC1
gc_data$Ar_percent <- (gc_data$Ar_a*gc_data$Ar_result_raw+gc_data$Ar_b)/gc_data$Withdrawal.for.GC1..ml.._injected.into.GC1

########################
#end of gc caclulation
###
# now plotting to see all gas composition using all sample data
sample_data <- read.csv("Sampledata_20220816.csv")



gas_data <- data.frame(sample=sample_data$sample_number ,depth=sample_data$Depth_meter,O2=sample_data$Oxygen_percent,CO2=gc_data$co2_percent,N2=gc_data$N2_percent,AR=gc_data$Ar_percent, ch4= gc_data$ch4_percent)

gas_data$total <- gas_data$O2+gas_data$CO2+gas_data$N2+ gas_data$AR

write.csv(gas_data, "gas_data_calculated_BubbleMS.csv", quote = F, row.names = F)




gas_data <- data.frame(sample=sample_data$sample_number ,depth=sample_data$Depth_meter,O2=sample_data$Oxygen_percent,CO2=gc_data$co2_percent,N2=gc_data$N2_percent,Ar=gc_data$Ar_percent)
gas_data <- gas_data[order(gas_data$sample),]
sample_data <- sample_data[order(sample_data$sample),]


gas_data <- gas_data[order(gas_data$depth),]
gas_data$H2O <- 2.1

gas_data_melted <- melt(gas_data, id.vars = c("sample", "depth"))
names(gas_data_melted) <- c("sample_number", "depth","gas", "percentage")
gas_data_melted<- gas_data_melted[order(gas_data_melted$sample_number),]
str(gas_data_melted)
gas_data_melted$depth_m <- paste0("Depth_",gas_data_melted$depth, " m _",rep(1:length(unique(gas_data_melted$sample_number)), each=5))
gas_data_melted<- gas_data_melted[order(gas_data_melted$depth_m),]

gas_data_melted$gas=factor(gas_data_melted$gas, levels=c("N2" , "Ar", "CO2" ,"O2","H2O"))



p2 <- ggplot(gas_data_melted, aes(x = percentage, y = rev(depth_m), fill = gas, label = percentage)) +
  geom_bar(stat = "identity") + theme_bw() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1), text = element_text(size=30)) + 
  ylab("Depth (m)") +
  xlab("Percentage (%)") + 
  scale_fill_manual(values = c("darkolivegreen3","darkorchid3","firebrick3", "deepskyblue2","black")) +
  scale_y_discrete(labels = rev(gas_data_melted$depth[seq(1, nrow(gas_data_melted), 5)])) + 
  geom_vline(xintercept=100, linetype="dashed", color = "black") + 
  scale_x_continuous(breaks = c(0,20,40,60,80,100,120), labels= c(0,20,40,60,80,100,120))

p2

ggsave(filename = "fig4_h2o_rotated.png",plot =  p2,  dpi = 300,width = 350,height = 350, units = 'mm')
