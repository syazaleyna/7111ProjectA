library(tidyverse)
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)


#read seedwash 30 min data from excel
seedwash30min2016<-read_excel('Seedwash Data for UQ- 30min_2015_2019.xlsx', sheet="2015", skip=6, na=c(""," ", "#N/A", "[-11059] No Good Data For Calculation"))
seedwash30min2017<-read_excel('Seedwash Data for UQ- 30min - Data Only - Set 2_March15.xlsx', skip=6, na=c(""," ", "#N/A", "[-11059] No Good Data For Calculation"))
seedwash30min2018<-read_excel('Seedwash Data for UQ- 30min - Data Only_March14.xlsx', skip=6, na=c(""," ", "#N/A", "[-11059] No Good Data For Calculation"))
seedwash30min2019<-read_excel('Seedwash Data for UQ- 30min_2015_2019.xlsx', sheet="2019", skip=6, na=c(""," ", "#N/A", "[-11059] No Good Data For Calculation"))

#read seedwash 20 sec data from excel
seedwash20sec2017<-read_excel('Seedwash Data for UQ- 20Sec - Data Only - Set 2_March15.xlsx', skip=6, na=c(""," ", "#N/A", "[-11059] No Good Data For Calculation"))
seedwash20sec2018<-read_excel('Seedwash Data for UQ- 20Sec - Data Only_March14.xlsx', skip=6, na=c(""," ", "#N/A", "[-11059] No Good Data For Calculation"))
seedwash20sec2019<-read_excel('Seedwash Data for UQ- 20Sec_2019.xlsx', skip=6, na=c(""," ", "#N/A", "[-11059] No Good Data For Calculation"))

#read seedwash 1 day data from excel
seedwashdaily<-read_excel('SeedwashData_1day_2016_2019.xlsx', skip=6, na=c(""," ", "#N/A", "[-11059] No Good Data For Calculation"))

#----------------------------------------------------------------------------------------------------------------------------------------

#extract feed data from daily observation
oxadestructdaily <- seedwashdaily$`2OOxDestREGD`
throughputdaily <- seedwashdaily$POFeedMQPV
spodaily <- seedwashdaily$POFeedAN.Ox
feedsodadaily <- seedwashdaily$POFeedAN.C
feeddensitydaily <- seedwashdaily$POFeedDTPV

dailytimestep <- c(1:length(seedwashdaily$Date))
for(i in 1:length(dailytimestep)){
  dailytimestep[i] <- seedwashdaily$Date[i] - seedwashdaily$Date[1462]
}


dailydata   <- data.frame(time = dailytimestep,           #day
                          oxalate = oxadestructdaily,     #%
                          throughput = throughputdaily,   #t/h
                          spo = spodaily*100,             #%*100
                          feedsoda = feedsodadaily,       #g/l
                          feeddensity = feeddensitydaily*100) #SG*100

multipledaily <- dailydata %>%
  gather(type, data, oxalate:feeddensity)

ggplot(multipledaily, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) + 
  geom_smooth(method = lm) + xlab("Time (day)") + ggtitle("Feed Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#observed against oxalate destruction
throughput <- multipledaily %>%
  filter(type == 'oxalate'| type == 'throughput')
ggplot(throughput, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) +
  xlab("Time (day)") + ylab("Throughput (t/h), Oxalate Destruction (%)") +
  ggtitle("Feed Throughput vs Oxalate Destruction")


spo <- multipledaily %>%
  filter(type == 'oxalate'| type == 'spo')
ggplot(spo, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) +
  xlab("Time (day)") + ylab("Solid Phase Oxalate (%*100), Oxalate Destruction (%)") +
  ggtitle("Feed Solid Phase Oxalate vs Oxalate Destruction")


feedsoda <- multipledaily %>%
  filter(type == 'oxalate'| type == 'feedsoda')
ggplot(feedsoda, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ylab("Feed Soda Conc. (g/l), Oxalate Destruction (%)") +
  ggtitle("Feed Soda Conc. vs Oxalate Destruction")


feeddensity <- multipledaily %>%
  filter(type == 'oxalate'| type == 'feeddensity')
ggplot(feeddensity, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ylab("Feed Density (SG*100), Oxalate Destruction (%)") +
  ggtitle("Feed Density vs Oxalate Destruction")


#------------------------------------------------------------------------------------------------
#extract A1 data from daily observation
statusdailyA1 <- seedwashdaily$PO1AFOLXBDI
drumspeeddailyA1 <- seedwashdaily$PO1AFDrumSTPV
bathleveldailyA1 <- seedwashdaily$PO1ABathLCPV
vacuumdailyA1 <- seedwashdaily$PO1AVacPCPV
feedflowdailyA1 <- seedwashdaily$PO1FeedFCPV
flocflowdailyA1 <- seedwashdaily$PO1FlocFTPV...17
cakewashdailyA1 <- seedwashdaily$PO1ASprayFCPV
clothwashdailyA1 <- seedwashdaily$PO1AFCoSFTPV
sodafiltdailyA1 <- seedwashdaily$PO1AFltAN.C
oxfiltdailyA1 <- seedwashdaily$PO1AFltAN.Ox

#soda concentration in ton/hr
sodaconcdailyA1 <- c(1:length(feedflowdailyA1))
for (i in 1:length(sodaconcdailyA1)){
  sodaconcdailyA1[i] <- feedflowdailyA1[i]*seedwashdaily$POFeedAN.C[i]/1000
}


dailydataA1 <- data.frame(time = dailytimestep,           #day
                          status = statusdailyA1,
                          oxalate = oxadestructdaily,     #%
                          throughput = throughputdaily,   #t/h
                          spo = spodaily*100,             #%
                          feedsoda = feedsodadaily,       #g/l
                          feeddensity = feeddensitydaily*100, #SG
                          drumspeed = drumspeeddailyA1,   #RPM
                          bathlevel = bathleveldailyA1,   #%
                          vacuum = vacuumdailyA1,         #kPa
                          feedflow = feedflowdailyA1,     #kl/h
                          flocflow = flocflowdailyA1,     #kl/h
                          cakewash = cakewashdailyA1,     #kl/h
                          clothwash = clothwashdailyA1,   #kl/h
                          sodafiltrate = sodafiltdailyA1, #g/l
                          oxfiltrate = oxfiltdailyA1,     #g/l
                          sodaconc = sodaconcdailyA1)     #t/h


multipledailyA1 <- dailydataA1 %>%
  filter(status == 'On') %>%
  gather(type, data, oxalate:sodaconc)

ggplot(multipledailyA1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  xlab("Time (day)") + ggtitle("Filter 1A Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#observed against oxalate destruction
drumspeedA1 <- multipledailyA1 %>%
  filter(type == 'drumspeed')
ggplot(drumspeedA1, aes(x=time, y=data)) + geom_line() +
  xlab("Time (day)") + ylab("Drum Speed (RPM)") +
  ggtitle("Filter 1A Drum Speed")


bathlevelA1 <- multipledailyA1 %>%
  filter(type == 'oxalate'| type == 'bathlevel')
ggplot(bathlevelA1, aes(x=time, y=data, color = type)) + geom_line() + 
  xlab("Time (day)") + ylab("Bath Level (%), Oxalate Destruction (%)") +
  ggtitle("Filter 1A Bath Level vs Oxalate Destruction")


vacuumA1 <- multipledailyA1 %>%
  filter(type == 'oxalate'| type == 'vacuum')
ggplot(vacuumA1, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ylab("Vacuum Pressure (kPa), Oxalate Destruction (%)") +
  ggtitle("Filter 1A Vacuum Pressure vs Oxalate Destruction")


feedflowA1 <- multipledailyA1 %>%
  filter(type == 'oxalate'| type == 'feedflow')
ggplot(feedflowA1, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ylab("Feed Flow (kl/h), Oxalate Destruction (%)") +
  ggtitle("Filter 1A Feed Flow vs Oxalate Destruction")


flocflowA1 <- multipledailyA1 %>%
  filter(type == 'oxalate'| type == 'flocflow')
ggplot(flocflowA1, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ylab("Floc Flow (kl/h), Oxalate Destruction (%)") +
  ggtitle("Filter 1A Floc Flow vs Oxalate Destruction")


cakewashA1 <- multipledailyA1 %>%
  filter(type == 'oxalate'| type == 'cakewash')
ggplot(cakewashA1, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ylab("Cake Wash Flow (kl/h), Oxalate Destruction (%)") +
  ggtitle("Filter 1A Cake Wash Flow vs Oxalate Destruction")


clothwashA1 <- multipledailyA1 %>%
  filter(type == 'oxalate'| type == 'clothwash')
ggplot(clothwashA1, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ylab("Cloth Wash Flow (kl/h), Oxalate Destruction (%)") +
  ggtitle("Filter 1A Cloth Wash Flow vs Oxalate Destruction")


sodafiltrateA1 <- multipledailyA1 %>%
  filter(type == 'oxalate'| type == 'sodafiltrate')
ggplot(sodafiltrateA1, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ylab("Filtrate Soda Conc. (g/l), Oxalate Destruction (%)") +
  ggtitle("Filter 1A Filtrate Soda Conc. vs Oxalate Destruction")


oxfiltrateA1 <- multipledailyA1 %>%
  filter(type == 'oxalate'| type == 'oxfiltrate')
ggplot(oxfiltrateA1, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ylab("Filtrate Oxalate (%), Oxalate Destruction (%)") +
  ggtitle("Filter 1A Filtrate Oxalate vs Oxalate Destruction")


sodaconcA1 <- multipledailyA1 %>%
  filter(type == 'oxalate'| type == 'sodaconc')
ggplot(sodaconcA1, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ylab("Inlet Soda (t/h), Oxalate Destruction (%)") +
  ggtitle("Filter 1A Inlet Soda vs Oxalate Destruction")


#-------------------------------------------------------------------------------------------
#extract A1 data from daily observation
drumspeeddailyA2 <- seedwashdaily$PO2AFDrumSTPV
bathleveldailyA2 <- seedwashdaily$PO2ABathLCPV
vacuumdailyA2 <- seedwashdaily$PO2AVacPCPV
feedflowdailyA2 <- seedwashdaily$PO2FeedFCPV
flocflowdailyA2 <- seedwashdaily$PO2FlocFTPV
cakewashdailyA2 <- seedwashdaily$PO2ASprayFCPV
clothwashdailyA2 <- seedwashdaily$PO2AFCoSFTPV
sodafiltdailyA2 <- seedwashdaily$PO2AFltAN.C
oxfiltdailyA2 <- seedwashdaily$PO2AFltAN.Ox

#soda concentration in ton/hr
sodaconcdailyA2 <- c(1:length(feedflowdailyA2))
for (i in 1:length(sodaconcdailyA2)){
  sodaconcdailyA2[i] <- feedflowdailyA2[i]*seedwashdaily$POFeedAN.C[i]/1000
}


dailydataA2 <- data.frame(time = dailytimestep,           #day
                          oxalate = oxadestructdaily,     #%
                          throughput = throughputdaily,   #t/h
                          spo = spodaily,                 #%
                          feedsoda = feedsodadaily,       #g/l
                          feeddensity = feeddensitydaily, #SG
                          drumspeed = drumspeeddailyA2,   #RPM
                          bathlevel = bathleveldailyA2,   #%
                          vacuum = vacuumdailyA2,         #kPa
                          feedflow = feedflowdailyA2,     #kl/h
                          flocflow = flocflowdailyA2,     #kl/h
                          cakewash = cakewashdailyA2,     #kl/h
                          clothwash = clothwashdailyA2,   #kl/h
                          sodafiltrate = sodafiltdailyA2, #g/l
                          oxfiltrate = oxfiltdailyA2,     #g/l
                          sodaconc = sodaconcdailyA2)     #t/h


multipledailyA2 <- dailydataA2 %>%
  gather(type, data, oxalate:sodaconc)

ggplot(multipledailyA2, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ggtitle("Filter 2A Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#observed against oxalate destruction
drumspeedA2 <- multipledailyA2 %>%
  filter(type == 'oxalate'| type == 'drumspeed')
ggplot(drumspeedA2, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ylab("Drum Speed (RPM), Oxalate Destruction (%)") +
  ggtitle("Filter 2A Drum Speed vs Oxalate Destruction")


bathlevelA2 <- multipledailyA2 %>%
  filter(type == 'oxalate'| type == 'bathlevel')
ggplot(bathlevelA2, aes(x=time, y=data, color = type)) + geom_line() + 
  xlab("Time (day)") + ylab("Bath Level (%), Oxalate Destruction (%)") +
  ggtitle("Filter 2A Bath Level vs Oxalate Destruction")


vacuumA2 <- multipledailyA2 %>%
  filter(type == 'oxalate'| type == 'vacuum')
ggplot(vacuumA2, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ylab("Vacuum Pressure (kPa), Oxalate Destruction (%)") +
  ggtitle("Filter 2A Vacuum Pressure vs Oxalate Destruction")


feedflowA2 <- multipledailyA2 %>%
  filter(type == 'oxalate'| type == 'feedflow')
ggplot(feedflowA2, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ylab("Feed Flow (kl/h), Oxalate Destruction (%)") +
  ggtitle("Filter 2A Feed Flow vs Oxalate Destruction")


flocflowA2 <- multipledailyA2 %>%
  filter(type == 'oxalate'| type == 'flocflow')
ggplot(flocflowA2, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ylab("Floc Flow (kl/h), Oxalate Destruction (%)") +
  ggtitle("Filter 2A Floc Flow vs Oxalate Destruction")


cakewashA2 <- multipledailyA2 %>%
  filter(type == 'oxalate'| type == 'cakewash')
ggplot(cakewashA2, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ylab("Cake Wash Flow (kl/h), Oxalate Destruction (%)") +
  ggtitle("Filter 2A Cake Wash Flow vs Oxalate Destruction")


clothwashA2 <- multipledailyA2 %>%
  filter(type == 'oxalate'| type == 'clothwash')
ggplot(clothwashA2, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ylab("Cloth Wash Flow (kl/h), Oxalate Destruction (%)") +
  ggtitle("Filter 2A Cloth Wash Flow vs Oxalate Destruction")


sodafiltrateA2 <- multipledailyA2 %>%
  filter(type == 'oxalate'| type == 'sodafiltrate')
ggplot(sodafiltrateA2, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ylab("Filtrate Soda Conc. (g/l), Oxalate Destruction (%)") +
  ggtitle("Filter 2A Filtrate Soda Conc. vs Oxalate Destruction")


oxfiltrateA2 <- multipledailyA2 %>%
  filter(type == 'oxalate'| type == 'oxfiltrate')
ggplot(oxfiltrateA2, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ylab("Filtrate Oxalate (%), Oxalate Destruction (%)") +
  ggtitle("Filter 2A Filtrate Oxalate vs Oxalate Destruction")


sodaconcA2 <- multipledailyA2 %>%
  filter(type == 'oxalate'| type == 'sodaconc')
ggplot(sodaconcA2, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ylab("Inlet Soda (t/h), Oxalate Destruction (%)") +
  ggtitle("Filter 2A Inlet Soda vs Oxalate Destruction")

























#assign bath level vector from each 30min observation
blevel30min0315 <- seedwash30min0315$PO2BFBathLCPV
blevel30min0314 <- seedwash30min0314$PO2BFBathLCPV

#assign time step of 30 mins
time <- seq(616, 0, by = -0.5)

bathlevel <- data.frame(time, blevel30min0314, blevel30min0315)
bathlevel0314 <- data.frame(time, level = blevel30min0314)
bathlevel0315 <- data.frame(time, level = blevel30min0315)

multiplebathlevel <- bathlevel %>% 
  rename(Dataset1 = blevel30min0315, 
         Dataset2 = blevel30min0314) %>%
  gather(type, level, Dataset1:Dataset2, na.rm = TRUE)

ggplot(bathlevel0314, aes(x=time, y = level)) + geom_line() + geom_smooth(method = lm) + 
  xlab("Time (hour)") + ylab("Level (%)")
ggplot(bathlevel0315, aes(x=time, y = level)) + geom_line() + geom_smooth(method = lm) + 
  xlab("Time (hour)") + ylab("Level (%)")
ggplot(multiplebathlevel, aes(x=time, y = level)) + geom_point() + geom_smooth(method = lm) +
  facet_wrap(~type) + xlab("Time (hour)") + ylab("Level (%)")

#ggplot(bathlevel0314, aes(x=level)) + geom_histogram()
#ggplot(bathlevel0315, aes(x=level)) + geom_histogram()
ggplot(multiplebathlevel, aes(x=level)) + geom_histogram() + facet_wrap(~type)

ggplot(multiplebathlevel, aes(x=type, y=level)) + geom_boxplot()

#-------------------------------------------------------------------------------------------------#
#possible cause of outliers is due to 2B Drum being offline
#extract status of 2B Drum

statusblevel0314 <- seedwash30min0314 %>%
  select(status = PO2BFOLXBDI, level = PO2BFBathLCPV) %>%
  mutate(type = "Dataset 2")
statusblevel0314 <- cbind(time, statusblevel0314)
statusblevel0314 <- na.omit(statusblevel0314)

statusblevel0315 <- seedwash30min0315 %>%
  select(status = PO2BFOLXBDI, level = PO2BFBathLCPV) %>%
  mutate(type = "Dataset 1")
statusblevel0315 <- cbind(time, statusblevel0315)
statusblevel0315 <- na.omit(statusblevel0315)

#group two datasets showing status of 2B Drum
statusblevel <- rbind(statusblevel0315, statusblevel0314)

#separate online and offline observation
onfilterstatus0314 <- statusblevel0314 %>%
  filter(status == "On")
offfilterstatus0314 <- statusblevel0314 %>%
  filter(status == "Off") 

onfilterstatus0315 <- statusblevel0315 %>%
  filter(status == "On") 
offfilterstatus0315 <- statusblevel0315 %>%
  filter(status == "Off") 

#group online and offline observations from two datasets
onfilter <- rbind(onfilterstatus0314, onfilterstatus0315)
offfilter <- rbind(offfilterstatus0314, offfilterstatus0315)

#plot graph during online status
#ggplot(onfilterstatus0314, aes(x=time, y=level)) + geom_point() + geom_smooth(method = lm) + 
#  geom_abline(slope = 0, intercept = 78.424) + xlab("Time (hour)") + ylab("Level (%)")
#ggplot(onfilterstatus0315, aes(x=time, y=level)) + geom_point() + geom_smooth(method = lm) + 
#  geom_abline(slope = 0, intercept = 81.282) + xlab("Time (hour)") + ylab("Level (%)")
ggplot(statusblevel, aes(x=time, y=level)) + geom_point() + 
  ggtitle("2B Drum Filter Bath Level: Online & Offline Data") +
  geom_point(aes(color = status)) + scale_colour_manual(values=c("red", "black")) + 
  theme(legend.position = c(0.05, 0.05), legend.justification = c("left","bottom")) + 
  geom_smooth(method = lm) + facet_wrap(~type) + xlab("Time (hour)") + ylab("Bath Level (%)")

ggplot(onfilter, aes(x=time, y=level)) + geom_point() + 
  geom_smooth(method = lm) + facet_wrap(~type) + xlab("Time (hour)") + ylab("Bath Level (%)") +
  ggtitle("2B Drum Filter Bath Level: Online Data Only")

#ggplot(onfilter, aes(x=level)) + geom_histogram() + facet_wrap(~type)
#ggplot(offfilter, aes(x=level)) + geom_histogram() + facet_wrap(~type)
ggplot(statusblevel, aes(x=level, fill = status)) + ggtitle("2B Drum Filter Bath Level (%)") + 
  xlab("Bath Level (%)") + geom_histogram(alpha = 0.9) + facet_wrap(~type)

#ggplot(onfilter, aes(x=type, y=level)) + geom_boxplot()
#ggplot(offfilter, aes(x=type, y=level)) + geom_boxplot()
ggplot(statusblevel, aes(x=type, y=level)) + geom_boxplot() + facet_wrap(~status)

ggplot(onfilter, aes(x=type, y=level)) + geom_boxplot() 

onfilterrev <- onfilter %>%
  filter(level > 5 & level < 95 )
ggplot(onfilterrev, aes(x=time, y=level)) + geom_line() + 
  geom_smooth(method = lm) + facet_wrap(~type) + xlab("Time (hour)") + ylab("Bath Level (%)") +
  ylim(0, 100)

library(zoo)
onfilter.ma <- SMA(onfilter$level, n=100)
onfilter.ma <- cbind(ma = onfilter.ma, onfilter)
ggplot(onfilter.ma, aes(x=type, y=ma)) + geom_boxplot() + facet_wrap(~status)

#library(zoo)
#sample <- rollmean(onfilterstatus0314$level, k=100, fill = NA)
#plot(sample) 
#library(TTR)
#sample2 <- SMA(onfilterstatus0314$level, n=100)
#sample2 <- cbind(sample2, onfilterstatus0314)
#ggplot(sample2, aes(x=type, y=sample2)) + geom_boxplot() 

#sample3 <- SMA(onfilterstatus0315$level, n=50)
#sample3 <- cbind(sample3, onfilterstatus0315)
#ggplot(sample3, aes(x=type, y=sample3)) + geom_boxplot() 

fourier <- abs(fft(onfilterrevDataset1$level))
freqfourier <- 1/(onfilterrevDataset1$time/24)
fourier <- cbind(fourier, freq=freqfourier, onfilterrevDataset1)
fourier <- fourier[2:1175,]
fourier <- fourier[1:(length(fourier$fourier)/2),]
ggplot(fourier, aes(x=freq, y=fourier)) + geom_col() + 
  xlab("Frequency (1/day)") + ylab("Magnitude") + 
  ggtitle("Dataset 1: Fourier Transformation of 2B Drum Bath Level")

fourier2 <- abs(fft(onfilterrevDataset2$level))
freqfourier2 <- 1/(onfilterrevDataset2$time/24)
fourier2 <- cbind(fourier = fourier2, freq=freqfourier2, onfilterrevDataset2)
fourier2 <- fourier2[2:1175,]
fourier2 <- fourier2[1:(length(fourier2$fourier)/2),]
ggplot(fourier2, aes(x=freq, y=fourier)) + geom_col() + 
  xlab("Frequency (1/day)") + ylab("Magnitude") + 
  ggtitle("Dataset 2: Fourier Transformation of 2B Drum Bath Level")

fourierdata <- rbind(fourier, fourier2)
ggplot(fourierdata, aes(x = freq, y = fourier)) + geom_col() + 
  xlab("Frequency (1/day)") + ylab("Magnitude") + 
  ggtitle("Fourier Transformation of 2B Drum Bath Level") + facet_wrap(~type)


rosnerDataset1 <- rosnerTest(onfilterstatus0314$level, k = 200, alpha = 0.05)

onfilterrevDataset1 <- onfilterrev %>%
  filter(type == "Dataset 1")
diff1 <- c(1:length(onfilterrevDataset1$level))
for (i in 1:length(onfilterrevDataset1$level)){
  diff1[i] <- onfilterrevDataset1$level[i+1] - onfilterrevDataset1$level[i]
}
diffDataset1 <- cbind(onfilterrevDataset1, diff = diff1[1:length(diff1)])

onfilterrevDataset2 <- onfilterrev %>%
  filter(type == "Dataset 2")
diff2 <- c(1:length(onfilterrevDataset2$level))
for (i in 1:length(onfilterrevDataset2$level)){
  diff2[i] <- onfilterrevDataset2$level[i+1] - onfilterrevDataset2$level[i]
}
diffDataset2 <- cbind(onfilterrevDataset2, diff = diff2[1:length(mean2)])

diffDataset <- rbind(diffDataset1, diffDataset2)
ggplot(diffDataset, aes(x=time, y = diff)) + geom_line() + facet_wrap(~type) + 
  xlab("Time (hour)") + ylab("Bath Level (%)") +
  ggtitle("Differencing of 2B Drum Filter Bath Level (%) Data")

hourinterval <- onfilterrevDataset1[1,]
for (i in 1:length(onfilterrevDataset1$level)) {
  if (i %% 2 == 0){
    hourinterval <- rbind(hourinterval, onfilterrevDataset1[i,])
    hourintervaladj <- hourinterval %>%
      mutate(interval = "1 hour", level = level*7)
  }
}
#ggplot(hourinterval, aes(x=time, y=level)) + geom_line()     
     
twohourinterval <- onfilterrevDataset1[1,]
for (i in 1:length(onfilterrevDataset1$level)) {
  if (i %% 4 == 0){
    twohourinterval <- rbind(twohourinterval, onfilterrevDataset1[i,])
    twohourintervaladj <- twohourinterval %>%
      mutate(interval = "2 hours", level = level*6)
  }
}     
#ggplot(twohourinterval, aes(x=time, y=level)) + geom_line()     

fourhourinterval <- onfilterrevDataset1[1,]
for (i in 1:length(onfilterrevDataset1$level)) {
  if (i %% 8 == 0){
    fourhourinterval <- rbind(fourhourinterval, onfilterrevDataset1[i,])
    fourhourintervaladj <- fourhourinterval %>%
      mutate(interval = "4 hours", level = level*5)
  }
}

sixhourinterval <- onfilterrevDataset1[1,]
for (i in 1:length(onfilterrevDataset1$level)) {
  if (i %% 12 == 0){
    sixhourinterval <- rbind(sixhourinterval, onfilterrevDataset1[i,])
    sixhourintervaladj <- sixhourinterval %>%
      mutate(interval = "6 hours", level = level*4)
  }
}     
#ggplot(sixhourinterval, aes(x=time, y=level)) + geom_line()  

eighthourinterval <- onfilterrevDataset1[1,]
for (i in 1:length(onfilterrevDataset1$level)) {
  if (i %% 16 == 0){
    eighthourinterval <- rbind(eighthourinterval, onfilterrevDataset1[i,])
    eighthourintervaladj <- eighthourinterval %>%
      mutate(interval = "8 hours", level = level*3)
  }
}     
#ggplot(eighthourinterval, aes(x=time, y=level)) + geom_line()  

twelvehourinterval <- onfilterrevDataset1[1,]
for (i in 1:length(onfilterrevDataset1$level)) {
  if (i %% 24 == 0){
    twelvehourinterval <- rbind(twelvehourinterval, onfilterrevDataset1[i,])
    twelvehourintervaladj <- twelvehourinterval %>%
      mutate(interval = "12 hours", level = level*2)
  }
}     
#ggplot(twelvehourinterval, aes(x=time, y=level)) + geom_line() 

twentyfourhourinterval <- onfilterrevDataset1[1,]
for (i in 1:length(onfilterrevDataset1$level)) {
  if (i %% 48 == 0){
    twentyfourhourinterval <- rbind(twentyfourhourinterval, onfilterrevDataset1[i,])
    twentyfourhourintervaladj <- twentyfourhourinterval %>%
      mutate(interval = "24 hours", level = level*1)
  }
}     
#ggplot(twentyfourhourinterval, aes(x=time, y=level)) + geom_line() 



timeinterval <- rbind(hourintervaladj, twohourintervaladj, fourhourintervaladj, 
                      sixhourintervaladj, eighthourintervaladj, twelvehourintervaladj, 
                      twentyfourhourintervaladj)


ggplot(timeinterval, aes(x=time, y=level, color = interval)) + geom_line() +
  xlab("Time (hour)") + ylab("Offset Bath Level") + 
  ggtitle("Resampling Dataset 1 of 2B Drum Bath Level")


#-----------------------------------------------------------------------------------------------

hourinterval2 <- onfilterrevDataset2[1,]
for (i in 1:length(onfilterrevDataset2$level)) {
  if (i %% 2 == 0){
    hourinterval2 <- rbind(hourinterval2, onfilterrevDataset2[i,])
    hourintervaladj2 <- hourinterval2 %>%
      mutate(interval = "1 hour", level = level*7)
  }
}

twohourinterval2 <- onfilterrevDataset2[1,]
for (i in 1:length(onfilterrevDataset2$level)) {
  if (i %% 4 == 0){
    twohourinterval2 <- rbind(twohourinterval2, onfilterrevDataset2[i,])
    twohourintervaladj2 <- twohourinterval2 %>%
      mutate(interval = "2 hours", level = level*6)
  }
}     

fourhourinterval2 <- onfilterrevDataset2[1,]
for (i in 1:length(onfilterrevDataset2$level)) {
  if (i %% 8 == 0){
    fourhourinterval2 <- rbind(fourhourinterval2, onfilterrevDataset1[i,])
    fourhourintervaladj2 <- fourhourinterval2 %>%
      mutate(interval = "4 hours", level = level*5)
  }
}

sixhourinterval2 <- onfilterrevDataset2[1,]
for (i in 1:length(onfilterrevDataset2$level)) {
  if (i %% 12 == 0){
    sixhourinterval2 <- rbind(sixhourinterval2, onfilterrevDataset2[i,])
    sixhourintervaladj2 <- sixhourinterval2 %>%
      mutate(interval = "6 hours", level = level*4)
  }
}     

eighthourinterval2 <- onfilterrevDataset2[1,]
for (i in 1:length(onfilterrevDataset2$level)) {
  if (i %% 16 == 0){
    eighthourinterval2 <- rbind(eighthourinterval2, onfilterrevDataset2[i,])
    eighthourintervaladj2 <- eighthourinterval2 %>%
      mutate(interval = "8 hours", level = level*3)
  }
}     

twelvehourinterval2 <- onfilterrevDataset2[1,]
for (i in 1:length(onfilterrevDataset2$level)) {
  if (i %% 24 == 0){
    twelvehourinterval2 <- rbind(twelvehourinterval2, onfilterrevDataset2[i,])
    twelvehourintervaladj2 <- twelvehourinterval2 %>%
      mutate(interval = "12 hours", level = level*2)
  }
}     

twentyfourhourinterval2 <- onfilterrevDataset2[1,]
for (i in 1:length(onfilterrevDataset2$level)) {
  if (i %% 48 == 0){
    twentyfourhourinterval2 <- rbind(twentyfourhourinterval2, onfilterrevDataset2[i,])
    twentyfourhourintervaladj2 <- twentyfourhourinterval2 %>%
      mutate(interval = "24 hours", level = level*1)
  }
}     


timeinterval2 <- rbind(hourintervaladj2, twohourintervaladj2, fourhourintervaladj2, 
                      sixhourintervaladj2, eighthourintervaladj2, twelvehourintervaladj2, 
                      twentyfourhourintervaladj2)


ggplot(timeinterval2, aes(x=time, y=level, color = interval)) + geom_line() +
  xlab("Time (hour)") + ylab("Offset Bath Level") + 
  ggtitle("Resampling Dataset 2 of 2B Drum Bath Level")


Dataset1 <- onfilterrevDataset1$level
Dataset2 <- onfilterrevDataset2$level

acf(Dataset1)
pacf(Dataset1)

acf(Dataset2)
pacf(Dataset2)

#low filter dataset 1
lowbf1 <- butter(3, 0.1, type = "low")
y1 <- filtfilt(lowbf1, onfilterrevDataset1$level)

lowpass1 <- data.frame(time = onfilterrevDataset1$time, filter = y1, 
                       nofilter = onfilterrevDataset1$level)
lowpass1fin <- lowpass1 %>%
  gather(type, level, filter:nofilter)

ggplot(lowpass1fin, aes(x=time, y=level, color = type, alpha = type)) + geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Bath Level (%)") + 
  ggtitle("Low Pass Filter Dataset 1 of 2B Drum Bath Level")

#low filter dataset 2
lowbf2 <- butter(3, 0.1, type = "low")
y2 <- filtfilt(lowbf2, onfilterrevDataset2$level)

lowpass2 <- data.frame(time = onfilterrevDataset2$time, filter = y2, 
                       nofilter = onfilterrevDataset2$level)
lowpass2fin <- lowpass2 %>%
  gather(type, level, filter:nofilter)

ggplot(lowpass2fin, aes(x=time, y=level, color = type, alpha = type)) + geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Bath Level (%)") + 
  ggtitle("Low Pass Filter Dataset 2 of 2B Drum Bath Level")


#high filter dataset 1
highbf1 <- butter(3, 0.1, type = "high")
z1 <- filtfilt(highbf1, onfilterrevDataset1$level)

highpass1 <- data.frame(time = onfilterrevDataset1$time, filter = z1, 
                       nofilter = onfilterrevDataset1$level)
highpass1fin <- highpass1 %>%
  gather(type, level, filter:nofilter)

ggplot(highpass1fin, aes(x=time, y=level, color = type, alpha = type)) + geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Bath Level (%)") + 
  ggtitle("High Pass Filter Dataset 1 of 2B Drum Bath Level")

#high filter dataset 2
highbf2 <- butter(3, 0.1, type = "high")
z2 <- filtfilt(highbf2, onfilterrevDataset1$level)

highpass2 <- data.frame(time = onfilterrevDataset1$time, filter = z2, 
                        nofilter = onfilterrevDataset1$level)
highpass2fin <- highpass2 %>%
  gather(type, level, filter:nofilter)

ggplot(highpass2fin, aes(x=time, y=level, color = type, alpha = type)) + geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Bath Level (%)") + 
  ggtitle("High Pass Filter Dataset 2 of 2B Drum Bath Level")

  