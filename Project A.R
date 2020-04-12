library(tidyverse)
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)


#read seedwash 30 min data from excel
seedwash30min2016<-read_excel('Seedwash Data for UQ- 30min_2015_2019.xlsx', sheet="2015", skip=6, na=c(""," ", "#N/A", "[-11059] No Good Data For Calculation", "Argument is not a string or cell reference"))
seedwash30min2017<-read_excel('Seedwash Data for UQ- 30min - Data Only - Set 2_March15.xlsx', skip=6, na=c(""," ", "#N/A", "[-11059] No Good Data For Calculation"))
seedwash30min2018<-read_excel('Seedwash Data for UQ- 30min - Data Only_March14.xlsx', skip=6, na=c(""," ", "#N/A", "[-11059] No Good Data For Calculation"))
seedwash30min2019<-read_excel('Seedwash Data for UQ- 30min_2015_2019.xlsx', sheet="2019", skip=6, na=c(""," ", "#N/A", "[-11059] No Good Data For Calculation", "Argument is not a string or cell reference"))

#read seedwash 20 sec data from excel
seedwash20sec2017<-read_excel('Seedwash Data for UQ- 20Sec - Data Only - Set 2_March15.xlsx', skip=6, na=c(""," ", "#N/A", "[-11059] No Good Data For Calculation"))
seedwash20sec2018<-read_excel('Seedwash Data for UQ- 20Sec - Data Only_March14.xlsx', skip=6, na=c(""," ", "#N/A", "[-11059] No Good Data For Calculation"))
seedwash20sec2019<-read_excel('Seedwash Data for UQ- 20Sec_2019.xlsx', skip=6, na=c(""," ", "#N/A", "[-11059] No Good Data For Calculation"))

#read seedwash 1 day data from excel
seedwashdaily<-read_excel('SeedwashData_1day_2016_2019.xlsx', skip=6, na=c(""," ", "#N/A", "[-11059] No Good Data For Calculation"))

#----------------------------------------------------------------------------------------------------------------------------------------

#extract feed data from daily observation
outputspodaily <- seedwashdaily$PONthTkAN.Ox
outputsodadaily <- seedwashdaily$POSOFATPV.S
outputaluminadaily <- seedwashdaily$TotalFlow
throughputdaily <- seedwashdaily$POFeedMQPV
spodaily <- seedwashdaily$POFeedAN.Ox
feedsodadaily <- seedwashdaily$POFeedAN.C
feeddensitydaily <- seedwashdaily$POFeedDTPV

dailytimestep <- c(1:length(seedwashdaily$Date))
for(i in 1:length(dailytimestep)){
  dailytimestep[i] <- seedwashdaily$Date[i] - seedwashdaily$Date[1462]
}


dailydata   <- data.frame(time = dailytimestep,              #day
                          outputspo = outputspodaily,        #%
                          outputsoda = outputsodadaily,      #g/l
                          outputalumina = outputaluminadaily,#kl/h
                          throughput = throughputdaily,      #t/h
                          feedspo = spodaily,                #%
                          feedsoda = feedsodadaily,          #g/l
                          feeddensity = feeddensitydaily)    #SG

multipledaily <- dailydata %>%
  gather(type, data, outputspo:feeddensity)

ggplot(multipledaily, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) + 
  geom_smooth(method = lm) + xlab("Time (day)") + ggtitle("Daily Feed Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#observed against Output Soda Conc.
outputspo <- multipledaily %>%
  filter(type == 'outputspo')
ggplot(outputspo, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) +
  xlab("Time (day)") + ylab("Output SPO (%)") +
  ggtitle("Daily Output SPO")

outputsoda <- multipledaily %>%
  filter(type == 'outputsoda')
ggplot(outputsoda, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) +
  xlab("Time (day)") + ylab("Output Soda Conc. (g/l)") +
  ggtitle("Daily Output Soda Conc.")

outputalumina <- multipledaily %>%
  filter(type == 'outputalumina')
ggplot(outputalumina, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) +
  xlab("Time (day)") + ylab("Output Alumina (kl/h)") +
  ggtitle("Daily Output Alumina")

throughput <- multipledaily %>%
  filter(type == 'throughput')
ggplot(throughput, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) +
  xlab("Time (day)") + ylab("Throughput (t/h)") +
  ggtitle("Daily Feed Throughput")


feedspo <- multipledaily %>%
  filter(type == 'feedspo')
ggplot(feedspo, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) +
  xlab("Time (day)") + ylab("Solid Phase Oxalate (%)") +
  ggtitle("Daily Feed Solid Phase Oxalate")


feedsoda <- multipledaily %>%
  filter(type == 'feedsoda')
ggplot(feedsoda, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) +
  xlab("Time (day)") + ylab("Feed Soda Conc. (g/l)") +
  ggtitle("Daily Feed Soda Conc.")


feeddensity <- multipledaily %>%
  filter(type == 'feeddensity')
ggplot(feeddensity, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) +
  xlab("Time (day)") + ylab("Feed Density (SG)") +
  ggtitle("Daily Feed Density")


#------------------------------------------------------------------------------------------------
#extract 1A data from daily observation
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
                          outputspo = outputspodaily,     #%
                          outputsoda = outputsodadaily,   #g/l
                          outputalumina = outputaluminadaily,#kl/h
                          throughput = throughputdaily,   #t/h
                          feedspo = spodaily,             #%
                          feedsoda = feedsodadaily,       #g/l
                          feeddensity = feeddensitydaily, #SG
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
  gather(type, data, outputspo:sodaconc)

ggplot(multipledailyA1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  xlab("Time (day)") + ggtitle("Filter 1A Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#observed against Output Soda Conc.
drumspeedA1 <- multipledailyA1 %>%
  filter(type == 'drumspeed')
ggplot(drumspeedA1, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) +
  xlab("Time (day)") + ylab("Drum Speed (RPM)") +
  ggtitle("Daily Filter 1A Drum Speed")


bathlevelA1 <- multipledailyA1 %>%
  filter(type == 'bathlevel')
ggplot(bathlevelA1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) + 
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Bath Level (%)") +
  ggtitle("Daily Filter 1A Bath Level")


vacuumA1 <- multipledailyA1 %>%
  filter(type == 'vacuum')
ggplot(vacuumA1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Vacuum Pressure (kPa)") +
  ggtitle("Daily Filter 1A Vacuum Pressure")


feedflowA1 <- multipledailyA1 %>%
  filter(type == 'feedflow')
ggplot(feedflowA1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Feed Flow (kl/h)") +
  ggtitle("Daily Filter 1A Feed Flow")


flocflowA1 <- multipledailyA1 %>%
  filter(type == 'flocflow')
ggplot(flocflowA1, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Floc Flow (kl/h)") +
  ggtitle("Daily Filter 1A Floc Flow")


cakewashA1 <- multipledailyA1 %>%
  filter( type == 'cakewash')
ggplot(cakewashA1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Cake Wash Flow (kl/h)") +
  ggtitle("Daily Filter 1A Cake Wash Flow")


clothwashA1 <- multipledailyA1 %>%
  filter( type == 'clothwash')
ggplot(clothwashA1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Cloth Wash Flow (kl/h)") +
  ggtitle("Daily Filter 1A Cloth Wash Flow")


sodafiltrateA1 <- multipledailyA1 %>%
  filter( type == 'sodafiltrate')
ggplot(sodafiltrateA1, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Filtrate Soda Conc. (g/l)") +
  ggtitle("Daily Filter 1A Filtrate Soda Conc.")


oxfiltrateA1 <- multipledailyA1 %>%
  filter( type == 'oxfiltrate')
ggplot(oxfiltrateA1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Filtrate Oxalate (%)") +
  ggtitle("Daily Filter 1A Filtrate Oxalate")


sodaconcA1 <- multipledailyA1 %>%
  filter( type == 'sodaconc')
ggplot(sodaconcA1, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Inlet Soda (t/h)") +
  ggtitle("Daily Filter 1A Inlet Soda")


#-------------------------------------------------------------------------------------------
#extract 2A data from daily observation
statusdailyA2 <- seedwashdaily$PO2AFOLXBDI
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
                          status = statusdailyA2,
                          outputspo = outputspodaily,     #%
                          outputsoda = outputsodadaily,   #g/l
                          outputalumina = outputaluminadaily,#kl/h
                          throughput = throughputdaily,   #t/h
                          feedspo = spodaily,             #%
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
  filter(status == 'On') %>%
  gather(type, data, outputspo:sodaconc)

ggplot(multipledailyA2, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ggtitle("Filter 2A Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#observed against Output Soda Conc.
drumspeedA2 <- multipledailyA2 %>%
  filter(type == 'drumspeed')
ggplot(drumspeedA2, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Drum Speed (RPM)") +
  ggtitle("Daily Filter 2A Drum Speed")


bathlevelA2 <- multipledailyA2 %>%
  filter( type == 'bathlevel')
ggplot(bathlevelA2, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) + 
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Bath Level (%)") +
  ggtitle("Daily Filter 2A Bath Level")


vacuumA2 <- multipledailyA2 %>%
  filter( type == 'vacuum')
ggplot(vacuumA2, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Vacuum Pressure (kPa)") +
  ggtitle("Daily Filter 2A Vacuum Pressure")


feedflowA2 <- multipledailyA2 %>%
  filter( type == 'feedflow')
ggplot(feedflowA2, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Feed Flow (kl/h)") +
  ggtitle("Daily Filter 2A Feed Flow")


flocflowA2 <- multipledailyA2 %>%
  filter(type == 'flocflow')
ggplot(flocflowA2, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Floc Flow (kl/h)") +
  ggtitle("Daily Filter 2A Floc Flow")


cakewashA2 <- multipledailyA2 %>%
  filter( type == 'cakewash')
ggplot(cakewashA2, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Cake Wash Flow (kl/h)") +
  ggtitle("Daily Filter 2A Cake Wash Flow")


clothwashA2 <- multipledailyA2 %>%
  filter( type == 'clothwash')
ggplot(clothwashA2, aes(x=time, y=data, color = type)) + geom_line(alpha=0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Cloth Wash Flow (kl/h)") +
  ggtitle("Daily Filter 2A Cloth Wash Flow")


sodafiltrateA2 <- multipledailyA2 %>%
  filter( type == 'sodafiltrate')
ggplot(sodafiltrateA2, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Filtrate Soda Conc. (g/l)") +
  ggtitle("Daily Filter 2A Filtrate Soda Conc.")


oxfiltrateA2 <- multipledailyA2 %>%
  filter( type == 'oxfiltrate')
ggplot(oxfiltrateA2, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Filtrate Oxalate (%)") +
  ggtitle("Daily Filter 2A Filtrate Oxalate")


sodaconcA2 <- multipledailyA2 %>%
  filter( type == 'sodaconc')
ggplot(sodaconcA2, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Inlet Soda (t/h)") +
  ggtitle("Daily Filter 2A Inlet Soda")


#-------------------------------------------------------------------------------------------
#extract 3A data from daily observation
statusdailyA3 <- seedwashdaily$PO3AFOLXBDI
drumspeeddailyA3 <- seedwashdaily$PO3AFDrumSTPV
bathleveldailyA3 <- seedwashdaily$PO3ABathLCPV
vacuumdailyA3 <- seedwashdaily$PO3AVacPCPV
feedflowdailyA3 <- seedwashdaily$PO3FeedFCPV
flocflowdailyA3 <- seedwashdaily$PO3FlocFTPV
cakewashdailyA3 <- seedwashdaily$PO3ASprayFCPV
clothwashdailyA3 <- seedwashdaily$POSW3AcfMXPV
sodafiltdailyA3 <- seedwashdaily$PO3AFltAN.C
oxfiltdailyA3 <- seedwashdaily$PO3AFltAN.Ox

#soda concentration in ton/hr
sodaconcdailyA3 <- c(1:length(feedflowdailyA3))
for (i in 1:length(sodaconcdailyA3)){
  sodaconcdailyA3[i] <- feedflowdailyA3[i]*seedwashdaily$POFeedAN.C[i]/1000
}


dailydataA3 <- data.frame(time = dailytimestep,           #day
                          status = statusdailyA3,
                          outputspo = outputspodaily,     #%
                          outputsoda = outputsodadaily,   #g/l
                          outputalumina = outputaluminadaily,#kl/h
                          throughput = throughputdaily,   #t/h
                          feedspo = spodaily,             #%
                          feedsoda = feedsodadaily,       #g/l
                          feeddensity = feeddensitydaily, #SG
                          drumspeed = drumspeeddailyA3,   #RPM
                          bathlevel = bathleveldailyA3,   #%
                          vacuum = vacuumdailyA3,         #kPa
                          feedflow = feedflowdailyA3,     #kl/h
                          flocflow = flocflowdailyA3,     #kl/h
                          cakewash = cakewashdailyA3,     #kl/h
                          clothwash = clothwashdailyA3,   #kl/h
                          sodafiltrate = sodafiltdailyA3, #g/l
                          oxfiltrate = oxfiltdailyA3,     #g/l
                          sodaconc = sodaconcdailyA3)     #t/h


multipledailyA3 <- dailydataA3 %>%
  filter(status == 'On') %>%
  gather(type, data, outputspo:sodaconc)

ggplot(multipledailyA3, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ggtitle("Daily Filter 3A Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#observed against Output Soda Conc.
drumspeedA3 <- multipledailyA3 %>%
  filter(type == 'drumspeed')
ggplot(drumspeedA3, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Drum Speed (RPM)") +
  ggtitle("Daily Filter 3A Drum Speed")


bathlevelA3 <- multipledailyA3 %>%
  filter( type == 'bathlevel')
ggplot(bathlevelA3, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) + 
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Bath Level (%)") +
  ggtitle("Daily Filter 3A Bath Level")


vacuumA3 <- multipledailyA3 %>%
  filter( type == 'vacuum')
ggplot(vacuumA3, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Vacuum Pressure (kPa)") +
  ggtitle("Daily Filter 3A Vacuum Pressure")


feedflowA3 <- multipledailyA3 %>%
  filter( type == 'feedflow')
ggplot(feedflowA3, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Feed Flow (kl/h)") +
  ggtitle("Daily Filter 3A Feed Flow")


flocflowA3 <- multipledailyA3 %>%
  filter(type == 'flocflow')
ggplot(flocflowA3, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Floc Flow (kl/h)") +
  ggtitle("Daily Filter 3A Floc Flow")


cakewashA3 <- multipledailyA3 %>%
  filter( type == 'cakewash')
ggplot(cakewashA3, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Cake Wash Flow (kl/h)") +
  ggtitle("Daily Filter 3A Cake Wash Flow")


clothwashA3 <- multipledailyA3 %>%
  filter( type == 'clothwash')
ggplot(clothwashA3, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Cloth Wash Flow (kl/h)") +
  ggtitle("Daily Filter 3A Cloth Wash Flow")


sodafiltrateA3 <- multipledailyA3 %>%
  filter( type == 'sodafiltrate')
ggplot(sodafiltrateA3, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Filtrate Soda Conc. (g/l)") +
  ggtitle("Daily Filter 3A Filtrate Soda Conc.")


oxfiltrateA3 <- multipledailyA3 %>%
  filter( type == 'oxfiltrate')
ggplot(oxfiltrateA3, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Filtrate Oxalate (%)") +
  ggtitle("Daily Filter 3A Filtrate Oxalate")


sodaconcA3 <- multipledailyA3 %>%
  filter( type == 'sodaconc')
ggplot(sodaconcA3, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Inlet Soda (t/h)") +
  ggtitle("Daily Filter 3A Inlet Soda")


#-------------------------------------------------------------------------------------------
#extract 4A data from daily observation
statusdailyA4 <- seedwashdaily$PO3BFiltAXBDI
drumspeeddailyA4 <- seedwashdaily$PO3BFDrumSTPV
bathleveldailyA4 <- seedwashdaily$PO4AFBathLCPV
vacuumdailyA4 <- seedwashdaily$PO3BVacPCPV
feedflowdailyA4 <- seedwashdaily$PO4AFFeedFCPV
flocflowdailyA4 <- seedwashdaily$PO4AFlocFTPV
cakewashdailyA4 <- seedwashdaily$PO4AFCakeWFTPV
clothwashdailyA4 <- seedwashdaily$PO4AFHCRFCPV...120
sodafiltdailyA4 <- seedwashdaily$PO4AFltAN.C
oxfiltdailyA4 <- seedwashdaily$PO4AFltAN.Ox
configdaily <- seedwashdaily$po3b4afxbdi

#soda concentration in ton/hr
sodaconcdailyA4 <- c(1:length(feedflowdailyA4))
for (i in 1:length(sodaconcdailyA4)){
  sodaconcdailyA4[i] <- feedflowdailyA4[i]*seedwashdaily$POFeedAN.C[i]/1000
}


dailydataA4 <- data.frame(time = dailytimestep,           #day
                          status = statusdailyA4,
                          config = configdaily,
                          outputspo = outputspodaily,     #%
                          outputsoda = outputsodadaily,   #g/l
                          outputalumina = outputaluminadaily,#kl/h
                          throughput = throughputdaily,   #t/h
                          feedspo = spodaily,             #%
                          feedsoda = feedsodadaily,       #g/l
                          feeddensity = feeddensitydaily, #SG
                          drumspeed = drumspeeddailyA4,   #RPM
                          bathlevel = bathleveldailyA4,   #%
                          vacuum = vacuumdailyA4,         #kPa
                          feedflow = feedflowdailyA4,     #kl/h
                          flocflow = flocflowdailyA4,     #kl/h
                          cakewash = cakewashdailyA4,     #kl/h
                          clothwash = clothwashdailyA4,   #kl/h
                          sodafiltrate = sodafiltdailyA4, #g/l
                          oxfiltrate = oxfiltdailyA4,     #g/l
                          sodaconc = sodaconcdailyA4)     #t/h


multipledailyA4 <- dailydataA4 %>%
  filter(status == 'Run' & config == '4A') %>%
  gather(type, data, outputspo:sodaconc)

ggplot(multipledailyA4, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ggtitle("Daily Filter 4A Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#observed against Output Soda Conc.
drumspeedA4 <- multipledailyA4 %>%
  filter(type == 'drumspeed')
ggplot(drumspeedA4, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Drum Speed (RPM)") +
  ggtitle("Daily Filter 4A Drum Speed")


bathlevelA4 <- multipledailyA4 %>%
  filter( type == 'bathlevel')
ggplot(bathlevelA4, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) + 
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Bath Level (%)") +
  ggtitle("Daily Filter 4A Bath Level")


vacuumA4 <- multipledailyA4 %>%
  filter( type == 'vacuum')
ggplot(vacuumA4, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Vacuum Pressure (kPa)") +
  ggtitle("Daily Filter 4A Vacuum Pressure")


feedflowA4 <- multipledailyA4 %>%
  filter( type == 'feedflow')
ggplot(feedflowA4, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Feed Flow (kl/h)") +
  ggtitle("Daily Filter 4A Feed Flow")


flocflowA4 <- multipledailyA4 %>%
  filter(type == 'flocflow')
ggplot(flocflowA4, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Floc Flow (kl/h)") +
  ggtitle("Daily Filter 4A Floc Flow")


cakewashA4 <- multipledailyA4 %>%
  filter( type == 'cakewash')
ggplot(cakewashA4, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Cake Wash Flow (kl/h)") +
  ggtitle("Daily Filter 4A Cake Wash Flow")


clothwashA4 <- multipledailyA4 %>%
  filter( type == 'clothwash')
ggplot(clothwashA4, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Cloth Wash Flow (kl/h)") +
  ggtitle("Daily Filter 4A Cloth Wash Flow")


sodafiltrateA4 <- multipledailyA4 %>%
  filter( type == 'sodafiltrate')
ggplot(sodafiltrateA4, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Filtrate Soda Conc. (g/l)") +
  ggtitle("Daily Filter 4A Filtrate Soda Conc.")


oxfiltrateA4 <- multipledailyA4 %>%
  filter( type == 'oxfiltrate')
ggplot(oxfiltrateA4, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Filtrate Oxalate (%)") +
  ggtitle("Daily Filter 4A Filtrate Oxalate")


sodaconcA4 <- multipledailyA4 %>%
  filter( type == 'sodaconc')
ggplot(sodaconcA4, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Inlet Soda (t/h)") +
  ggtitle("Daily Filter 4A Inlet Soda")


#-------------------------------------------------------------------------------------------
#extract 1B data from daily observation
statusdailyB1 <- seedwashdaily$PO1BFOLXBDI
drumspeeddailyB1 <- seedwashdaily$PO1BFDrumSTPV
bathleveldailyB1 <- seedwashdaily$PO1BFBathLCPV
vacuumdailyB1 <- seedwashdaily$PO1BVacPCPV
feedflowdailyB1 <- seedwashdaily$PO1BFRSFFCPV
flocflowdailyB1 <- seedwashdaily$PO1FlocFTPV...149
cakewashdailyB1 <- seedwashdaily$PO1BFCaSFTPV
clothwashdailyB1 <- seedwashdaily$PO1BFCoSFTPV
sodafiltdailyB1 <- seedwashdaily$PO1BFiltATPV
oxfiltdailyB1 <- seedwashdaily$PO1AN.Ox...161


dailydataB1 <- data.frame(time = dailytimestep,           #day
                          status = statusdailyB1,
                          outputspo = outputspodaily,     #%
                          outputsoda = outputsodadaily,   #g/l
                          outputalumina = outputaluminadaily,#kl/h
                          throughput = throughputdaily,   #t/h
                          feedspo = spodaily,         #%
                          feedsoda = feedsodadaily,       #g/l
                          feeddensity = feeddensitydaily, #SG
                          drumspeed = drumspeeddailyB1,   #RPM
                          bathlevel = bathleveldailyB1,   #%
                          vacuum = vacuumdailyB1,         #kPa
                          feedflow = feedflowdailyB1,     #kl/h
                          flocflow = flocflowdailyB1,     #kl/h
                          cakewash = cakewashdailyB1,     #kl/h
                          clothwash = clothwashdailyB1,   #kl/h
                          sodafiltrate = sodafiltdailyB1, #g/l
                          oxfiltrate = oxfiltdailyB1)     #g/l


multipledailyB1 <- dailydataB1 %>%
  filter(status == 'On') %>%
  gather(type, data, outputspo:oxfiltrate)

ggplot(multipledailyB1, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ggtitle("Daily Filter 1B Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#observed against Output Soda Conc.
drumspeedB1 <- multipledailyB1 %>%
  filter(type == 'drumspeed')
ggplot(drumspeedB1, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Drum Speed (RPM)") +
  ggtitle("Daily Filter 1B Drum Speed")


bathlevelB1 <- multipledailyB1 %>%
  filter( type == 'bathlevel')
ggplot(bathlevelB1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) + 
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Bath Level (%)") +
  ggtitle("Daily Filter 1B Bath Level")


vacuumB1 <- multipledailyB1 %>%
  filter( type == 'vacuum')
ggplot(vacuumB1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Vacuum Pressure (kPa)") +
  ggtitle("Daily Filter 1B Vacuum Pressure")


feedflowB1 <- multipledailyB1 %>%
  filter( type == 'feedflow')
ggplot(feedflowB1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Feed Flow (kl/h)") +
  ggtitle("Daily Filter 1B Feed Flow")


flocflowB1 <- multipledailyB1 %>%
  filter(type == 'flocflow')
ggplot(flocflowB1, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Floc Flow (kl/h)") +
  ggtitle("Daily Filter 1B Floc Flow")


cakewashB1 <- multipledailyB1 %>%
  filter( type == 'cakewash')
ggplot(cakewashB1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Cake Wash Flow (kl/h)") +
  ggtitle("Daily Filter 1B Cake Wash Flow")


clothwashB1 <- multipledailyB1 %>%
  filter( type == 'clothwash')
ggplot(clothwashB1, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Cloth Wash Flow (kl/h)") +
  ggtitle("Daily Filter 1B Cloth Wash Flow")


sodafiltrateB1 <- multipledailyB1 %>%
  filter( type == 'sodafiltrate')
ggplot(sodafiltrateB1, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Filtrate Soda Conc. (g/l)") +
  ggtitle("Daily Filter 1B Filtrate Soda Conc.")


oxfiltrateB1 <- multipledailyB1 %>%
  filter( type == 'oxfiltrate')
ggplot(oxfiltrateB1, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Filtrate Oxalate (%)") +
  ggtitle("Daily Filter 1B Filtrate Oxalate")


#-------------------------------------------------------------------------------------------
#extract 2B data from daily observation
statusdailyB2 <- seedwashdaily$PO2BFOLXBDI
drumspeeddailyB2 <- seedwashdaily$PO2BFDrumSTPV
bathleveldailyB2 <- seedwashdaily$PO2BFBathLCPV
vacuumdailyB2 <- seedwashdaily$PO2BVacPCPV
feedflowdailyB2 <- seedwashdaily$PO2BFRSFFCPV
flocflowdailyB2 <- seedwashdaily$PO1FlocFTPV...171
cakewashdailyB2 <- seedwashdaily$PO2BFCaSFTPV
clothwashdailyB2 <- seedwashdaily$PO2BFCoSFTPV
sodafiltdailyB2 <- seedwashdaily$PO2BFiltATPV
oxfiltdailyB2 <- seedwashdaily$PO1AN.Ox...183


dailydataB2 <- data.frame(time = dailytimestep,           #day
                          status = statusdailyB2,
                          outputspo = outputspodaily,     #%
                          outputsoda = outputsodadaily,   #g/l
                          outputalumina = outputaluminadaily,#kl/h
                          throughput = throughputdaily,   #t/h
                          feedspo = spodaily,         #%
                          feedsoda = feedsodadaily,       #g/l
                          feeddensity = feeddensitydaily, #SG
                          drumspeed = drumspeeddailyB2,   #RPM
                          bathlevel = bathleveldailyB2,   #%
                          vacuum = vacuumdailyB2,         #kPa
                          feedflow = feedflowdailyB2,     #kl/h
                          flocflow = flocflowdailyB2,     #kl/h
                          cakewash = cakewashdailyB2,     #kl/h
                          clothwash = clothwashdailyB2,   #kl/h
                          sodafiltrate = sodafiltdailyB2, #g/l
                          oxfiltrate = oxfiltdailyB2)     #g/l


multipledailyB2 <- dailydataB2 %>%
  filter(status == 'On') %>%
  gather(type, data, outputspo:oxfiltrate)

ggplot(multipledailyB2, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ggtitle("Daily Filter 2B Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#observed against Output Soda Conc.
drumspeedB2 <- multipledailyB2 %>%
  filter(type == 'drumspeed')
ggplot(drumspeedB2, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Drum Speed (RPM)") +
  ggtitle("Daily Filter 2B Drum Speed")


bathlevelB2 <- multipledailyB2 %>%
  filter( type == 'bathlevel')
ggplot(bathlevelB2, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) + 
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Bath Level (%)") +
  ggtitle("Daily Filter 2B Bath Level")


vacuumB2 <- multipledailyB2 %>%
  filter(type == 'outpoutsoda'| type == 'vacuum')
ggplot(vacuumB2, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Vacuum Pressure (kPa)") +
  ggtitle("Daily Filter 2B Vacuum Pressure")


feedflowB2 <- multipledailyB2 %>%
  filter( type == 'feedflow')
ggplot(feedflowB2, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Feed Flow (kl/h)") +
  ggtitle("Daily Filter 2B Feed Flow")


flocflowB2 <- multipledailyB2 %>%
  filter(type == 'flocflow')
ggplot(flocflowB2, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Floc Flow (kl/h)") +
  ggtitle("Daily Filter 2B Floc Flow")


cakewashB2 <- multipledailyB2 %>%
  filter( type == 'cakewash')
ggplot(cakewashB2, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Cake Wash Flow (kl/h)") +
  ggtitle("Daily Filter 2B Cake Wash Flow")


clothwashB2 <- multipledailyB2 %>%
  filter( type == 'clothwash')
ggplot(clothwashB2, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Cloth Wash Flow (kl/h)") +
  ggtitle("Daily Filter 2B Cloth Wash Flow")


sodafiltrateB2 <- multipledailyB2 %>%
  filter( type == 'sodafiltrate')
ggplot(sodafiltrateB2, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Filtrate Soda Conc. (g/l)") +
  ggtitle("Daily Filter 2B Filtrate Soda Conc.")


oxfiltrateB2 <- multipledailyB2 %>%
  filter( type == 'oxfiltrate')
ggplot(oxfiltrateB2, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Filtrate Oxalate (%)") +
  ggtitle("Daily Filter 2B Filtrate Oxalate")


#------------------------------------------------------------------------------------------
#extract 3B data from daily observation
multipledailyB3 <- dailydataA4 %>%
  filter(status == 'Run' & config == '3B') %>%
  gather(type, data, outputspo:sodaconc)

ggplot(multipledailyB3, aes(x=time, y=data, color = type)) + geom_line() +
  xlab("Time (day)") + ggtitle("Daily Filter 3B Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#observed against Output Soda Conc.
drumspeedB3 <- multipledailyB3 %>%
  filter(type == 'drumspeed')
ggplot(drumspeedB3, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Drum Speed (RPM)") +
  ggtitle("Daily Filter 3B Drum Speed")


bathlevelB3 <- multipledailyB3 %>%
  filter( type == 'bathlevel')
ggplot(bathlevelB3, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) + 
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Bath Level (%)") +
  ggtitle("Daily Filter 3B Bath Level")


vacuumB3 <- multipledailyB3 %>%
  filter( type == 'vacuum')
ggplot(vacuumB3, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Vacuum Pressure (kPa)") +
  ggtitle("Daily Filter 3B Vacuum Pressure")


feedflowB3 <- multipledailyB3 %>%
  filter( type == 'feedflow')
ggplot(feedflowB3, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Feed Flow (kl/h)") +
  ggtitle("Daily Filter 3B Feed Flow")


flocflowB3 <- multipledailyB3 %>%
  filter(type == 'flocflow')
ggplot(flocflowB3, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Floc Flow (kl/h)") +
  ggtitle("Daily Filter 3B Floc Flow")


cakewashB3 <- multipledailyB3 %>%
  filter( type == 'cakewash')
ggplot(cakewashB3, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Cake Wash Flow (kl/h)") +
  ggtitle("Daily Filter 3B Cake Wash Flow")


clothwashB3 <- multipledailyB3 %>%
  filter( type == 'clothwash')
ggplot(clothwashB3, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Cloth Wash Flow (kl/h)") +
  ggtitle("Daily Filter 3B Cloth Wash Flow")


sodafiltrateB3 <- multipledailyB3 %>%
  filter( type == 'sodafiltrate')
ggplot(sodafiltrateB3, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Filtrate Soda Conc. (g/l)") +
  ggtitle("Daily Filter 3B Filtrate Soda Conc.")


oxfiltrateB3 <- multipledailyB3 %>%
  filter( type == 'oxfiltrate')
ggplot(oxfiltrateB3, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Filtrate Oxalate (%)") +
  ggtitle("Daily Filter 3B Filtrate Oxalate")



#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#extract feed data from 30mins observation (2016)
outputspo30min2016 <- seedwash30min2016$PONthTkAN.Ox
outputsoda30min2016 <- seedwash30min2016$POSOFATPV.S
outputalumina30min2016 <- seedwash30min2016$TotalFlow
throughput30min2016 <- seedwash30min2016$POFeedMQPV
spo30min2016 <- seedwash30min2016$POFeedAN.Ox
feedsoda30min2016 <- seedwash30min2016$POFeedAN.C
feeddensity30min2016 <- seedwash30min2016$POFeedDTPV

timestep30min2016 <- seq(616, 0, by = -0.5)

counterspo2016 <- 0
counterfeedsoda2016 <- 0
counteroutputspo2016 <- 0
modspo30min2016 <- length(spo30min2016)
modfeedsoda30min2016 <- length(feedsoda30min2016)
modoutputspo30min2016 <- length(outputspo30min2016)


for(i in length(spo30min2016):1){
  if (is.na(spo30min2016[i]) == FALSE){
    counterspo2016 <- spo30min2016[i]
    modspo30min2016[i] <- spo30min2016[i]
  }
  else if (is.na(spo30min2016[i]) == TRUE & counterspo2016 > 0){
    modspo30min2016[i] <- counterspo2016
  }
  else if (is.na(spo30min2016[i]) == TRUE){
    modspo30min2016[i] <- spo30min2016[i]
  }
  
  if (is.na(feedsoda30min2016[i]) == FALSE){
    counterfeedsoda2016 <- feedsoda30min2016[i]
    modfeedsoda30min2016[i] <- feedsoda30min2016[i]
  }
  else if (is.na(feedsoda30min2016[i]) == TRUE & counterfeedsoda2016 > 0){
    modfeedsoda30min2016[i] <- counterfeedsoda2016
  }
  else if (is.na(feedsoda30min2016[i]) == TRUE){
    modfeedsoda30min2016[i] <- feedsoda30min2016[i]
  }
  
  if (is.na(outputspo30min2016[i]) == FALSE){
    counteroutputspo2016 <- outputspo30min2016[i]
    modoutputspo30min2016[i] <- outputspo30min2016[i]
  }
  else if (is.na(outputspo30min2016[i]) == TRUE & counteroutputspo2016 > -0.1){
    modoutputspo30min2016[i] <- counteroutputspo2016
  }
  else if (is.na(outputspo30min2016[i]) == TRUE){
    modoutputspo30min2016[i] <- outputspo30min2016[i]
  }
}

data30min2016 <- data.frame(time = timestep30min2016,           #day
                            outputspo = outputspo30min2016,     #%
                            outputsoda = outputsoda30min2016,   #g/l
                            outputalumina = outputalumina30min2016, #kl/h
                            throughput = throughput30min2016,   #t/h
                            feedspo = spo30min2016,             #%
                            feedsoda = feedsoda30min2016,       #g/l
                            feeddensity = feeddensity30min2016) #SG


multiple30min2016 <- data30min2016 %>%
  gather(type, data, outputspo:feeddensity)

ggplot(multiple30min2016, aes(x=time, y=data, color = type)) + 
  geom_line(data = outputspomin2016[!is.na(outputspomin2016$data),],aes(x=time, y=data)) +
  geom_line(data=spomin2016[!is.na(spomin2016$data),],aes(x=time, y=data)) +
  geom_line(data=feedsodamin2016[!is.na(feedsodamin2016$data),], aes(x=time,y=data)) +
  geom_line(na.rm = TRUE) +
  xlab("Time (hour)") + ggtitle("2016 Feed Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#observed against Output Soda Conc.
outputspomin2016 <- multiple30min2016 %>%
  filter(type == 'outputspo')
ggplot(outputspomin2016) + geom_point(aes(x=time, y=data, color = type)) + 
  geom_line(data = outputspomin2016[!is.na(outputspomin2016$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Output SPO (%)") +
  ggtitle("2016 Output SPO")

outputsodamin2016 <- multiple30min2016 %>%
  filter(type == 'outputsoda')
ggplot(outputsodamin2016, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) +
  xlab("Time (hour)") + ylab("Output Soda Conc. (g/l)") +
  ggtitle("2016 Output Soda Conc.")

outputaluminamin2016 <- multiple30min2016 %>%
  filter(type == 'outputalumina')
ggplot(outputaluminamin2016, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) +
  xlab("Time (hour)") + ylab("Output Alumina (kl/h)") +
  ggtitle("2016 Output Alumina")

throughputmin2016 <- multiple30min2016 %>%
  filter( type == 'throughput')
ggplot(throughputmin2016, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Throughput (t/h)") +
  ggtitle("2016 Feed Throughput")


spomin2016 <- multiple30min2016 %>%
  filter( type == 'feedspo')
ggplot(spomin2016) + geom_point(aes(x=time, y=data, color = type)) + 
  geom_line(data=spomin2016[!is.na(spomin2016$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Solid Phase Oxalate (%)") +
  ggtitle("2016 Feed Solid Phase Oxalate")


feedsodamin2016 <- multiple30min2016 %>%
  filter( type == 'feedsoda')
ggplot(feedsodamin2016) + geom_point(aes(x=time, y=data, color = type)) + 
  geom_line(data=feedsodamin2016[!is.na(feedsodamin2016$data),], aes(x=time,y=data)) +
  xlab("Time (hour)") + ylab("Feed Soda Conc. (g/l)") +
  ggtitle("2016 Feed Soda Conc.")


feeddensitymin2016 <- multiple30min2016 %>%
  filter( type == 'feeddensity')
ggplot(feeddensitymin2016, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Feed Density (SG)") +
  ggtitle("2016 Feed Density")


#------------------------------------------------------------------------------------------------
#extract 1A data from 30mins observation
status30min2016A1 <- seedwash30min2016$PO1AFOLXBDI
drumspeed30min2016A1 <- seedwash30min2016$PO1AFDrumSTPV
bathlevel30min2016A1 <- seedwash30min2016$PO1ABathLCPV
vacuum30min2016A1 <- seedwash30min2016$PO1AVacPCPV
feedflow30min2016A1 <- seedwash30min2016$PO1FeedFCPV
flocflow30min2016A1 <- seedwash30min2016$PO1FlocFTPV...17
cakewash30min2016A1 <- seedwash30min2016$PO1ASprayFCPV
clothwash30min2016A1 <- seedwash30min2016$PO1AFCoSFTPV
sodafilt30min2016A1 <- seedwash30min2016$PO1AFltAN.C
oxfilt30min2016A1 <- seedwash30min2016$PO1AFltAN.Ox

#soda concentration in ton/hr
sodaconc30min2016A1 <- c(1:length(feedflow30min2016A1))
for (i in 1:length(sodaconc30min2016A1)){
  sodaconc30min2016A1[i] <- feedflow30min2016A1[i]*modfeedsoda30min2016[i]/1000
}

countersodafiltrate30min2016A1 <- 0
counteroxfiltrate30min2016A1 <- 0
modsodafiltrate30min2016A1 <- length(sodafilt30min2016A1)
modoxfiltrate30min2016A1 <- length(oxfilt30min2016A1)

for(i in length(sodafilt30min2016A1):1){
  if (is.na(sodafilt30min2016A1[i]) == FALSE){
    countersodafiltrate30min2016A1 <- sodafilt30min2016A1[i]
    modsodafiltrate30min2016A1[i] <- sodafilt30min2016A1[i]
  }
  else if (is.na(sodafilt30min2016A1[i]) == TRUE & countersodafiltrate30min2016A1 > 0){
    modsodafiltrate30min2016A1[i] <- countersodafiltrate30min2016A1
  }
  else if (is.na(sodafilt30min2016A1[i]) == TRUE){
    modsodafiltrate30min2016A1[i] <- sodafilt30min2016A1[i]
  }
  
  if (is.na(oxfilt30min2016A1[i]) == FALSE){
    counteroxfiltrate30min2016A1 <- oxfilt30min2016A1[i]
    modoxfiltrate30min2016A1[i] <- oxfilt30min2016A1[i]
  }
  else if (is.na(oxfilt30min2016A1[i]) == TRUE & counteroxfiltrate30min2016A1 > 0){
    modoxfiltrate30min2016A1[i] <- counteroxfiltrate30min2016A1
  }
  else if (is.na(oxfilt30min2016A1[i]) == TRUE){
    modoxfiltrate30min2016A1[i] <- oxfilt30min2016A1[i]
  }
}

data30min2016A1 <- data.frame(time = timestep30min2016,           #day
                              status = status30min2016A1,
                              outputspo = outputspo30min2016,     #%
                              outputsoda = outputsoda30min2016,   #g/l
                              outputalumina = outputalumina30min2016, #kl/h
                              throughput = throughput30min2016,   #t/h
                              feedspo = spo30min2016,      #%
                              feedsoda = feedsoda30min2016,    #g/l
                              feeddensity = feeddensity30min2016, #SG
                              drumspeed = drumspeed30min2016A1,   #RPM
                              bathlevel = bathlevel30min2016A1,   #%
                              vacuum = vacuum30min2016A1,         #kPa
                              feedflow = feedflow30min2016A1,     #kl/h
                              flocflow = flocflow30min2016A1,     #kl/h
                              cakewash = cakewash30min2016A1,     #kl/h
                              clothwash = clothwash30min2016A1,   #kl/h
                              sodafiltrate = sodafilt30min2016A1, #g/l
                              oxfiltrate = oxfilt30min2016A1,     #g/l
                              sodaconc = sodaconc30min2016A1)     #t/h


multiple30min2016A1 <- data30min2016A1 %>%
  filter(status == 'On') %>%
  gather(type, data, outputspo:sodaconc)

ggplot(multiple30min2016A1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  xlab("Time (hour)") + ggtitle("Filter 1A Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#observed against Output Soda Conc.
drumspeedmin2016A1 <- multiple30min2016A1 %>%
  filter(type == 'drumspeed')
ggplot(drumspeedmin2016A1, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Drum Speed (RPM)") +
  ggtitle("2016 Filter 1A Drum Speed")


bathlevelmin2016A1 <- multiple30min2016A1 %>%
  filter( type == 'bathlevel')
ggplot(bathlevelmin2016A1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) + 
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Bath Level (%)") +
  ggtitle("2016 Filter 1A Bath Level")


vacuummin2016A1 <- multiple30min2016A1 %>%
  filter( type == 'vacuum')
ggplot(vacuummin2016A1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") +
  ggtitle("2016 Filter 1A Vacuum Pressure")


feedflowmin2016A1 <- multiple30min2016A1 %>%
  filter( type == 'feedflow')
ggplot(feedflowmin2016A1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Feed Flow (kl/h)") +
  ggtitle("2016 Filter 1A Feed Flow")


flocflowmin2016A1 <- multiple30min2016A1 %>%
  filter(type == 'flocflow')
ggplot(flocflowmin2016A1, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Floc Flow (kl/h)") +
  ggtitle("2016 Filter 1A Floc Flow")


cakewashmin2016A1 <- multiple30min2016A1 %>%
  filter( type == 'cakewash')
ggplot(cakewashmin2016A1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") +
  ggtitle("2016 Filter 1A Cake Wash Flow")


clothwashmin2016A1 <- multiple30min2016A1 %>%
  filter( type == 'clothwash')
ggplot(clothwashmin2016A1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Cloth Wash Flow (kl/h)") +
  ggtitle("2016 Filter 1A Cloth Wash Flow")


sodafiltratemin2016A1 <- multiple30min2016A1 %>%
  filter( type == 'sodafiltrate')
ggplot(sodafiltratemin2016A1) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data = sodafiltratemin2016A1[!is.na(sodafiltratemin2016A1$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Filtrate Soda Conc. (g/l)") +
  ggtitle("2016 Filter 1A Filtrate Soda Conc.")


oxfiltratemin2016A1 <- multiple30min2016A1 %>%
  filter( type == 'oxfiltrate')
ggplot(oxfiltratemin2016A1) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data = oxfiltratemin2016A1[!is.na(oxfiltratemin2016A1$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Filtrate Oxalate (%)") +
  ggtitle("2016 Filter 1A Filtrate Oxalate")


sodaconcmin2016A1 <- multiple30min2016A1 %>%
  filter( type == 'sodaconc')
ggplot(sodaconcmin2016A1, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Inlet Soda (t/h)") +
  ggtitle("2016 Filter 1A Inlet Soda")


#-------------------------------------------------------------------------------------------
#extract 1B data from 30min observation
status30min2016B1 <- seedwash30min2016$PO1BFOLXBDI
drumspeed30min2016B1 <- seedwash30min2016$PO1BFDrumSTPV
bathlevel30min2016B1 <- seedwash30min2016$PO1BFBathLCPV
vacuum30min2016B1 <- seedwash30min2016$PO1BVacPCPV
feedflow30min2016B1 <- seedwash30min2016$PO1BFRSFFCPV
flocflow30min2016B1 <- seedwash30min2016$PO1FlocFTPV...148
cakewash30min2016B1 <- seedwash30min2016$PO1BFCaSFTPV
clothwash30min2016B1 <- seedwash30min2016$PO1BFCoSFTPV
sodafilt30min2016B1 <- seedwash30min2016$PO1BFiltATPV
oxfilt30min2016B1 <- seedwash30min2016$PO1AN.Ox...160

countersodafiltrate30min2016B1 <- 0
counteroxfiltrate30min2016B1 <- 0
modsodafiltrate30min2016B1 <- length(sodafilt30min2016B1)
modoxfiltrate30min2016B1 <- length(oxfilt30min2016B1)

for(i in length(sodafilt30min2016B1):1){
  if (is.na(sodafilt30min2016B1[i]) == FALSE){
    countersodafiltrate30min2016B1 <- sodafilt30min2016B1[i]
    modsodafiltrate30min2016B1[i] <- sodafilt30min2016B1[i]
  }
  else if (is.na(sodafilt30min2016B1[i]) == TRUE & countersodafiltrate30min2016B1 > 0){
    modsodafiltrate30min2016B1[i] <- countersodafiltrate30min2016B1
  }
  else if (is.na(sodafilt30min2016B1[i]) == TRUE){
    modsodafiltrate30min2016B1[i] <- sodafilt30min2016B1[i]
  }
  
  if (is.na(oxfilt30min2016B1[i]) == FALSE){
    counteroxfiltrate30min2016B1 <- oxfilt30min2016B1[i]
    modoxfiltrate30min2016B1[i] <- oxfilt30min2016B1[i]
  }
  else if (is.na(oxfilt30min2016B1[i]) == TRUE & counteroxfiltrate30min2016B1 > 0){
    modoxfiltrate30min2016B1[i] <- counteroxfiltrate30min2016B1
  }
  else if (is.na(oxfilt30min2016B1[i]) == TRUE){
    modoxfiltrate30min2016B1[i] <- oxfilt30min2016B1[i]
  }
}

data30min2016B1 <- data.frame(time = timestep30min2016,           #day
                              status = status30min2016B1,
                              outputspo = outputspo30min2016,  #%
                              outputsoda = outputsoda30min2016,   #g/l
                              outputalumina = outputalumina30min2016, #kl/h
                              throughput = throughput30min2016,   #t/h
                              feedspo = spo30min2016,      #%
                              feedsoda = feedsoda30min2016,    #g/l
                              feeddensity = feeddensity30min2016, #SG
                              drumspeed = drumspeed30min2016B1,   #RPM
                              bathlevel = bathlevel30min2016B1,   #%
                              vacuum = vacuum30min2016B1,         #kPa
                              feedflow = feedflow30min2016B1,     #kl/h
                              flocflow = flocflow30min2016B1,     #kl/h
                              cakewash = cakewash30min2016B1,     #kl/h
                              clothwash = clothwash30min2016B1,   #kl/h
                              sodafiltrate = sodafilt30min2016B1, #g/l
                              oxfiltrate = oxfilt30min2016B1)     #g/l


multiple30min2016B1 <- data30min2016B1 %>%
  filter(status == 'On') %>%
  gather(type, data, outputspo:oxfiltrate)

ggplot(multiple30min2016B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  xlab("Time (hour)") + ggtitle("2016 Filter 1B Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#observed against Output Soda Conc.
drumspeedmin2016B1 <- multiple30min2016B1 %>%
  filter(type == 'drumspeed')
ggplot(drumspeedmin2016B1, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Drum Speed (RPM)") +
  ggtitle("2016 Filter 1B Drum Speed")


bathlevelmin2016B1 <- multiple30min2016B1 %>%
  filter( type == 'bathlevel')
ggplot(bathlevelmin2016B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) + 
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Bath Level (%)") +
  ggtitle("2016 Filter 1B Bath Level")


vacuummin2016B1 <- multiple30min2016B1 %>%
  filter( type == 'vacuum')
ggplot(vacuummin2016B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") +
  ggtitle("2016 Filter 1B Vacuum Pressure")


feedflowmin2016B1 <- multiple30min2016B1 %>%
  filter( type == 'feedflow')
ggplot(feedflowmin2016B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Feed Flow (kl/h)") +
  ggtitle("2016 Filter 1B Feed Flow")


flocflowmin2016B1 <- multiple30min2016B1 %>%
  filter(type == 'flocflow')
ggplot(flocflowmin2016B1, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Floc Flow (kl/h)") +
  ggtitle("2016 Filter 1B Floc Flow")


cakewashmin2016B1 <- multiple30min2016B1 %>%
  filter( type == 'cakewash')
ggplot(cakewashmin2016B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") +
  ggtitle("2016 Filter 1B Cake Wash Flow")


clothwashmin2016B1 <- multiple30min2016B1 %>%
  filter( type == 'clothwash')
ggplot(clothwashmin2016B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Cloth Wash Flow (kl/h)") +
  ggtitle("2016 Filter 1B Cloth Wash Flow")


sodafiltratemin2016B1 <- multiple30min2016B1 %>%
  filter( type == 'sodafiltrate')
ggplot(sodafiltratemin2016B1) + geom_line(aes(x=time, y=data, color = type)) +
  geom_line(data = sodafiltratemin2016B1[!is.na(sodafiltratemin2016B1$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Filtrate Soda Conc. (g/l)") +
  ggtitle("2016 Filter 1B Filtrate Soda Conc.")


oxfiltratemin2016B1 <- multiple30min2016B1 %>%
  filter( type == 'oxfiltrate')
ggplot(oxfiltratemin2016B1) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data = oxfiltratemin2016B1[!is.na(oxfiltratemin2016B1$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Filtrate Oxalate (%)") +
  ggtitle("2016 Filter 1B Filtrate Oxalate")


#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#extract feed data from 30mins observation (2017)
outputspo30min2017 <- seedwash30min2017$PONthTkAN.Ox
outputsoda30min2017 <- seedwash30min2017$POSOFATPV.S
outputalumina30min2017 <- seedwash30min2017$TotalFlow
throughput30min2017 <- seedwash30min2017$POFeedMQPV
spo30min2017 <- seedwash30min2017$POFeedAN.Ox
feedsoda30min2017 <- seedwash30min2017$POFeedAN.C
feeddensity30min2017 <- seedwash30min2017$POFeedDTPV

timestep30min2017 <- seq(616, 0, by = -0.5)

counterspo2017 <- 0
counterfeedsoda2017 <- 0
counteroutputspo2017 <- 0
modspo30min2017 <- length(spo30min2017)
modfeedsoda30min2017 <- length(feedsoda30min2017)
modoutputspo30min2017 <- length(outputspo30min2017)


for(i in length(spo30min2017):1){
  if (is.na(spo30min2017[i]) == FALSE){
    counterspo2017 <- spo30min2017[i]
    modspo30min2017[i] <- spo30min2017[i]
  }
  else if (is.na(spo30min2017[i]) == TRUE & counterspo2017 > 0){
    modspo30min2017[i] <- counterspo2017
  }
  else if (is.na(spo30min2017[i]) == TRUE){
    modspo30min2017[i] <- spo30min2017[i]
  }
  
  if (is.na(feedsoda30min2017[i]) == FALSE){
    counterfeedsoda2017 <- feedsoda30min2017[i]
    modfeedsoda30min2017[i] <- feedsoda30min2017[i]
  }
  else if (is.na(feedsoda30min2017[i]) == TRUE & counterfeedsoda2017 > 0){
    modfeedsoda30min2017[i] <- counterfeedsoda2017
  }
  else if (is.na(feedsoda30min2017[i]) == TRUE){
    modfeedsoda30min2017[i] <- feedsoda30min2017[i]
  }
  
  if (is.na(outputspo30min2017[i]) == FALSE){
    counteroutputspo2017 <- outputspo30min2017[i]
    modoutputspo30min2017[i] <- outputspo30min2017[i]
  }
  else if (is.na(outputspo30min2017[i]) == TRUE & counteroutputspo2017 > -0.1){
    modoutputspo30min2017[i] <- counteroutputspo2017
  }
  else if (is.na(outputspo30min2017[i]) == TRUE){
    modoutputspo30min2017[i] <- outputspo30min2017[i]
  }
}

data30min2017 <- data.frame(time = timestep30min2017,           #day
                            outputspo = outputspo30min2017,  #%
                            outputsoda = outputsoda30min2017,   #g/l
                            outputalumina = outputalumina30min2017, #kl/h
                            throughput = throughput30min2017,   #t/h
                            feedspo = spo30min2017,          #%
                            feedsoda = feedsoda30min2017,    #g/l
                            feeddensity = feeddensity30min2017) #SG


multiple30min2017 <- data30min2017 %>%
  gather(type, data, outputspo:feeddensity)

ggplot(multiple30min2017, aes(x=time, y=data, color = type)) +
  geom_line(data = outputspomin2017[!is.na(outputspomin2017$data),],aes(x=time, y=data)) +
  geom_line(data = spomin2017[!is.na(spomin2017$data),],aes(x=time, y=data)) +
  geom_line(data = feedsodamin2017[!is.na(feedsodamin2017$data),],aes(x=time, y=data)) +
  geom_line(na.rm = TRUE) + 
  xlab("Time (hour)") + ggtitle("2017 Feed Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#observed against Output Soda Conc.
outputspomin2017 <- multiple30min2017 %>%
  filter(type == 'outputspo')
ggplot(outputspomin2017) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data = outputspomin2017[!is.na(outputspomin2017$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Output SPO (%)") +
  ggtitle("2017 Output SPO")

outputsodamin2017 <- multiple30min2017 %>%
  filter(type == 'outputsoda')
ggplot(outputsodamin2017, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) +
  xlab("Time (hour)") + ylab("Output Soda Conc. (g/l)") +
  ggtitle("2017 Output Soda Conc.")

outputaluminamin2017 <- multiple30min2017 %>%
  filter(type == 'outputalumina')
ggplot(outputsodamin2017, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) +
  xlab("Time (hour)") + ylab("Output Alumina (kl/h)") +
  ggtitle("2017 Output Alumina")

throughputmin2017 <- multiple30min2017 %>%
  filter( type == 'throughput')
ggplot(throughputmin2017, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Throughput (t/h)") +
  ggtitle("2017 Feed Throughput")


spomin2017 <- multiple30min2017 %>%
  filter( type == 'feedspo')
ggplot(spomin2017) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data = spomin2017[!is.na(spomin2017$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Solid Phase Oxalate (%)") +
  ggtitle("2017 Feed Solid Phase Oxalate")


feedsodamin2017 <- multiple30min2017 %>%
  filter( type == 'feedsoda')
ggplot(feedsodamin2017) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data = feedsodamin2017[!is.na(feedsodamin2017$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Feed Soda Conc. (g/l)") +
  ggtitle("2017 Feed Soda Conc.")


feeddensitymin2017 <- multiple30min2017 %>%
  filter( type == 'feeddensity')
ggplot(feeddensitymin2017, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Feed Density (SG)") +
  ggtitle("2017 Feed Density")


#------------------------------------------------------------------------------------------------
#extract 1A data from 30mins observation
status30min2017A1 <- seedwash30min2017$PO1AFOLXBDI
drumspeed30min2017A1 <- seedwash30min2017$PO1AFDrumSTPV
bathlevel30min2017A1 <- seedwash30min2017$PO1ABathLCPV
vacuum30min2017A1 <- seedwash30min2017$PO1AVacPCPV
feedflow30min2017A1 <- seedwash30min2017$PO1FeedFCPV
flocflow30min2017A1 <- seedwash30min2017$PO1FlocFTPV...17
cakewash30min2017A1 <- seedwash30min2017$PO1ASprayFCPV
clothwash30min2017A1 <- seedwash30min2017$PO1AFCoSFTPV
sodafilt30min2017A1 <- seedwash30min2017$PO1AFltAN.C
oxfilt30min2017A1 <- seedwash30min2017$PO1AFltAN.Ox

#soda concentration in ton/hr
sodaconc30min2017A1 <- c(1:length(feedflow30min2017A1))
for (i in 1:length(sodaconc30min2017A1)){
  sodaconc30min2017A1[i] <- feedflow30min2017A1[i]*modfeedsoda30min2017[i]/1000
}

countersodafiltrate30min2017A1 <- 0
counteroxfiltrate30min2017A1 <- 0
modsodafiltrate30min2017A1 <- length(sodafilt30min2017A1)
modoxfiltrate30min2017A1 <- length(oxfilt30min2017A1)

for(i in length(sodafilt30min2017A1):1){
  if (is.na(sodafilt30min2017A1[i]) == FALSE){
    countersodafiltrate30min2017A1 <- sodafilt30min2017A1[i]
    modsodafiltrate30min2017A1[i] <- sodafilt30min2017A1[i]
  }
  else if (is.na(sodafilt30min2017A1[i]) == TRUE & countersodafiltrate30min2017A1 > 0){
    modsodafiltrate30min2017A1[i] <- countersodafiltrate30min2017A1
  }
  else if (is.na(sodafilt30min2017A1[i]) == TRUE){
    modsodafiltrate30min2017A1[i] <- sodafilt30min2017A1[i]
  }
  
  if (is.na(oxfilt30min2017A1[i]) == FALSE){
    counteroxfiltrate30min2017A1 <- oxfilt30min2017A1[i]
    modoxfiltrate30min2017A1[i] <- oxfilt30min2017A1[i]
  }
  else if (is.na(oxfilt30min2017A1[i]) == TRUE & counteroxfiltrate30min2017A1 > 0){
    modoxfiltrate30min2017A1[i] <- counteroxfiltrate30min2017A1
  }
  else if (is.na(oxfilt30min2017A1[i]) == TRUE){
    modoxfiltrate30min2017A1[i] <- oxfilt30min2017A1[i]
  }
}

data30min2017A1 <- data.frame(time = timestep30min2017,           #day
                              status = status30min2017A1,
                              outputspo = outputspo30min2017,     #%
                              outputsoda = outputsoda30min2017,   #g/l
                              outputalumina = outputalumina30min2017, #kl/h
                              throughput = throughput30min2017,   #t/h
                              feedspo = spo30min2017,      #%
                              feedsoda = feedsoda30min2017,    #g/l
                              feeddensity = feeddensity30min2017, #SG
                              drumspeed = drumspeed30min2017A1,   #RPM
                              bathlevel = bathlevel30min2017A1,   #%
                              vacuum = vacuum30min2017A1,         #kPa
                              feedflow = feedflow30min2017A1,     #kl/h
                              flocflow = flocflow30min2017A1,     #kl/h
                              cakewash = cakewash30min2017A1,     #kl/h
                              clothwash = clothwash30min2017A1,   #kl/h
                              sodafiltrate = sodafilt30min2017A1, #g/l
                              oxfiltrate = oxfilt30min2017A1,     #g/l
                              sodaconc = sodaconc30min2017A1)     #t/h


multiple30min2017A1 <- data30min2017A1 %>%
  filter(status == 'On') %>%
  gather(type, data, outputspo:sodaconc)

ggplot(multiple30min2017A1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  xlab("Time (hour)") + ggtitle("Filter 1A Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#observed against Output Soda Conc.
drumspeedmin2017A1 <- multiple30min2017A1 %>%
  filter(type == 'drumspeed')
ggplot(drumspeedmin2017A1, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Drum Speed (RPM)") +
  ggtitle("2017 Filter 1A Drum Speed")


bathlevelmin2017A1 <- multiple30min2017A1 %>%
  filter( type == 'bathlevel')
ggplot(bathlevelmin2017A1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) + 
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Bath Level (%)") +
  ggtitle("2017 Filter 1A Bath Level")


vacuummin2017A1 <- multiple30min2017A1 %>%
  filter( type == 'vacuum')
ggplot(vacuummin2017A1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") +
  ggtitle("2017 Filter 1A Vacuum Pressure")


feedflowmin2017A1 <- multiple30min2017A1 %>%
  filter( type == 'feedflow')
ggplot(feedflowmin2017A1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Feed Flow (kl/h)") +
  ggtitle("2017 Filter 1A Feed Flow")


flocflowmin2017A1 <- multiple30min2017A1 %>%
  filter(type == 'flocflow')
ggplot(flocflowmin2017A1, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Floc Flow (kl/h)") +
  ggtitle("2017 Filter 1A Floc Flow")


cakewashmin2017A1 <- multiple30min2017A1 %>%
  filter( type == 'cakewash')
ggplot(cakewashmin2017A1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") +
  ggtitle("2017 Filter 1A Cake Wash Flow")


clothwashmin2017A1 <- multiple30min2017A1 %>%
  filter( type == 'clothwash')
ggplot(clothwashmin2017A1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Cloth Wash Flow (kl/h)") +
  ggtitle("2017 Filter 1A Cloth Wash Flow")


sodafiltratemin2017A1 <- multiple30min2017A1 %>%
  filter( type == 'sodafiltrate')
ggplot(sodafiltratemin2017A1) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data=sodafiltratemin2017A1[!is.na(sodafiltratemin2017A1$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Filtrate Soda Conc. (g/l)") +
  ggtitle("2017 Filter 1A Filtrate Soda Conc.")


oxfiltratemin2017A1 <- multiple30min2017A1 %>%
  filter( type == 'oxfiltrate')
ggplot(oxfiltratemin2017A1) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data = oxfiltratemin2017A1[!is.na(oxfiltratemin2017A1$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Filtrate Oxalate (%)") +
  ggtitle("2017 Filter 1A Filtrate Oxalate")


sodaconcmin2017A1 <- multiple30min2017A1 %>%
  filter( type == 'sodaconc')
ggplot(sodaconcmin2017A1, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Inlet Soda (t/h)") +
  ggtitle("2017 Filter 1A Inlet Soda")


#-------------------------------------------------------------------------------------------
#extract 1B data from 30min observation
status30min2017B1 <- seedwash30min2017$PO1BFOLXBDI
drumspeed30min2017B1 <- seedwash30min2017$PO1BFDrumSTPV
bathlevel30min2017B1 <- seedwash30min2017$PO1BFBathLCPV
vacuum30min2017B1 <- seedwash30min2017$PO1BVacPCPV
feedflow30min2017B1 <- seedwash30min2017$PO1BFRSFFCPV
flocflow30min2017B1 <- seedwash30min2017$PO1FlocFTPV...148
cakewash30min2017B1 <- seedwash30min2017$PO1BFCaSFTPV
clothwash30min2017B1 <- seedwash30min2017$PO1BFCoSFTPV
sodafilt30min2017B1 <- seedwash30min2017$PO1BFiltATPV
oxfilt30min2017B1 <- seedwash30min2017$PO1AN.Ox...160

countersodafiltrate30min2017B1 <- 0
counteroxfiltrate30min2017B1 <- 0
modsodafiltrate30min2017B1 <- length(sodafilt30min2017B1)
modoxfiltrate30min2017B1 <- length(oxfilt30min2017B1)

for(i in length(sodafilt30min2017B1):1){
  if (is.na(sodafilt30min2017B1[i]) == FALSE){
    countersodafiltrate30min2017B1 <- sodafilt30min2017B1[i]
    modsodafiltrate30min2017B1[i] <- sodafilt30min2017B1[i]
  }
  else if (is.na(sodafilt30min2017B1[i]) == TRUE & countersodafiltrate30min2017B1 > 0){
    modsodafiltrate30min2017B1[i] <- countersodafiltrate30min2017B1
  }
  else if (is.na(sodafilt30min2017B1[i]) == TRUE){
    modsodafiltrate30min2017B1[i] <- sodafilt30min2017B1[i]
  }
  
  if (is.na(oxfilt30min2017B1[i]) == FALSE){
    counteroxfiltrate30min2017B1 <- oxfilt30min2017B1[i]
    modoxfiltrate30min2017B1[i] <- oxfilt30min2017B1[i]
  }
  else if (is.na(oxfilt30min2017B1[i]) == TRUE & counteroxfiltrate30min2017B1 > 0){
    modoxfiltrate30min2017B1[i] <- counteroxfiltrate30min2017B1
  }
  else if (is.na(oxfilt30min2017B1[i]) == TRUE){
    modoxfiltrate30min2017B1[i] <- oxfilt30min2017B1[i]
  }
}

data30min2017B1 <- data.frame(time = timestep30min2017,           #day
                              status = status30min2017B1,
                              outputspo = outputspo30min2017,  #%
                              outputsoda = outputsoda30min2017,   #g/l
                              outputalumina = outputalumina30min2017, #kl/h
                              throughput = throughput30min2017,   #t/h
                              feedspo = spo30min2017,      #%
                              feedsoda = feedsoda30min2017,    #g/l
                              feeddensity = feeddensity30min2017, #SG
                              drumspeed = drumspeed30min2017B1,   #RPM
                              bathlevel = bathlevel30min2017B1,   #%
                              vacuum = vacuum30min2017B1,         #kPa
                              feedflow = feedflow30min2017B1,     #kl/h
                              flocflow = flocflow30min2017B1,     #kl/h
                              cakewash = cakewash30min2017B1,     #kl/h
                              clothwash = clothwash30min2017B1,   #kl/h
                              sodafiltrate = sodafilt30min2017B1, #g/l
                              oxfiltrate = oxfilt30min2017B1)     #g/l


multiple30min2017B1 <- data30min2017B1 %>%
  filter(status == 'On') %>%
  gather(type, data, outputspo:oxfiltrate)

ggplot(multiple30min2017B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  xlab("Time (hour)") + ggtitle("2017 Filter 1B Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#observed against Output Soda Conc.
drumspeedmin2017B1 <- multiple30min2017B1 %>%
  filter(type == 'drumspeed')
ggplot(drumspeedmin2017B1, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Drum Speed (RPM)") +
  ggtitle("2017 Filter 1B Drum Speed")


bathlevelmin2017B1 <- multiple30min2017B1 %>%
  filter( type == 'bathlevel')
ggplot(bathlevelmin2017B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) + 
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Bath Level (%)") +
  ggtitle("2017 Filter 1B Bath Level")


vacuummin2017B1 <- multiple30min2017B1 %>%
  filter( type == 'vacuum')
ggplot(vacuummin2017B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") +
  ggtitle("2017 Filter 1B Vacuum Pressure")


feedflowmin2017B1 <- multiple30min2017B1 %>%
  filter( type == 'feedflow')
ggplot(feedflowmin2017B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Feed Flow (kl/h)") +
  ggtitle("2017 Filter 1B Feed Flow")


flocflowmin2017B1 <- multiple30min2017B1 %>%
  filter(type == 'flocflow')
ggplot(flocflowmin2017B1, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Floc Flow (kl/h)") +
  ggtitle("2017 Filter 1B Floc Flow")


cakewashmin2017B1 <- multiple30min2017B1 %>%
  filter( type == 'cakewash')
ggplot(cakewashmin2017B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") +
  ggtitle("2017 Filter 1B Cake Wash Flow")


clothwashmin2017B1 <- multiple30min2017B1 %>%
  filter( type == 'clothwash')
ggplot(clothwashmin2017B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Cloth Wash Flow (kl/h)") +
  ggtitle("2017 Filter 1B Cloth Wash Flow")


sodafiltratemin2017B1 <- multiple30min2017B1 %>%
  filter( type == 'sodafiltrate')
ggplot(sodafiltratemin2017B1, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Filtrate Soda Conc. (g/l)") +
  ggtitle("2017 Filter 1B Filtrate Soda Conc.")


oxfiltratemin2017B1 <- multiple30min2017B1 %>%
  filter( type == 'oxfiltrate')
ggplot(oxfiltratemin2017B1) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data = oxfiltratemin2017B1[!is.na(oxfiltratemin2017B1$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Filtrate Oxalate (%)") +
  ggtitle("2017 Filter 1B Filtrate Oxalate")


#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#extract feed data from 30mins observation (2018)
outputspo30min2018 <- seedwash30min2018$PONthTkAN.Ox
outputsoda30min2018 <- seedwash30min2018$POSOFATPV.S
outputalumina30min2018 <- seedwash30min2018$TotalFlow
throughput30min2018 <- seedwash30min2018$POFeedMQPV
spo30min2018 <- seedwash30min2018$POFeedAN.Ox
feedsoda30min2018 <- seedwash30min2018$POFeedAN.C
feeddensity30min2018 <- seedwash30min2018$POFeedDTPV

timestep30min2018 <- seq(616, 0, by = -0.5)

counterspo2018 <- 0
counterfeedsoda2018 <- 0
counteroutputspo2018 <- 0
modspo30min2018 <- length(spo30min2018)
modfeedsoda30min2018 <- length(feedsoda30min2018)
modoutputspo30min2018 <- length(outputspo30min2018)

for(i in length(spo30min2018):1){
  if (is.na(spo30min2018[i]) == FALSE){
    counterspo2018 <- spo30min2018[i]
    modspo30min2018[i] <- spo30min2018[i]
  }
  else if (is.na(spo30min2018[i]) == TRUE & counterspo2018 > 0){
    modspo30min2018[i] <- counterspo2018
  }
  else if (is.na(spo30min2018[i]) == TRUE){
    modspo30min2018[i] <- spo30min2018[i]
  }
  
  if (is.na(feedsoda30min2018[i]) == FALSE){
    counterfeedsoda2018 <- feedsoda30min2018[i]
    modfeedsoda30min2018[i] <- feedsoda30min2018[i]
  }
  else if (is.na(feedsoda30min2018[i]) == TRUE & counterfeedsoda2018 > 0){
    modfeedsoda30min2018[i] <- counterfeedsoda2018
  }
  else if (is.na(feedsoda30min2018[i]) == TRUE){
    modfeedsoda30min2018[i] <- feedsoda30min2018[i]
  }
  
  if (is.na(outputspo30min2018[i]) == FALSE){
    counteroutputspo2018 <- outputspo30min2018[i]
    modoutputspo30min2018[i] <- outputspo30min2018[i]
  }
  else if (is.na(outputspo30min2018[i]) == TRUE & counteroutputspo2018 > -0.1){
    modoutputspo30min2018[i] <- counteroutputspo2018
  }
  else if (is.na(outputspo30min2018[i]) == TRUE){
    modoutputspo30min2018[i] <- outputspo30min2018[i]
  }
}

data30min2018 <- data.frame(time = timestep30min2018,           #day
                            outputspo = outputspo30min2018,  #%
                            outputsoda = outputsoda30min2018,   #g/l
                            outputalumina = outputalumina30min2018,#kl/h
                            throughput = throughput30min2018,   #t/h
                            feedspo = spo30min2018,          #%
                            feedsoda = feedsoda30min2018,    #g/l
                            feeddensity = feeddensity30min2018) #SG


multiple30min2018 <- data30min2018 %>%
  gather(type, data, outputspo:feeddensity)

ggplot(multiple30min2018, aes(x=time, y=data, color = type)) +
  geom_line(data = outputspomin2018[!is.na(outputspomin2018$data),],aes(x=time, y=data)) +
  geom_line(data = spomin2018[!is.na(spomin2018$data),],aes(x=time, y=data)) +
  geom_line(data = feedsodamin2018[!is.na(feedsodamin2018$data),],aes(x=time, y=data)) +
  geom_line(na.rm = TRUE) + 
  xlab("Time (hour)") + ggtitle("2018 Feed Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#observed against Output Soda Conc.
outputspomin2018 <- multiple30min2018 %>%
  filter(type == 'outputspo')
ggplot(outputspomin2018) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data = outputspomin2018[!is.na(outputspomin2018$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Output SPO (%)") +
  ggtitle("2018 Output SPO")

outputsodamin2018 <- multiple30min2018 %>%
  filter(type == 'outputsoda')
ggplot(outputsodamin2018, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) +
  xlab("Time (hour)") + ylab("Output Soda Conc. (g/l)") +
  ggtitle("2018 Output Soda Conc.")

outputaluminamin2018 <- multiple30min2018 %>%
  filter(type == 'outputalumina')
ggplot(outputsodamin2018, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) +
  xlab("Time (hour)") + ylab("Output Alumina (kl/h)") +
  ggtitle("2018 Output Alumina")

throughputmin2018 <- multiple30min2018 %>%
  filter( type == 'throughput')
ggplot(throughputmin2018, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Throughput (t/h)") +
  ggtitle("2018 Feed Throughput")


spomin2018 <- multiple30min2018 %>%
  filter( type == 'feedspo')
ggplot(spomin2018) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data = spomin2018[!is.na(spomin2018$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Solid Phase Oxalate (%)") +
  ggtitle("2018 Feed Solid Phase Oxalate")


feedsodamin2018 <- multiple30min2018 %>%
  filter( type == 'feedsoda')
ggplot(feedsodamin2018) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data = feedsodamin2018[!is.na(feedsodamin2018$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Feed Soda Conc. (g/l)") +
  ggtitle("2018 Feed Soda Conc.")


feeddensitymin2018 <- multiple30min2018 %>%
  filter( type == 'feeddensity')
ggplot(feeddensitymin2018, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Feed Density (SG)") +
  ggtitle("2018 Feed Density")


#------------------------------------------------------------------------------------------------
#extract 1A data from 30mins observation
status30min2018A1 <- seedwash30min2018$PO1AFOLXBDI
drumspeed30min2018A1 <- seedwash30min2018$PO1AFDrumSTPV
bathlevel30min2018A1 <- seedwash30min2018$PO1ABathLCPV
vacuum30min2018A1 <- seedwash30min2018$PO1AVacPCPV
feedflow30min2018A1 <- seedwash30min2018$PO1FeedFCPV
flocflow30min2018A1 <- seedwash30min2018$PO1FlocFTPV...17
cakewash30min2018A1 <- seedwash30min2018$PO1ASprayFCPV
clothwash30min2018A1 <- seedwash30min2018$PO1AFCoSFTPV
sodafilt30min2018A1 <- seedwash30min2018$PO1AFltAN.C
oxfilt30min2018A1 <- seedwash30min2018$PO1AFltAN.Ox

#soda concentration in ton/hr
sodaconc30min2018A1 <- c(1:length(feedflow30min2018A1))
for (i in 1:length(sodaconc30min2018A1)){
  sodaconc30min2018A1[i] <- feedflow30min2018A1[i]*modfeedsoda30min2018[i]/1000
}

countersodafiltrate30min2018A1 <- 0
counteroxfiltrate30min2018A1 <- 0
modsodafiltrate30min2018A1 <- length(sodafilt30min2018A1)
modoxfiltrate30min2018A1 <- length(oxfilt30min2018A1)

for(i in length(sodafilt30min2018A1):1){
  if (is.na(sodafilt30min2018A1[i]) == FALSE){
    countersodafiltrate30min2018A1 <- sodafilt30min2018A1[i]
    modsodafiltrate30min2018A1[i] <- sodafilt30min2018A1[i]
  }
  else if (is.na(sodafilt30min2018A1[i]) == TRUE & countersodafiltrate30min2018A1 > 0){
    modsodafiltrate30min2018A1[i] <- countersodafiltrate30min2018A1
  }
  else if (is.na(sodafilt30min2018A1[i]) == TRUE){
    modsodafiltrate30min2018A1[i] <- sodafilt30min2018A1[i]
  }
  
  if (is.na(oxfilt30min2018A1[i]) == FALSE){
    counteroxfiltrate30min2018A1 <- oxfilt30min2018A1[i]
    modoxfiltrate30min2018A1[i] <- oxfilt30min2018A1[i]
  }
  else if (is.na(oxfilt30min2018A1[i]) == TRUE & counteroxfiltrate30min2018A1 > 0){
    modoxfiltrate30min2018A1[i] <- counteroxfiltrate30min2018A1
  }
  else if (is.na(oxfilt30min2018A1[i]) == TRUE){
    modoxfiltrate30min2018A1[i] <- oxfilt30min2018A1[i]
  }
}

data30min2018A1 <- data.frame(time = timestep30min2018,           #day
                              status = status30min2018A1,
                              outputspo = outputspo30min2018,     #%
                              outputsoda = outputsoda30min2018,   #g/l
                              outputalumina = outputalumina30min2018, #kl/h
                              throughput = throughput30min2018,   #t/h
                              feedspo = spo30min2018,      #%
                              feedsoda = feedsoda30min2018,    #g/l
                              feeddensity = feeddensity30min2018, #SG
                              drumspeed = drumspeed30min2018A1,   #RPM
                              bathlevel = bathlevel30min2018A1,   #%
                              vacuum = vacuum30min2018A1,         #kPa
                              feedflow = feedflow30min2018A1,     #kl/h
                              flocflow = flocflow30min2018A1,     #kl/h
                              cakewash = cakewash30min2018A1,     #kl/h
                              clothwash = clothwash30min2018A1,   #kl/h
                              sodafiltrate = sodafilt30min2018A1, #g/l
                              oxfiltrate = oxfilt30min2018A1,     #g/l
                              sodaconc = sodaconc30min2018A1)     #t/h


multiple30min2018A1 <- data30min2018A1 %>%
  filter(status == 'On') %>%
  gather(type, data, outputspo:sodaconc)

ggplot(multiple30min2018A1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  xlab("Time (hour)") + ggtitle("Filter 1A Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#observed against Output Soda Conc.
drumspeedmin2018A1 <- multiple30min2018A1 %>%
  filter(type == 'drumspeed')
ggplot(drumspeedmin2018A1, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Drum Speed (RPM)") +
  ggtitle("2018 Filter 1A Drum Speed")


bathlevelmin2018A1 <- multiple30min2018A1 %>%
  filter( type == 'bathlevel')
ggplot(bathlevelmin2018A1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) + 
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Bath Level (%)") +
  ggtitle("2018 Filter 1A Bath Level")


vacuummin2018A1 <- multiple30min2018A1 %>%
  filter( type == 'vacuum')
ggplot(vacuummin2018A1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") +
  ggtitle("2018 Filter 1A Vacuum Pressure")


feedflowmin2018A1 <- multiple30min2018A1 %>%
  filter( type == 'feedflow')
ggplot(feedflowmin2018A1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Feed Flow (kl/h)") +
  ggtitle("2018 Filter 1A Feed Flow")


flocflowmin2018A1 <- multiple30min2018A1 %>%
  filter(type == 'flocflow')
ggplot(flocflowmin2018A1, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Floc Flow (kl/h)") +
  ggtitle("2018 Filter 1A Floc Flow")


cakewashmin2018A1 <- multiple30min2018A1 %>%
  filter( type == 'cakewash')
ggplot(cakewashmin2018A1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") +
  ggtitle("2018 Filter 1A Cake Wash Flow")


clothwashmin2018A1 <- multiple30min2018A1 %>%
  filter( type == 'clothwash')
ggplot(clothwashmin2018A1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Cloth Wash Flow (kl/h)") +
  ggtitle("2018 Filter 1A Cloth Wash Flow")


sodafiltratemin2018A1 <- multiple30min2018A1 %>%
  filter( type == 'sodafiltrate')
ggplot(sodafiltratemin2018A1) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data=sodafiltratemin2018A1[!is.na(sodafiltratemin2018A1$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Filtrate Soda Conc. (g/l)") +
  ggtitle("2018 Filter 1A Filtrate Soda Conc.")


oxfiltratemin2018A1 <- multiple30min2018A1 %>%
  filter( type == 'oxfiltrate')
ggplot(oxfiltratemin2018A1) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data = oxfiltratemin2018A1[!is.na(oxfiltratemin2018A1$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Filtrate Oxalate (%)") +
  ggtitle("2018 Filter 1A Filtrate Oxalate")


sodaconcmin2018A1 <- multiple30min2018A1 %>%
  filter( type == 'sodaconc')
ggplot(sodaconcmin2018A1, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Inlet Soda (t/h)") +
  ggtitle("2018 Filter 1A Inlet Soda")


#-------------------------------------------------------------------------------------------
#extract 1B data from 30min observation
status30min2018B1 <- seedwash30min2018$PO1BFOLXBDI
drumspeed30min2018B1 <- seedwash30min2018$PO1BFDrumSTPV
bathlevel30min2018B1 <- seedwash30min2018$PO1BFBathLCPV
vacuum30min2018B1 <- seedwash30min2018$PO1BVacPCPV
feedflow30min2018B1 <- seedwash30min2018$PO1BFRSFFCPV
flocflow30min2018B1 <- seedwash30min2018$PO1FlocFTPV...148
cakewash30min2018B1 <- seedwash30min2018$PO1BFCaSFTPV
clothwash30min2018B1 <- seedwash30min2018$PO1BFCoSFTPV
sodafilt30min2018B1 <- seedwash30min2018$PO1BFiltATPV
oxfilt30min2018B1 <- seedwash30min2018$PO1AN.Ox...160

countersodafiltrate30min2018B1 <- 0
counteroxfiltrate30min2018B1 <- 0
modsodafiltrate30min2018B1 <- length(sodafilt30min2018B1)
modoxfiltrate30min2018B1 <- length(oxfilt30min2018B1)

for(i in length(sodafilt30min2018B1):1){
  if (is.na(sodafilt30min2018B1[i]) == FALSE){
    countersodafiltrate30min2018B1 <- sodafilt30min2018B1[i]
    modsodafiltrate30min2018B1[i] <- sodafilt30min2018B1[i]
  }
  else if (is.na(sodafilt30min2018B1[i]) == TRUE & countersodafiltrate30min2018B1 > 0){
    modsodafiltrate30min2018B1[i] <- countersodafiltrate30min2018B1
  }
  else if (is.na(sodafilt30min2018B1[i]) == TRUE){
    modsodafiltrate30min2018B1[i] <- sodafilt30min2018B1[i]
  }
  
  if (is.na(oxfilt30min2018B1[i]) == FALSE){
    counteroxfiltrate30min2018B1 <- oxfilt30min2018B1[i]
    modoxfiltrate30min2018B1[i] <- oxfilt30min2018B1[i]
  }
  else if (is.na(oxfilt30min2018B1[i]) == TRUE & counteroxfiltrate30min2018B1 > 0){
    modoxfiltrate30min2018B1[i] <- counteroxfiltrate30min2018B1
  }
  else if (is.na(oxfilt30min2018B1[i]) == TRUE){
    modoxfiltrate30min2018B1[i] <- oxfilt30min2018B1[i]
  }
}

data30min2018B1 <- data.frame(time = timestep30min2018,           #day
                              status = status30min2018B1,
                              outputspo = outputspo30min2018,  #%
                              outputsoda = outputsoda30min2018,   #g/l
                              outputalumina = outputalumina30min2018, #kl/h
                              throughput = throughput30min2018,   #t/h
                              feedspo = spo30min2018,      #%
                              feedsoda = feedsoda30min2018,    #g/l
                              feeddensity = feeddensity30min2018, #SG
                              drumspeed = drumspeed30min2018B1,   #RPM
                              bathlevel = bathlevel30min2018B1,   #%
                              vacuum = vacuum30min2018B1,         #kPa
                              feedflow = feedflow30min2018B1,     #kl/h
                              flocflow = flocflow30min2018B1,     #kl/h
                              cakewash = cakewash30min2018B1,     #kl/h
                              clothwash = clothwash30min2018B1,   #kl/h
                              sodafiltrate = sodafilt30min2018B1, #g/l
                              oxfiltrate = oxfilt30min2018B1)     #g/l


multiple30min2018B1 <- data30min2018B1 %>%
  filter(status == 'On') %>%
  gather(type, data, outputspo:oxfiltrate)

ggplot(multiple30min2018B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  xlab("Time (hour)") + ggtitle("2018 Filter 1B Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#observed against Output Soda Conc.
drumspeedmin2018B1 <- multiple30min2018B1 %>%
  filter(type == 'drumspeed')
ggplot(drumspeedmin2018B1, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Drum Speed (RPM)") +
  ggtitle("2018 Filter 1B Drum Speed")


bathlevelmin2018B1 <- multiple30min2018B1 %>%
  filter( type == 'bathlevel')
ggplot(bathlevelmin2018B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) + 
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Bath Level (%)") +
  ggtitle("2018 Filter 1B Bath Level")


vacuummin2018B1 <- multiple30min2018B1 %>%
  filter( type == 'vacuum')
ggplot(vacuummin2018B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") +
  ggtitle("2018 Filter 1B Vacuum Pressure")


feedflowmin2018B1 <- multiple30min2018B1 %>%
  filter( type == 'feedflow')
ggplot(feedflowmin2018B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Feed Flow (kl/h)") +
  ggtitle("2018 Filter 1B Feed Flow")


flocflowmin2018B1 <- multiple30min2018B1 %>%
  filter(type == 'flocflow')
ggplot(flocflowmin2018B1, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Floc Flow (kl/h)") +
  ggtitle("2018 Filter 1B Floc Flow")


cakewashmin2018B1 <- multiple30min2018B1 %>%
  filter( type == 'cakewash')
ggplot(cakewashmin2018B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") +
  ggtitle("2018 Filter 1B Cake Wash Flow")


clothwashmin2018B1 <- multiple30min2018B1 %>%
  filter( type == 'clothwash')
ggplot(clothwashmin2018B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Cloth Wash Flow (kl/h)") +
  ggtitle("2018 Filter 1B Cloth Wash Flow")


sodafiltratemin2018B1 <- multiple30min2018B1 %>%
  filter( type == 'sodafiltrate')
ggplot(sodafiltratemin2018B1, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Filtrate Soda Conc. (g/l)") +
  ggtitle("2018 Filter 1B Filtrate Soda Conc.")


oxfiltratemin2018B1 <- multiple30min2018B1 %>%
  filter( type == 'oxfiltrate')
ggplot(oxfiltratemin2018B1) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data = oxfiltratemin2018B1[!is.na(oxfiltratemin2018B1$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Filtrate Oxalate (%)") +
  ggtitle("2018 Filter 1B Filtrate Oxalate")


#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#extract feed data from 30mins observation (2019)
outputspo30min2019 <- seedwash30min2019$PONthTkAN.Ox
outputsoda30min2019 <- seedwash30min2019$POSOFATPV.S
outputalumina30min2019 <- seedwash30min2019$TotalFlow
throughput30min2019 <- seedwash30min2019$POFeedMQPV
spo30min2019 <- seedwash30min2019$POFeedAN.Ox
feedsoda30min2019 <- seedwash30min2019$POFeedAN.C
feeddensity30min2019 <- seedwash30min2019$POFeedDTPV

timestep30min2019 <- seq(616, 0, by = -0.5)

counterspo2019 <- 0
counterfeedsoda2019 <- 0
counteroutputspo2019 <- 0
modspo30min2019 <- length(spo30min2019)
modfeedsoda30min2019 <- length(feedsoda30min2019)
modoutputspo30min2019 <- length(outputspo30min2019)


for(i in length(spo30min2019):1){
  if (is.na(spo30min2019[i]) == FALSE){
    counterspo2019 <- spo30min2019[i]
    modspo30min2019[i] <- spo30min2019[i]
  }
  else if (is.na(spo30min2019[i]) == TRUE & counterspo2019 > 0){
    modspo30min2019[i] <- counterspo2019
  }
  else if (is.na(spo30min2019[i]) == TRUE){
    modspo30min2019[i] <- spo30min2019[i]
  }
  
  if (is.na(feedsoda30min2019[i]) == FALSE){
    counterfeedsoda2019 <- feedsoda30min2019[i]
    modfeedsoda30min2019[i] <- feedsoda30min2019[i]
  }
  else if (is.na(feedsoda30min2019[i]) == TRUE & counterfeedsoda2019 > 0){
    modfeedsoda30min2019[i] <- counterfeedsoda2019
  }
  else if (is.na(feedsoda30min2019[i]) == TRUE){
    modfeedsoda30min2019[i] <- feedsoda30min2019[i]
  }
  
  if (is.na(outputspo30min2019[i]) == FALSE){
    counteroutputspo2019 <- outputspo30min2019[i]
    modoutputspo30min2019[i] <- outputspo30min2019[i]
  }
  else if (is.na(outputspo30min2019[i]) == TRUE & counteroutputspo2019 > -0.1){
    modoutputspo30min2019[i] <- counteroutputspo2019
  }
  else if (is.na(outputspo30min2019[i]) == TRUE){
    modoutputspo30min2019[i] <- outputspo30min2019[i]
  }
}

data30min2019 <- data.frame(time = timestep30min2019,           #day
                            outputspo = outputspo30min2019,  #%
                            outputsoda = outputsoda30min2019,   #g/l
                            outputalumina = outputalumina30min2019, #kl/h
                            throughput = throughput30min2019,   #t/h
                            feedspo = spo30min2019,          #%
                            feedsoda = feedsoda30min2019,    #g/l
                            feeddensity = feeddensity30min2019) #SG


multiple30min2019 <- data30min2019 %>%
  gather(type, data, outputspo:feeddensity)

ggplot(multiple30min2019, aes(x=time, y=data, color = type)) +
  geom_line(data = outputspomin2019[!is.na(outputspomin2019$data),],aes(x=time, y=data)) +
  geom_line(data = spomin2019[!is.na(spomin2019$data),],aes(x=time, y=data)) +
  geom_line(data = feedsodamin2019[!is.na(feedsodamin2019$data),],aes(x=time, y=data)) +
  geom_line(na.rm = TRUE) + 
  xlab("Time (hour)") + ggtitle("2019 Feed Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#observed against Output Soda Conc.
outputspomin2019 <- multiple30min2019 %>%
  filter(type == 'outputspo')
ggplot(outputspomin2019) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data = outputspomin2019[!is.na(outputspomin2019$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Output SPO (%)") +
  ggtitle("2019 Output SPO")

outputsodamin2019 <- multiple30min2019 %>%
  filter(type == 'outputsoda')
ggplot(outputsodamin2019, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) +
  xlab("Time (hour)") + ylab("Output Soda Conc. (g/l)") +
  ggtitle("2019 Output Soda Conc.")

outputaluminamin2019 <- multiple30min2019 %>%
  filter(type == 'outputalumina')
ggplot(outputsodamin2019, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) +
  xlab("Time (hour)") + ylab("Output Alumina (kl/h)") +
  ggtitle("2019 Output Alumina")

throughputmin2019 <- multiple30min2019 %>%
  filter( type == 'throughput')
ggplot(throughputmin2019, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Throughput (t/h)") +
  ggtitle("2019 Feed Throughput")


spomin2019 <- multiple30min2019 %>%
  filter( type == 'feedspo')
ggplot(spomin2019) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data = spomin2019[!is.na(spomin2019$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Solid Phase Oxalate (%)") +
  ggtitle("2019 Feed Solid Phase Oxalate")


feedsodamin2019 <- multiple30min2019 %>%
  filter( type == 'feedsoda')
ggplot(feedsodamin2019) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data = feedsodamin2019[!is.na(feedsodamin2019$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Feed Soda Conc. (g/l)") +
  ggtitle("2019 Feed Soda Conc.")


feeddensitymin2019 <- multiple30min2019 %>%
  filter( type == 'feeddensity')
ggplot(feeddensitymin2019, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Feed Density (SG)") +
  ggtitle("2019 Feed Density")


#------------------------------------------------------------------------------------------------
#extract 1A data from 30mins observation
status30min2019A1 <- seedwash30min2019$PO1AFOLXBDI
drumspeed30min2019A1 <- seedwash30min2019$PO1AFDrumSTPV
bathlevel30min2019A1 <- seedwash30min2019$PO1ABathLCPV
vacuum30min2019A1 <- seedwash30min2019$PO1AVacPCPV
feedflow30min2019A1 <- seedwash30min2019$PO1FeedFCPV
flocflow30min2019A1 <- seedwash30min2019$PO1FlocFTPV...17
cakewash30min2019A1 <- seedwash30min2019$PO1ASprayFCPV
clothwash30min2019A1 <- seedwash30min2019$PO1AFCoSFTPV
sodafilt30min2019A1 <- seedwash30min2019$PO1AFltAN.C
oxfilt30min2019A1 <- seedwash30min2019$PO1AFltAN.Ox

#soda concentration in ton/hr
sodaconc30min2019A1 <- c(1:length(feedflow30min2019A1))
for (i in 1:length(sodaconc30min2019A1)){
  sodaconc30min2019A1[i] <- feedflow30min2019A1[i]*modfeedsoda30min2019[i]/1000
}

countersodafiltrate30min2019A1 <- 0
counteroxfiltrate30min2019A1 <- 0
modsodafiltrate30min2019A1 <- length(sodafilt30min2019A1)
modoxfiltrate30min2019A1 <- length(oxfilt30min2019A1)

for(i in length(sodafilt30min2019A1):1){
  if (is.na(sodafilt30min2019A1[i]) == FALSE){
    countersodafiltrate30min2019A1 <- sodafilt30min2019A1[i]
    modsodafiltrate30min2019A1[i] <- sodafilt30min2019A1[i]
  }
  else if (is.na(sodafilt30min2019A1[i]) == TRUE & countersodafiltrate30min2019A1 > 0){
    modsodafiltrate30min2019A1[i] <- countersodafiltrate30min2019A1
  }
  else if (is.na(sodafilt30min2019A1[i]) == TRUE){
    modsodafiltrate30min2019A1[i] <- sodafilt30min2019A1[i]
  }
  
  if (is.na(oxfilt30min2019A1[i]) == FALSE){
    counteroxfiltrate30min2019A1 <- oxfilt30min2019A1[i]
    modoxfiltrate30min2019A1[i] <- oxfilt30min2019A1[i]
  }
  else if (is.na(oxfilt30min2019A1[i]) == TRUE & counteroxfiltrate30min2019A1 > 0){
    modoxfiltrate30min2019A1[i] <- counteroxfiltrate30min2019A1
  }
  else if (is.na(oxfilt30min2019A1[i]) == TRUE){
    modoxfiltrate30min2019A1[i] <- oxfilt30min2019A1[i]
  }
}

data30min2019A1 <- data.frame(time = timestep30min2019,           #day
                              status = status30min2019A1,
                              outputspo = outputspo30min2019,     #%
                              outputsoda = outputsoda30min2019,   #g/l
                              outputalumina = outputalumina30min2019, #kl/h
                              throughput = throughput30min2019,   #t/h
                              feedspo = spo30min2019,      #%
                              feedsoda = feedsoda30min2019,    #g/l
                              feeddensity = feeddensity30min2019, #SG
                              drumspeed = drumspeed30min2019A1,   #RPM
                              bathlevel = bathlevel30min2019A1,   #%
                              vacuum = vacuum30min2019A1,         #kPa
                              feedflow = feedflow30min2019A1,     #kl/h
                              flocflow = flocflow30min2019A1,     #kl/h
                              cakewash = cakewash30min2019A1,     #kl/h
                              clothwash = clothwash30min2019A1,   #kl/h
                              sodafiltrate = sodafilt30min2019A1, #g/l
                              oxfiltrate = oxfilt30min2019A1,     #g/l
                              sodaconc = sodaconc30min2019A1)     #t/h


multiple30min2019A1 <- data30min2019A1 %>%
  filter(status == 'On') %>%
  gather(type, data, outputspo:sodaconc)

ggplot(multiple30min2019A1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  xlab("Time (hour)") + ggtitle("Filter 1A Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#observed against Output Soda Conc.
drumspeedmin2019A1 <- multiple30min2019A1 %>%
  filter(type == 'drumspeed')
ggplot(drumspeedmin2019A1, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Drum Speed (RPM)") +
  ggtitle("2019 Filter 1A Drum Speed")


bathlevelmin2019A1 <- multiple30min2019A1 %>%
  filter( type == 'bathlevel')
ggplot(bathlevelmin2019A1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) + 
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Bath Level (%)") +
  ggtitle("2019 Filter 1A Bath Level")


vacuummin2019A1 <- multiple30min2019A1 %>%
  filter( type == 'vacuum')
ggplot(vacuummin2019A1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") +
  ggtitle("2019 Filter 1A Vacuum Pressure")


feedflowmin2019A1 <- multiple30min2019A1 %>%
  filter( type == 'feedflow')
ggplot(feedflowmin2019A1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Feed Flow (kl/h)") +
  ggtitle("2019 Filter 1A Feed Flow")


flocflowmin2019A1 <- multiple30min2019A1 %>%
  filter(type == 'flocflow')
ggplot(flocflowmin2019A1, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Floc Flow (kl/h)") +
  ggtitle("2019 Filter 1A Floc Flow")


cakewashmin2019A1 <- multiple30min2019A1 %>%
  filter( type == 'cakewash')
ggplot(cakewashmin2019A1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") +
  ggtitle("2019 Filter 1A Cake Wash Flow")


clothwashmin2019A1 <- multiple30min2019A1 %>%
  filter( type == 'clothwash')
ggplot(clothwashmin2019A1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Cloth Wash Flow (kl/h)") +
  ggtitle("2019 Filter 1A Cloth Wash Flow")


sodafiltratemin2019A1 <- multiple30min2019A1 %>%
  filter( type == 'sodafiltrate')
ggplot(sodafiltratemin2019A1) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data=sodafiltratemin2019A1[!is.na(sodafiltratemin2019A1$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Filtrate Soda Conc. (g/l)") +
  ggtitle("2019 Filter 1A Filtrate Soda Conc.")


oxfiltratemin2019A1 <- multiple30min2019A1 %>%
  filter( type == 'oxfiltrate')
ggplot(oxfiltratemin2019A1) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data = oxfiltratemin2019A1[!is.na(oxfiltratemin2019A1$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Filtrate Oxalate (%)") +
  ggtitle("2019 Filter 1A Filtrate Oxalate")


sodaconcmin2019A1 <- multiple30min2019A1 %>%
  filter( type == 'sodaconc')
ggplot(sodaconcmin2019A1, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Inlet Soda (t/h)") +
  ggtitle("2019 Filter 1A Inlet Soda")


#-------------------------------------------------------------------------------------------
#extract 1B data from 30min observation
status30min2019B1 <- seedwash30min2019$PO1BFOLXBDI
drumspeed30min2019B1 <- seedwash30min2019$PO1BFDrumSTPV
bathlevel30min2019B1 <- seedwash30min2019$PO1BFBathLCPV
vacuum30min2019B1 <- seedwash30min2019$PO1BVacPCPV
feedflow30min2019B1 <- seedwash30min2019$PO1BFRSFFCPV
flocflow30min2019B1 <- seedwash30min2019$PO1FlocFTPV...148
cakewash30min2019B1 <- seedwash30min2019$PO1BFCaSFTPV
clothwash30min2019B1 <- seedwash30min2019$PO1BFCoSFTPV
sodafilt30min2019B1 <- seedwash30min2019$PO1BFiltATPV
oxfilt30min2019B1 <- seedwash30min2019$PO1AN.Ox...160

countersodafiltrate30min2019B1 <- 0
counteroxfiltrate30min2019B1 <- 0
modsodafiltrate30min2019B1 <- length(sodafilt30min2019B1)
modoxfiltrate30min2019B1 <- length(oxfilt30min2019B1)

for(i in length(sodafilt30min2019B1):1){
  if (is.na(sodafilt30min2019B1[i]) == FALSE){
    countersodafiltrate30min2019B1 <- sodafilt30min2019B1[i]
    modsodafiltrate30min2019B1[i] <- sodafilt30min2019B1[i]
  }
  else if (is.na(sodafilt30min2019B1[i]) == TRUE & countersodafiltrate30min2019B1 > 0){
    modsodafiltrate30min2019B1[i] <- countersodafiltrate30min2019B1
  }
  else if (is.na(sodafilt30min2019B1[i]) == TRUE){
    modsodafiltrate30min2019B1[i] <- sodafilt30min2019B1[i]
  }
  
  if (is.na(oxfilt30min2019B1[i]) == FALSE){
    counteroxfiltrate30min2019B1 <- oxfilt30min2019B1[i]
    modoxfiltrate30min2019B1[i] <- oxfilt30min2019B1[i]
  }
  else if (is.na(oxfilt30min2019B1[i]) == TRUE & counteroxfiltrate30min2019B1 > 0){
    modoxfiltrate30min2019B1[i] <- counteroxfiltrate30min2019B1
  }
  else if (is.na(oxfilt30min2019B1[i]) == TRUE){
    modoxfiltrate30min2019B1[i] <- oxfilt30min2019B1[i]
  }
}

data30min2019B1 <- data.frame(time = timestep30min2019,           #day
                              status = status30min2019B1,
                              outputspo = outputspo30min2019,  #%
                              outputsoda = outputsoda30min2019,   #g/l
                              outputalumina = outputalumina30min2019, #kl/h
                              throughput = throughput30min2019,   #t/h
                              feedspo = spo30min2019,      #%
                              feedsoda = feedsoda30min2019,    #g/l
                              feeddensity = feeddensity30min2019, #SG
                              drumspeed = drumspeed30min2019B1,   #RPM
                              bathlevel = bathlevel30min2019B1,   #%
                              vacuum = vacuum30min2019B1,         #kPa
                              feedflow = feedflow30min2019B1,     #kl/h
                              flocflow = flocflow30min2019B1,     #kl/h
                              cakewash = cakewash30min2019B1,     #kl/h
                              clothwash = clothwash30min2019B1,   #kl/h
                              sodafiltrate = sodafilt30min2019B1, #g/l
                              oxfiltrate = oxfilt30min2019B1)     #g/l


multiple30min2019B1 <- data30min2019B1 %>%
  filter(status == 'On') %>%
  gather(type, data, outputspo:oxfiltrate)

ggplot(multiple30min2019B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  xlab("Time (hour)") + ggtitle("2019 Filter 1B Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#observed against Output Soda Conc.
#drum speed
drumspeedmin2019B1 <- multiple30min2019B1 %>%
  filter(type == 'drumspeed')
ggplot(drumspeedmin2019B1, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Drum Speed (RPM)") +
  ggtitle("2019 Filter 1B Drum Speed")

#rosner test
rtestdrumspeedmin2019B1 <- rosnerTest(drumspeedmin2019B1$data, k=100)

if (rtestdrumspeedmin2019B1$n.outliers > 0){
  finalrtestdrumspeedmin2019B1 <- rosnerTest(drumspeedmin2019B1$data, k=rtestdrumspeedmin2019B1$n.outliers)
  
  filterdrumspeedmin2019B1 <- drumspeedmin2019B1[-c(finalrtestdrumspeedmin2019B1$all.stats$Obs.Num),]
  ggplot(filterdrumspeedmin2019B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
    geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Drum Speed (RPM)") +
    ggtitle("2019 Filter 1B Drum Speed (filtered)")
}

#bath level
bathlevelmin2019B1 <- multiple30min2019B1 %>%
  filter( type == 'bathlevel')
ggplot(bathlevelmin2019B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) + 
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Bath Level (%)") +
  ggtitle("2019 Filter 1B Bath Level")

#rosner test
rtestbathlevelmin2019B1 <- rosnerTest(bathlevelmin2019B1$data, k=100)

if (rtestbathlevelmin2019B1$n.outliers > 0){
  finalrtestbathlevelmin2019B1 <- rosnerTest(bathlevelmin2019B1$data, k=rtestbathlevelmin2019B1$n.outliers)
  
  filterbathlevelmin2019B1 <- bathlevelmin2019B1[-c(finalrtestbathlevelmin2019B1$all.stats$Obs.Num),]
  ggplot(filterbathlevelmin2019B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
    geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Bath Level (%)") +
    ggtitle("2019 Filter 1B Bath Level (filtered)")
}

#vacuum pressure
vacuummin2019B1 <- multiple30min2019B1 %>%
  filter( type == 'vacuum')
ggplot(vacuummin2019B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") +
  ggtitle("2019 Filter 1B Vacuum Pressure")

#rosner test
rtestvacuummin2019B1 <- rosnerTest(vacuummin2019B1$data, k=100)

if (rtestvacuummin2019B1$n.outliers > 0){
  finalrtestvacuummin2019B1 <- rosnerTest(vacuummin2019B1$data, k=rtestvacuummin2019B1$n.outliers)
  
  filtervacuummin2019B1 <- vacuummin2019B1[-c(finalrtestvacuummin2019B1$all.stats$Obs.Num),]
  ggplot(filtervacuummin2019B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
    geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") +
    ggtitle("2019 Filter 1B Vacuum Pressure (filtered)")
}


#feed flow
feedflowmin2019B1 <- multiple30min2019B1 %>%
  filter( type == 'feedflow')
ggplot(feedflowmin2019B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Feed Flow (kl/h)") +
  ggtitle("2019 Filter 1B Feed Flow")

#rosner test
rtestfeedflowmin2019B1 <- rosnerTest(feedflowmin2019B1$data, k=100)

if (rtestfeedflowmin2019B1$n.outliers > 0){
  finalrtestfeedflowmin2019B1 <- rosnerTest(feedflowmin2019B1$data, k=rtestfeedflowmin2019B1$n.outliers)
  
  filterfeedflowmin2019B1 <- feedflowmin2019B1[-c(finalrtestfeedflowmin2019B1$all.stats$Obs.Num),]
  beforefeedflowmin2019B1 <- feedflowmin2019B1 %>%
    mutate(outlier = "Before")
  afterfeedflowmin2019B1 <- filterfeedflowmin2019B1 %>%
    mutate(outlier = "After")
  finalfilterfeedflowmin2019B1 <- rbind(beforefeedflowmin2019B1, afterfeedflowmin2019B1)
  
  ggplot(finalfilterfeedflowmin2019B1, aes(x=time, y=data, color = outlier)) + 
    geom_point(aes(color = outlier)) + geom_line() +
    scale_colour_manual(values=c("red", "blue")) + 
    xlab("Time (hour)") + ylab("Feed Flow (kl/h)") +
    ggtitle("2019 Filter 1B Feed Flow")
  
  ggplot(filterfeedflowmin2019B1, aes(x=time, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Feed Flow (kl/h)") + ylim(0,180) +
    ggtitle("2019 Filter 1B Feed Flow (filtered)")
}



flocflowmin2019B1 <- multiple30min2019B1 %>%
  filter(type == 'flocflow')
ggplot(flocflowmin2019B1, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Floc Flow (kl/h)") +
  ggtitle("2019 Filter 1B Floc Flow")


cakewashmin2019B1 <- multiple30min2019B1 %>%
  filter( type == 'cakewash')
ggplot(cakewashmin2019B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") +
  ggtitle("2019 Filter 1B Cake Wash Flow")

#rosner test
rtestcakewashmin2019B1 <- rosnerTest(cakewashmin2019B1$data, k=100)

if (rtestcakewashmin2019B1$n.outliers > 0){
  finalrtestcakewashmin2019B1 <- rosnerTest(cakewashmin2019B1$data, k=rtestcakewashmin2019B1$n.outliers)
  
  filtercakewashmin2019B1 <- cakewashmin2019B1[-c(finalrtestcakewashmin2019B1$all.stats$Obs.Num),]
  ggplot(filtercakewashmin2019B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
    geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") +
    ggtitle("2019 Filter 1B Cake Wash Flow (filtered)")
}

clothwashmin2019B1 <- multiple30min2019B1 %>%
  filter( type == 'clothwash')
ggplot(clothwashmin2019B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Cloth Wash Flow (kl/h)") +
  ggtitle("2019 Filter 1B Cloth Wash Flow")


sodafiltratemin2019B1 <- multiple30min2019B1 %>%
  filter( type == 'sodafiltrate')
ggplot(sodafiltratemin2019B1, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Filtrate Soda Conc. (g/l)") +
  ggtitle("2019 Filter 1B Filtrate Soda Conc.")

#rosner test
rtestsodafiltratemin2019B1 <- rosnerTest(sodafiltratemin2019B1$data, k=100)

if (rtestsodafiltratemin2019B1$n.outliers > 0){
  finalrtestsodafiltratemin2019B1 <- rosnerTest(sodafiltratemin2019B1$data, k=rtestsodafiltratemin2019B1$n.outliers)
  
  filtersodafiltratemin2019B1 <- sodafiltratemin2019B1[-c(finalrtestsodafiltratemin2019B1$all.stats$Obs.Num),]
  ggplot(filtersodafiltratemin2019B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
    geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Filtrate Soda Conc. (g/l)") +
    ggtitle("2019 Filter 1B Filtrate Soda Conc. (filtered)")
}


oxfiltratemin2019B1 <- multiple30min2019B1 %>%
  filter( type == 'oxfiltrate')
ggplot(oxfiltratemin2019B1) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data = oxfiltratemin2019B1[!is.na(oxfiltratemin2019B1$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Filtrate Oxalate (%)") +
  ggtitle("2019 Filter 1B Filtrate Oxalate")





















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

