library(tidyverse)
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)

#read seedwash 1 day data from excel
seedwashdaily<-read_excel('SeedwashData_1day_2016_2019.xlsx', skip=6, na=c(""," ", "#N/A", "[-11059] No Good Data For Calculation"))

#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#extract feed data from daily observation
datedaily <- seedwashdaily$Date
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
                          date = datedaily,
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
                          date = datedaily,
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


cakewashA1 <- multipledailyA1 %>%
  filter( type == 'cakewash')
ggplot(cakewashA1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Cake Wash Flow (kl/h)") +
  ggtitle("Daily Filter 1A Cake Wash Flow")


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
                          date = datedaily,
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


cakewashA2 <- multipledailyA2 %>%
  filter( type == 'cakewash')
ggplot(cakewashA2, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Cake Wash Flow (kl/h)") +
  ggtitle("Daily Filter 2A Cake Wash Flow")


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
                          date = datedaily,
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


cakewashB1 <- multipledailyB1 %>%
  filter( type == 'cakewash')
ggplot(cakewashB1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Cake Wash Flow (kl/h)") +
  ggtitle("Daily Filter 1B Cake Wash Flow")


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
                          date = datedaily,
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

cakewashB2 <- multipledailyB2 %>%
  filter( type == 'cakewash')
ggplot(cakewashB2, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (day)") + ylab("Cake Wash Flow (kl/h)") +
  ggtitle("Daily Filter 2B Cake Wash Flow")

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






#-------------------------------------------------------------------------------------------------
#filtering & spectral analysis
library(stats)
library(EnvStats)
library(signal)


#-------------------------------------------------------------------------------------------------
#rosner test--------------------------------------------------------------------------------------
#throughput
throughputdaily <- throughput[1:552,]
rtestthroughputdaily <- rosnerTest(throughputdaily$data, k=100)

if (rtestthroughputdaily$n.outliers > 0){
  finalrtestthroughputdaily <- rosnerTest(throughputdaily$data, k=rtestthroughputdaily$n.outliers)
  
  filterthroughputdaily <- throughputdaily[-c(finalrtestthroughputdaily$all.stats$Obs.Num),]
  
  ggplot(filterthroughputdaily, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Throughput (t/h)") +
    ggtitle("Daily Throughput (filtered)")
} else {
  filterthroughputdaily <- throughputdaily
}

# 1A Drum-----------------------------------------------------------------------------------------
# 1A feedflow 
feedflowdailyA1 <- feedflowA1[1:480,]
rtestfeedflowdailyA1 <- rosnerTest(feedflowdailyA1$data, k=100)

if (rtestfeedflowdailyA1$n.outliers > 0){
  finalrtestfeedflowdailyA1 <- rosnerTest(feedflowdailyA1$data, k=rtestfeedflowdailyA1$n.outliers)
  
  filterfeedflowdailyA1 <- feedflowdailyA1[-c(finalrtestfeedflowdailyA1$all.stats$Obs.Num),]
  
  ggplot(filterfeedflowdailyA1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Feed Flow (kl/h)") +
    ggtitle("Daily Filter 1A Feed Flow (filtered)")
} else {
  filterfeedflowdailyA1 <- feedflowdailyA1
}

# 1A drum speed
drumspeeddailyA1 <- drumspeedA1[1:480,]
rtestdrumspeeddailyA1 <- rosnerTest(drumspeeddailyA1$data, k=100)

if (rtestdrumspeeddailyA1$n.outliers > 0){
  finalrtestdrumspeeddailyA1 <- rosnerTest(drumspeeddailyA1$data, k=rtestdrumspeeddailyA1$n.outliers)
  
  filterdrumspeeddailyA1 <- drumspeeddailyA1[-c(finalrtestdrumspeeddailyA1$all.stats$Obs.Num),]
  
  ggplot(filterdrumspeeddailyA1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Drum Speed (RPM)") + 
    ggtitle("Daily Filter 1A Drum Speed (filtered)")
} else {
  filterdrumspeeddailyA1 <- drumspeeddailyA1
}

# 1A bath level
bathleveldailyA1 <- bathlevelA1[1:480,]
rtestbathleveldailyA1 <- rosnerTest(bathleveldailyA1$data, k=100)

if (rtestbathleveldailyA1$n.outliers > 0){
  finalrtestbathleveldailyA1 <- rosnerTest(bathleveldailyA1$data, k=rtestbathleveldailyA1$n.outliers)
  
  filterbathleveldailyA1 <- bathleveldailyA1[-c(finalrtestbathleveldailyA1$all.stats$Obs.Num),]
  
  ggplot(filterbathleveldailyA1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Bath Level (%)") + 
    ggtitle("Daily Filter 1A Bath Level (filtered)")
} else {
  filterbathleveldailyA1 <- bathleveldailyA1
}

# 1A vacuum
vacuumdailyA1 <- vacuumA1[1:480,]
rtestvacuumdailyA1 <- rosnerTest(vacuumdailyA1$data, k=100)

if (rtestvacuumdailyA1$n.outliers > 0){
  finalrtestvacuumdailyA1 <- rosnerTest(vacuumdailyA1$data, k=rtestvacuumdailyA1$n.outliers)
  
  filtervacuumdailyA1 <- vacuumdailyA1[-c(finalrtestvacuumdailyA1$all.stats$Obs.Num),]
  
  ggplot(filtervacuumdailyA1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") +
    ggtitle("Daily Filter 1A Vacuum Pressure (filtered)")
} else {
  filtervacuumdailyA1 <- vacuumdailyA1
}

# 1A cake wash
cakewashdailyA1 <- cakewashA1[1:480,]
rtestcakewashdailyA1 <- rosnerTest(cakewashdailyA1$data, k=100)

if (rtestcakewashdailyA1$n.outliers > 0){
  finalrtestcakewashdailyA1 <- rosnerTest(cakewashdailyA1$data, k=rtestcakewashdailyA1$n.outliers)
  
  filtercakewashdailyA1 <- cakewashdailyA1[-c(finalrtestcakewashdailyA1$all.stats$Obs.Num),]
  
  ggplot(filtercakewashdailyA1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") + 
    ggtitle("Daily Filter 1A Cake Wash Flow (filtered)")
} else {
  filtercakewashdailyA1 <- cakewashdailyA1
}

# 1A soda filtrate conc.
sodafiltratedailyA1 <- sodafiltrateA1[1:480,]
rtestsodafiltratedailyA1 <- rosnerTest(sodafiltratedailyA1$data, k=100)

if (rtestsodafiltratedailyA1$n.outliers > 0){
  finalrtestsodafiltratedailyA1 <- rosnerTest(sodafiltratedailyA1$data, k=rtestsodafiltratedailyA1$n.outliers)
  
  filtersodafiltratedailyA1 <- sodafiltratedailyA1[-c(finalrtestsodafiltratedailyA1$all.stats$Obs.Num),]
  
  ggplot(filtersodafiltratedailyA1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Soda Filtrate Conc. (g/l)") + 
    ggtitle("Daily Filter 1A Soda Filtrate Conc. (filtered)")
} else {
  filtersodafiltratedailyA1 <- sodafiltratedailyA1
}

# 1A oxalate filtrate 
oxfiltratedailyA1 <- oxfiltrateA1[1:480,]
rtestoxfiltratedailyA1 <- rosnerTest(oxfiltratedailyA1$data, k=100)

if (rtestoxfiltratedailyA1$n.outliers > 0){
  finalrtestoxfiltratedailyA1 <- rosnerTest(oxfiltratedailyA1$data, k=rtestoxfiltratedailyA1$n.outliers)
  
  filteroxfiltratedailyA1 <- oxfiltratedailyA1[-c(finalrtestoxfiltratedailyA1$all.stats$Obs.Num),]
  
  ggplot(filteroxfiltratedailyA1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Oxalate Filtrate (%)") + 
    ggtitle("Daily Filter 1A Oxalate Filtrate (filtered)")
} else {
  filteroxfiltratedailyA1 <- oxfiltratedailyA1
}



# 2A Drum-----------------------------------------------------------------------------------------
# 2A feedflow 
feedflowdailyA2 <- feedflowA2[1:480,]
rtestfeedflowdailyA2 <- rosnerTest(feedflowdailyA2$data, k=100)

if (rtestfeedflowdailyA2$n.outliers > 0){
  finalrtestfeedflowdailyA2 <- rosnerTest(feedflowdailyA2$data, k=rtestfeedflowdailyA2$n.outliers)
  
  filterfeedflowdailyA2 <- feedflowdailyA2[-c(finalrtestfeedflowdailyA2$all.stats$Obs.Num),]
  
  ggplot(filterfeedflowdailyA2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Feed Flow (kl/h)") +
    ggtitle("Daily Filter 2A Feed Flow (filtered)")
} else {
  filterfeedflowdailyA2 <- feedflowdailyA2
}

# 2A drum speed
drumspeeddailyA2 <- drumspeedA2[1:480,]
rtestdrumspeeddailyA2 <- rosnerTest(drumspeeddailyA2$data, k=100)

if (rtestdrumspeeddailyA2$n.outliers > 0){
  finalrtestdrumspeeddailyA2 <- rosnerTest(drumspeeddailyA2$data, k=rtestdrumspeeddailyA2$n.outliers)
  
  filterdrumspeeddailyA2 <- drumspeeddailyA2[-c(finalrtestdrumspeeddailyA2$all.stats$Obs.Num),]
  
  ggplot(filterdrumspeeddailyA2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Drum Speed (RPM)") + 
    ggtitle("Daily Filter 2A Drum Speed (filtered)")
} else {
  filterdrumspeeddailyA2 <- drumspeeddailyA2
}

# 2A bath level
bathleveldailyA2 <- bathlevelA2[1:480,]
rtestbathleveldailyA2 <- rosnerTest(bathleveldailyA2$data, k=100)

if (rtestbathleveldailyA2$n.outliers > 0){
  finalrtestbathleveldailyA2 <- rosnerTest(bathleveldailyA2$data, k=rtestbathleveldailyA2$n.outliers)
  
  filterbathleveldailyA2 <- bathleveldailyA2[-c(finalrtestbathleveldailyA2$all.stats$Obs.Num),]
  
  ggplot(filterbathleveldailyA2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Bath Level (%)") + 
    ggtitle("Daily Filter 2A Bath Level (filtered)")
} else {
  filterbathleveldailyA2 <- bathleveldailyA2
}

# 2A vacuum
vacuumdailyA2 <- vacuumA2[1:480,]
rtestvacuumdailyA2 <- rosnerTest(vacuumdailyA2$data, k=100)

if (rtestvacuumdailyA2$n.outliers > 0){
  finalrtestvacuumdailyA2 <- rosnerTest(vacuumdailyA2$data, k=rtestvacuumdailyA2$n.outliers)
  
  filtervacuumdailyA2 <- vacuumdailyA2[-c(finalrtestvacuumdailyA2$all.stats$Obs.Num),]
  
  ggplot(filtervacuumdailyA2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") +
    ggtitle("Daily Filter 2A Vacuum Pressure (filtered)")
} else {
  filtervacuumdailyA2 <- vacuumdailyA2
}

# 2A cake wash
cakewashdailyA2 <- cakewashA2[1:480,]
rtestcakewashdailyA2 <- rosnerTest(cakewashdailyA2$data, k=100)

if (rtestcakewashdailyA2$n.outliers > 0){
  finalrtestcakewashdailyA2 <- rosnerTest(cakewashdailyA2$data, k=rtestcakewashdailyA2$n.outliers)
  
  filtercakewashdailyA2 <- cakewashdailyA2[-c(finalrtestcakewashdailyA2$all.stats$Obs.Num),]
  
  ggplot(filtercakewashdailyA2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") + 
    ggtitle("Daily Filter 2A Cake Wash Flow (filtered)")
} else {
  filtercakewashdailyA2 <- cakewashdailyA2
}

# 2A soda filtrate conc.
sodafiltratedailyA2 <- sodafiltrateA2[1:480,]
rtestsodafiltratedailyA2 <- rosnerTest(sodafiltratedailyA2$data, k=100)

if (rtestsodafiltratedailyA2$n.outliers > 0){
  finalrtestsodafiltratedailyA2 <- rosnerTest(sodafiltratedailyA2$data, k=rtestsodafiltratedailyA2$n.outliers)
  
  filtersodafiltratedailyA2 <- sodafiltratedailyA2[-c(finalrtestsodafiltratedailyA2$all.stats$Obs.Num),]
  
  ggplot(filtersodafiltratedailyA2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Soda Filtrate Conc. (g/l)") + 
    ggtitle("Daily Filter 2A Soda Filtrate Conc. (filtered)")
} else {
  filtersodafiltratedailyA2 <- sodafiltratedailyA2
}

# 2A oxalate filtrate 
oxfiltratedailyA2 <- oxfiltrateA2[1:480,]
rtestoxfiltratedailyA2 <- rosnerTest(oxfiltratedailyA2$data, k=100)

if (rtestoxfiltratedailyA2$n.outliers > 0){
  finalrtestoxfiltratedailyA2 <- rosnerTest(oxfiltratedailyA2$data, k=rtestoxfiltratedailyA2$n.outliers)
  
  filteroxfiltratedailyA2 <- oxfiltratedailyA2[-c(finalrtestoxfiltratedailyA2$all.stats$Obs.Num),]
  
  ggplot(filteroxfiltratedailyA2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Oxalate Filtrate (%)") + 
    ggtitle("Daily Filter 2A Oxalate Filtrate (filtered)")
} else {
  filteroxfiltratedailyA2 <- oxfiltratedailyA2
}



# 1B Drum-----------------------------------------------------------------------------------------
# 1B feedflow 
feedflowdailyB1 <- feedflowB1[1:480,]
rtestfeedflowdailyB1 <- rosnerTest(feedflowdailyB1$data, k=100)

if (rtestfeedflowdailyB1$n.outliers > 0){
  finalrtestfeedflowdailyB1 <- rosnerTest(feedflowdailyB1$data, k=rtestfeedflowdailyB1$n.outliers)
  
  filterfeedflowdailyB1 <- feedflowdailyB1[-c(finalrtestfeedflowdailyB1$all.stats$Obs.Num),]
  
  ggplot(filterfeedflowdailyB1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Feed Flow (kl/h)") +
    ggtitle("Daily Filter 1B Feed Flow (filtered)")
} else {
  filterfeedflowdailyB1 <- feedflowdailyB1
}

# 1B drum speed
drumspeeddailyB1 <- drumspeedB1[1:480,]
rtestdrumspeeddailyB1 <- rosnerTest(drumspeeddailyB1$data, k=100)

if (rtestdrumspeeddailyB1$n.outliers > 0){
  finalrtestdrumspeeddailyB1 <- rosnerTest(drumspeeddailyB1$data, k=rtestdrumspeeddailyB1$n.outliers)
  
  filterdrumspeeddailyB1 <- drumspeeddailyB1[-c(finalrtestdrumspeeddailyB1$all.stats$Obs.Num),]
  
  ggplot(filterdrumspeeddailyB1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Drum Speed (RPM)") + 
    ggtitle("Daily Filter 1B Drum Speed (filtered)")
} else {
  filterdrumspeeddailyB1 <- drumspeeddailyB1
}

# 1B bath level
bathleveldailyB1 <- bathlevelB1[1:480,]
rtestbathleveldailyB1 <- rosnerTest(bathleveldailyB1$data, k=100)

if (rtestbathleveldailyB1$n.outliers > 0){
  finalrtestbathleveldailyB1 <- rosnerTest(bathleveldailyB1$data, k=rtestbathleveldailyB1$n.outliers)
  
  filterbathleveldailyB1 <- bathleveldailyB1[-c(finalrtestbathleveldailyB1$all.stats$Obs.Num),]
  
  ggplot(filterbathleveldailyB1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Bath Level (%)") + 
    ggtitle("Daily Filter 1B Bath Level (filtered)")
} else {
  filterbathleveldailyB1 <- bathleveldailyB1
}

# 1B vacuum
vacuumdailyB1 <- vacuumB1[1:480,]
rtestvacuumdailyB1 <- rosnerTest(vacuumdailyB1$data, k=100)

if (rtestvacuumdailyB1$n.outliers > 0){
  finalrtestvacuumdailyB1 <- rosnerTest(vacuumdailyB1$data, k=rtestvacuumdailyB1$n.outliers)
  
  filtervacuumdailyB1 <- vacuumdailyB1[-c(finalrtestvacuumdailyB1$all.stats$Obs.Num),]
  
  ggplot(filtervacuumdailyB1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") +
    ggtitle("Daily Filter 1B Vacuum Pressure (filtered)")
} else {
  filtervacuumdailyB1 <- vacuumdailyB1
}

# 1B cake wash
cakewashdailyB1 <- cakewashB1[1:480,]
rtestcakewashdailyB1 <- rosnerTest(cakewashdailyB1$data, k=100)

if (rtestcakewashdailyB1$n.outliers > 0){
  finalrtestcakewashdailyB1 <- rosnerTest(cakewashdailyB1$data, k=rtestcakewashdailyB1$n.outliers)
  
  filtercakewashdailyB1 <- cakewashdailyB1[-c(finalrtestcakewashdailyB1$all.stats$Obs.Num),]
  
  ggplot(filtercakewashdailyB1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") + 
    ggtitle("Daily Filter 1B Cake Wash Flow (filtered)")
} else {
  filtercakewashdailyB1 <- cakewashdailyB1
}

# 1B soda filtrate conc.
sodafiltratedailyB1 <- sodafiltrateB1[1:480,]
rtestsodafiltratedailyB1 <- rosnerTest(sodafiltratedailyB1$data, k=100)

if (rtestsodafiltratedailyB1$n.outliers > 0){
  finalrtestsodafiltratedailyB1 <- rosnerTest(sodafiltratedailyB1$data, k=rtestsodafiltratedailyB1$n.outliers)
  
  filtersodafiltratedailyB1 <- sodafiltratedailyB1[-c(finalrtestsodafiltratedailyB1$all.stats$Obs.Num),]
  
  ggplot(filtersodafiltratedailyB1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Soda Filtrate Conc. (g/l)") + 
    ggtitle("Daily Filter 1B Soda Filtrate Conc. (filtered)")
} else {
  filtersodafiltratedailyB1 <- sodafiltratedailyB1
}

# 1B oxalate filtrate 
oxfiltratedailyB1 <- oxfiltrateB1[1:480,]
rtestoxfiltratedailyB1 <- rosnerTest(oxfiltratedailyB1$data, k=100)

if (rtestoxfiltratedailyB1$n.outliers > 0){
  finalrtestoxfiltratedailyB1 <- rosnerTest(oxfiltratedailyB1$data, k=rtestoxfiltratedailyB1$n.outliers)
  
  filteroxfiltratedailyB1 <- oxfiltratedailyB1[-c(finalrtestoxfiltratedailyB1$all.stats$Obs.Num),]
  
  ggplot(filteroxfiltratedailyB1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Oxalate Filtrate (%)") + 
    ggtitle("Daily Filter 1B Oxalate Filtrate (filtered)")
} else {
  filteroxfiltratedailyB1 <- oxfiltratedailyB1
}


# 2B Drum-----------------------------------------------------------------------------------------
# 2B feedflow 
feedflowdailyB2 <- feedflowB2[1:480,]
rtestfeedflowdailyB2 <- rosnerTest(feedflowdailyB2$data, k=100)

if (rtestfeedflowdailyB2$n.outliers > 0){
  finalrtestfeedflowdailyB2 <- rosnerTest(feedflowdailyB2$data, k=rtestfeedflowdailyB2$n.outliers)
  
  filterfeedflowdailyB2 <- feedflowdailyB2[-c(finalrtestfeedflowdailyB2$all.stats$Obs.Num),]
  
  ggplot(filterfeedflowdailyB2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Feed Flow (kl/h)") +
    ggtitle("Daily Filter 2B Feed Flow (filtered)")
} else {
  filterfeedflowdailyB2 <- feedflowdailyB2
}

# 2B drum speed
drumspeeddailyB2 <- drumspeedB2[1:480,]
rtestdrumspeeddailyB2 <- rosnerTest(drumspeeddailyB2$data, k=100)

if (rtestdrumspeeddailyB2$n.outliers > 0){
  finalrtestdrumspeeddailyB2 <- rosnerTest(drumspeeddailyB2$data, k=rtestdrumspeeddailyB2$n.outliers)
  
  filterdrumspeeddailyB2 <- drumspeeddailyB2[-c(finalrtestdrumspeeddailyB2$all.stats$Obs.Num),]
  
  ggplot(filterdrumspeeddailyB2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Drum Speed (RPM)") + 
    ggtitle("Daily Filter 2B Drum Speed (filtered)")
} else {
  filterdrumspeeddailyB2 <- drumspeeddailyB2
}

# 2B bath level
bathleveldailyB2 <- bathlevelB2[1:480,]
rtestbathleveldailyB2 <- rosnerTest(bathleveldailyB2$data, k=100)

if (rtestbathleveldailyB2$n.outliers > 0){
  finalrtestbathleveldailyB2 <- rosnerTest(bathleveldailyB2$data, k=rtestbathleveldailyB2$n.outliers)
  
  filterbathleveldailyB2 <- bathleveldailyB2[-c(finalrtestbathleveldailyB2$all.stats$Obs.Num),]
  
  ggplot(filterbathleveldailyB2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Bath Level (%)") + 
    ggtitle("Daily Filter 2B Bath Level (filtered)")
} else {
  filterbathleveldailyB2 <- bathleveldailyB2
}

# 2B vacuum
vacuumdailyB2 <- vacuumB2[1:480,]
rtestvacuumdailyB2 <- rosnerTest(vacuumdailyB2$data, k=100)

if (rtestvacuumdailyB2$n.outliers > 0){
  finalrtestvacuumdailyB2 <- rosnerTest(vacuumdailyB2$data, k=rtestvacuumdailyB2$n.outliers)
  
  filtervacuumdailyB2 <- vacuumdailyB2[-c(finalrtestvacuumdailyB2$all.stats$Obs.Num),]
  
  ggplot(filtervacuumdailyB2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") +
    ggtitle("Daily Filter 2B Vacuum Pressure (filtered)")
} else {
  filtervacuumdailyB2 <- vacuumdailyB2
}

# 2B cake wash
cakewashdailyB2 <- cakewashB2[1:480,]
rtestcakewashdailyB2 <- rosnerTest(cakewashdailyB2$data, k=100)

if (rtestcakewashdailyB2$n.outliers > 0){
  finalrtestcakewashdailyB2 <- rosnerTest(cakewashdailyB2$data, k=rtestcakewashdailyB2$n.outliers)
  
  filtercakewashdailyB2 <- cakewashdailyB2[-c(finalrtestcakewashdailyB2$all.stats$Obs.Num),]
  
  ggplot(filtercakewashdailyB2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") + 
    ggtitle("Daily Filter 2B Cake Wash Flow (filtered)")
} else {
  filtercakewashdailyB2 <- cakewashdailyB2
}

# 2B soda filtrate conc.
sodafiltratedailyB2 <- sodafiltrateB2[1:480,]
rtestsodafiltratedailyB2 <- rosnerTest(sodafiltratedailyB2$data, k=100)

if (rtestsodafiltratedailyB2$n.outliers > 0){
  finalrtestsodafiltratedailyB2 <- rosnerTest(sodafiltratedailyB2$data, k=rtestsodafiltratedailyB2$n.outliers)
  
  filtersodafiltratedailyB2 <- sodafiltratedailyB2[-c(finalrtestsodafiltratedailyB2$all.stats$Obs.Num),]
  
  ggplot(filtersodafiltratedailyB2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Soda Filtrate Conc. (g/l)") + 
    ggtitle("Daily Filter 2B Soda Filtrate Conc. (filtered)")
} else {
  filtersodafiltratedailyB2 <- sodafiltratedailyB2
}

# 2B oxalate filtrate 
oxfiltratedailyB2 <- oxfiltrateB2[1:480,]
rtestoxfiltratedailyB2 <- rosnerTest(oxfiltratedailyB2$data, k=100)

if (rtestoxfiltratedailyB2$n.outliers > 0){
  finalrtestoxfiltratedailyB2 <- rosnerTest(oxfiltratedailyB2$data, k=rtestoxfiltratedailyB2$n.outliers)
  
  filteroxfiltratedailyB2 <- oxfiltratedailyB2[-c(finalrtestoxfiltratedailyB2$all.stats$Obs.Num),]
  
  ggplot(filteroxfiltratedailyB2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Oxalate Filtrate (%)") + 
    ggtitle("Daily Filter 2B Oxalate Filtrate (filtered)")
} else {
  filteroxfiltratedailyB2 <- oxfiltratedailyB2
}



#------------------------------------------------------------------------------------------------
#low filter-------------------------------------------------------------------------------------- 
lowbf <- butter(8, 0.1, type = "low")

#throughput
filterthroughputdaily <- na.omit(filterthroughputdaily)
lowfilterthroughputdaily <- filtfilt(lowbf, filterthroughputdaily$data)
lowpassthroughputdaily <- data.frame(time = filterthroughputdaily$time,
                                       date = filterthroughputdaily$date,
                                       filter = lowfilterthroughputdaily,
                                       nofilter = filterthroughputdaily$data)
lowpassthroughputdailyfin <- lowpassthroughputdaily %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassthroughputdailyfin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Throughput (t/h)") + 
  ggtitle("Throughput (Daily Data)")


# 1A Drum----------------------------------------------------------------------------------------
# 1A feed flow
filterfeedflowdailyA1 <- na.omit(filterfeedflowdailyA1)
lowfilterfeedflowdailyA1 <- filtfilt(lowbf, filterfeedflowdailyA1$data)
lowpassfeedflowdailyA1 <- data.frame(time = filterfeedflowdailyA1$time,
                                       date = filterfeedflowdailyA1$date,
                                       filter = lowfilterfeedflowdailyA1,
                                     nofilter = filterfeedflowdailyA1$data)
lowpassfeedflowdailyA1fin <- lowpassfeedflowdailyA1 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassfeedflowdailyA1fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Feed Flow (kl/h)") + 
  ggtitle("1A Drum Feed Flow (Daily Data)")

# 1A drum speed
filterdrumspeeddailyA1 <- na.omit(filterdrumspeeddailyA1)
lowfilterdrumspeeddailyA1 <- filtfilt(lowbf, filterdrumspeeddailyA1$data)
lowpassdrumspeeddailyA1 <- data.frame(time = filterdrumspeeddailyA1$time,
                                        date = filterdrumspeeddailyA1$date,
                                        filter = lowfilterdrumspeeddailyA1,
                                      nofilter = filterdrumspeeddailyA1$data)
lowpassdrumspeeddailyA1fin <- lowpassdrumspeeddailyA1 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassdrumspeeddailyA1fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Drum Speed (RPM)") + 
  ggtitle("1A Drum Drum Speed (Daily Data)")

# 1A bath level
filterbathleveldailyA1 <- na.omit(filterbathleveldailyA1)
lowfilterbathleveldailyA1 <- filtfilt(lowbf, filterbathleveldailyA1$data)
lowpassbathleveldailyA1 <- data.frame(time = filterbathleveldailyA1$time,
                                        date = filterbathleveldailyA1$date,
                                        filter = lowfilterbathleveldailyA1,
                                      nofilter = filterbathleveldailyA1$data)
lowpassbathleveldailyA1fin <- lowpassbathleveldailyA1 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassbathleveldailyA1fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Bath Level (%)") + 
  ggtitle("1A Drum Bath Level (Daily Data)")

# 1A vacuum 
filtervacuumdailyA1 <- na.omit(filtervacuumdailyA1)
lowfiltervacuumdailyA1 <- filtfilt(lowbf, filtervacuumdailyA1$data)
lowpassvacuumdailyA1 <- data.frame(time = filtervacuumdailyA1$time,
                                     date = filtervacuumdailyA1$date,
                                     filter = lowfiltervacuumdailyA1,
                                   nofilter = filtervacuumdailyA1$data)
lowpassvacuumdailyA1fin <- lowpassvacuumdailyA1 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassvacuumdailyA1fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") + 
  ggtitle("1A Drum Vacuum Pressure (Daily Data)")

# 1A cake wash
filtercakewashdailyA1 <- na.omit(filtercakewashdailyA1)
lowfiltercakewashdailyA1 <- filtfilt(lowbf, filtercakewashdailyA1$data)
lowpasscakewashdailyA1 <- data.frame(time = filtercakewashdailyA1$time,
                                       date = filtercakewashdailyA1$date,
                                      filter = lowfiltercakewashdailyA1,
                                     nofilter = filtercakewashdailyA1$data)
lowpasscakewashdailyA1fin <- lowpasscakewashdailyA1 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpasscakewashdailyA1fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") + 
  ggtitle("1A Drum Cake Wash Flow (Daily Data)")

# 1A soda filtrate conc.
filtersodafiltratedailyA1 <- na.omit(filtersodafiltratedailyA1)
lowfiltersodafiltratedailyA1 <- filtfilt(lowbf, filtersodafiltratedailyA1$data)
lowpasssodafiltratedailyA1 <- data.frame(time = filtersodafiltratedailyA1$time,
                                           date = filtersodafiltratedailyA1$date,
                                           filter = lowfiltersodafiltratedailyA1,
                                           nofilter = filtersodafiltratedailyA1$data)
lowpasssodafiltratedailyA1fin <- lowpasssodafiltratedailyA1 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpasssodafiltratedailyA1fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Soda Filtrate Conc. (g/l)") + 
  ggtitle("1A Drum Soda Filtrate Conc. (Daily Data)")


# 1A oxalate filtrate
filteroxfiltratedailyA1 <- na.omit(filteroxfiltratedailyA1)
lowfilteroxfiltratedailyA1 <- filtfilt(lowbf, filteroxfiltratedailyA1$data)
lowpassoxfiltratedailyA1 <- data.frame(time = filteroxfiltratedailyA1$time,
                                         date = filteroxfiltratedailyA1$date,
                                         filter = lowfilteroxfiltratedailyA1,
                                         nofilter = filteroxfiltratedailyA1$data)
lowpassoxfiltratedailyA1fin <- lowpassoxfiltratedailyA1 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassoxfiltratedailyA1fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Oxalate Filtrate (%)") + 
  ggtitle("1A Drum Oxalate Filtrate (Daily Data)")


# 2A Drum----------------------------------------------------------------------------------------
# 2A feed flow
filterfeedflowdailyA2 <- na.omit(filterfeedflowdailyA2)
lowfilterfeedflowdailyA2 <- filtfilt(lowbf, filterfeedflowdailyA2$data)
lowpassfeedflowdailyA2 <- data.frame(time = filterfeedflowdailyA2$time,
                                       date = filterfeedflowdailyA2$date,
                                       filter = lowfilterfeedflowdailyA2,
                                       nofilter = filterfeedflowdailyA2$data)
lowpassfeedflowdailyA2fin <- lowpassfeedflowdailyA2 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassfeedflowdailyA2fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Feed Flow (kl/h)") + 
  ggtitle("2A Drum Feed Flow (Daily Data)")

# 2A drum speed
filterdrumspeeddailyA2 <- na.omit(filterdrumspeeddailyA2)
lowfilterdrumspeeddailyA2 <- filtfilt(lowbf, filterdrumspeeddailyA2$data)
lowpassdrumspeeddailyA2 <- data.frame(time = filterdrumspeeddailyA2$time,
                                        date = filterdrumspeeddailyA2$date,
                                        drumspeed = lowfilterdrumspeeddailyA2)
lowpassdrumspeeddailyA2fin <- lowpassdrumspeeddailyA2 %>%
  gather(type, data, drumspeed)

# 2A bath level
filterbathleveldailyA2 <- na.omit(filterbathleveldailyA2)
lowfilterbathleveldailyA2 <- filtfilt(lowbf, filterbathleveldailyA2$data)
lowpassbathleveldailyA2 <- data.frame(time = filterbathleveldailyA2$time,
                                      date = filterbathleveldailyA2$date,
                                      filter = lowfilterbathleveldailyA2,
                                      nofilter = filterbathleveldailyA2$data)
lowpassbathleveldailyA2fin <- lowpassbathleveldailyA2 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassbathleveldailyA2fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Bath Level (%)") + 
  ggtitle("2A Drum Bath Level (Daily Data)")

# 2A vacuum 
filtervacuumdailyA2 <- na.omit(filtervacuumdailyA2)
lowfiltervacuumdailyA2 <- filtfilt(lowbf, filtervacuumdailyA2$data)
lowpassvacuumdailyA2 <- data.frame(time = filtervacuumdailyA2$time,
                                     date = filtervacuumdailyA2$date,
                                     vacuum = lowfiltervacuumdailyA2)
lowpassvacuumdailyA2fin <- lowpassvacuumdailyA2 %>%
  gather(type, data, vacuum)

# 2A cake wash
filtercakewashdailyA2 <- na.omit(filtercakewashdailyA2)
lowfiltercakewashdailyA2 <- filtfilt(lowbf, filtercakewashdailyA2$data)
lowpasscakewashdailyA2 <- data.frame(time = filtercakewashdailyA2$time,
                                       date = filtercakewashdailyA2$date,
                                       cakewash = lowfiltercakewashdailyA2)
lowpasscakewashdailyA2fin <- lowpasscakewashdailyA2 %>%
  gather(type, data, cakewash)

# 2A soda filtrate conc.
filtersodafiltratedailyA2 <- na.omit(filtersodafiltratedailyA2)
lowfiltersodafiltratedailyA2 <- filtfilt(lowbf, filtersodafiltratedailyA2$data)
lowpasssodafiltratedailyA2 <- data.frame(time = filtersodafiltratedailyA2$time,
                                           date = filtersodafiltratedailyA2$date,
                                           filter = lowfiltersodafiltratedailyA2,
                                           nofilter = filtersodafiltratedailyA2$data)
lowpasssodafiltratedailyA2fin <- lowpasssodafiltratedailyA2 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpasssodafiltratedailyA2fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Soda Filtrate Conc. (g/l)") + 
  ggtitle("2A Drum Soda Filtrate Conc. (Daily Data)")


# 2A oxalate filtrate
filteroxfiltratedailyA2 <- na.omit(filteroxfiltratedailyA2)
lowfilteroxfiltratedailyA2 <- filtfilt(lowbf, filteroxfiltratedailyA2$data)
lowpassoxfiltratedailyA2 <- data.frame(time = filteroxfiltratedailyA2$time,
                                         date = filteroxfiltratedailyA2$date,
                                         filter = lowfilteroxfiltratedailyA2,
                                         nofilter = filteroxfiltratedailyA2$data)
lowpassoxfiltratedailyA2fin <- lowpassoxfiltratedailyA2 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassoxfiltratedailyA2fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Oxalate Filtrate (%)") + 
  ggtitle("2A Drum Oxalate Filtrate (Daily Data)")


# 1B Drum----------------------------------------------------------------------------------------
# 1B feed flow
filterfeedflowdailyB1 <- na.omit(filterfeedflowdailyB1)
lowfilterfeedflowdailyB1 <- filtfilt(lowbf, filterfeedflowdailyB1$data)
lowpassfeedflowdailyB1 <- data.frame(time = filterfeedflowdailyB1$time,
                                       date = filterfeedflowdailyB1$date,
                                       filter = lowfilterfeedflowdailyB1,
                                       nofilter = filterfeedflowdailyB1$data)
lowpassfeedflowdailyB1fin <- lowpassfeedflowdailyB1 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassfeedflowdailyB1fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Feed Flow (kl/h)") + 
  ggtitle("1B Drum Feed Flow (Daily Data)")

# 1B drum speed
filterdrumspeeddailyB1 <- na.omit(filterdrumspeeddailyB1)
lowfilterdrumspeeddailyB1 <- filtfilt(lowbf, filterdrumspeeddailyB1$data)
lowpassdrumspeeddailyB1 <- data.frame(time = filterdrumspeeddailyB1$time,
                                        date = filterdrumspeeddailyB1$date,
                                        drumspeed = lowfilterdrumspeeddailyB1)
lowpassdrumspeeddailyB1fin <- lowpassdrumspeeddailyB1 %>%
  gather(type, data, drumspeed)

# 1B bath level
filterbathleveldailyB1 <- na.omit(filterbathleveldailyB1)
lowfilterbathleveldailyB1 <- filtfilt(lowbf, filterbathleveldailyB1$data)
lowpassbathleveldailyB1 <- data.frame(time = filterbathleveldailyB1$time,
                                        date = filterbathleveldailyB1$date,
                                        bathlevel = lowfilterbathleveldailyB1)
lowpassbathleveldailyB1fin <- lowpassbathleveldailyB1 %>%
  gather(type, data, bathlevel)

# 1B vacuum 
filtervacuumdailyB1 <- na.omit(filtervacuumdailyB1)
lowfiltervacuumdailyB1 <- filtfilt(lowbf, filtervacuumdailyB1$data)
lowpassvacuumdailyB1 <- data.frame(time = filtervacuumdailyB1$time,
                                     date = filtervacuumdailyB1$date,
                                     vacuum = lowfiltervacuumdailyB1)
lowpassvacuumdailyB1fin <- lowpassvacuumdailyB1 %>%
  gather(type, data, vacuum)

# 1B cake wash
filtercakewashdailyB1 <- na.omit(filtercakewashdailyB1)
lowfiltercakewashdailyB1 <- filtfilt(lowbf, filtercakewashdailyB1$data)
lowpasscakewashdailyB1 <- data.frame(time = filtercakewashdailyB1$time,
                                       date = filtercakewashdailyB1$date,
                                       cakewash = lowfiltercakewashdailyB1)
lowpasscakewashdailyB1fin <- lowpasscakewashdailyB1 %>%
  gather(type, data, cakewash)

# 1B soda filtrate conc.
filtersodafiltratedailyB1 <- na.omit(filtersodafiltratedailyB1)
lowfiltersodafiltratedailyB1 <- filtfilt(lowbf, filtersodafiltratedailyB1$data)
lowpasssodafiltratedailyB1 <- data.frame(time = filtersodafiltratedailyB1$time,
                                           date = filtersodafiltratedailyB1$date,
                                           filter = lowfiltersodafiltratedailyB1,
                                           nofilter = filtersodafiltratedailyB1$data)
lowpasssodafiltratedailyB1fin <- lowpasssodafiltratedailyB1 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpasssodafiltratedailyB1fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Soda Filtrate Conc. (g/l)") + 
  ggtitle("1B Drum Soda Filtrate Conc. (Daily Data)")


# 1B oxalate filtrate
filteroxfiltratedailyB1 <- na.omit(filteroxfiltratedailyB1)
lowfilteroxfiltratedailyB1 <- filtfilt(lowbf, filteroxfiltratedailyB1$data)
lowpassoxfiltratedailyB1 <- data.frame(time = filteroxfiltratedailyB1$time,
                                         date = filteroxfiltratedailyB1$date,
                                         filter = lowfilteroxfiltratedailyB1,
                                         nofilter = filteroxfiltratedailyB1$data)
lowpassoxfiltratedailyB1fin <- lowpassoxfiltratedailyB1 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassoxfiltratedailyB1fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Oxalate Filtrate (%)") + 
  ggtitle("1B Drum Oxalate Filtrate (Daily Data)")


# 2B Drum----------------------------------------------------------------------------------------
# 2B feed flow
filterfeedflowdailyB2 <- na.omit(filterfeedflowdailyB2)
lowfilterfeedflowdailyB2 <- filtfilt(lowbf, filterfeedflowdailyB2$data)
lowpassfeedflowdailyB2 <- data.frame(time = filterfeedflowdailyB2$time,
                                       date = filterfeedflowdailyB2$date,
                                       filter = lowfilterfeedflowdailyB2,
                                       nofilter = filterfeedflowdailyB2$data)
lowpassfeedflowdailyB2fin <- lowpassfeedflowdailyB2 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassfeedflowdailyB2fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Feed Flow (kl/h)") + 
  ggtitle("2B Drum Feed Flow (Daily Data)")

# 2B drum speed
filterdrumspeeddailyB2 <- na.omit(filterdrumspeeddailyB2)
lowfilterdrumspeeddailyB2 <- filtfilt(lowbf, filterdrumspeeddailyB2$data)
lowpassdrumspeeddailyB2 <- data.frame(time = filterdrumspeeddailyB2$time,
                                        date = filterdrumspeeddailyB2$date,
                                        drumspeed = lowfilterdrumspeeddailyB2)
lowpassdrumspeeddailyB2fin <- lowpassdrumspeeddailyB2 %>%
  gather(type, data, drumspeed)

# 2B bath level
filterbathleveldailyB2 <- na.omit(filterbathleveldailyB2)
lowfilterbathleveldailyB2 <- filtfilt(lowbf, filterbathleveldailyB2$data)
lowpassbathleveldailyB2 <- data.frame(time = filterbathleveldailyB2$time,
                                        date = filterbathleveldailyB2$date,
                                        bathlevel = lowfilterbathleveldailyB2)
lowpassbathleveldailyB2fin <- lowpassbathleveldailyB2 %>%
  gather(type, data, bathlevel)

# 2B vacuum 
filtervacuumdailyB2 <- na.omit(filtervacuumdailyB2)
lowfiltervacuumdailyB2 <- filtfilt(lowbf, filtervacuumdailyB2$data)
lowpassvacuumdailyB2 <- data.frame(time = filtervacuumdailyB2$time,
                                     date = filtervacuumdailyB2$date,
                                     vacuum = lowfiltervacuumdailyB2)
lowpassvacuumdailyB2fin <- lowpassvacuumdailyB2 %>%
  gather(type, data, vacuum)


# 2B cake wash
filtercakewashdailyB2 <- na.omit(filtercakewashdailyB2)
lowfiltercakewashdailyB2 <- filtfilt(lowbf, filtercakewashdailyB2$data)
lowpasscakewashdailyB2 <- data.frame(time = filtercakewashdailyB2$time,
                                       date = filtercakewashdailyB2$date,
                                       filter = lowfiltercakewashdailyB2,
                                       nofilter = filtercakewashdailyB2$data)
lowpasscakewashdailyB2fin <- lowpasscakewashdailyB2 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpasscakewashdailyB2fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") + 
  ggtitle("2B Drum Cake Wash Flow (Daily Data)")

# 2B soda filtrate conc.
filtersodafiltratedailyB2 <- na.omit(filtersodafiltratedailyB2)
lowfiltersodafiltratedailyB2 <- filtfilt(lowbf, filtersodafiltratedailyB2$data)
lowpasssodafiltratedailyB2 <- data.frame(time = filtersodafiltratedailyB2$time,
                                           date = filtersodafiltratedailyB2$date,
                                           filter = lowfiltersodafiltratedailyB2,
                                           nofilter = filtersodafiltratedailyB2$data)
lowpasssodafiltratedailyB2fin <- lowpasssodafiltratedailyB2 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpasssodafiltratedailyB2fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Soda Filtrate Conc. (g/l)") + 
  ggtitle("2B Drum Soda Filtrate Conc. (Daily Data)")


# 2B oxalate filtrate
filteroxfiltratedailyB2 <- na.omit(filteroxfiltratedailyB2)
lowfilteroxfiltratedailyB2 <- filtfilt(lowbf, filteroxfiltratedailyB2$data)
lowpassoxfiltratedailyB2 <- data.frame(time = filteroxfiltratedailyB2$time,
                                         date = filteroxfiltratedailyB2$date,
                                         filter = lowfilteroxfiltratedailyB2,
                                         nofilter = filteroxfiltratedailyB2$data)
lowpassoxfiltratedailyB2fin <- lowpassoxfiltratedailyB2 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassoxfiltratedailyB2fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Oxalate Filtrate (%)") + 
  ggtitle("2B Drum Oxalate Filtrate (Daily Data)")





#------------------------------------------------------------------------------------------------
#fourier analysis--------------------------------------------------------------------------------
#throughput
throughputdaily.ts <- ts(filterthroughputdaily$data) #change to time series
throughputdaily.freq <- (1/(24*60*60))  #sample frequency in Hz 
throughputdaily.dur <- length(throughputdaily.ts)*24*60*60 # length of signal in seconds 
throughputdaily.tot <- throughputdaily.freq*throughputdaily.dur #total number of sample

throughputdaily.x <- seq(0, throughputdaily.dur, length.out = throughputdaily.tot)
throughputdaily.fourier <- fft(throughputdaily.ts)
throughputdaily.amo <- 2*Mod(throughputdaily.fourier)/(throughputdaily.tot) #amplitude
throughputdaily.freqvec <- 0:(length(throughputdaily.amo)-1) #vector from 0 to end of signal -> new x-axis

throughputdaily.amo[throughputdaily.freqvec == 0] <- 
  throughputdaily.amo[throughputdaily.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
throughputdaily.freqvec <-
  throughputdaily.freqvec/throughputdaily.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
throughputdaily.dfFouri <- data.frame(freq = throughputdaily.freqvec[1:as.integer(0.5*throughputdaily.freq*throughputdaily.dur)]*60*60*24,
                                        amount = throughputdaily.amo[1:as.integer(0.5*throughputdaily.freq*throughputdaily.dur)])
throughputdaily.dfFouri <- throughputdaily.dfFouri %>%
  mutate(period = ifelse(throughputdaily.dfFouri$amount >= 1,
                         paste(round(1/throughputdaily.dfFouri$freq,2),"days"),""))
throughputdaily.dfFouri[1,3] <- ""
ggplot(throughputdaily.dfFouri,aes(x=freq,y=amount)) + 
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label = period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) +
  labs(x = "Freq, 1/day",y = "Magnitude", title = "Throughput Fourier Analysis")

# 1A Drum----------------------------------------------------------------------------------------
# 1A feed flow
feedflowdailyA1.ts <- ts(filterfeedflowdailyA1$data) #change to time series
feedflowdailyA1.freq <- (1/(30*60))  #sample frequency in Hz 
feedflowdailyA1.dur <- length(feedflowdailyA1.ts)*30*60 # length of signal in seconds 
feedflowdailyA1.tot <- feedflowdailyA1.freq*feedflowdailyA1.dur #total number of sample

feedflowdailyA1.x <- seq(0, feedflowdailyA1.dur, length.out = tot)
feedflowdailyA1.fourier <- fft(feedflowdailyA1.ts)
feedflowdailyA1.amo <- 2*Mod(feedflowdailyA1.fourier)/(feedflowdailyA1.tot) #amplitude
feedflowdailyA1.freqvec <- 0:(length(feedflowdailyA1.amo)-1) #vector from 0 to end of signal -> new x-axis

feedflowdailyA1.amo[feedflowdailyA1.freqvec == 0] <-
  feedflowdailyA1.amo[feedflowdailyA1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
feedflowdailyA1.freqvec <- feedflowdailyA1.freqvec/feedflowdailyA1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
feedflowdailyA1.dfFouri <- data.frame(freq = feedflowdailyA1.freqvec[1:as.integer(0.5*feedflowdailyA1.freq*feedflowdailyA1.dur)]*60*60*24,
                                        amount = feedflowdailyA1.amo[1:as.integer(0.5*feedflowdailyA1.freq*feedflowdailyA1.dur)])
feedflowdailyA1.dfFouri <- feedflowdailyA1.dfFouri %>%
  mutate(period = ifelse(feedflowdailyA1.dfFouri$amount >= 1,
                         paste(round(1/feedflowdailyA1.dfFouri$freq, 2),"days"),""))
feedflowdailyA1.dfFouri[1,3] <- ""
ggplot(feedflowdailyA1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1A Drum Feed Flow Fourier Analysis")

# 1A drum speed
drumspeeddailyA1.ts <- ts(filterdrumspeeddailyA1$data) #change to time series
drumspeeddailyA1.freq <- (1/(30*60))  #sample frequency in Hz 
drumspeeddailyA1.dur <- length(drumspeeddailyA1.ts)*30*60 # length of signal in seconds 
drumspeeddailyA1.tot <- drumspeeddailyA1.freq*drumspeeddailyA1.dur #total number of sample

drumspeeddailyA1.x <- seq(0, drumspeeddailyA1.dur, length.out = tot)
drumspeeddailyA1.fourier <- fft(drumspeeddailyA1.ts)
drumspeeddailyA1.amo <- 2*Mod(drumspeeddailyA1.fourier)/(drumspeeddailyA1.tot) #amplitude
drumspeeddailyA1.freqvec <- 0:(length(drumspeeddailyA1.amo)-1) #vector from 0 to end of signal -> new x-axis

drumspeeddailyA1.amo[drumspeeddailyA1.freqvec == 0] <-
  drumspeeddailyA1.amo[drumspeeddailyA1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
drumspeeddailyA1.freqvec <- drumspeeddailyA1.freqvec/drumspeeddailyA1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
drumspeeddailyA1.dfFouri <- data.frame(freq = drumspeeddailyA1.freqvec[1:as.integer(0.5*drumspeeddailyA1.freq*drumspeeddailyA1.dur)]*60*60*24,
                                         amount = drumspeeddailyA1.amo[1:as.integer(0.5*drumspeeddailyA1.freq*drumspeeddailyA1.dur)])
drumspeeddailyA1.dfFouri <- drumspeeddailyA1.dfFouri %>%
  mutate(period = ifelse(drumspeeddailyA1.dfFouri$amount >= 1,
                         paste(round(1/drumspeeddailyA1.dfFouri$freq, 2),"days"),""))
drumspeeddailyA1.dfFouri[1,3] <- ""
ggplot(drumspeeddailyA1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1A Drum Speed Fourier Analysis")

# 1A bath level
bathleveldailyA1.ts <- ts(filterbathleveldailyA1$data) #change to time series
bathleveldailyA1.freq <- (1/(30*60))  #sample frequency in Hz 
bathleveldailyA1.dur <- length(bathleveldailyA1.ts)*30*60 # length of signal in seconds 
bathleveldailyA1.tot <- bathleveldailyA1.freq*bathleveldailyA1.dur #total number of sample

bathleveldailyA1.x <- seq(0, bathleveldailyA1.dur, length.out = tot)
bathleveldailyA1.fourier <- fft(bathleveldailyA1.ts)
bathleveldailyA1.amo <- 2*Mod(bathleveldailyA1.fourier)/(bathleveldailyA1.tot) #amplitude
bathleveldailyA1.freqvec <- 0:(length(bathleveldailyA1.amo)-1) #vector from 0 to end of signal -> new x-axis

bathleveldailyA1.amo[bathleveldailyA1.freqvec == 0] <-
  bathleveldailyA1.amo[bathleveldailyA1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
bathleveldailyA1.freqvec <- bathleveldailyA1.freqvec/bathleveldailyA1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
bathleveldailyA1.dfFouri <- data.frame(freq = bathleveldailyA1.freqvec[1:as.integer(0.5*bathleveldailyA1.freq*bathleveldailyA1.dur)]*60*60*24,
                                         amount = bathleveldailyA1.amo[1:as.integer(0.5*bathleveldailyA1.freq*bathleveldailyA1.dur)])
bathleveldailyA1.dfFouri <- bathleveldailyA1.dfFouri %>%
  mutate(period = ifelse(bathleveldailyA1.dfFouri$amount >= 1,
                         paste(round(1/bathleveldailyA1.dfFouri$freq, 2),"days"),""))
bathleveldailyA1.dfFouri[1,3] <- ""
ggplot(bathleveldailyA1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1A Drum Bath Level Fourier Analysis")

# 1A vacuum
vacuumdailyA1.ts <- ts(filtervacuumdailyA1$data) #change to time series
vacuumdailyA1.freq <- (1/(30*60))  #sample frequency in Hz 
vacuumdailyA1.dur <- length(vacuumdailyA1.ts)*30*60 # length of signal in seconds 
vacuumdailyA1.tot <- vacuumdailyA1.freq*vacuumdailyA1.dur #total number of sample

vacuumdailyA1.x <- seq(0, vacuumdailyA1.dur, length.out = tot)
vacuumdailyA1.fourier <- fft(vacuumdailyA1.ts)
vacuumdailyA1.amo <- 2*Mod(vacuumdailyA1.fourier)/(vacuumdailyA1.tot) #amplitude
vacuumdailyA1.freqvec <- 0:(length(vacuumdailyA1.amo)-1) #vector from 0 to end of signal -> new x-axis

vacuumdailyA1.amo[vacuumdailyA1.freqvec == 0] <-
  vacuumdailyA1.amo[vacuumdailyA1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
vacuumdailyA1.freqvec <- vacuumdailyA1.freqvec/vacuumdailyA1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
vacuumdailyA1.dfFouri <- data.frame(freq = vacuumdailyA1.freqvec[1:as.integer(0.5*vacuumdailyA1.freq*vacuumdailyA1.dur)]*60*60*24,
                                      amount = vacuumdailyA1.amo[1:as.integer(0.5*vacuumdailyA1.freq*vacuumdailyA1.dur)])
vacuumdailyA1.dfFouri <- vacuumdailyA1.dfFouri %>%
  mutate(period = ifelse(vacuumdailyA1.dfFouri$amount >= 1,
                         paste(round(1/vacuumdailyA1.dfFouri$freq, 2),"days"),""))
vacuumdailyA1.dfFouri[1,3] <- ""
ggplot(vacuumdailyA1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1A Drum Vacuum Pressure Fourier Analysis")


# 1A cake wash
cakewashdailyA1.ts <- ts(filtercakewashdailyA1$data) #change to time series
cakewashdailyA1.freq <- (1/(30*60))  #sample frequency in Hz 
cakewashdailyA1.dur <- length(cakewashdailyA1.ts)*30*60 # length of signal in seconds 
cakewashdailyA1.tot <- cakewashdailyA1.freq*cakewashdailyA1.dur #total number of sample

cakewashdailyA1.x <- seq(0, cakewashdailyA1.dur, length.out = tot)
cakewashdailyA1.fourier <- fft(cakewashdailyA1.ts)
cakewashdailyA1.amo <- 2*Mod(cakewashdailyA1.fourier)/(cakewashdailyA1.tot) #amplitude
cakewashdailyA1.freqvec <- 0:(length(cakewashdailyA1.amo)-1) #vector from 0 to end of signal -> new x-axis

cakewashdailyA1.amo[cakewashdailyA1.freqvec == 0] <-
  cakewashdailyA1.amo[cakewashdailyA1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
cakewashdailyA1.freqvec <- cakewashdailyA1.freqvec/cakewashdailyA1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
cakewashdailyA1.dfFouri <- data.frame(freq = cakewashdailyA1.freqvec[1:as.integer(0.5*cakewashdailyA1.freq*cakewashdailyA1.dur)]*60*60*24,
                                        amount = cakewashdailyA1.amo[1:as.integer(0.5*cakewashdailyA1.freq*cakewashdailyA1.dur)])
cakewashdailyA1.dfFouri <- cakewashdailyA1.dfFouri %>%
  mutate(period = ifelse(cakewashdailyA1.dfFouri$amount >= 1,
                         paste(round(1/cakewashdailyA1.dfFouri$freq, 2),"days"),""))
cakewashdailyA1.dfFouri[1,3] <- ""
ggplot(cakewashdailyA1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1A Drum Cake Wash Fourier Analysis")

# 1A soda filtrate conc.
sodafiltratedailyA1.ts <- ts(filtersodafiltratedailyA1$data) #change to time series
sodafiltratedailyA1.freq <- (1/(30*60))  #sample frequency in Hz 
sodafiltratedailyA1.dur <- length(sodafiltratedailyA1.ts)*30*60 # length of signal in seconds 
sodafiltratedailyA1.tot <- sodafiltratedailyA1.freq*sodafiltratedailyA1.dur #total number of sample

sodafiltratedailyA1.x <- seq(0, sodafiltratedailyA1.dur, length.out = tot)
sodafiltratedailyA1.fourier <- fft(sodafiltratedailyA1.ts)
sodafiltratedailyA1.amo <- 2*Mod(sodafiltratedailyA1.fourier)/(sodafiltratedailyA1.tot) #amplitude
sodafiltratedailyA1.freqvec <- 0:(length(sodafiltratedailyA1.amo)-1) #vector from 0 to end of signal -> new x-axis

sodafiltratedailyA1.amo[sodafiltratedailyA1.freqvec == 0] <-
  sodafiltratedailyA1.amo[sodafiltratedailyA1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
sodafiltratedailyA1.freqvec <- sodafiltratedailyA1.freqvec/sodafiltratedailyA1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
sodafiltratedailyA1.dfFouri <- data.frame(freq = sodafiltratedailyA1.freqvec[1:as.integer(0.5*sodafiltratedailyA1.freq*sodafiltratedailyA1.dur)]*60*60*24,
                                            amount = sodafiltratedailyA1.amo[1:as.integer(0.5*sodafiltratedailyA1.freq*sodafiltratedailyA1.dur)])
sodafiltratedailyA1.dfFouri <- sodafiltratedailyA1.dfFouri %>%
  mutate(period = ifelse(sodafiltratedailyA1.dfFouri$amount >= 1,
                         paste(round(1/sodafiltratedailyA1.dfFouri$freq, 2),"days"),""))
sodafiltratedailyA1.dfFouri[1,3] <- ""
ggplot(sodafiltratedailyA1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1A Drum Soda Filtrate Fourier Analysis")


# 1A oxalate filtrate
oxfiltratedailyA1.ts <- ts(filteroxfiltratedailyA1$data) #change to time series
oxfiltratedailyA1.freq <- (1/(30*60))  #sample frequency in Hz 
oxfiltratedailyA1.dur <- length(oxfiltratedailyA1.ts)*30*60 # length of signal in seconds 
oxfiltratedailyA1.tot <- oxfiltratedailyA1.freq*oxfiltratedailyA1.dur #total number of sample

oxfiltratedailyA1.x <- seq(0, oxfiltratedailyA1.dur, length.out = tot)
oxfiltratedailyA1.fourier <- fft(oxfiltratedailyA1.ts)
oxfiltratedailyA1.amo <- 2*Mod(oxfiltratedailyA1.fourier)/(oxfiltratedailyA1.tot) #amplitude
oxfiltratedailyA1.freqvec <- 0:(length(oxfiltratedailyA1.amo)-1) #vector from 0 to end of signal -> new x-axis

oxfiltratedailyA1.amo[oxfiltratedailyA1.freqvec == 0] <-
  oxfiltratedailyA1.amo[oxfiltratedailyA1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
oxfiltratedailyA1.freqvec <- oxfiltratedailyA1.freqvec/oxfiltratedailyA1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
oxfiltratedailyA1.dfFouri <- data.frame(freq = oxfiltratedailyA1.freqvec[1:as.integer(0.5*oxfiltratedailyA1.freq*oxfiltratedailyA1.dur)]*60*60*24,
                                          amount = oxfiltratedailyA1.amo[1:as.integer(0.5*oxfiltratedailyA1.freq*oxfiltratedailyA1.dur)])
oxfiltratedailyA1.dfFouri <- oxfiltratedailyA1.dfFouri %>%
  mutate(period = ifelse(oxfiltratedailyA1.dfFouri$amount >= 1,
                         paste(round(1/oxfiltratedailyA1.dfFouri$freq, 2),"days"),""))
oxfiltratedailyA1.dfFouri[1,3] <- ""
ggplot(oxfiltratedailyA1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1A Drum Oxalate Filtrate Fourier Analysis")


# 2A Drum----------------------------------------------------------------------------------------
# 2A feed flow
feedflowdailyA2.ts <- ts(filterfeedflowdailyA2$data) #change to time series
feedflowdailyA2.freq <- (1/(30*60))  #sample frequency in Hz 
feedflowdailyA2.dur <- length(feedflowdailyA2.ts)*30*60 # length of signal in seconds 
feedflowdailyA2.tot <- feedflowdailyA2.freq*feedflowdailyA2.dur #total number of sample

feedflowdailyA2.x <- seq(0, feedflowdailyA2.dur, length.out = tot)
feedflowdailyA2.fourier <- fft(feedflowdailyA2.ts)
feedflowdailyA2.amo <- 2*Mod(feedflowdailyA2.fourier)/(feedflowdailyA2.tot) #amplitude
feedflowdailyA2.freqvec <- 0:(length(feedflowdailyA2.amo)-1) #vector from 0 to end of signal -> new x-axis

feedflowdailyA2.amo[feedflowdailyA2.freqvec == 0] <-
  feedflowdailyA2.amo[feedflowdailyA2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
feedflowdailyA2.freqvec <- feedflowdailyA2.freqvec/feedflowdailyA2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
feedflowdailyA2.dfFouri <- data.frame(freq = feedflowdailyA2.freqvec[1:as.integer(0.5*feedflowdailyA2.freq*feedflowdailyA2.dur)]*60*60*24,
                                        amount = feedflowdailyA2.amo[1:as.integer(0.5*feedflowdailyA2.freq*feedflowdailyA2.dur)])
feedflowdailyA2.dfFouri <- feedflowdailyA2.dfFouri %>%
  mutate(period = ifelse(feedflowdailyA2.dfFouri$amount >= 1,
                         paste(round(1/feedflowdailyA2.dfFouri$freq, 2),"days"),""))
feedflowdailyA2.dfFouri[1,3] <- ""
ggplot(feedflowdailyA2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2A Drum Feed Flow Fourier Analysis")

# 2A drum speed
drumspeeddailyA2.ts <- ts(filterdrumspeeddailyA2$data) #change to time series
drumspeeddailyA2.freq <- (1/(30*60))  #sample frequency in Hz 
drumspeeddailyA2.dur <- length(drumspeeddailyA2.ts)*30*60 # length of signal in seconds 
drumspeeddailyA2.tot <- drumspeeddailyA2.freq*drumspeeddailyA2.dur #total number of sample

drumspeeddailyA2.x <- seq(0, drumspeeddailyA2.dur, length.out = tot)
drumspeeddailyA2.fourier <- fft(drumspeeddailyA2.ts)
drumspeeddailyA2.amo <- 2*Mod(drumspeeddailyA2.fourier)/(drumspeeddailyA2.tot) #amplitude
drumspeeddailyA2.freqvec <- 0:(length(drumspeeddailyA2.amo)-1) #vector from 0 to end of signal -> new x-axis

drumspeeddailyA2.amo[drumspeeddailyA2.freqvec == 0] <-
  drumspeeddailyA2.amo[drumspeeddailyA2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
drumspeeddailyA2.freqvec <- drumspeeddailyA2.freqvec/drumspeeddailyA2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
drumspeeddailyA2.dfFouri <- data.frame(freq = drumspeeddailyA2.freqvec[1:as.integer(0.5*drumspeeddailyA2.freq*drumspeeddailyA2.dur)]*60*60*24,
                                         amount = drumspeeddailyA2.amo[1:as.integer(0.5*drumspeeddailyA2.freq*drumspeeddailyA2.dur)])
drumspeeddailyA2.dfFouri <- drumspeeddailyA2.dfFouri %>%
  mutate(period = ifelse(drumspeeddailyA2.dfFouri$amount >= 1,
                         paste(round(1/drumspeeddailyA2.dfFouri$freq, 2),"days"),""))
drumspeeddailyA2.dfFouri[1,3] <- ""
ggplot(drumspeeddailyA2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2A Drum Speed Fourier Analysis")

# 2A bath level
bathleveldailyA2.ts <- ts(filterbathleveldailyA2$data) #change to time series
bathleveldailyA2.freq <- (1/(30*60))  #sample frequency in Hz 
bathleveldailyA2.dur <- length(bathleveldailyA2.ts)*30*60 # length of signal in seconds 
bathleveldailyA2.tot <- bathleveldailyA2.freq*bathleveldailyA2.dur #total number of sample

bathleveldailyA2.x <- seq(0, bathleveldailyA2.dur, length.out = tot)
bathleveldailyA2.fourier <- fft(bathleveldailyA2.ts)
bathleveldailyA2.amo <- 2*Mod(bathleveldailyA2.fourier)/(bathleveldailyA2.tot) #amplitude
bathleveldailyA2.freqvec <- 0:(length(bathleveldailyA2.amo)-1) #vector from 0 to end of signal -> new x-axis

bathleveldailyA2.amo[bathleveldailyA2.freqvec == 0] <-
  bathleveldailyA2.amo[bathleveldailyA2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
bathleveldailyA2.freqvec <- bathleveldailyA2.freqvec/bathleveldailyA2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
bathleveldailyA2.dfFouri <- data.frame(freq = bathleveldailyA2.freqvec[1:as.integer(0.5*bathleveldailyA2.freq*bathleveldailyA2.dur)]*60*60*24,
                                         amount = bathleveldailyA2.amo[1:as.integer(0.5*bathleveldailyA2.freq*bathleveldailyA2.dur)])
bathleveldailyA2.dfFouri <- bathleveldailyA2.dfFouri %>%
  mutate(period = ifelse(bathleveldailyA2.dfFouri$amount >= 1,
                         paste(round(1/bathleveldailyA2.dfFouri$freq, 2),"days"),""))
bathleveldailyA2.dfFouri[1,3] <- ""
ggplot(bathleveldailyA2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2A Drum Bath Level Fourier Analysis")

# 2A vacuum
vacuumdailyA2.ts <- ts(filtervacuumdailyA2$data) #change to time series
vacuumdailyA2.freq <- (1/(30*60))  #sample frequency in Hz 
vacuumdailyA2.dur <- length(vacuumdailyA2.ts)*30*60 # length of signal in seconds 
vacuumdailyA2.tot <- vacuumdailyA2.freq*vacuumdailyA2.dur #total number of sample

vacuumdailyA2.x <- seq(0, vacuumdailyA2.dur, length.out = tot)
vacuumdailyA2.fourier <- fft(vacuumdailyA2.ts)
vacuumdailyA2.amo <- 2*Mod(vacuumdailyA2.fourier)/(vacuumdailyA2.tot) #amplitude
vacuumdailyA2.freqvec <- 0:(length(vacuumdailyA2.amo)-1) #vector from 0 to end of signal -> new x-axis

vacuumdailyA2.amo[vacuumdailyA2.freqvec == 0] <-
  vacuumdailyA2.amo[vacuumdailyA2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
vacuumdailyA2.freqvec <- vacuumdailyA2.freqvec/vacuumdailyA2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
vacuumdailyA2.dfFouri <- data.frame(freq = vacuumdailyA2.freqvec[1:as.integer(0.5*vacuumdailyA2.freq*vacuumdailyA2.dur)]*60*60*24,
                                      amount = vacuumdailyA2.amo[1:as.integer(0.5*vacuumdailyA2.freq*vacuumdailyA2.dur)])
vacuumdailyA2.dfFouri <- vacuumdailyA2.dfFouri %>%
  mutate(period = ifelse(vacuumdailyA2.dfFouri$amount >= 1,
                         paste(round(1/vacuumdailyA2.dfFouri$freq, 2),"days"),""))
vacuumdailyA2.dfFouri[1,3] <- ""
ggplot(vacuumdailyA2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2A Drum Vacuum Pressure Fourier Analysis")


# 2A cake wash
cakewashdailyA2.ts <- ts(filtercakewashdailyA2$data) #change to time series
cakewashdailyA2.freq <- (1/(30*60))  #sample frequency in Hz 
cakewashdailyA2.dur <- length(cakewashdailyA2.ts)*30*60 # length of signal in seconds 
cakewashdailyA2.tot <- cakewashdailyA2.freq*cakewashdailyA2.dur #total number of sample

cakewashdailyA2.x <- seq(0, cakewashdailyA2.dur, length.out = tot)
cakewashdailyA2.fourier <- fft(cakewashdailyA2.ts)
cakewashdailyA2.amo <- 2*Mod(cakewashdailyA2.fourier)/(cakewashdailyA2.tot) #amplitude
cakewashdailyA2.freqvec <- 0:(length(cakewashdailyA2.amo)-1) #vector from 0 to end of signal -> new x-axis

cakewashdailyA2.amo[cakewashdailyA2.freqvec == 0] <-
  cakewashdailyA2.amo[cakewashdailyA2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
cakewashdailyA2.freqvec <- cakewashdailyA2.freqvec/cakewashdailyA2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
cakewashdailyA2.dfFouri <- data.frame(freq = cakewashdailyA2.freqvec[1:as.integer(0.5*cakewashdailyA2.freq*cakewashdailyA2.dur)]*60*60*24,
                                        amount = cakewashdailyA2.amo[1:as.integer(0.5*cakewashdailyA2.freq*cakewashdailyA2.dur)])
cakewashdailyA2.dfFouri <- cakewashdailyA2.dfFouri %>%
  mutate(period = ifelse(cakewashdailyA2.dfFouri$amount >= 1,
                         paste(round(1/cakewashdailyA2.dfFouri$freq, 2),"days"),""))
cakewashdailyA2.dfFouri[1,3] <- ""
ggplot(cakewashdailyA2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2A Drum Cake Wash Fourier Analysis")

# 2A soda filtrate conc.
sodafiltratedailyA2.ts <- ts(filtersodafiltratedailyA2$data) #change to time series
sodafiltratedailyA2.freq <- (1/(30*60))  #sample frequency in Hz 
sodafiltratedailyA2.dur <- length(sodafiltratedailyA2.ts)*30*60 # length of signal in seconds 
sodafiltratedailyA2.tot <- sodafiltratedailyA2.freq*sodafiltratedailyA2.dur #total number of sample

sodafiltratedailyA2.x <- seq(0, sodafiltratedailyA2.dur, length.out = tot)
sodafiltratedailyA2.fourier <- fft(sodafiltratedailyA2.ts)
sodafiltratedailyA2.amo <- 2*Mod(sodafiltratedailyA2.fourier)/(sodafiltratedailyA2.tot) #amplitude
sodafiltratedailyA2.freqvec <- 0:(length(sodafiltratedailyA2.amo)-1) #vector from 0 to end of signal -> new x-axis

sodafiltratedailyA2.amo[sodafiltratedailyA2.freqvec == 0] <-
  sodafiltratedailyA2.amo[sodafiltratedailyA2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
sodafiltratedailyA2.freqvec <- sodafiltratedailyA2.freqvec/sodafiltratedailyA2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
sodafiltratedailyA2.dfFouri <- data.frame(freq = sodafiltratedailyA2.freqvec[1:as.integer(0.5*sodafiltratedailyA2.freq*sodafiltratedailyA2.dur)]*60*60*24,
                                            amount = sodafiltratedailyA2.amo[1:as.integer(0.5*sodafiltratedailyA2.freq*sodafiltratedailyA2.dur)])
sodafiltratedailyA2.dfFouri <- sodafiltratedailyA2.dfFouri %>%
  mutate(period = ifelse(sodafiltratedailyA2.dfFouri$amount >= 1,
                         paste(round(1/sodafiltratedailyA2.dfFouri$freq, 2),"days"),""))
sodafiltratedailyA2.dfFouri[1,3] <- ""
ggplot(sodafiltratedailyA2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2A Drum Soda Filtrate Fourier Analysis")


# 2A oxalate filtrate
oxfiltratedailyA2.ts <- ts(filteroxfiltratedailyA2$data) #change to time series
oxfiltratedailyA2.freq <- (1/(30*60))  #sample frequency in Hz 
oxfiltratedailyA2.dur <- length(oxfiltratedailyA2.ts)*30*60 # length of signal in seconds 
oxfiltratedailyA2.tot <- oxfiltratedailyA2.freq*oxfiltratedailyA2.dur #total number of sample

oxfiltratedailyA2.x <- seq(0, oxfiltratedailyA2.dur, length.out = tot)
oxfiltratedailyA2.fourier <- fft(oxfiltratedailyA2.ts)
oxfiltratedailyA2.amo <- 2*Mod(oxfiltratedailyA2.fourier)/(oxfiltratedailyA2.tot) #amplitude
oxfiltratedailyA2.freqvec <- 0:(length(oxfiltratedailyA2.amo)-1) #vector from 0 to end of signal -> new x-axis

oxfiltratedailyA2.amo[oxfiltratedailyA2.freqvec == 0] <-
  oxfiltratedailyA2.amo[oxfiltratedailyA2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
oxfiltratedailyA2.freqvec <- oxfiltratedailyA2.freqvec/oxfiltratedailyA2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
oxfiltratedailyA2.dfFouri <- data.frame(freq = oxfiltratedailyA2.freqvec[1:as.integer(0.5*oxfiltratedailyA2.freq*oxfiltratedailyA2.dur)]*60*60*24,
                                          amount = oxfiltratedailyA2.amo[1:as.integer(0.5*oxfiltratedailyA2.freq*oxfiltratedailyA2.dur)])
oxfiltratedailyA2.dfFouri <- oxfiltratedailyA2.dfFouri %>%
  mutate(period = ifelse(oxfiltratedailyA2.dfFouri$amount >= 1,
                         paste(round(1/oxfiltratedailyA2.dfFouri$freq, 2),"days"),""))
oxfiltratedailyA2.dfFouri[1,3] <- ""
ggplot(oxfiltratedailyA2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2A Drum Oxalate Filtrate Fourier Analysis")

# 1B Drum----------------------------------------------------------------------------------------
# 1B feed flow
feedflowdailyB1.ts <- ts(filterfeedflowdailyB1$data) #change to time series
feedflowdailyB1.freq <- (1/(30*60))  #sample frequency in Hz 
feedflowdailyB1.dur <- length(feedflowdailyB1.ts)*30*60 # length of signal in seconds 
feedflowdailyB1.tot <- feedflowdailyB1.freq*feedflowdailyB1.dur #total number of sample

feedflowdailyB1.x <- seq(0, feedflowdailyB1.dur, length.out = tot)
feedflowdailyB1.fourier <- fft(feedflowdailyB1.ts)
feedflowdailyB1.amo <- 2*Mod(feedflowdailyB1.fourier)/(feedflowdailyB1.tot) #amplitude
feedflowdailyB1.freqvec <- 0:(length(feedflowdailyB1.amo)-1) #vector from 0 to end of signal -> new x-axis

feedflowdailyB1.amo[feedflowdailyB1.freqvec == 0] <-
  feedflowdailyB1.amo[feedflowdailyB1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
feedflowdailyB1.freqvec <- feedflowdailyB1.freqvec/feedflowdailyB1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
feedflowdailyB1.dfFouri <- data.frame(freq = feedflowdailyB1.freqvec[1:as.integer(0.5*feedflowdailyB1.freq*feedflowdailyB1.dur)]*60*60*24,
                                        amount = feedflowdailyB1.amo[1:as.integer(0.5*feedflowdailyB1.freq*feedflowdailyB1.dur)])
feedflowdailyB1.dfFouri <- feedflowdailyB1.dfFouri %>%
  mutate(period = ifelse(feedflowdailyB1.dfFouri$amount >= 1,
                         paste(round(1/feedflowdailyB1.dfFouri$freq, 2),"days"),""))
feedflowdailyB1.dfFouri[1,3] <- ""
ggplot(feedflowdailyB1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1B Drum Feed Flow Fourier Analysis")

# 1B drum speed
drumspeeddailyB1.ts <- ts(filterdrumspeeddailyB1$data) #change to time series
drumspeeddailyB1.freq <- (1/(30*60))  #sample frequency in Hz 
drumspeeddailyB1.dur <- length(drumspeeddailyB1.ts)*30*60 # length of signal in seconds 
drumspeeddailyB1.tot <- drumspeeddailyB1.freq*drumspeeddailyB1.dur #total number of sample

drumspeeddailyB1.x <- seq(0, drumspeeddailyB1.dur, length.out = tot)
drumspeeddailyB1.fourier <- fft(drumspeeddailyB1.ts)
drumspeeddailyB1.amo <- 2*Mod(drumspeeddailyB1.fourier)/(drumspeeddailyB1.tot) #amplitude
drumspeeddailyB1.freqvec <- 0:(length(drumspeeddailyB1.amo)-1) #vector from 0 to end of signal -> new x-axis

drumspeeddailyB1.amo[drumspeeddailyB1.freqvec == 0] <-
  drumspeeddailyB1.amo[drumspeeddailyB1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
drumspeeddailyB1.freqvec <- drumspeeddailyB1.freqvec/drumspeeddailyB1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
drumspeeddailyB1.dfFouri <- data.frame(freq = drumspeeddailyB1.freqvec[1:as.integer(0.5*drumspeeddailyB1.freq*drumspeeddailyB1.dur)]*60*60*24,
                                         amount = drumspeeddailyB1.amo[1:as.integer(0.5*drumspeeddailyB1.freq*drumspeeddailyB1.dur)])
drumspeeddailyB1.dfFouri <- drumspeeddailyB1.dfFouri %>%
  mutate(period = ifelse(drumspeeddailyB1.dfFouri$amount >= 1,
                         paste(round(1/drumspeeddailyB1.dfFouri$freq, 2),"days"),""))
drumspeeddailyB1.dfFouri[1,3] <- ""
ggplot(drumspeeddailyB1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1B Drum Speed Fourier Analysis")

# 1B bath level
bathleveldailyB1.ts <- ts(filterbathleveldailyB1$data) #change to time series
bathleveldailyB1.freq <- (1/(30*60))  #sample frequency in Hz 
bathleveldailyB1.dur <- length(bathleveldailyB1.ts)*30*60 # length of signal in seconds 
bathleveldailyB1.tot <- bathleveldailyB1.freq*bathleveldailyB1.dur #total number of sample

bathleveldailyB1.x <- seq(0, bathleveldailyB1.dur, length.out = tot)
bathleveldailyB1.fourier <- fft(bathleveldailyB1.ts)
bathleveldailyB1.amo <- 2*Mod(bathleveldailyB1.fourier)/(bathleveldailyB1.tot) #amplitude
bathleveldailyB1.freqvec <- 0:(length(bathleveldailyB1.amo)-1) #vector from 0 to end of signal -> new x-axis

bathleveldailyB1.amo[bathleveldailyB1.freqvec == 0] <-
  bathleveldailyB1.amo[bathleveldailyB1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
bathleveldailyB1.freqvec <- bathleveldailyB1.freqvec/bathleveldailyB1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
bathleveldailyB1.dfFouri <- data.frame(freq = bathleveldailyB1.freqvec[1:as.integer(0.5*bathleveldailyB1.freq*bathleveldailyB1.dur)]*60*60*24,
                                         amount = bathleveldailyB1.amo[1:as.integer(0.5*bathleveldailyB1.freq*bathleveldailyB1.dur)])
bathleveldailyB1.dfFouri <- bathleveldailyB1.dfFouri %>%
  mutate(period = ifelse(bathleveldailyB1.dfFouri$amount >= 1,
                         paste(round(1/bathleveldailyB1.dfFouri$freq, 2),"days"),""))
bathleveldailyB1.dfFouri[1,3] <- ""
ggplot(bathleveldailyB1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1B Drum Bath Level Fourier Analysis")

# 1B vacuum
vacuumdailyB1.ts <- ts(filtervacuumdailyB1$data) #change to time series
vacuumdailyB1.freq <- (1/(30*60))  #sample frequency in Hz 
vacuumdailyB1.dur <- length(vacuumdailyB1.ts)*30*60 # length of signal in seconds 
vacuumdailyB1.tot <- vacuumdailyB1.freq*vacuumdailyB1.dur #total number of sample

vacuumdailyB1.x <- seq(0, vacuumdailyB1.dur, length.out = tot)
vacuumdailyB1.fourier <- fft(vacuumdailyB1.ts)
vacuumdailyB1.amo <- 2*Mod(vacuumdailyB1.fourier)/(vacuumdailyB1.tot) #amplitude
vacuumdailyB1.freqvec <- 0:(length(vacuumdailyB1.amo)-1) #vector from 0 to end of signal -> new x-axis

vacuumdailyB1.amo[vacuumdailyB1.freqvec == 0] <-
  vacuumdailyB1.amo[vacuumdailyB1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
vacuumdailyB1.freqvec <- vacuumdailyB1.freqvec/vacuumdailyB1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
vacuumdailyB1.dfFouri <- data.frame(freq = vacuumdailyB1.freqvec[1:as.integer(0.5*vacuumdailyB1.freq*vacuumdailyB1.dur)]*60*60*24,
                                      amount = vacuumdailyB1.amo[1:as.integer(0.5*vacuumdailyB1.freq*vacuumdailyB1.dur)])
vacuumdailyB1.dfFouri <- vacuumdailyB1.dfFouri %>%
  mutate(period = ifelse(vacuumdailyB1.dfFouri$amount >= 1,
                         paste(round(1/vacuumdailyB1.dfFouri$freq, 2),"days"),""))
vacuumdailyB1.dfFouri[1,3] <- ""
ggplot(vacuumdailyB1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1B Drum Vacuum Pressure Fourier Analysis")


# 1B cake wash
cakewashdailyB1.ts <- ts(filtercakewashdailyB1$data) #change to time series
cakewashdailyB1.freq <- (1/(30*60))  #sample frequency in Hz 
cakewashdailyB1.dur <- length(cakewashdailyB1.ts)*30*60 # length of signal in seconds 
cakewashdailyB1.tot <- cakewashdailyB1.freq*cakewashdailyB1.dur #total number of sample

cakewashdailyB1.x <- seq(0, cakewashdailyB1.dur, length.out = tot)
cakewashdailyB1.fourier <- fft(cakewashdailyB1.ts)
cakewashdailyB1.amo <- 2*Mod(cakewashdailyB1.fourier)/(cakewashdailyB1.tot) #amplitude
cakewashdailyB1.freqvec <- 0:(length(cakewashdailyB1.amo)-1) #vector from 0 to end of signal -> new x-axis

cakewashdailyB1.amo[cakewashdailyB1.freqvec == 0] <-
  cakewashdailyB1.amo[cakewashdailyB1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
cakewashdailyB1.freqvec <- cakewashdailyB1.freqvec/cakewashdailyB1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
cakewashdailyB1.dfFouri <- data.frame(freq = cakewashdailyB1.freqvec[1:as.integer(0.5*cakewashdailyB1.freq*cakewashdailyB1.dur)]*60*60*24,
                                        amount = cakewashdailyB1.amo[1:as.integer(0.5*cakewashdailyB1.freq*cakewashdailyB1.dur)])
cakewashdailyB1.dfFouri <- cakewashdailyB1.dfFouri %>%
  mutate(period = ifelse(cakewashdailyB1.dfFouri$amount >= 1,
                         paste(round(1/cakewashdailyB1.dfFouri$freq, 2),"days"),""))
cakewashdailyB1.dfFouri[1,3] <- ""
ggplot(cakewashdailyB1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1B Drum Cake Wash Fourier Analysis")

# 1B soda filtrate conc.
sodafiltratedailyB1.ts <- ts(filtersodafiltratedailyB1$data) #change to time series
sodafiltratedailyB1.freq <- (1/(30*60))  #sample frequency in Hz 
sodafiltratedailyB1.dur <- length(sodafiltratedailyB1.ts)*30*60 # length of signal in seconds 
sodafiltratedailyB1.tot <- sodafiltratedailyB1.freq*sodafiltratedailyB1.dur #total number of sample

sodafiltratedailyB1.x <- seq(0, sodafiltratedailyB1.dur, length.out = tot)
sodafiltratedailyB1.fourier <- fft(sodafiltratedailyB1.ts)
sodafiltratedailyB1.amo <- 2*Mod(sodafiltratedailyB1.fourier)/(sodafiltratedailyB1.tot) #amplitude
sodafiltratedailyB1.freqvec <- 0:(length(sodafiltratedailyB1.amo)-1) #vector from 0 to end of signal -> new x-axis

sodafiltratedailyB1.amo[sodafiltratedailyB1.freqvec == 0] <-
  sodafiltratedailyB1.amo[sodafiltratedailyB1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
sodafiltratedailyB1.freqvec <- sodafiltratedailyB1.freqvec/sodafiltratedailyB1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
sodafiltratedailyB1.dfFouri <- data.frame(freq = sodafiltratedailyB1.freqvec[1:as.integer(0.5*sodafiltratedailyB1.freq*sodafiltratedailyB1.dur)]*60*60*24,
                                            amount = sodafiltratedailyB1.amo[1:as.integer(0.5*sodafiltratedailyB1.freq*sodafiltratedailyB1.dur)])
sodafiltratedailyB1.dfFouri <- sodafiltratedailyB1.dfFouri %>%
  mutate(period = ifelse(sodafiltratedailyB1.dfFouri$amount >= 1,
                         paste(round(1/sodafiltratedailyB1.dfFouri$freq, 2),"days"),""))
sodafiltratedailyB1.dfFouri[1,3] <- ""
ggplot(sodafiltratedailyB1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1B Drum Soda Filtrate Fourier Analysis")


# 1B oxalate filtrate
oxfiltratedailyB1.ts <- ts(filteroxfiltratedailyB1$data) #change to time series
oxfiltratedailyB1.freq <- (1/(30*60))  #sample frequency in Hz 
oxfiltratedailyB1.dur <- length(oxfiltratedailyB1.ts)*30*60 # length of signal in seconds 
oxfiltratedailyB1.tot <- oxfiltratedailyB1.freq*oxfiltratedailyB1.dur #total number of sample

oxfiltratedailyB1.x <- seq(0, oxfiltratedailyB1.dur, length.out = tot)
oxfiltratedailyB1.fourier <- fft(oxfiltratedailyB1.ts)
oxfiltratedailyB1.amo <- 2*Mod(oxfiltratedailyB1.fourier)/(oxfiltratedailyB1.tot) #amplitude
oxfiltratedailyB1.freqvec <- 0:(length(oxfiltratedailyB1.amo)-1) #vector from 0 to end of signal -> new x-axis

oxfiltratedailyB1.amo[oxfiltratedailyB1.freqvec == 0] <-
  oxfiltratedailyB1.amo[oxfiltratedailyB1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
oxfiltratedailyB1.freqvec <- oxfiltratedailyB1.freqvec/oxfiltratedailyB1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
oxfiltratedailyB1.dfFouri <- data.frame(freq = oxfiltratedailyB1.freqvec[1:as.integer(0.5*oxfiltratedailyB1.freq*oxfiltratedailyB1.dur)]*60*60*24,
                                          amount = oxfiltratedailyB1.amo[1:as.integer(0.5*oxfiltratedailyB1.freq*oxfiltratedailyB1.dur)])
oxfiltratedailyB1.dfFouri <- oxfiltratedailyB1.dfFouri %>%
  mutate(period = ifelse(oxfiltratedailyB1.dfFouri$amount >= 1,
                         paste(round(1/oxfiltratedailyB1.dfFouri$freq, 2),"days"),""))
oxfiltratedailyB1.dfFouri[1,3] <- ""
ggplot(oxfiltratedailyB1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1B Drum Oxalate Filtrate Fourier Analysis")


# 2B Drum----------------------------------------------------------------------------------------
# 2B feed flow
feedflowdailyB2.ts <- ts(filterfeedflowdailyB2$data) #change to time series
feedflowdailyB2.freq <- (1/(30*60))  #sample frequency in Hz 
feedflowdailyB2.dur <- length(feedflowdailyB2.ts)*30*60 # length of signal in seconds 
feedflowdailyB2.tot <- feedflowdailyB2.freq*feedflowdailyB2.dur #total number of sample

feedflowdailyB2.x <- seq(0, feedflowdailyB2.dur, length.out = tot)
feedflowdailyB2.fourier <- fft(feedflowdailyB2.ts)
feedflowdailyB2.amo <- 2*Mod(feedflowdailyB2.fourier)/(feedflowdailyB2.tot) #amplitude
feedflowdailyB2.freqvec <- 0:(length(feedflowdailyB2.amo)-1) #vector from 0 to end of signal -> new x-axis

feedflowdailyB2.amo[feedflowdailyB2.freqvec == 0] <-
  feedflowdailyB2.amo[feedflowdailyB2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
feedflowdailyB2.freqvec <- feedflowdailyB2.freqvec/feedflowdailyB2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
feedflowdailyB2.dfFouri <- data.frame(freq = feedflowdailyB2.freqvec[1:as.integer(0.5*feedflowdailyB2.freq*feedflowdailyB2.dur)]*60*60*24,
                                        amount = feedflowdailyB2.amo[1:as.integer(0.5*feedflowdailyB2.freq*feedflowdailyB2.dur)])
feedflowdailyB2.dfFouri <- feedflowdailyB2.dfFouri %>%
  mutate(period = ifelse(feedflowdailyB2.dfFouri$amount >= 1,
                         paste(round(1/feedflowdailyB2.dfFouri$freq, 2),"days"),""))
feedflowdailyB2.dfFouri[1,3] <- ""
ggplot(feedflowdailyB2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2B Drum Feed Flow Fourier Analysis")

# 2B drum speed
drumspeeddailyB2.ts <- ts(filterdrumspeeddailyB2$data) #change to time series
drumspeeddailyB2.freq <- (1/(30*60))  #sample frequency in Hz 
drumspeeddailyB2.dur <- length(drumspeeddailyB2.ts)*30*60 # length of signal in seconds 
drumspeeddailyB2.tot <- drumspeeddailyB2.freq*drumspeeddailyB2.dur #total number of sample

drumspeeddailyB2.x <- seq(0, drumspeeddailyB2.dur, length.out = tot)
drumspeeddailyB2.fourier <- fft(drumspeeddailyB2.ts)
drumspeeddailyB2.amo <- 2*Mod(drumspeeddailyB2.fourier)/(drumspeeddailyB2.tot) #amplitude
drumspeeddailyB2.freqvec <- 0:(length(drumspeeddailyB2.amo)-1) #vector from 0 to end of signal -> new x-axis

drumspeeddailyB2.amo[drumspeeddailyB2.freqvec == 0] <-
  drumspeeddailyB2.amo[drumspeeddailyB2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
drumspeeddailyB2.freqvec <- drumspeeddailyB2.freqvec/drumspeeddailyB2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
drumspeeddailyB2.dfFouri <- data.frame(freq = drumspeeddailyB2.freqvec[1:as.integer(0.5*drumspeeddailyB2.freq*drumspeeddailyB2.dur)]*60*60*24,
                                         amount = drumspeeddailyB2.amo[1:as.integer(0.5*drumspeeddailyB2.freq*drumspeeddailyB2.dur)])
drumspeeddailyB2.dfFouri <- drumspeeddailyB2.dfFouri %>%
  mutate(period = ifelse(drumspeeddailyB2.dfFouri$amount >= 1,
                         paste(round(1/drumspeeddailyB2.dfFouri$freq, 2),"days"),""))
drumspeeddailyB2.dfFouri[1,3] <- ""
ggplot(drumspeeddailyB2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2B Drum Speed Fourier Analysis")

# 2B bath level
bathleveldailyB2.ts <- ts(filterbathleveldailyB2$data) #change to time series
bathleveldailyB2.freq <- (1/(30*60))  #sample frequency in Hz 
bathleveldailyB2.dur <- length(bathleveldailyB2.ts)*30*60 # length of signal in seconds 
bathleveldailyB2.tot <- bathleveldailyB2.freq*bathleveldailyB2.dur #total number of sample

bathleveldailyB2.x <- seq(0, bathleveldailyB2.dur, length.out = tot)
bathleveldailyB2.fourier <- fft(bathleveldailyB2.ts)
bathleveldailyB2.amo <- 2*Mod(bathleveldailyB2.fourier)/(bathleveldailyB2.tot) #amplitude
bathleveldailyB2.freqvec <- 0:(length(bathleveldailyB2.amo)-1) #vector from 0 to end of signal -> new x-axis

bathleveldailyB2.amo[bathleveldailyB2.freqvec == 0] <-
  bathleveldailyB2.amo[bathleveldailyB2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
bathleveldailyB2.freqvec <- bathleveldailyB2.freqvec/bathleveldailyB2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
bathleveldailyB2.dfFouri <- data.frame(freq = bathleveldailyB2.freqvec[1:as.integer(0.5*bathleveldailyB2.freq*bathleveldailyB2.dur)]*60*60*24,
                                         amount = bathleveldailyB2.amo[1:as.integer(0.5*bathleveldailyB2.freq*bathleveldailyB2.dur)])
bathleveldailyB2.dfFouri <- bathleveldailyB2.dfFouri %>%
  mutate(period = ifelse(bathleveldailyB2.dfFouri$amount >= 1,
                         paste(round(1/bathleveldailyB2.dfFouri$freq, 2),"days"),""))
bathleveldailyB2.dfFouri[1,3] <- ""
ggplot(bathleveldailyB2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2B Drum Bath Level Fourier Analysis")

# 2B vacuum
vacuumdailyB2.ts <- ts(filtervacuumdailyB2$data) #change to time series
vacuumdailyB2.freq <- (1/(30*60))  #sample frequency in Hz 
vacuumdailyB2.dur <- length(vacuumdailyB2.ts)*30*60 # length of signal in seconds 
vacuumdailyB2.tot <- vacuumdailyB2.freq*vacuumdailyB2.dur #total number of sample

vacuumdailyB2.x <- seq(0, vacuumdailyB2.dur, length.out = tot)
vacuumdailyB2.fourier <- fft(vacuumdailyB2.ts)
vacuumdailyB2.amo <- 2*Mod(vacuumdailyB2.fourier)/(vacuumdailyB2.tot) #amplitude
vacuumdailyB2.freqvec <- 0:(length(vacuumdailyB2.amo)-1) #vector from 0 to end of signal -> new x-axis

vacuumdailyB2.amo[vacuumdailyB2.freqvec == 0] <-
  vacuumdailyB2.amo[vacuumdailyB2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
vacuumdailyB2.freqvec <- vacuumdailyB2.freqvec/vacuumdailyB2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
vacuumdailyB2.dfFouri <- data.frame(freq = vacuumdailyB2.freqvec[1:as.integer(0.5*vacuumdailyB2.freq*vacuumdailyB2.dur)]*60*60*24,
                                      amount = vacuumdailyB2.amo[1:as.integer(0.5*vacuumdailyB2.freq*vacuumdailyB2.dur)])
vacuumdailyB2.dfFouri <- vacuumdailyB2.dfFouri %>%
  mutate(period = ifelse(vacuumdailyB2.dfFouri$amount >= 1,
                         paste(round(1/vacuumdailyB2.dfFouri$freq, 2),"days"),""))
vacuumdailyB2.dfFouri[1,3] <- ""
ggplot(vacuumdailyB2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2B Drum Vacuum Pressure Fourier Analysis")


# 2B cake wash
cakewashdailyB2.ts <- ts(filtercakewashdailyB2$data) #change to time series
cakewashdailyB2.freq <- (1/(30*60))  #sample frequency in Hz 
cakewashdailyB2.dur <- length(cakewashdailyB2.ts)*30*60 # length of signal in seconds 
cakewashdailyB2.tot <- cakewashdailyB2.freq*cakewashdailyB2.dur #total number of sample

cakewashdailyB2.x <- seq(0, cakewashdailyB2.dur, length.out = tot)
cakewashdailyB2.fourier <- fft(cakewashdailyB2.ts)
cakewashdailyB2.amo <- 2*Mod(cakewashdailyB2.fourier)/(cakewashdailyB2.tot) #amplitude
cakewashdailyB2.freqvec <- 0:(length(cakewashdailyB2.amo)-1) #vector from 0 to end of signal -> new x-axis

cakewashdailyB2.amo[cakewashdailyB2.freqvec == 0] <-
  cakewashdailyB2.amo[cakewashdailyB2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
cakewashdailyB2.freqvec <- cakewashdailyB2.freqvec/cakewashdailyB2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
cakewashdailyB2.dfFouri <- data.frame(freq = cakewashdailyB2.freqvec[1:as.integer(0.5*cakewashdailyB2.freq*cakewashdailyB2.dur)]*60*60*24,
                                        amount = cakewashdailyB2.amo[1:as.integer(0.5*cakewashdailyB2.freq*cakewashdailyB2.dur)])
cakewashdailyB2.dfFouri <- cakewashdailyB2.dfFouri %>%
  mutate(period = ifelse(cakewashdailyB2.dfFouri$amount >= 1,
                         paste(round(1/cakewashdailyB2.dfFouri$freq, 2),"days"),""))
cakewashdailyB2.dfFouri[1,3] <- ""
ggplot(cakewashdailyB2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2B Drum Cake Wash Fourier Analysis")

# 2B soda filtrate conc.
sodafiltratedailyB2.ts <- ts(filtersodafiltratedailyB2$data) #change to time series
sodafiltratedailyB2.freq <- (1/(30*60))  #sample frequency in Hz 
sodafiltratedailyB2.dur <- length(sodafiltratedailyB2.ts)*30*60 # length of signal in seconds 
sodafiltratedailyB2.tot <- sodafiltratedailyB2.freq*sodafiltratedailyB2.dur #total number of sample

sodafiltratedailyB2.x <- seq(0, sodafiltratedailyB2.dur, length.out = tot)
sodafiltratedailyB2.fourier <- fft(sodafiltratedailyB2.ts)
sodafiltratedailyB2.amo <- 2*Mod(sodafiltratedailyB2.fourier)/(sodafiltratedailyB2.tot) #amplitude
sodafiltratedailyB2.freqvec <- 0:(length(sodafiltratedailyB2.amo)-1) #vector from 0 to end of signal -> new x-axis

sodafiltratedailyB2.amo[sodafiltratedailyB2.freqvec == 0] <-
  sodafiltratedailyB2.amo[sodafiltratedailyB2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
sodafiltratedailyB2.freqvec <- sodafiltratedailyB2.freqvec/sodafiltratedailyB2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
sodafiltratedailyB2.dfFouri <- data.frame(freq = sodafiltratedailyB2.freqvec[1:as.integer(0.5*sodafiltratedailyB2.freq*sodafiltratedailyB2.dur)]*60*60*24,
                                            amount = sodafiltratedailyB2.amo[1:as.integer(0.5*sodafiltratedailyB2.freq*sodafiltratedailyB2.dur)])
sodafiltratedailyB2.dfFouri <- sodafiltratedailyB2.dfFouri %>%
  mutate(period = ifelse(sodafiltratedailyB2.dfFouri$amount >= 1,
                         paste(round(1/sodafiltratedailyB2.dfFouri$freq, 2),"days"),""))
sodafiltratedailyB2.dfFouri[1,3] <- ""
ggplot(sodafiltratedailyB2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2B Drum Soda Filtrate Fourier Analysis")


# 2B oxalate filtrate
oxfiltratedailyB2.ts <- ts(filteroxfiltratedailyB2$data) #change to time series
oxfiltratedailyB2.freq <- (1/(30*60))  #sample frequency in Hz 
oxfiltratedailyB2.dur <- length(oxfiltratedailyB2.ts)*30*60 # length of signal in seconds 
oxfiltratedailyB2.tot <- oxfiltratedailyB2.freq*oxfiltratedailyB2.dur #total number of sample

oxfiltratedailyB2.x <- seq(0, oxfiltratedailyB2.dur, length.out = tot)
oxfiltratedailyB2.fourier <- fft(oxfiltratedailyB2.ts)
oxfiltratedailyB2.amo <- 2*Mod(oxfiltratedailyB2.fourier)/(oxfiltratedailyB2.tot) #amplitude
oxfiltratedailyB2.freqvec <- 0:(length(oxfiltratedailyB2.amo)-1) #vector from 0 to end of signal -> new x-axis

oxfiltratedailyB2.amo[oxfiltratedailyB2.freqvec == 0] <-
  oxfiltratedailyB2.amo[oxfiltratedailyB2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
oxfiltratedailyB2.freqvec <- oxfiltratedailyB2.freqvec/oxfiltratedailyB2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
oxfiltratedailyB2.dfFouri <- data.frame(freq = oxfiltratedailyB2.freqvec[1:as.integer(0.5*oxfiltratedailyB2.freq*oxfiltratedailyB2.dur)]*60*60*24,
                                          amount = oxfiltratedailyB2.amo[1:as.integer(0.5*oxfiltratedailyB2.freq*oxfiltratedailyB2.dur)])
oxfiltratedailyB2.dfFouri <- oxfiltratedailyB2.dfFouri %>%
  mutate(period = ifelse(oxfiltratedailyB2.dfFouri$amount >= 1,
                         paste(round(1/oxfiltratedailyB2.dfFouri$freq, 2),"days"),""))
oxfiltratedailyB2.dfFouri[1,3] <- ""
ggplot(oxfiltratedailyB2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2B Drum Oxalate Filtrate Fourier Analysis")
