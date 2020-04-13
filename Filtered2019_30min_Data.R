library(tidyverse)
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)

seedwash30min2019<-read_excel('Seedwash Data for UQ- 30min_2015_2019.xlsx', sheet="2019", skip=6, na=c(""," ", "#N/A", "[-11059] No Good Data For Calculation", "Argument is not a string or cell reference"))

#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#extract feed data from 30mins observation (2019)
date30min2019 <- seedwash30min2019$Date
outputspo30min2019 <- seedwash30min2019$PONthTkAN.Ox
outputsoda30min2019 <- seedwash30min2019$POSOFATPV.S
outputalumina30min2019 <- seedwash30min2019$TotalFlow
throughput30min2019 <- seedwash30min2019$POFeedMQPV
spo30min2019 <- seedwash30min2019$POFeedAN.Ox
feedsoda30min2019 <- seedwash30min2019$POFeedAN.C
feeddensity30min2019 <- seedwash30min2019$POFeedDTPV

timestep30min2019 <- seq(616, 0, by = -0.5)

data30min2019 <- data.frame(time = timestep30min2019,           #day
                            date = date30min2019,
                            outputspo = outputspo30min2019,     #%
                            outputsoda = outputsoda30min2019,   #g/l
                            outputalumina = outputalumina30min2019, #kl/h
                            throughput = throughput30min2019,   #t/h
                            feedspo = spo30min2019,             #%
                            feedsoda = feedsoda30min2019,       #g/l
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
  sodaconc30min2019A1[i] <- feedflow30min2019A1[i]*feedsoda30min2019[i]/1000
}


data30min2019A1 <- data.frame(time = timestep30min2019,           #day
                              date = date30min2019,
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


#------------------------------------------------------------------------------------------------
#extract 2A data from 30mins observation
status30min2019A2 <- seedwash30min2019$PO2AFOLXBDI
drumspeed30min2019A2 <- seedwash30min2019$PO2AFDrumSTPV
bathlevel30min2019A2 <- seedwash30min2019$PO2ABathLCPV
vacuum30min2019A2 <- seedwash30min2019$PO2AVacPCPV
feedflow30min2019A2 <- seedwash30min2019$PO2FeedFCPV
flocflow30min2019A2 <- seedwash30min2019$PO2FlocFTPV
cakewash30min2019A2 <- seedwash30min2019$PO2ASprayFCPV
clothwash30min2019A2 <- seedwash30min2019$PO2AFCoSFTPV
sodafilt30min2019A2 <- seedwash30min2019$PO2AFltAN.C
oxfilt30min2019A2 <- seedwash30min2019$PO2AFltAN.Ox

#soda concentration in ton/hr
sodaconc30min2019A2 <- c(1:length(feedflow30min2019A2))
for (i in 1:length(sodaconc30min2019A2)){
  sodaconc30min2019A2[i] <- feedflow30min2019A2[i]*feedsoda30min2019[i]/1000
}


data30min2019A2 <- data.frame(time = timestep30min2019,           #day
                              date = date30min2019,
                              status = status30min2019A2,
                              outputspo = outputspo30min2019,     #%
                              outputsoda = outputsoda30min2019,   #g/l
                              outputalumina = outputalumina30min2019, #kl/h
                              throughput = throughput30min2019,   #t/h
                              feedspo = spo30min2019,      #%
                              feedsoda = feedsoda30min2019,    #g/l
                              feeddensity = feeddensity30min2019, #SG
                              drumspeed = drumspeed30min2019A2,   #RPM
                              bathlevel = bathlevel30min2019A2,   #%
                              vacuum = vacuum30min2019A2,         #kPa
                              feedflow = feedflow30min2019A2,     #kl/h
                              flocflow = flocflow30min2019A2,     #kl/h
                              cakewash = cakewash30min2019A2,     #kl/h
                              clothwash = clothwash30min2019A2,   #kl/h
                              sodafiltrate = sodafilt30min2019A2, #g/l
                              oxfiltrate = oxfilt30min2019A2,     #g/l
                              sodaconc = sodaconc30min2019A2)     #t/h


multiple30min2019A2 <- data30min2019A2 %>%
  filter(status == 'On') %>%
  gather(type, data, outputspo:sodaconc)

ggplot(multiple30min2019A2, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  xlab("Time (hour)") + ggtitle("Filter 1A Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#observed against Output Soda Conc.
drumspeedmin2019A2 <- multiple30min2019A2 %>%
  filter(type == 'drumspeed')
ggplot(drumspeedmin2019A2, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Drum Speed (RPM)") +
  ggtitle("2019 Filter 1A Drum Speed")


bathlevelmin2019A2 <- multiple30min2019A2 %>%
  filter( type == 'bathlevel')
ggplot(bathlevelmin2019A2, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) + 
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Bath Level (%)") +
  ggtitle("2019 Filter 1A Bath Level")


vacuummin2019A2 <- multiple30min2019A2 %>%
  filter( type == 'vacuum')
ggplot(vacuummin2019A2, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") +
  ggtitle("2019 Filter 1A Vacuum Pressure")


feedflowmin2019A2 <- multiple30min2019A2 %>%
  filter( type == 'feedflow')
ggplot(feedflowmin2019A2, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Feed Flow (kl/h)") +
  ggtitle("2019 Filter 1A Feed Flow")


flocflowmin2019A2 <- multiple30min2019A2 %>%
  filter(type == 'flocflow')
ggplot(flocflowmin2019A2, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Floc Flow (kl/h)") +
  ggtitle("2019 Filter 1A Floc Flow")


cakewashmin2019A2 <- multiple30min2019A2 %>%
  filter( type == 'cakewash')
ggplot(cakewashmin2019A2, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") +
  ggtitle("2019 Filter 1A Cake Wash Flow")


clothwashmin2019A2 <- multiple30min2019A2 %>%
  filter( type == 'clothwash')
ggplot(clothwashmin2019A2, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Cloth Wash Flow (kl/h)") +
  ggtitle("2019 Filter 1A Cloth Wash Flow")


sodafiltratemin2019A2 <- multiple30min2019A2 %>%
  filter( type == 'sodafiltrate')
ggplot(sodafiltratemin2019A2) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data=sodafiltratemin2019A2[!is.na(sodafiltratemin2019A2$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Filtrate Soda Conc. (g/l)") +
  ggtitle("2019 Filter 1A Filtrate Soda Conc.")


oxfiltratemin2019A2 <- multiple30min2019A2 %>%
  filter( type == 'oxfiltrate')
ggplot(oxfiltratemin2019A2) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data = oxfiltratemin2019A2[!is.na(oxfiltratemin2019A2$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Filtrate Oxalate (%)") +
  ggtitle("2019 Filter 1A Filtrate Oxalate")


sodaconcmin2019A2 <- multiple30min2019A2 %>%
  filter( type == 'sodaconc')
ggplot(sodaconcmin2019A2, aes(x=time, y=data, color = type)) + geom_line() +
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


data30min2019B1 <- data.frame(time = timestep30min2019,           #day
                              date = date30min2019,
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

#drum speed
drumspeedmin2019B1 <- multiple30min2019B1 %>%
  filter(type == 'drumspeed')
ggplot(drumspeedmin2019B1, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Drum Speed (RPM)") +
  ggtitle("2019 Filter 1B Drum Speed")

#bath level
bathlevelmin2019B1 <- multiple30min2019B1 %>%
  filter( type == 'bathlevel')
ggplot(bathlevelmin2019B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) + 
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Bath Level (%)") +
  ggtitle("2019 Filter 1B Bath Level")

#vacuum pressure
vacuummin2019B1 <- multiple30min2019B1 %>%
  filter( type == 'vacuum')
ggplot(vacuummin2019B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") +
  ggtitle("2019 Filter 1B Vacuum Pressure")


#feed flow
feedflowmin2019B1 <- multiple30min2019B1 %>%
  filter( type == 'feedflow')
ggplot(feedflowmin2019B1, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Feed Flow (kl/h)") +
  ggtitle("2019 Filter 1B Feed Flow")


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

oxfiltratemin2019B1 <- multiple30min2019B1 %>%
  filter( type == 'oxfiltrate')
ggplot(oxfiltratemin2019B1) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data = oxfiltratemin2019B1[!is.na(oxfiltratemin2019B1$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Filtrate Oxalate (%)") +
  ggtitle("2019 Filter 1B Filtrate Oxalate")





#-------------------------------------------------------------------------------------------------
#filtering & spectral analysis
library(stats)
library(EnvStats)
library(signal)


#-------------------------------------------------------------------------------------------------
#rosner test--------------------------------------------------------------------------------------
#throughput
rtestthroughputmin2019 <- rosnerTest(throughputmin2019$data, k=100)

if (rtestthroughputmin2019$n.outliers > 0){
  finalrtestthroughputmin2019 <- rosnerTest(throughputmin2019$data, k=rtestthroughputmin2019$n.outliers)
  
  filterthroughputmin2019 <- throughputmin2019[-c(finalrtestthroughputmin2019$all.stats$Obs.Num),]
  
  ggplot(filterthroughputmin2019, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Throughput (t/h)") +
    ggtitle("2019 Throughput (filtered)")
} else {
  filterthroughputmin2019 <- throughputmin2019
}

# 1A Drum-----------------------------------------------------------------------------------------
# 1A feedflow 
rtestfeedflowmin2019A1 <- rosnerTest(feedflowmin2019A1$data, k=100)

if (rtestfeedflowmin2019A1$n.outliers > 0){
  finalrtestfeedflowmin2019A1 <- rosnerTest(feedflowmin2019A1$data, k=rtestfeedflowmin2019A1$n.outliers)
  
  filterfeedflowmin2019A1 <- feedflowmin2019A1[-c(finalrtestfeedflowmin2019A1$all.stats$Obs.Num),]
  
  ggplot(filterfeedflowmin2019A1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Feed Flow (kl/h)") +
    ggtitle("2019 Filter 1A Feed Flow (filtered)")
} else {
  filterfeedflowmin2019A1 <- feedflowmin2019A1
}

# 1A drum speed
rtestdrumspeedmin2019A1 <- rosnerTest(drumspeedmin2019A1$data, k=100)

if (rtestdrumspeedmin2019A1$n.outliers > 0){
  finalrtestdrumspeedmin2019A1 <- rosnerTest(drumspeedmin2019A1$data, k=rtestdrumspeedmin2019A1$n.outliers)
  
  filterdrumspeedmin2019A1 <- drumspeedmin2019A1[-c(finalrtestdrumspeedmin2019A1$all.stats$Obs.Num),]
  
  ggplot(filterdrumspeedmin2019A1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Drum Speed (RPM)") + 
    ggtitle("2019 Filter 1A Drum Speed (filtered)")
} else {
  filterdrumspeedmin2019A1 <- drumspeedmin2019A1
}

# 1A bath level
rtestbathlevelmin2019A1 <- rosnerTest(bathlevelmin2019A1$data, k=100)

if (rtestbathlevelmin2019A1$n.outliers > 0){
  finalrtestbathlevelmin2019A1 <- rosnerTest(bathlevelmin2019A1$data, k=rtestbathlevelmin2019A1$n.outliers)
  
  filterbathlevelmin2019A1 <- bathlevelmin2019A1[-c(finalrtestbathlevelmin2019A1$all.stats$Obs.Num),]
  
  ggplot(filterbathlevelmin2019A1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Bath Level (%)") + 
    ggtitle("2019 Filter 1A Bath Level (filtered)")
} else {
  filterbathlevelmin2019A1 <- bathlevelmin2019A1
}

# 1A vacuum
rtestvacuummin2019A1 <- rosnerTest(vacuummin2019A1$data, k=100)

if (rtestvacuummin2019A1$n.outliers > 0){
  finalrtestvacuummin2019A1 <- rosnerTest(vacuummin2019A1$data, k=rtestvacuummin2019A1$n.outliers)
  
  filtervacuummin2019A1 <- vacuummin2019A1[-c(finalrtestvacuummin2019A1$all.stats$Obs.Num),]
  
  ggplot(filtervacuummin2019A1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") +
    ggtitle("2019 Filter 1A Vacuum Pressure (filtered)")
} else {
  filtervacuummin2019A1 <- vacuummin2019A1
}

# 1A cake wash
rtestcakewashmin2019A1 <- rosnerTest(cakewashmin2019A1$data, k=100)

if (rtestcakewashmin2019A1$n.outliers > 0){
  finalrtestcakewashmin2019A1 <- rosnerTest(cakewashmin2019A1$data, k=rtestcakewashmin2019A1$n.outliers)
  
  filtercakewashmin2019A1 <- cakewashmin2019A1[-c(finalrtestcakewashmin2019A1$all.stats$Obs.Num),]
  
  ggplot(filtercakewashmin2019A1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") + 
    ggtitle("2019 Filter 1A Cake Wash Flow (filtered)")
} else {
  filtercakewashmin2019A1 <- cakewashmin2019A1
}


# 2A Drum-----------------------------------------------------------------------------------------
# 2A feedflow 
rtestfeedflowmin2019A2 <- rosnerTest(feedflowmin2019A2$data, k=100)

if (rtestfeedflowmin2019A2$n.outliers > 0){
  finalrtestfeedflowmin2019A2 <- rosnerTest(feedflowmin2019A2$data, k=rtestfeedflowmin2019A2$n.outliers)
  
  filterfeedflowmin2019A2 <- feedflowmin2019A2[-c(finalrtestfeedflowmin2019A2$all.stats$Obs.Num),]
  
  ggplot(filterfeedflowmin2019A2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Feed Flow (kl/h)") +
    ggtitle("2019 Filter 2A Feed Flow (filtered)")
} else {
  filterfeedflowmin2019A2 <- feedflowmin2019A2
}

# 2A drum speed
rtestdrumspeedmin2019A2 <- rosnerTest(drumspeedmin2019A2$data, k=100)

if (rtestdrumspeedmin2019A2$n.outliers > 0){
  finalrtestdrumspeedmin2019A2 <- rosnerTest(drumspeedmin2019A2$data, k=rtestdrumspeedmin2019A2$n.outliers)
  
  filterdrumspeedmin2019A2 <- drumspeedmin2019A2[-c(finalrtestdrumspeedmin2019A2$all.stats$Obs.Num),]
  
  ggplot(filterdrumspeedmin2019A2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Drum Speed (RPM)") + 
    ggtitle("2019 Filter 2A Drum Speed (filtered)")
} else {
  filterdrumspeedmin2019A2 <- drumspeedmin2019A2
}

# 2A bath level
rtestbathlevelmin2019A2 <- rosnerTest(bathlevelmin2019A2$data, k=100)

if (rtestbathlevelmin2019A2$n.outliers > 0){
  finalrtestbathlevelmin2019A2 <- rosnerTest(bathlevelmin2019A2$data, k=rtestbathlevelmin2019A2$n.outliers)
  
  filterbathlevelmin2019A2 <- bathlevelmin2019A2[-c(finalrtestbathlevelmin2019A2$all.stats$Obs.Num),]
  
  ggplot(filterbathlevelmin2019A2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Bath Level (%)") + 
    ggtitle("2019 Filter 2A Bath Level (filtered)")
} else {
  filterbathlevelmin2019A2 <- bathlevelmin2019A2
}

# 2A vacuum
rtestvacuummin2019A2 <- rosnerTest(vacuummin2019A2$data, k=100)

if (rtestvacuummin2019A2$n.outliers > 0){
  finalrtestvacuummin2019A2 <- rosnerTest(vacuummin2019A2$data, k=rtestvacuummin2019A2$n.outliers)
  
  filtervacuummin2019A2 <- vacuummin2019A2[-c(finalrtestvacuummin2019A2$all.stats$Obs.Num),]
  
  ggplot(filtervacuummin2019A2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") +
    ggtitle("2019 Filter 2A Vacuum Pressure (filtered)")
} else {
  filtervacuummin2019A2 <- vacuummin2019A2
}

# 2A cake wash
rtestcakewashmin2019A2 <- rosnerTest(cakewashmin2019A2$data, k=100)

if (rtestcakewashmin2019A2$n.outliers > 0){
  finalrtestcakewashmin2019A2 <- rosnerTest(cakewashmin2019A2$data, k=rtestcakewashmin2019A2$n.outliers)
  
  filtercakewashmin2019A2 <- cakewashmin2019A2[-c(finalrtestcakewashmin2019A2$all.stats$Obs.Num),]
  
  ggplot(filtercakewashmin2019A2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") + 
    ggtitle("2019 Filter 2A Cake Wash Flow (filtered)")
} else {
  filtercakewashmin2019A2 <- cakewashmin2019A2
}

# 1B Drum-----------------------------------------------------------------------------------------
# 1B feedflow 
rtestfeedflowmin2019B1 <- rosnerTest(feedflowmin2019B1$data, k=100)

if (rtestfeedflowmin2019B1$n.outliers > 0){
  finalrtestfeedflowmin2019B1 <- rosnerTest(feedflowmin2019B1$data, k=rtestfeedflowmin2019B1$n.outliers)
  
  filterfeedflowmin2019B1 <- feedflowmin2019B1[-c(finalrtestfeedflowmin2019B1$all.stats$Obs.Num),]
  
  ggplot(filterfeedflowmin2019B1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Feed Flow (kl/h)") +
    ggtitle("2019 Filter 1B Feed Flow (filtered)")
} else {
  filterfeedflowmin2019B1 <- feedflowmin2019B1
}

# 1B drum speed
rtestdrumspeedmin2019B1 <- rosnerTest(drumspeedmin2019B1$data, k=100)

if (rtestdrumspeedmin2019B1$n.outliers > 0){
  finalrtestdrumspeedmin2019B1 <- rosnerTest(drumspeedmin2019B1$data, k=rtestdrumspeedmin2019B1$n.outliers)
  
  filterdrumspeedmin2019B1 <- drumspeedmin2019B1[-c(finalrtestdrumspeedmin2019B1$all.stats$Obs.Num),]
  
  ggplot(filterdrumspeedmin2019B1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Drum Speed (RPM)") + 
    ggtitle("2019 Filter 1B Drum Speed (filtered)")
} else {
  filterdrumspeedmin2019B1 <- drumspeedmin2019B1
}

# 1B bath level
rtestbathlevelmin2019B1 <- rosnerTest(bathlevelmin2019B1$data, k=100)

if (rtestbathlevelmin2019B1$n.outliers > 0){
  finalrtestbathlevelmin2019B1 <- rosnerTest(bathlevelmin2019B1$data, k=rtestbathlevelmin2019B1$n.outliers)
  
  filterbathlevelmin2019B1 <- bathlevelmin2019B1[-c(finalrtestbathlevelmin2019B1$all.stats$Obs.Num),]
  
  ggplot(filterbathlevelmin2019B1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Bath Level (%)") + 
    ggtitle("2019 Filter 1B Bath Level (filtered)")
} else {
  filterbathlevelmin2019B1 <- bathlevelmin2019B1
}

# 1B vacuum
rtestvacuummin2019B1 <- rosnerTest(vacuummin2019B1$data, k=100)

if (rtestvacuummin2019B1$n.outliers > 0){
  finalrtestvacuummin2019B1 <- rosnerTest(vacuummin2019B1$data, k=rtestvacuummin2019B1$n.outliers)
  
  filtervacuummin2019B1 <- vacuummin2019B1[-c(finalrtestvacuummin2019B1$all.stats$Obs.Num),]
  
  ggplot(filtervacuummin2019B1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") +
    ggtitle("2019 Filter 1B Vacuum Pressure (filtered)")
} else {
  filtervacuummin2019B1 <- vacuummin2019B1
}

# 1B cake wash
rtestcakewashmin2019B1 <- rosnerTest(cakewashmin2019B1$data, k=100)

if (rtestcakewashmin2019B1$n.outliers > 0){
  finalrtestcakewashmin2019B1 <- rosnerTest(cakewashmin2019B1$data, k=rtestcakewashmin2019B1$n.outliers)
  
  filtercakewashmin2019B1 <- cakewashmin2019B1[-c(finalrtestcakewashmin2019B1$all.stats$Obs.Num),]
  
  ggplot(filtercakewashmin2019B1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") + 
    ggtitle("2019 Filter 1B Cake Wash Flow (filtered)")
} else {
  filtercakewashmin2019B1 <- cakewashmin2019B1
}






#------------------------------------------------------------------------------------------------
#low filter-------------------------------------------------------------------------------------- 
lowbf <- butter(8, 0.1, type = "low")

#throughput
filterthroughputmin2019 <- na.omit(filterthroughputmin2019)
lowfilterthroughputmin2019 <- filtfilt(lowbf, filterthroughputmin2019$data)
lowpassthroughputmin2019 <- data.frame(time = filterthroughputmin2019$time,
                                       date = filterthroughputmin2019$date,
                                       filter = lowfilterthroughputmin2019,
                                       nofilter = filterthroughputmin2019$data)
lowpassthroughputmin2019fin <- lowpassthroughputmin2019 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassthroughputmin2019fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Feed Flow (kl/h)") + 
  ggtitle("Throughput (2019: 30min data)")


# 1A Drum----------------------------------------------------------------------------------------
# 1A feed flow
filterfeedflowmin2019A1 <- na.omit(filterfeedflowmin2019A1)
lowfilterfeedflowmin2019A1 <- filtfilt(lowbf, filterfeedflowmin2019A1$data)
lowpassfeedflowmin2019A1 <- data.frame(time = filterfeedflowmin2019A1$time,
                                       date = filterfeedflowmin2019A1$date,
                                       feedflow = lowfilterfeedflowmin2019A1)
lowpassfeedflowmin2019A1fin <- lowpassfeedflowmin2019A1 %>%
  gather(type, data, feedflow)

# 1A drum speed
filterdrumspeedmin2019A1 <- na.omit(filterdrumspeedmin2019A1)
lowfilterdrumspeedmin2019A1 <- filtfilt(lowbf, filterdrumspeedmin2019A1$data)
lowpassdrumspeedmin2019A1 <- data.frame(time = filterdrumspeedmin2019A1$time,
                                        date = filterdrumspeedmin2019A1$date,
                                        drumspeed = lowfilterdrumspeedmin2019A1)
lowpassdrumspeedmin2019A1fin <- lowpassdrumspeedmin2019A1 %>%
  gather(type, data, drumspeed)

# 1A bath level
filterbathlevelmin2019A1 <- na.omit(filterbathlevelmin2019A1)
lowfilterbathlevelmin2019A1 <- filtfilt(lowbf, filterbathlevelmin2019A1$data)
lowpassbathlevelmin2019A1 <- data.frame(time = filterbathlevelmin2019A1$time,
                                        date = filterbathlevelmin2019A1$date,
                                        bathlevel = lowfilterbathlevelmin2019A1)
lowpassbathlevelmin2019A1fin <- lowpassbathlevelmin2019A1 %>%
  gather(type, data, bathlevel)

# 1A vacuum 
filtervacuummin2019A1 <- na.omit(filtervacuummin2019A1)
lowfiltervacuummin2019A1 <- filtfilt(lowbf, filtervacuummin2019A1$data)
lowpassvacuummin2019A1 <- data.frame(time = filtervacuummin2019A1$time,
                                     date = filtervacuummin2019A1$date,
                                     vacuum = lowfiltervacuummin2019A1)
lowpassvacuummin2019A1fin <- lowpassvacuummin2019A1 %>%
  gather(type, data, vacuum)

# 1A cake wash
filtercakewashmin2019A1 <- na.omit(filtercakewashmin2019A1)
lowfiltercakewashmin2019A1 <- filtfilt(lowbf, filtercakewashmin2019A1$data)
lowpasscakewashmin2019A1 <- data.frame(time = filtercakewashmin2019A1$time,
                                       date = filtercakewashmin2019A1$date,
                                       cakewash = lowfiltercakewashmin2019A1)
lowpasscakewashmin2019A1fin <- lowpasscakewashmin2019A1 %>%
  gather(type, data, cakewash)


# 2A Drum----------------------------------------------------------------------------------------
# 2A feed flow
filterfeedflowmin2019A2 <- na.omit(filterfeedflowmin2019A2)
lowfilterfeedflowmin2019A2 <- filtfilt(lowbf, filterfeedflowmin2019A2$data)
lowpassfeedflowmin2019A2 <- data.frame(time = filterfeedflowmin2019A2$time,
                                       date = filterfeedflowmin2019A2$date,
                                       filter = lowfilterfeedflowmin2019A2,
                                       nofilter = filterfeedflowmin2019A2$data)
lowpassfeedflowmin2019A2fin <- lowpassfeedflowmin2019A2 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassfeedflowmin2019A2fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Feed Flow (kl/h)") + 
  ggtitle("2A Drum Feed Flow (2019: 30min data)")

# 2A drum speed
filterdrumspeedmin2019A2 <- na.omit(filterdrumspeedmin2019A2)
lowfilterdrumspeedmin2019A2 <- filtfilt(lowbf, filterdrumspeedmin2019A2$data)
lowpassdrumspeedmin2019A2 <- data.frame(time = filterdrumspeedmin2019A2$time,
                                       date = filterdrumspeedmin2019A2$date,
                                       drumspeed = lowfilterdrumspeedmin2019A2)
lowpassdrumspeedmin2019A2fin <- lowpassdrumspeedmin2019A2 %>%
  gather(type, data, drumspeed)

# 2A bath level
filterbathlevelmin2019A2 <- na.omit(filterbathlevelmin2019A2)
lowfilterbathlevelmin2019A2 <- filtfilt(lowbf, filterbathlevelmin2019A2$data)
lowpassbathlevelmin2019A2 <- data.frame(time = filterbathlevelmin2019A2$time,
                                       date = filterbathlevelmin2019A2$date,
                                       bathlevel = lowfilterbathlevelmin2019A2)
lowpassbathlevelmin2019A2fin <- lowpassbathlevelmin2019A2 %>%
  gather(type, data, bathlevel)

# 2A vacuum 
filtervacuummin2019A2 <- na.omit(filtervacuummin2019A2)
lowfiltervacuummin2019A2 <- filtfilt(lowbf, filtervacuummin2019A2$data)
lowpassvacuummin2019A2 <- data.frame(time = filtervacuummin2019A2$time,
                                       date = filtervacuummin2019A2$date,
                                       vacuum = lowfiltervacuummin2019A2)
lowpassvacuummin2019A2fin <- lowpassvacuummin2019A2 %>%
  gather(type, data, vacuum)

# 2A cake wash
filtercakewashmin2019A2 <- na.omit(filtercakewashmin2019A2)
lowfiltercakewashmin2019A2 <- filtfilt(lowbf, filtercakewashmin2019A2$data)
lowpasscakewashmin2019A2 <- data.frame(time = filtercakewashmin2019A2$time,
                                       date = filtercakewashmin2019A2$date,
                                       cakewash = lowfiltercakewashmin2019A2)
lowpasscakewashmin2019A2fin <- lowpasscakewashmin2019A2 %>%
  gather(type, data, cakewash)

# 1B Drum----------------------------------------------------------------------------------------
# 1B feed flow
filterfeedflowmin2019B1 <- na.omit(filterfeedflowmin2019B1)
lowfilterfeedflowmin2019B1 <- filtfilt(lowbf, filterfeedflowmin2019B1$data)
lowpassfeedflowmin2019B1 <- data.frame(time = filterfeedflowmin2019B1$time,
                                       date = filterfeedflowmin2019B1$date,
                                       filter = lowfilterfeedflowmin2019B1,
                                       nofilter = filterfeedflowmin2019B1$data)
lowpassfeedflowmin2019B1fin <- lowpassfeedflowmin2019B1 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassfeedflowmin2019B1fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Feed Flow (kl/h)") + 
  ggtitle("1B Drum Feed Flow (2019: 30min data)")

# 1B drum speed
filterdrumspeedmin2019B1 <- na.omit(filterdrumspeedmin2019B1)
lowfilterdrumspeedmin2019B1 <- filtfilt(lowbf, filterdrumspeedmin2019B1$data)
lowpassdrumspeedmin2019B1 <- data.frame(time = filterdrumspeedmin2019B1$time,
                                        date = filterdrumspeedmin2019B1$date,
                                        drumspeed = lowfilterdrumspeedmin2019B1)
lowpassdrumspeedmin2019B1fin <- lowpassdrumspeedmin2019B1 %>%
  gather(type, data, drumspeed)

# 1B bath level
filterbathlevelmin2019B1 <- na.omit(filterbathlevelmin2019B1)
lowfilterbathlevelmin2019B1 <- filtfilt(lowbf, filterbathlevelmin2019B1$data)
lowpassbathlevelmin2019B1 <- data.frame(time = filterbathlevelmin2019B1$time,
                                        date = filterbathlevelmin2019B1$date,
                                        bathlevel = lowfilterbathlevelmin2019B1)
lowpassbathlevelmin2019B1fin <- lowpassbathlevelmin2019B1 %>%
  gather(type, data, bathlevel)

# 1B vacuum 
filtervacuummin2019B1 <- na.omit(filtervacuummin2019B1)
lowfiltervacuummin2019B1 <- filtfilt(lowbf, filtervacuummin2019B1$data)
lowpassvacuummin2019B1 <- data.frame(time = filtervacuummin2019B1$time,
                                     date = filtervacuummin2019B1$date,
                                     vacuum = lowfiltervacuummin2019B1)
lowpassvacuummin2019B1fin <- lowpassvacuummin2019B1 %>%
  gather(type, data, vacuum)

# 1B cake wash
filtercakewashmin2019B1 <- na.omit(filtercakewashmin2019B1)
lowfiltercakewashmin2019B1 <- filtfilt(lowbf, filtercakewashmin2019B1$data)
lowpasscakewashmin2019B1 <- data.frame(time = filtercakewashmin2019B1$time,
                                       date = filtercakewashmin2019B1$date,
                                       cakewash = lowfiltercakewashmin2019B1)
lowpasscakewashmin2019B1fin <- lowpasscakewashmin2019B1 %>%
  gather(type, data, cakewash)








#------------------------------------------------------------------------------------------------
#fourier analysis--------------------------------------------------------------------------------
#throughput
throughputmin2019.ts <- ts(filterthroughputmin2019$data) #change to time series
throughputmin2019.freq <- (1/(30*60))  #sample frequency in Hz 
throughputmin2019.dur <- length(throughputmin2019.ts)*30*60 # length of signal in seconds 
throughputmin2019.tot <- throughputmin2019.freq*throughputmin2019.dur #total number of sample

throughputmin2019.x <- seq(0, throughputmin2019.dur, length.out = throughputmin2019.tot)
throughputmin2019.fourier <- fft(throughputmin2019.ts)
throughputmin2019.amo <- 2*Mod(throughputmin2019.fourier)/(throughputmin2019.tot) #amplitude
throughputmin2019.freqvec <- 0:(length(throughputmin2019.amo)-1) #vector from 0 to end of signal -> new x-axis

throughputmin2019.amo[throughputmin2019.freqvec == 0] <- 
  throughputmin2019.amo[throughputmin2019.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
throughputmin2019.freqvec <-
  throughputmin2019.freqvec/throughputmin2019.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
throughputmin2019.dfFouri <- data.frame(freq = throughputmin2019.freqvec[1:as.integer(0.5*throughputmin2019.freq*throughputmin2019.dur)]*60*60*24,
                                        amount = throughputmin2019.amo[1:as.integer(0.5*throughputmin2019.freq*throughputmin2019.dur)])
throughputmin2019.dfFouri <- throughputmin2019.dfFouri %>%
  mutate(period = ifelse(throughputmin2019.dfFouri$amount >= 1,
                         paste(round(1/throughputmin2019.dfFouri$freq,2),"days"),""))
throughputmin2019.dfFouri[1,3] <- ""
ggplot(throughputmin2019.dfFouri,aes(x=freq,y=amount)) + 
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label = period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) +
  labs(x = "Freq, 1/day",y = "Magnitude", title = "Throughput Fourier Analysis")

# 2A Drum----------------------------------------------------------------------------------------
# 2A feed flow
feedflowmin2019A2.ts <- ts(filterfeedflowmin2019A2$data) #change to time series
feedflowmin2019A2.freq <- (1/(30*60))  #sample frequency in Hz 
feedflowmin2019A2.dur <- length(feedflowmin2019A2.ts)*30*60 # length of signal in seconds 
feedflowmin2019A2.tot <- feedflowmin2019A2.freq*feedflowmin2019A2.dur #total number of sample

feedflowmin2019A2.x <- seq(0, feedflowmin2019A2.dur, length.out = tot)
feedflowmin2019A2.fourier <- fft(feedflowmin2019A2.ts)
feedflowmin2019A2.amo <- 2*Mod(feedflowmin2019A2.fourier)/(feedflowmin2019A2.tot) #amplitude
feedflowmin2019A2.freqvec <- 0:(length(feedflowmin2019A2.amo)-1) #vector from 0 to end of signal -> new x-axis

feedflowmin2019A2.amo[feedflowmin2019A2.freqvec == 0] <-
  feedflowmin2019A2.amo[feedflowmin2019A2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
feedflowmin2019A2.freqvec <- feedflowmin2019A2.freqvec/feedflowmin2019A2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
feedflowmin2019A2.dfFouri <- data.frame(freq = feedflowmin2019A2.freqvec[1:as.integer(0.5*feedflowmin2019A2.freq*feedflowmin2019A2.dur)]*60*60*24,
                                        amount = feedflowmin2019A2.amo[1:as.integer(0.5*feedflowmin2019A2.freq*feedflowmin2019A2.dur)])
feedflowmin2019A2.dfFouri <- feedflowmin2019A2.dfFouri %>%
  mutate(period = ifelse(feedflowmin2019A2.dfFouri$amount >= 1,
                         paste(round(1/feedflowmin2019A2.dfFouri$freq, 2),"days"),""))
feedflowmin2019A2.dfFouri[1,3] <- ""
ggplot(feedflowmin2019A2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", title ="2A Drum Fourier Analysis")
