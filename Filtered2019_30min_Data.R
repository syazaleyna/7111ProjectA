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

#-------------------------------------------------------------------------------------------
#extract 2B data from 30min observation
status30min2019B2 <- seedwash30min2019$PO2BFOLXBDI
drumspeed30min2019B2 <- seedwash30min2019$PO2BFDrumSTPV
bathlevel30min2019B2 <- seedwash30min2019$PO2BFBathLCPV
vacuum30min2019B2 <- seedwash30min2019$PO2BVacPCPV
feedflow30min2019B2 <- seedwash30min2019$PO2BFRSFFCPV
flocflow30min2019B2 <- seedwash30min2019$PO1FlocFTPV...170
cakewash30min2019B2 <- seedwash30min2019$PO2BFCaSFTPV
clothwash30min2019B2 <- seedwash30min2019$PO2BFCoSFTPV
sodafilt30min2019B2 <- seedwash30min2019$PO2BFiltATPV
oxfilt30min2019B2 <- seedwash30min2019$PO1AN.Ox...182


data30min2019B2 <- data.frame(time = timestep30min2019,           #day
                              date = date30min2019,
                              status = status30min2019B2,
                              outputspo = outputspo30min2019,  #%
                              outputsoda = outputsoda30min2019,   #g/l
                              outputalumina = outputalumina30min2019, #kl/h
                              throughput = throughput30min2019,   #t/h
                              feedspo = spo30min2019,      #%
                              feedsoda = feedsoda30min2019,    #g/l
                              feeddensity = feeddensity30min2019, #SG
                              drumspeed = drumspeed30min2019B2,   #RPM
                              bathlevel = bathlevel30min2019B2,   #%
                              vacuum = vacuum30min2019B2,         #kPa
                              feedflow = feedflow30min2019B2,     #kl/h
                              flocflow = flocflow30min2019B2,     #kl/h
                              cakewash = cakewash30min2019B2,     #kl/h
                              clothwash = clothwash30min2019B2,   #kl/h
                              sodafiltrate = sodafilt30min2019B2, #g/l
                              oxfiltrate = oxfilt30min2019B2)     #g/l


multiple30min2019B2 <- data30min2019B2 %>%
  filter(status == 'On') %>%
  gather(type, data, outputspo:oxfiltrate)

ggplot(multiple30min2019B2, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  xlab("Time (hour)") + ggtitle("2019 Filter 2B Data Comparison")
# ggplot(multipledaily, aes(x=time, y=data)) + geom_line() + facet_wrap(~type)

#drum speed
drumspeedmin2019B2 <- multiple30min2019B2 %>%
  filter(type == 'drumspeed')
ggplot(drumspeedmin2019B2, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Drum Speed (RPM)") +
  ggtitle("2019 Filter 2B Drum Speed")

#bath level
bathlevelmin2019B2 <- multiple30min2019B2 %>%
  filter( type == 'bathlevel')
ggplot(bathlevelmin2019B2, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) + 
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Bath Level (%)") +
  ggtitle("2019 Filter 2B Bath Level")

#vacuum pressure
vacuummin2019B2 <- multiple30min2019B2 %>%
  filter( type == 'vacuum')
ggplot(vacuummin2019B2, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") +
  ggtitle("2019 Filter 2B Vacuum Pressure")


#feed flow
feedflowmin2019B2 <- multiple30min2019B2 %>%
  filter( type == 'feedflow')
ggplot(feedflowmin2019B2, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Feed Flow (kl/h)") +
  ggtitle("2019 Filter 2B Feed Flow")


flocflowmin2019B2 <- multiple30min2019B2 %>%
  filter(type == 'flocflow')
ggplot(flocflowmin2019B2, aes(x=time, y=data)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Floc Flow (kl/h)") +
  ggtitle("2019 Filter 2B Floc Flow")


cakewashmin2019B2 <- multiple30min2019B2 %>%
  filter( type == 'cakewash')
ggplot(cakewashmin2019B2, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") +
  ggtitle("2019 Filter 2B Cake Wash Flow")

clothwashmin2019B2 <- multiple30min2019B2 %>%
  filter( type == 'clothwash')
ggplot(clothwashmin2019B2, aes(x=time, y=data, color = type)) + geom_line(alpha = 0.8) +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Cloth Wash Flow (kl/h)") +
  ggtitle("2019 Filter 2B Cloth Wash Flow")


sodafiltratemin2019B2 <- multiple30min2019B2 %>%
  filter( type == 'sodafiltrate')
ggplot(sodafiltratemin2019B2, aes(x=time, y=data, color = type)) + geom_line() +
  geom_smooth(method = lm) + xlab("Time (hour)") + ylab("Filtrate Soda Conc. (g/l)") +
  ggtitle("2019 Filter 2B Filtrate Soda Conc.")

oxfiltratemin2019B2 <- multiple30min2019B2 %>%
  filter( type == 'oxfiltrate')
ggplot(oxfiltratemin2019B2) + geom_point(aes(x=time, y=data, color = type)) +
  geom_line(data = oxfiltratemin2019B2[!is.na(oxfiltratemin2019B1$data),],aes(x=time, y=data)) +
  xlab("Time (hour)") + ylab("Filtrate Oxalate (%)") +
  ggtitle("2019 Filter 2B Filtrate Oxalate")
          
          
          




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
rtestfeedflowmin2019A1 <- rosnerTest(feedflowmin2019A1$data, k=50)

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

# 1A soda filtrate conc.
rtestsodafiltratemin2019A1 <- rosnerTest(sodafiltratemin2019A1$data, k=30)

if (rtestsodafiltratemin2019A1$n.outliers > 0){
  finalrtestsodafiltratemin2019A1 <- rosnerTest(sodafiltratemin2019A1$data, k=rtestsodafiltratemin2019A1$n.outliers)
  
  filtersodafiltratemin2019A1 <- sodafiltratemin2019A1[-c(finalrtestsodafiltratemin2019A1$all.stats$Obs.Num),]
  
  ggplot(filtersodafiltratemin2019A1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Soda Filtrate Conc. (g/l)") + 
    ggtitle("2019 Filter 1A Soda Filtrate Conc. (filtered)")
} else {
  filtersodafiltratemin2019A1 <- sodafiltratemin2019A1
}

# 1A oxalate filtrate 
rtestoxfiltratemin2019A1 <- rosnerTest(oxfiltratemin2019A1$data, k=30)

if (rtestoxfiltratemin2019A1$n.outliers > 0){
  finalrtestoxfiltratemin2019A1 <- rosnerTest(oxfiltratemin2019A1$data, k=rtestoxfiltratemin2019A1$n.outliers)
  
  filteroxfiltratemin2019A1 <- oxfiltratemin2019A1[-c(finalrtestoxfiltratemin2019A1$all.stats$Obs.Num),]
  
  ggplot(filteroxfiltratemin2019A1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Oxalate Filtrate (%)") + 
    ggtitle("2019 Filter 1A Oxalate Filtrate (filtered)")
} else {
  filteroxfiltratemin2019A1 <- oxfiltratemin2019A1
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

# 2A soda filtrate conc.
rtestsodafiltratemin2019A2 <- rosnerTest(sodafiltratemin2019A2$data, k=30)

if (rtestsodafiltratemin2019A2$n.outliers > 0){
  finalrtestsodafiltratemin2019A2 <- rosnerTest(sodafiltratemin2019A2$data, k=rtestsodafiltratemin2019A2$n.outliers)
  
  filtersodafiltratemin2019A2 <- sodafiltratemin2019A2[-c(finalrtestsodafiltratemin2019A2$all.stats$Obs.Num),]
  
  ggplot(filtersodafiltratemin2019A2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Soda Filtrate Conc. (g/l)") + 
    ggtitle("2019 Filter 2A Soda Filtrate Conc. (filtered)")
} else {
  filtersodafiltratemin2019A2 <- sodafiltratemin2019A2
}

# 2A oxalate filtrate 
rtestoxfiltratemin2019A2 <- rosnerTest(oxfiltratemin2019A2$data, k=30)

if (rtestoxfiltratemin2019A2$n.outliers > 0){
  finalrtestoxfiltratemin2019A2 <- rosnerTest(oxfiltratemin2019A2$data, k=rtestoxfiltratemin2019A2$n.outliers)
  
  filteroxfiltratemin2019A2 <- oxfiltratemin2019A2[-c(finalrtestoxfiltratemin2019A2$all.stats$Obs.Num),]
  
  ggplot(filteroxfiltratemin2019A2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Oxalate Filtrate (%)") + 
    ggtitle("2019 Filter 2A Oxalate Filtrate (filtered)")
} else {
  filteroxfiltratemin2019A2 <- oxfiltratemin2019A2
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

# 1B soda filtrate conc.
rtestsodafiltratemin2019B1 <- rosnerTest(sodafiltratemin2019B1$data, k=100)

if (rtestsodafiltratemin2019B1$n.outliers > 0){
  finalrtestsodafiltratemin2019B1 <- rosnerTest(sodafiltratemin2019B1$data, k=rtestsodafiltratemin2019B1$n.outliers)
  
  filtersodafiltratemin2019B1 <- sodafiltratemin2019B1[-c(finalrtestsodafiltratemin2019B1$all.stats$Obs.Num),]
  
  ggplot(filtersodafiltratemin2019B1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Soda Filtrate Conc. (g/l)") + 
    ggtitle("2019 Filter 1B Soda Filtrate Conc. (filtered)")
} else {
  filtersodafiltratemin2019B1 <- sodafiltratemin2019B1
}

# 1B oxalate filtrate 
rtestoxfiltratemin2019B1 <- rosnerTest(oxfiltratemin2019B1$data, k=30)

if (rtestoxfiltratemin2019B1$n.outliers > 0){
  finalrtestoxfiltratemin2019B1 <- rosnerTest(oxfiltratemin2019B1$data, k=rtestoxfiltratemin2019B1$n.outliers)
  
  filteroxfiltratemin2019B1 <- oxfiltratemin2019B1[-c(finalrtestoxfiltratemin2019B1$all.stats$Obs.Num),]
  
  ggplot(filteroxfiltratemin2019B1, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Oxalate Filtrate (%)") + 
    ggtitle("2019 Filter 1B Oxalate Filtrate (filtered)")
} else {
  filteroxfiltratemin2019B1 <- oxfiltratemin2019B1
}


# 2B Drum-----------------------------------------------------------------------------------------
# 2B feedflow 
rtestfeedflowmin2019B2 <- rosnerTest(feedflowmin2019B2$data, k=100)

if (rtestfeedflowmin2019B2$n.outliers > 0){
  finalrtestfeedflowmin2019B2 <- rosnerTest(feedflowmin2019B2$data, k=rtestfeedflowmin2019B2$n.outliers)
  
  filterfeedflowmin2019B2 <- feedflowmin2019B2[-c(finalrtestfeedflowmin2019B2$all.stats$Obs.Num),]
  
  ggplot(filterfeedflowmin2019B2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Feed Flow (kl/h)") +
    ggtitle("2019 Filter 2B Feed Flow (filtered)")
} else {
  filterfeedflowmin2019B2 <- feedflowmin2019B2
}

# 2B drum speed
rtestdrumspeedmin2019B2 <- rosnerTest(drumspeedmin2019B2$data, k=100)

if (rtestdrumspeedmin2019B2$n.outliers > 0){
  finalrtestdrumspeedmin2019B2 <- rosnerTest(drumspeedmin2019B2$data, k=rtestdrumspeedmin2019B2$n.outliers)
  
  filterdrumspeedmin2019B2 <- drumspeedmin2019B2[-c(finalrtestdrumspeedmin2019B2$all.stats$Obs.Num),]
  
  ggplot(filterdrumspeedmin2019B2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Drum Speed (RPM)") + 
    ggtitle("2019 Filter 2B Drum Speed (filtered)")
} else {
  filterdrumspeedmin2019B2 <- drumspeedmin2019B2
}

# 2B bath level
rtestbathlevelmin2019B2 <- rosnerTest(bathlevelmin2019B2$data, k=100)

if (rtestbathlevelmin2019B2$n.outliers > 0){
  finalrtestbathlevelmin2019B2 <- rosnerTest(bathlevelmin2019B2$data, k=rtestbathlevelmin2019B2$n.outliers)
  
  filterbathlevelmin2019B2 <- bathlevelmin2019B2[-c(finalrtestbathlevelmin2019B2$all.stats$Obs.Num),]
  
  ggplot(filterbathlevelmin2019B2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Bath Level (%)") + 
    ggtitle("2019 Filter 2B Bath Level (filtered)")
} else {
  filterbathlevelmin2019B2 <- bathlevelmin2019B2
}

# 2B vacuum
rtestvacuummin2019B2 <- rosnerTest(vacuummin2019B2$data, k=100)

if (rtestvacuummin2019B2$n.outliers > 0){
  finalrtestvacuummin2019B2 <- rosnerTest(vacuummin2019B2$data, k=rtestvacuummin2019B2$n.outliers)
  
  filtervacuummin2019B2 <- vacuummin2019B2[-c(finalrtestvacuummin2019B2$all.stats$Obs.Num),]
  
  ggplot(filtervacuummin2019B2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") +
    ggtitle("2019 Filter 2B Vacuum Pressure (filtered)")
} else {
  filtervacuummin2019B2 <- vacuummin2019B2
}

# 2B cake wash
rtestcakewashmin2019B2 <- rosnerTest(cakewashmin2019B2$data, k=100)

if (rtestcakewashmin2019B2$n.outliers > 0){
  finalrtestcakewashmin2019B2 <- rosnerTest(cakewashmin2019B2$data, k=rtestcakewashmin2019B2$n.outliers)
  
  filtercakewashmin2019B2 <- cakewashmin2019B2[-c(finalrtestcakewashmin2019B2$all.stats$Obs.Num),]
  
  ggplot(filtercakewashmin2019B2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") + 
    ggtitle("2019 Filter 2B Cake Wash Flow (filtered)")
} else {
  filtercakewashmin2019B2 <- cakewashmin2019B2
}

# 2B soda filtrate conc.
rtestsodafiltratemin2019B2 <- rosnerTest(sodafiltratemin2019B2$data, k=100)

if (rtestsodafiltratemin2019B2$n.outliers > 0){
  finalrtestsodafiltratemin2019B2 <- rosnerTest(sodafiltratemin2019B2$data, k=rtestsodafiltratemin2019B2$n.outliers)
  
  filtersodafiltratemin2019B2 <- sodafiltratemin2019B2[-c(finalrtestsodafiltratemin2019B2$all.stats$Obs.Num),]
  
  ggplot(filtersodafiltratemin2019B2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Soda Filtrate Conc. (g/l)") + 
    ggtitle("2019 Filter 2B Soda Filtrate Conc. (filtered)")
} else {
  filtersodafiltratemin2019B2 <- sodafiltratemin2019B2
}

# 2B oxalate filtrate 
rtestoxfiltratemin2019B2 <- rosnerTest(oxfiltratemin2019B2$data, k=30)

if (rtestoxfiltratemin2019B2$n.outliers > 0){
  finalrtestoxfiltratemin2019B2 <- rosnerTest(oxfiltratemin2019B2$data, k=rtestoxfiltratemin2019B2$n.outliers)
  
  filteroxfiltratemin2019B2 <- oxfiltratemin2019B2[-c(finalrtestoxfiltratemin2019B2$all.stats$Obs.Num),]
  
  ggplot(filteroxfiltratemin2019B2, aes(x=date, y=data, color = type)) + 
    geom_line() + geom_point(aes(color = type)) +
    scale_colour_manual(values=c("blue")) + 
    xlab("Time (hour)") + ylab("Oxalate Filtrate (%)") + 
    ggtitle("2019 Filter 2B Oxalate Filtrate (filtered)")
} else {
  filteroxfiltratemin2019B2 <- oxfiltratemin2019B2
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
  xlab("Time (hour)") + ylab("Throughput (t/h)") + 
  ggtitle("Throughput (2019: 30min data)")


# 1A Drum----------------------------------------------------------------------------------------
# 1A feed flow
filterfeedflowmin2019A1 <- na.omit(filterfeedflowmin2019A1)
lowfilterfeedflowmin2019A1 <- filtfilt(lowbf, filterfeedflowmin2019A1$data)
lowpassfeedflowmin2019A1 <- data.frame(time = filterfeedflowmin2019A1$time,
                                       date = filterfeedflowmin2019A1$date,
                                       filter = lowfilterfeedflowmin2019A1,
                                       nofilter = filterfeedflowmin2019A1$data)
lowpassfeedflowmin2019A1fin <- lowpassfeedflowmin2019A1 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassfeedflowmin2019A1fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Feed Flow (kl/h)") + 
  ggtitle("1A Drum Feed Flow (2019: 30min data)")

# 1A drum speed
filterdrumspeedmin2019A1 <- na.omit(filterdrumspeedmin2019A1)
lowfilterdrumspeedmin2019A1 <- filtfilt(lowbf, filterdrumspeedmin2019A1$data)
lowpassdrumspeedmin2019A1 <- data.frame(time = filterdrumspeedmin2019A1$time,
                                        date = filterdrumspeedmin2019A1$date,
                                        filter = lowfilterdrumspeedmin2019A1,
                                        nofilter = filterdrumspeedmin2019A1$data)
lowpassdrumspeedmin2019A1fin <- lowpassdrumspeedmin2019A1 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassdrumspeedmin2019A1fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Drum Speed (RPM)") + 
  ggtitle("1A Drum Drum Speed (2019: 30min data)")

# 1A bath level
filterbathlevelmin2019A1 <- na.omit(filterbathlevelmin2019A1)
lowfilterbathlevelmin2019A1 <- filtfilt(lowbf, filterbathlevelmin2019A1$data)
lowpassbathlevelmin2019A1 <- data.frame(time = filterbathlevelmin2019A1$time,
                                        date = filterbathlevelmin2019A1$date,
                                        filter = lowfilterbathlevelmin2019A1,
                                        nofilter = filterbathlevelmin2019A1$data)
lowpassbathlevelmin2019A1fin <- lowpassbathlevelmin2019A1 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassbathlevelmin2019A1fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Bath Level (%)") + 
  ggtitle("1A Drum Bath Level (2019: 30min data)")

# 1A vacuum 
filtervacuummin2019A1 <- na.omit(filtervacuummin2019A1)
lowfiltervacuummin2019A1 <- filtfilt(lowbf, filtervacuummin2019A1$data)
lowpassvacuummin2019A1 <- data.frame(time = filtervacuummin2019A1$time,
                                     date = filtervacuummin2019A1$date,
                                     filter = lowfiltervacuummin2019A1,
                                     nofilter = filtervacuummin2019A1$data)
lowpassvacuummin2019A1fin <- lowpassvacuummin2019A1 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassvacuummin2019A1fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") + 
  ggtitle("1A Drum Vacuum Pressure (2019: 30min data)")

# 1A cake wash
filtercakewashmin2019A1 <- na.omit(filtercakewashmin2019A1)
lowfiltercakewashmin2019A1 <- filtfilt(lowbf, filtercakewashmin2019A1$data)
lowpasscakewashmin2019A1 <- data.frame(time = filtercakewashmin2019A1$time,
                                       date = filtercakewashmin2019A1$date,
                                       filter = lowfiltercakewashmin2019A1,
                                       nofilter = filtercakewashmin2019A1$data)
lowpasscakewashmin2019A1fin <- lowpasscakewashmin2019A1 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpasscakewashmin2019A1fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") + 
  ggtitle("1A Drum Cake Wash Flow (2019: 30min data)")

# 1A soda filtrate conc.
filtersodafiltratemin2019A1 <- na.omit(filtersodafiltratemin2019A1)
lowfiltersodafiltratemin2019A1 <- filtfilt(lowbf, filtersodafiltratemin2019A1$data)
lowpasssodafiltratemin2019A1 <- data.frame(time = filtersodafiltratemin2019A1$time,
                                       date = filtersodafiltratemin2019A1$date,
                                       filter = lowfiltersodafiltratemin2019A1,
                                       nofilter = filtersodafiltratemin2019A1$data)
lowpasssodafiltratemin2019A1fin <- lowpasssodafiltratemin2019A1 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpasssodafiltratemin2019A1fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Soda Filtrate Conc. (g/l)") + 
  ggtitle("1A Drum Soda Filtrate Conc. (2019: 30min data)")


# 1A oxalate filtrate
filteroxfiltratemin2019A1 <- na.omit(filteroxfiltratemin2019A1)
lowfilteroxfiltratemin2019A1 <- filtfilt(lowbf, filteroxfiltratemin2019A1$data)
lowpassoxfiltratemin2019A1 <- data.frame(time = filteroxfiltratemin2019A1$time,
                                           date = filteroxfiltratemin2019A1$date,
                                           filter = lowfilteroxfiltratemin2019A1,
                                           nofilter = filteroxfiltratemin2019A1$data)
lowpassoxfiltratemin2019A1fin <- lowpassoxfiltratemin2019A1 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassoxfiltratemin2019A1fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Oxalate Filtrate (%)") + 
  ggtitle("1A Drum Oxalate Filtrate (2019: 30min data)")


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
                                       filter = lowfilterdrumspeedmin2019A2,
                                       nofilter = filterdrumspeedmin2019A2$data)
lowpassdrumspeedmin2019A2fin <- lowpassdrumspeedmin2019A2 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassdrumspeedmin2019A2fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Drum Speed (RPM)") + 
  ggtitle("2A Drum Drum Speed (2019: 30min data)")

# 2A bath level
filterbathlevelmin2019A2 <- na.omit(filterbathlevelmin2019A2)
lowfilterbathlevelmin2019A2 <- filtfilt(lowbf, filterbathlevelmin2019A2$data)
lowpassbathlevelmin2019A2 <- data.frame(time = filterbathlevelmin2019A2$time,
                                       date = filterbathlevelmin2019A2$date,
                                       filter = lowfilterbathlevelmin2019A2,
                                       nofilter = filterbathlevelmin2019A2$data)
lowpassbathlevelmin2019A2fin <- lowpassbathlevelmin2019A2 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassbathlevelmin2019A2fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Bath Level (%)") + 
  ggtitle("2A Drum Bath Level (2019: 30min data)")

# 2A vacuum 
filtervacuummin2019A2 <- na.omit(filtervacuummin2019A2)
lowfiltervacuummin2019A2 <- filtfilt(lowbf, filtervacuummin2019A2$data)
lowpassvacuummin2019A2 <- data.frame(time = filtervacuummin2019A2$time,
                                       date = filtervacuummin2019A2$date,
                                       filter = lowfiltervacuummin2019A2,
                                     nofilter = filtervacuummin2019A2$data)
lowpassvacuummin2019A2fin <- lowpassvacuummin2019A2 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassvacuummin2019A2fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") + 
  ggtitle("2A Drum Vacuum Pressure (2019: 30min data)")

# 2A cake wash
filtercakewashmin2019A2 <- na.omit(filtercakewashmin2019A2)
lowfiltercakewashmin2019A2 <- filtfilt(lowbf, filtercakewashmin2019A2$data)
lowpasscakewashmin2019A2 <- data.frame(time = filtercakewashmin2019A2$time,
                                       date = filtercakewashmin2019A2$date,
                                       filter = lowfiltercakewashmin2019A2,
                                       nofilter = filtercakewashmin2019A2$data)
lowpasscakewashmin2019A2fin <- lowpasscakewashmin2019A2 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpasscakewashmin2019A2fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") + 
  ggtitle("2A Drum Cake Wash Flow (2019: 30min data)")

# 2A soda filtrate conc.
filtersodafiltratemin2019A2 <- na.omit(filtersodafiltratemin2019A2)
lowfiltersodafiltratemin2019A2 <- filtfilt(lowbf, filtersodafiltratemin2019A2$data)
lowpasssodafiltratemin2019A2 <- data.frame(time = filtersodafiltratemin2019A2$time,
                                           date = filtersodafiltratemin2019A2$date,
                                           filter = lowfiltersodafiltratemin2019A2,
                                           nofilter = filtersodafiltratemin2019A2$data)
lowpasssodafiltratemin2019A2fin <- lowpasssodafiltratemin2019A2 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpasssodafiltratemin2019A2fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Soda Filtrate Conc. (g/l)") + 
  ggtitle("2A Drum Soda Filtrate Conc. (2019: 30min data)")


# 2A oxalate filtrate
filteroxfiltratemin2019A2 <- na.omit(filteroxfiltratemin2019A2)
lowfilteroxfiltratemin2019A2 <- filtfilt(lowbf, filteroxfiltratemin2019A2$data)
lowpassoxfiltratemin2019A2 <- data.frame(time = filteroxfiltratemin2019A2$time,
                                         date = filteroxfiltratemin2019A2$date,
                                         filter = lowfilteroxfiltratemin2019A2,
                                         nofilter = filteroxfiltratemin2019A2$data)
lowpassoxfiltratemin2019A2fin <- lowpassoxfiltratemin2019A2 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassoxfiltratemin2019A2fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Oxalate Filtrate (%)") + 
  ggtitle("2A Drum Oxalate Filtrate (2019: 30min data)")


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
                                        filter = lowfilterdrumspeedmin2019B1,
                                        nofilter = filterdrumspeedmin2019B1$data)
lowpassdrumspeedmin2019B1fin <- lowpassdrumspeedmin2019B1 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassdrumspeedmin2019B1fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Drum Speed (RPM)") + 
  ggtitle("1B Drum Drum Speed (2019: 30min data)")

# 1B bath level
filterbathlevelmin2019B1 <- na.omit(filterbathlevelmin2019B1)
lowfilterbathlevelmin2019B1 <- filtfilt(lowbf, filterbathlevelmin2019B1$data)
lowpassbathlevelmin2019B1 <- data.frame(time = filterbathlevelmin2019B1$time,
                                        date = filterbathlevelmin2019B1$date,
                                        filter = lowfilterbathlevelmin2019B1,
                                        nofilter = filterbathlevelmin2019B1$data)
lowpassbathlevelmin2019B1fin <- lowpassbathlevelmin2019B1 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassbathlevelmin2019B1fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Bath Level (%)") + 
  ggtitle("1B Drum Bath Level (2019: 30min data)")

# 1B vacuum 
filtervacuummin2019B1 <- na.omit(filtervacuummin2019B1)
lowfiltervacuummin2019B1 <- filtfilt(lowbf, filtervacuummin2019B1$data)
lowpassvacuummin2019B1 <- data.frame(time = filtervacuummin2019B1$time,
                                     date = filtervacuummin2019B1$date,
                                     filter = lowfiltervacuummin2019B1,
                                     nofilter = filtervacuummin2019B1$data)
lowpassvacuummin2019B1fin <- lowpassvacuummin2019B1 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassvacuummin2019B1fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") + 
  ggtitle("1B Drum Vacuum Pressure (2019: 30min data)")

# 1B cake wash
filtercakewashmin2019B1 <- na.omit(filtercakewashmin2019B1)
lowfiltercakewashmin2019B1 <- filtfilt(lowbf, filtercakewashmin2019B1$data)
lowpasscakewashmin2019B1 <- data.frame(time = filtercakewashmin2019B1$time,
                                       date = filtercakewashmin2019B1$date,
                                       filter = lowfiltercakewashmin2019B1,
                                       nofilter = filtercakewashmin2019B1$data)
lowpasscakewashmin2019B1fin <- lowpasscakewashmin2019B1 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpasscakewashmin2019B1fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") + 
  ggtitle("1B Drum Cake Wash Flow (2019: 30min data)")

# 1B soda filtrate conc.
filtersodafiltratemin2019B1 <- na.omit(filtersodafiltratemin2019B1)
lowfiltersodafiltratemin2019B1 <- filtfilt(lowbf, filtersodafiltratemin2019B1$data)
lowpasssodafiltratemin2019B1 <- data.frame(time = filtersodafiltratemin2019B1$time,
                                           date = filtersodafiltratemin2019B1$date,
                                           filter = lowfiltersodafiltratemin2019B1,
                                           nofilter = filtersodafiltratemin2019B1$data)
lowpasssodafiltratemin2019B1fin <- lowpasssodafiltratemin2019B1 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpasssodafiltratemin2019B1fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Soda Filtrate Conc. (g/l)") + 
  ggtitle("1B Drum Soda Filtrate Conc. (2019: 30min data)")


# 1B oxalate filtrate
filteroxfiltratemin2019B1 <- na.omit(filteroxfiltratemin2019B1)
lowfilteroxfiltratemin2019B1 <- filtfilt(lowbf, filteroxfiltratemin2019B1$data)
lowpassoxfiltratemin2019B1 <- data.frame(time = filteroxfiltratemin2019B1$time,
                                         date = filteroxfiltratemin2019B1$date,
                                         filter = lowfilteroxfiltratemin2019B1,
                                         nofilter = filteroxfiltratemin2019B1$data)
lowpassoxfiltratemin2019B1fin <- lowpassoxfiltratemin2019B1 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassoxfiltratemin2019B1fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Oxalate Filtrate (%)") + 
  ggtitle("1B Drum Oxalate Filtrate (2019: 30min data)")


# 2B Drum----------------------------------------------------------------------------------------
# 2B feed flow
filterfeedflowmin2019B2 <- na.omit(filterfeedflowmin2019B2)
lowfilterfeedflowmin2019B2 <- filtfilt(lowbf, filterfeedflowmin2019B2$data)
lowpassfeedflowmin2019B2 <- data.frame(time = filterfeedflowmin2019B2$time,
                                       date = filterfeedflowmin2019B2$date,
                                       filter = lowfilterfeedflowmin2019B2,
                                       nofilter = filterfeedflowmin2019B2$data)
lowpassfeedflowmin2019B2fin <- lowpassfeedflowmin2019B2 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassfeedflowmin2019B2fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Feed Flow (kl/h)") + 
  ggtitle("2B Drum Feed Flow (2019: 30min data)")

# 2B drum speed
filterdrumspeedmin2019B2 <- na.omit(filterdrumspeedmin2019B2)
lowfilterdrumspeedmin2019B2 <- filtfilt(lowbf, filterdrumspeedmin2019B2$data)
lowpassdrumspeedmin2019B2 <- data.frame(time = filterdrumspeedmin2019B2$time,
                                        date = filterdrumspeedmin2019B2$date,
                                        filter = lowfilterdrumspeedmin2019B2,
                                        nofilter = filterdrumspeedmin2019B2$data)
lowpassdrumspeedmin2019B2fin <- lowpassdrumspeedmin2019B2 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassdrumspeedmin2019B2fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Drum Speed (RPM)") + 
  ggtitle("2B Drum Drum Speed (2019: 30min data)")

# 2B bath level
filterbathlevelmin2019B2 <- na.omit(filterbathlevelmin2019B2)
lowfilterbathlevelmin2019B2 <- filtfilt(lowbf, filterbathlevelmin2019B2$data)
lowpassbathlevelmin2019B2 <- data.frame(time = filterbathlevelmin2019B2$time,
                                        date = filterbathlevelmin2019B2$date,
                                        filter = lowfilterbathlevelmin2019B2,
                                        nofilter = filterbathlevelmin2019B2$data)
lowpassbathlevelmin2019B2fin <- lowpassbathlevelmin2019B2 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassbathlevelmin2019B2fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Bath Level (%)") + 
  ggtitle("2B Drum Bath Level (2019: 30min data)")


# 2B vacuum 
filtervacuummin2019B2 <- na.omit(filtervacuummin2019B2)
lowfiltervacuummin2019B2 <- filtfilt(lowbf, filtervacuummin2019B2$data)
lowpassvacuummin2019B2 <- data.frame(time = filtervacuummin2019B2$time,
                                     date = filtervacuummin2019B2$date,
                                     filter = lowfiltervacuummin2019B2,
                                     nofilter = filtervacuummin2019B2$data)
lowpassvacuummin2019B2fin <- lowpassvacuummin2019B2 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassvacuummin2019B2fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Vacuum Pressure (kPa)") + 
  ggtitle("2B Drum Vacuum Pressure (2019: 30min data)")


# 2B cake wash
filtercakewashmin2019B2 <- na.omit(filtercakewashmin2019B2)
lowfiltercakewashmin2019B2 <- filtfilt(lowbf, filtercakewashmin2019B2$data)
lowpasscakewashmin2019B2 <- data.frame(time = filtercakewashmin2019B2$time,
                                       date = filtercakewashmin2019B2$date,
                                       filter = lowfiltercakewashmin2019B2,
                                       nofilter = filtercakewashmin2019B2$data)
lowpasscakewashmin2019B2fin <- lowpasscakewashmin2019B2 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpasscakewashmin2019B2fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Cake Wash Flow (kl/h)") + 
  ggtitle("2B Drum Cake Wash Flow (2019: 30min data)")

# 2B soda filtrate conc.
filtersodafiltratemin2019B2 <- na.omit(filtersodafiltratemin2019B2)
lowfiltersodafiltratemin2019B2 <- filtfilt(lowbf, filtersodafiltratemin2019B2$data)
lowpasssodafiltratemin2019B2 <- data.frame(time = filtersodafiltratemin2019B2$time,
                                           date = filtersodafiltratemin2019B2$date,
                                           filter = lowfiltersodafiltratemin2019B2,
                                           nofilter = filtersodafiltratemin2019B2$data)
lowpasssodafiltratemin2019B2fin <- lowpasssodafiltratemin2019B2 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpasssodafiltratemin2019B2fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Soda Filtrate Conc. (g/l)") + 
  ggtitle("2B Drum Soda Filtrate Conc. (2019: 30min data)")


# 2B oxalate filtrate
filteroxfiltratemin2019B2 <- na.omit(filteroxfiltratemin2019B2)
lowfilteroxfiltratemin2019B2 <- filtfilt(lowbf, filteroxfiltratemin2019B2$data)
lowpassoxfiltratemin2019B2 <- data.frame(time = filteroxfiltratemin2019B2$time,
                                         date = filteroxfiltratemin2019B2$date,
                                         filter = lowfilteroxfiltratemin2019B2,
                                         nofilter = filteroxfiltratemin2019B2$data)
lowpassoxfiltratemin2019B2fin <- lowpassoxfiltratemin2019B2 %>%
  gather(type, data, filter:nofilter)

ggplot(lowpassoxfiltratemin2019B2fin, aes(x=time, y=data, color = type, alpha = type)) + 
  geom_line() + 
  scale_alpha_manual(values=c(1,0.5)) + 
  scale_colour_manual(values=c("red", "blue")) + 
  xlab("Time (hour)") + ylab("Oxalate Filtrate (%)") + 
  ggtitle("2B Drum Oxalate Filtrate (2019: 30min data)")





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
  xlim(0,2) + ylim(0,20) +
  labs(x = "Freq, 1/day",y = "Magnitude", title = "Throughput Fourier Analysis")

# 1A Drum----------------------------------------------------------------------------------------
# 1A feed flow
feedflowmin2019A1.ts <- ts(filterfeedflowmin2019A1$data) #change to time series
feedflowmin2019A1.freq <- (1/(30*60))  #sample frequency in Hz 
feedflowmin2019A1.dur <- length(feedflowmin2019A1.ts)*30*60 # length of signal in seconds 
feedflowmin2019A1.tot <- feedflowmin2019A1.freq*feedflowmin2019A1.dur #total number of sample

feedflowmin2019A1.x <- seq(0, feedflowmin2019A1.dur, length.out = feedflowmin2019A1.tot)
feedflowmin2019A1.fourier <- fft(feedflowmin2019A1.ts)
feedflowmin2019A1.amo <- 2*Mod(feedflowmin2019A1.fourier)/(feedflowmin2019A1.tot) #amplitude
feedflowmin2019A1.freqvec <- 0:(length(feedflowmin2019A1.amo)-1) #vector from 0 to end of signal -> new x-axis

feedflowmin2019A1.amo[feedflowmin2019A1.freqvec == 0] <-
  feedflowmin2019A1.amo[feedflowmin2019A1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
feedflowmin2019A1.freqvec <- feedflowmin2019A1.freqvec/feedflowmin2019A1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
feedflowmin2019A1.dfFouri <- data.frame(freq = feedflowmin2019A1.freqvec[1:as.integer(0.5*feedflowmin2019A1.freq*feedflowmin2019A1.dur)]*60*60*24,
                                        amount = feedflowmin2019A1.amo[1:as.integer(0.5*feedflowmin2019A1.freq*feedflowmin2019A1.dur)])
feedflowmin2019A1.dfFouri <- feedflowmin2019A1.dfFouri %>%
  mutate(period = ifelse(feedflowmin2019A1.dfFouri$amount >= 1,
                         paste(round(1/feedflowmin2019A1.dfFouri$freq, 2),"days"),""))
feedflowmin2019A1.dfFouri[1,3] <- ""
ggplot(feedflowmin2019A1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1A Drum Feed Flow Fourier Analysis")

# 1A drum speed
drumspeedmin2019A1.ts <- ts(filterdrumspeedmin2019A1$data) #change to time series
drumspeedmin2019A1.freq <- (1/(30*60))  #sample frequency in Hz 
drumspeedmin2019A1.dur <- length(drumspeedmin2019A1.ts)*30*60 # length of signal in seconds 
drumspeedmin2019A1.tot <- drumspeedmin2019A1.freq*drumspeedmin2019A1.dur #total number of sample

drumspeedmin2019A1.x <- seq(0, drumspeedmin2019A1.dur, length.out = drumspeedmin2019A1.tot)
drumspeedmin2019A1.fourier <- fft(drumspeedmin2019A1.ts)
drumspeedmin2019A1.amo <- 2*Mod(drumspeedmin2019A1.fourier)/(drumspeedmin2019A1.tot) #amplitude
drumspeedmin2019A1.freqvec <- 0:(length(drumspeedmin2019A1.amo)-1) #vector from 0 to end of signal -> new x-axis

drumspeedmin2019A1.amo[drumspeedmin2019A1.freqvec == 0] <-
  drumspeedmin2019A1.amo[drumspeedmin2019A1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
drumspeedmin2019A1.freqvec <- drumspeedmin2019A1.freqvec/drumspeedmin2019A1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
drumspeedmin2019A1.dfFouri <- data.frame(freq = drumspeedmin2019A1.freqvec[1:as.integer(0.5*drumspeedmin2019A1.freq*drumspeedmin2019A1.dur)]*60*60*24,
                                         amount = drumspeedmin2019A1.amo[1:as.integer(0.5*drumspeedmin2019A1.freq*drumspeedmin2019A1.dur)])
drumspeedmin2019A1.dfFouri <- drumspeedmin2019A1.dfFouri %>%
  mutate(period = ifelse(drumspeedmin2019A1.dfFouri$amount >= 0.1,
                         paste(round(1/drumspeedmin2019A1.dfFouri$freq, 2),"days"),""))
drumspeedmin2019A1.dfFouri[1,3] <- ""
ggplot(drumspeedmin2019A1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,0.5) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1A Drum Speed Fourier Analysis")

# 1A bath level
bathlevelmin2019A1.ts <- ts(filterbathlevelmin2019A1$data) #change to time series
bathlevelmin2019A1.freq <- (1/(30*60))  #sample frequency in Hz 
bathlevelmin2019A1.dur <- length(bathlevelmin2019A1.ts)*30*60 # length of signal in seconds 
bathlevelmin2019A1.tot <- bathlevelmin2019A1.freq*bathlevelmin2019A1.dur #total number of sample

bathlevelmin2019A1.x <- seq(0, bathlevelmin2019A1.dur, length.out = bathlevelmin2019A1.tot)
bathlevelmin2019A1.fourier <- fft(bathlevelmin2019A1.ts)
bathlevelmin2019A1.amo <- 2*Mod(bathlevelmin2019A1.fourier)/(bathlevelmin2019A1.tot) #amplitude
bathlevelmin2019A1.freqvec <- 0:(length(bathlevelmin2019A1.amo)-1) #vector from 0 to end of signal -> new x-axis

bathlevelmin2019A1.amo[bathlevelmin2019A1.freqvec == 0] <-
  bathlevelmin2019A1.amo[bathlevelmin2019A1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
bathlevelmin2019A1.freqvec <- bathlevelmin2019A1.freqvec/bathlevelmin2019A1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
bathlevelmin2019A1.dfFouri <- data.frame(freq = bathlevelmin2019A1.freqvec[1:as.integer(0.5*bathlevelmin2019A1.freq*bathlevelmin2019A1.dur)]*60*60*24,
                                         amount = bathlevelmin2019A1.amo[1:as.integer(0.5*bathlevelmin2019A1.freq*bathlevelmin2019A1.dur)])
bathlevelmin2019A1.dfFouri <- bathlevelmin2019A1.dfFouri %>%
  mutate(period = ifelse(bathlevelmin2019A1.dfFouri$amount >= 1,
                         paste(round(1/bathlevelmin2019A1.dfFouri$freq, 2),"days"),""))
bathlevelmin2019A1.dfFouri[1,3] <- ""
ggplot(bathlevelmin2019A1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,7.5) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1A Drum Bath Level Fourier Analysis")

# 1A vacuum
vacuummin2019A1.ts <- ts(filtervacuummin2019A1$data) #change to time series
vacuummin2019A1.freq <- (1/(30*60))  #sample frequency in Hz 
vacuummin2019A1.dur <- length(vacuummin2019A1.ts)*30*60 # length of signal in seconds 
vacuummin2019A1.tot <- vacuummin2019A1.freq*vacuummin2019A1.dur #total number of sample

vacuummin2019A1.x <- seq(0, vacuummin2019A1.dur, length.out = vacuummin2019A1.tot)
vacuummin2019A1.fourier <- fft(vacuummin2019A1.ts)
vacuummin2019A1.amo <- 2*Mod(vacuummin2019A1.fourier)/(vacuummin2019A1.tot) #amplitude
vacuummin2019A1.freqvec <- 0:(length(vacuummin2019A1.amo)-1) #vector from 0 to end of signal -> new x-axis

vacuummin2019A1.amo[vacuummin2019A1.freqvec == 0] <-
  vacuummin2019A1.amo[vacuummin2019A1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
vacuummin2019A1.freqvec <- vacuummin2019A1.freqvec/vacuummin2019A1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
vacuummin2019A1.dfFouri <- data.frame(freq = vacuummin2019A1.freqvec[1:as.integer(0.5*vacuummin2019A1.freq*vacuummin2019A1.dur)]*60*60*24,
                                      amount = vacuummin2019A1.amo[1:as.integer(0.5*vacuummin2019A1.freq*vacuummin2019A1.dur)])
vacuummin2019A1.dfFouri <- vacuummin2019A1.dfFouri %>%
  mutate(period = ifelse(vacuummin2019A1.dfFouri$amount >= 0.5,
                         paste(round(1/vacuummin2019A1.dfFouri$freq, 2),"days"),""))
vacuummin2019A1.dfFouri[1,3] <- ""
ggplot(vacuummin2019A1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,3) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1A Drum Vacuum Pressure Fourier Analysis")


# 1A cake wash
cakewashmin2019A1.ts <- ts(filtercakewashmin2019A1$data) #change to time series
cakewashmin2019A1.freq <- (1/(30*60))  #sample frequency in Hz 
cakewashmin2019A1.dur <- length(cakewashmin2019A1.ts)*30*60 # length of signal in seconds 
cakewashmin2019A1.tot <- cakewashmin2019A1.freq*cakewashmin2019A1.dur #total number of sample

cakewashmin2019A1.x <- seq(0, cakewashmin2019A1.dur, length.out = cakewashmin2019A1.tot)
cakewashmin2019A1.fourier <- fft(cakewashmin2019A1.ts)
cakewashmin2019A1.amo <- 2*Mod(cakewashmin2019A1.fourier)/(cakewashmin2019A1.tot) #amplitude
cakewashmin2019A1.freqvec <- 0:(length(cakewashmin2019A1.amo)-1) #vector from 0 to end of signal -> new x-axis

cakewashmin2019A1.amo[cakewashmin2019A1.freqvec == 0] <-
  cakewashmin2019A1.amo[cakewashmin2019A1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
cakewashmin2019A1.freqvec <- cakewashmin2019A1.freqvec/cakewashmin2019A1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
cakewashmin2019A1.dfFouri <- data.frame(freq = cakewashmin2019A1.freqvec[1:as.integer(0.5*cakewashmin2019A1.freq*cakewashmin2019A1.dur)]*60*60*24,
                                        amount = cakewashmin2019A1.amo[1:as.integer(0.5*cakewashmin2019A1.freq*cakewashmin2019A1.dur)])
cakewashmin2019A1.dfFouri <- cakewashmin2019A1.dfFouri %>%
  mutate(period = ifelse(cakewashmin2019A1.dfFouri$amount >= 0.1,
                         paste(round(1/cakewashmin2019A1.dfFouri$freq, 2),"days"),""))
cakewashmin2019A1.dfFouri[1,3] <- ""
ggplot(cakewashmin2019A1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,1.5) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1A Drum Cake Wash Fourier Analysis")

# 1A soda filtrate conc.
sodafiltratemin2019A1.ts <- ts(filtersodafiltratemin2019A1$data) #change to time series
sodafiltratemin2019A1.freq <- (1/(30*60))  #sample frequency in Hz 
sodafiltratemin2019A1.dur <- length(sodafiltratemin2019A1.ts)*30*60 # length of signal in seconds 
sodafiltratemin2019A1.tot <- sodafiltratemin2019A1.freq*sodafiltratemin2019A1.dur #total number of sample

sodafiltratemin2019A1.x <- seq(0, sodafiltratemin2019A1.dur, length.out = sodafiltratemin2019A1.tot)
sodafiltratemin2019A1.fourier <- fft(sodafiltratemin2019A1.ts)
sodafiltratemin2019A1.amo <- 2*Mod(sodafiltratemin2019A1.fourier)/(sodafiltratemin2019A1.tot) #amplitude
sodafiltratemin2019A1.freqvec <- 0:(length(sodafiltratemin2019A1.amo)-1) #vector from 0 to end of signal -> new x-axis

sodafiltratemin2019A1.amo[sodafiltratemin2019A1.freqvec == 0] <-
  sodafiltratemin2019A1.amo[sodafiltratemin2019A1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
sodafiltratemin2019A1.freqvec <- sodafiltratemin2019A1.freqvec/sodafiltratemin2019A1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
sodafiltratemin2019A1.dfFouri <- data.frame(freq = sodafiltratemin2019A1.freqvec[1:as.integer(0.5*sodafiltratemin2019A1.freq*sodafiltratemin2019A1.dur)]*60*60*24,
                                            amount = sodafiltratemin2019A1.amo[1:as.integer(0.5*sodafiltratemin2019A1.freq*sodafiltratemin2019A1.dur)])
sodafiltratemin2019A1.dfFouri <- sodafiltratemin2019A1.dfFouri %>%
  mutate(period = ifelse(sodafiltratemin2019A1.dfFouri$amount >= 1,
                         paste(round(1/sodafiltratemin2019A1.dfFouri$freq, 2),"days"),""))
sodafiltratemin2019A1.dfFouri[1,3] <- ""
ggplot(sodafiltratemin2019A1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1A Drum Soda Filtrate Fourier Analysis")


# 1A oxalate filtrate
oxfiltratemin2019A1.ts <- ts(filteroxfiltratemin2019A1$data) #change to time series
oxfiltratemin2019A1.freq <- (1/(30*60))  #sample frequency in Hz 
oxfiltratemin2019A1.dur <- length(oxfiltratemin2019A1.ts)*30*60 # length of signal in seconds 
oxfiltratemin2019A1.tot <- oxfiltratemin2019A1.freq*oxfiltratemin2019A1.dur #total number of sample

oxfiltratemin2019A1.x <- seq(0, oxfiltratemin2019A1.dur, length.out = oxfiltratemin2019A1.tot)
oxfiltratemin2019A1.fourier <- fft(oxfiltratemin2019A1.ts)
oxfiltratemin2019A1.amo <- 2*Mod(oxfiltratemin2019A1.fourier)/(oxfiltratemin2019A1.tot) #amplitude
oxfiltratemin2019A1.freqvec <- 0:(length(oxfiltratemin2019A1.amo)-1) #vector from 0 to end of signal -> new x-axis

oxfiltratemin2019A1.amo[oxfiltratemin2019A1.freqvec == 0] <-
  oxfiltratemin2019A1.amo[oxfiltratemin2019A1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
oxfiltratemin2019A1.freqvec <- oxfiltratemin2019A1.freqvec/oxfiltratemin2019A1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
oxfiltratemin2019A1.dfFouri <- data.frame(freq = oxfiltratemin2019A1.freqvec[1:as.integer(0.5*oxfiltratemin2019A1.freq*oxfiltratemin2019A1.dur)]*60*60*24,
                                          amount = oxfiltratemin2019A1.amo[1:as.integer(0.5*oxfiltratemin2019A1.freq*oxfiltratemin2019A1.dur)])
oxfiltratemin2019A1.dfFouri <- oxfiltratemin2019A1.dfFouri %>%
  mutate(period = ifelse(oxfiltratemin2019A1.dfFouri$amount >= 1,
                         paste(round(1/oxfiltratemin2019A1.dfFouri$freq, 2),"days"),""))
oxfiltratemin2019A1.dfFouri[1,3] <- ""
ggplot(oxfiltratemin2019A1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1A Drum Oxalate Filtrate Fourier Analysis")


# 2A Drum----------------------------------------------------------------------------------------
# 2A feed flow
feedflowmin2019A2.ts <- ts(filterfeedflowmin2019A2$data) #change to time series
feedflowmin2019A2.freq <- (1/(30*60))  #sample frequency in Hz 
feedflowmin2019A2.dur <- length(feedflowmin2019A2.ts)*30*60 # length of signal in seconds 
feedflowmin2019A2.tot <- feedflowmin2019A2.freq*feedflowmin2019A2.dur #total number of sample

feedflowmin2019A2.x <- seq(0, feedflowmin2019A2.dur, length.out = feedflowmin2019A2.tot)
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
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2A Drum Feed Flow Fourier Analysis")

# 2A drum speed
drumspeedmin2019A2.ts <- ts(filterdrumspeedmin2019A2$data) #change to time series
drumspeedmin2019A2.freq <- (1/(30*60))  #sample frequency in Hz 
drumspeedmin2019A2.dur <- length(drumspeedmin2019A2.ts)*30*60 # length of signal in seconds 
drumspeedmin2019A2.tot <- drumspeedmin2019A2.freq*drumspeedmin2019A2.dur #total number of sample

drumspeedmin2019A2.x <- seq(0, drumspeedmin2019A2.dur, length.out = drumspeedmin2019A2.tot)
drumspeedmin2019A2.fourier <- fft(drumspeedmin2019A2.ts)
drumspeedmin2019A2.amo <- 2*Mod(drumspeedmin2019A2.fourier)/(drumspeedmin2019A2.tot) #amplitude
drumspeedmin2019A2.freqvec <- 0:(length(drumspeedmin2019A2.amo)-1) #vector from 0 to end of signal -> new x-axis

drumspeedmin2019A2.amo[drumspeedmin2019A2.freqvec == 0] <-
  drumspeedmin2019A2.amo[drumspeedmin2019A2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
drumspeedmin2019A2.freqvec <- drumspeedmin2019A2.freqvec/drumspeedmin2019A2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
drumspeedmin2019A2.dfFouri <- data.frame(freq = drumspeedmin2019A2.freqvec[1:as.integer(0.5*drumspeedmin2019A2.freq*drumspeedmin2019A2.dur)]*60*60*24,
                                        amount = drumspeedmin2019A2.amo[1:as.integer(0.5*drumspeedmin2019A2.freq*drumspeedmin2019A2.dur)])
drumspeedmin2019A2.dfFouri <- drumspeedmin2019A2.dfFouri %>%
  mutate(period = ifelse(drumspeedmin2019A2.dfFouri$amount >= 0.1,
                         paste(round(1/drumspeedmin2019A2.dfFouri$freq, 2),"days"),""))
drumspeedmin2019A2.dfFouri[1,3] <- ""
ggplot(drumspeedmin2019A2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,0.5) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2A Drum Speed Fourier Analysis")

# 2A bath level
bathlevelmin2019A2.ts <- ts(filterbathlevelmin2019A2$data) #change to time series
bathlevelmin2019A2.freq <- (1/(30*60))  #sample frequency in Hz 
bathlevelmin2019A2.dur <- length(bathlevelmin2019A2.ts)*30*60 # length of signal in seconds 
bathlevelmin2019A2.tot <- bathlevelmin2019A2.freq*bathlevelmin2019A2.dur #total number of sample

bathlevelmin2019A2.x <- seq(0, bathlevelmin2019A2.dur, length.out = bathlevelmin2019A2.tot)
bathlevelmin2019A2.fourier <- fft(bathlevelmin2019A2.ts)
bathlevelmin2019A2.amo <- 2*Mod(bathlevelmin2019A2.fourier)/(bathlevelmin2019A2.tot) #amplitude
bathlevelmin2019A2.freqvec <- 0:(length(bathlevelmin2019A2.amo)-1) #vector from 0 to end of signal -> new x-axis

bathlevelmin2019A2.amo[bathlevelmin2019A2.freqvec == 0] <-
  bathlevelmin2019A2.amo[bathlevelmin2019A2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
bathlevelmin2019A2.freqvec <- bathlevelmin2019A2.freqvec/bathlevelmin2019A2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
bathlevelmin2019A2.dfFouri <- data.frame(freq = bathlevelmin2019A2.freqvec[1:as.integer(0.5*bathlevelmin2019A2.freq*bathlevelmin2019A2.dur)]*60*60*24,
                                        amount = bathlevelmin2019A2.amo[1:as.integer(0.5*bathlevelmin2019A2.freq*bathlevelmin2019A2.dur)])
bathlevelmin2019A2.dfFouri <- bathlevelmin2019A2.dfFouri %>%
  mutate(period = ifelse(bathlevelmin2019A2.dfFouri$amount >= 1,
                         paste(round(1/bathlevelmin2019A2.dfFouri$freq, 2),"days"),""))
bathlevelmin2019A2.dfFouri[1,3] <- ""
ggplot(bathlevelmin2019A2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2A Drum Bath Level Fourier Analysis")

# 2A vacuum
vacuummin2019A2.ts <- ts(filtervacuummin2019A2$data) #change to time series
vacuummin2019A2.freq <- (1/(30*60))  #sample frequency in Hz 
vacuummin2019A2.dur <- length(vacuummin2019A2.ts)*30*60 # length of signal in seconds 
vacuummin2019A2.tot <- vacuummin2019A2.freq*vacuummin2019A2.dur #total number of sample

vacuummin2019A2.x <- seq(0, vacuummin2019A2.dur, length.out = vacuummin2019A2.tot)
vacuummin2019A2.fourier <- fft(vacuummin2019A2.ts)
vacuummin2019A2.amo <- 2*Mod(vacuummin2019A2.fourier)/(vacuummin2019A2.tot) #amplitude
vacuummin2019A2.freqvec <- 0:(length(vacuummin2019A2.amo)-1) #vector from 0 to end of signal -> new x-axis

vacuummin2019A2.amo[vacuummin2019A2.freqvec == 0] <-
  vacuummin2019A2.amo[vacuummin2019A2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
vacuummin2019A2.freqvec <- vacuummin2019A2.freqvec/vacuummin2019A2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
vacuummin2019A2.dfFouri <- data.frame(freq = vacuummin2019A2.freqvec[1:as.integer(0.5*vacuummin2019A2.freq*vacuummin2019A2.dur)]*60*60*24,
                                        amount = vacuummin2019A2.amo[1:as.integer(0.5*vacuummin2019A2.freq*vacuummin2019A2.dur)])
vacuummin2019A2.dfFouri <- vacuummin2019A2.dfFouri %>%
  mutate(period = ifelse(vacuummin2019A2.dfFouri$amount >= 0.5,
                         paste(round(1/vacuummin2019A2.dfFouri$freq, 2),"days"),""))
vacuummin2019A2.dfFouri[1,3] <- ""
ggplot(vacuummin2019A2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,3) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2A Drum Vacuum Pressure Fourier Analysis")


# 2A cake wash
cakewashmin2019A2.ts <- ts(filtercakewashmin2019A2$data) #change to time series
cakewashmin2019A2.freq <- (1/(30*60))  #sample frequency in Hz 
cakewashmin2019A2.dur <- length(cakewashmin2019A2.ts)*30*60 # length of signal in seconds 
cakewashmin2019A2.tot <- cakewashmin2019A2.freq*cakewashmin2019A2.dur #total number of sample

cakewashmin2019A2.x <- seq(0, cakewashmin2019A2.dur, length.out = cakewashmin2019A2.tot)
cakewashmin2019A2.fourier <- fft(cakewashmin2019A2.ts)
cakewashmin2019A2.amo <- 2*Mod(cakewashmin2019A2.fourier)/(cakewashmin2019A2.tot) #amplitude
cakewashmin2019A2.freqvec <- 0:(length(cakewashmin2019A2.amo)-1) #vector from 0 to end of signal -> new x-axis

cakewashmin2019A2.amo[cakewashmin2019A2.freqvec == 0] <-
  cakewashmin2019A2.amo[cakewashmin2019A2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
cakewashmin2019A2.freqvec <- cakewashmin2019A2.freqvec/cakewashmin2019A2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
cakewashmin2019A2.dfFouri <- data.frame(freq = cakewashmin2019A2.freqvec[1:as.integer(0.5*cakewashmin2019A2.freq*cakewashmin2019A2.dur)]*60*60*24,
                                        amount = cakewashmin2019A2.amo[1:as.integer(0.5*cakewashmin2019A2.freq*cakewashmin2019A2.dur)])
cakewashmin2019A2.dfFouri <- cakewashmin2019A2.dfFouri %>%
  mutate(period = ifelse(cakewashmin2019A2.dfFouri$amount >= 0.2,
                         paste(round(1/cakewashmin2019A2.dfFouri$freq, 2),"days"),""))
cakewashmin2019A2.dfFouri[1,3] <- ""
ggplot(cakewashmin2019A2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,2.5) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2A Drum Cake Wash Fourier Analysis")

# 2A soda filtrate conc.
sodafiltratemin2019A2.ts <- ts(filtersodafiltratemin2019A2$data) #change to time series
sodafiltratemin2019A2.freq <- (1/(30*60))  #sample frequency in Hz 
sodafiltratemin2019A2.dur <- length(sodafiltratemin2019A2.ts)*30*60 # length of signal in seconds 
sodafiltratemin2019A2.tot <- sodafiltratemin2019A2.freq*sodafiltratemin2019A2.dur #total number of sample

sodafiltratemin2019A2.x <- seq(0, sodafiltratemin2019A2.dur, length.out = sodafiltratemin2019A2.tot)
sodafiltratemin2019A2.fourier <- fft(sodafiltratemin2019A2.ts)
sodafiltratemin2019A2.amo <- 2*Mod(sodafiltratemin2019A2.fourier)/(sodafiltratemin2019A2.tot) #amplitude
sodafiltratemin2019A2.freqvec <- 0:(length(sodafiltratemin2019A2.amo)-1) #vector from 0 to end of signal -> new x-axis

sodafiltratemin2019A2.amo[sodafiltratemin2019A2.freqvec == 0] <-
  sodafiltratemin2019A2.amo[sodafiltratemin2019A2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
sodafiltratemin2019A2.freqvec <- sodafiltratemin2019A2.freqvec/sodafiltratemin2019A2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
sodafiltratemin2019A2.dfFouri <- data.frame(freq = sodafiltratemin2019A2.freqvec[1:as.integer(0.5*sodafiltratemin2019A2.freq*sodafiltratemin2019A2.dur)]*60*60*24,
                                        amount = sodafiltratemin2019A2.amo[1:as.integer(0.5*sodafiltratemin2019A2.freq*sodafiltratemin2019A2.dur)])
sodafiltratemin2019A2.dfFouri <- sodafiltratemin2019A2.dfFouri %>%
  mutate(period = ifelse(sodafiltratemin2019A2.dfFouri$amount >= 1,
                         paste(round(1/sodafiltratemin2019A2.dfFouri$freq, 2),"days"),""))
sodafiltratemin2019A2.dfFouri[1,3] <- ""
ggplot(sodafiltratemin2019A2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2A Drum Soda Filtrate Fourier Analysis")


# 2A oxalate filtrate
oxfiltratemin2019A2.ts <- ts(filteroxfiltratemin2019A2$data) #change to time series
oxfiltratemin2019A2.freq <- (1/(30*60))  #sample frequency in Hz 
oxfiltratemin2019A2.dur <- length(oxfiltratemin2019A2.ts)*30*60 # length of signal in seconds 
oxfiltratemin2019A2.tot <- oxfiltratemin2019A2.freq*oxfiltratemin2019A2.dur #total number of sample

oxfiltratemin2019A2.x <- seq(0, oxfiltratemin2019A2.dur, length.out = oxfiltratemin2019A2.tot)
oxfiltratemin2019A2.fourier <- fft(oxfiltratemin2019A2.ts)
oxfiltratemin2019A2.amo <- 2*Mod(oxfiltratemin2019A2.fourier)/(oxfiltratemin2019A2.tot) #amplitude
oxfiltratemin2019A2.freqvec <- 0:(length(oxfiltratemin2019A2.amo)-1) #vector from 0 to end of signal -> new x-axis

oxfiltratemin2019A2.amo[oxfiltratemin2019A2.freqvec == 0] <-
  oxfiltratemin2019A2.amo[oxfiltratemin2019A2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
oxfiltratemin2019A2.freqvec <- oxfiltratemin2019A2.freqvec/oxfiltratemin2019A2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
oxfiltratemin2019A2.dfFouri <- data.frame(freq = oxfiltratemin2019A2.freqvec[1:as.integer(0.5*oxfiltratemin2019A2.freq*oxfiltratemin2019A2.dur)]*60*60*24,
                                        amount = oxfiltratemin2019A2.amo[1:as.integer(0.5*oxfiltratemin2019A2.freq*oxfiltratemin2019A2.dur)])
oxfiltratemin2019A2.dfFouri <- oxfiltratemin2019A2.dfFouri %>%
  mutate(period = ifelse(oxfiltratemin2019A2.dfFouri$amount >= 1,
                         paste(round(1/oxfiltratemin2019A2.dfFouri$freq, 2),"days"),""))
oxfiltratemin2019A2.dfFouri[1,3] <- ""
ggplot(oxfiltratemin2019A2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2A Drum Oxalate Filtrate Fourier Analysis")

# 1B Drum----------------------------------------------------------------------------------------
# 1B feed flow
feedflowmin2019B1.ts <- ts(filterfeedflowmin2019B1$data) #change to time series
feedflowmin2019B1.freq <- (1/(30*60))  #sample frequency in Hz 
feedflowmin2019B1.dur <- length(feedflowmin2019B1.ts)*30*60 # length of signal in seconds 
feedflowmin2019B1.tot <- feedflowmin2019B1.freq*feedflowmin2019B1.dur #total number of sample

feedflowmin2019B1.x <- seq(0, feedflowmin2019B1.dur, length.out = feedflowmin2019B1.tot)
feedflowmin2019B1.fourier <- fft(feedflowmin2019B1.ts)
feedflowmin2019B1.amo <- 2*Mod(feedflowmin2019B1.fourier)/(feedflowmin2019B1.tot) #amplitude
feedflowmin2019B1.freqvec <- 0:(length(feedflowmin2019B1.amo)-1) #vector from 0 to end of signal -> new x-axis

feedflowmin2019B1.amo[feedflowmin2019B1.freqvec == 0] <-
  feedflowmin2019B1.amo[feedflowmin2019B1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
feedflowmin2019B1.freqvec <- feedflowmin2019B1.freqvec/feedflowmin2019B1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
feedflowmin2019B1.dfFouri <- data.frame(freq = feedflowmin2019B1.freqvec[1:as.integer(0.5*feedflowmin2019B1.freq*feedflowmin2019B1.dur)]*60*60*24,
                                        amount = feedflowmin2019B1.amo[1:as.integer(0.5*feedflowmin2019B1.freq*feedflowmin2019B1.dur)])
feedflowmin2019B1.dfFouri <- feedflowmin2019B1.dfFouri %>%
  mutate(period = ifelse(feedflowmin2019B1.dfFouri$amount >= 1,
                         paste(round(1/feedflowmin2019B1.dfFouri$freq, 2),"days"),""))
feedflowmin2019B1.dfFouri[1,3] <- ""
ggplot(feedflowmin2019B1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1B Drum Feed Flow Fourier Analysis")

# 1B drum speed
drumspeedmin2019B1.ts <- ts(filterdrumspeedmin2019B1$data) #change to time series
drumspeedmin2019B1.freq <- (1/(30*60))  #sample frequency in Hz 
drumspeedmin2019B1.dur <- length(drumspeedmin2019B1.ts)*30*60 # length of signal in seconds 
drumspeedmin2019B1.tot <- drumspeedmin2019B1.freq*drumspeedmin2019B1.dur #total number of sample

drumspeedmin2019B1.x <- seq(0, drumspeedmin2019B1.dur, length.out = drumspeedmin2019B1.tot)
drumspeedmin2019B1.fourier <- fft(drumspeedmin2019B1.ts)
drumspeedmin2019B1.amo <- 2*Mod(drumspeedmin2019B1.fourier)/(drumspeedmin2019B1.tot) #amplitude
drumspeedmin2019B1.freqvec <- 0:(length(drumspeedmin2019B1.amo)-1) #vector from 0 to end of signal -> new x-axis

drumspeedmin2019B1.amo[drumspeedmin2019B1.freqvec == 0] <-
  drumspeedmin2019B1.amo[drumspeedmin2019B1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
drumspeedmin2019B1.freqvec <- drumspeedmin2019B1.freqvec/drumspeedmin2019B1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
drumspeedmin2019B1.dfFouri <- data.frame(freq = drumspeedmin2019B1.freqvec[1:as.integer(0.5*drumspeedmin2019B1.freq*drumspeedmin2019B1.dur)]*60*60*24,
                                         amount = drumspeedmin2019B1.amo[1:as.integer(0.5*drumspeedmin2019B1.freq*drumspeedmin2019B1.dur)])
drumspeedmin2019B1.dfFouri <- drumspeedmin2019B1.dfFouri %>%
  mutate(period = ifelse(drumspeedmin2019B1.dfFouri$amount >= 0.1,
                         paste(round(1/drumspeedmin2019B1.dfFouri$freq, 2),"days"),""))
drumspeedmin2019B1.dfFouri[1,3] <- ""
ggplot(drumspeedmin2019B1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,0.25) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1B Drum Speed Fourier Analysis")

# 1B bath level
bathlevelmin2019B1.ts <- ts(filterbathlevelmin2019B1$data) #change to time series
bathlevelmin2019B1.freq <- (1/(30*60))  #sample frequency in Hz 
bathlevelmin2019B1.dur <- length(bathlevelmin2019B1.ts)*30*60 # length of signal in seconds 
bathlevelmin2019B1.tot <- bathlevelmin2019B1.freq*bathlevelmin2019B1.dur #total number of sample

bathlevelmin2019B1.x <- seq(0, bathlevelmin2019B1.dur, length.out = bathlevelmin2019B1.tot)
bathlevelmin2019B1.fourier <- fft(bathlevelmin2019B1.ts)
bathlevelmin2019B1.amo <- 2*Mod(bathlevelmin2019B1.fourier)/(bathlevelmin2019B1.tot) #amplitude
bathlevelmin2019B1.freqvec <- 0:(length(bathlevelmin2019B1.amo)-1) #vector from 0 to end of signal -> new x-axis

bathlevelmin2019B1.amo[bathlevelmin2019B1.freqvec == 0] <-
  bathlevelmin2019B1.amo[bathlevelmin2019B1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
bathlevelmin2019B1.freqvec <- bathlevelmin2019B1.freqvec/bathlevelmin2019B1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
bathlevelmin2019B1.dfFouri <- data.frame(freq = bathlevelmin2019B1.freqvec[1:as.integer(0.5*bathlevelmin2019B1.freq*bathlevelmin2019B1.dur)]*60*60*24,
                                         amount = bathlevelmin2019B1.amo[1:as.integer(0.5*bathlevelmin2019B1.freq*bathlevelmin2019B1.dur)])
bathlevelmin2019B1.dfFouri <- bathlevelmin2019B1.dfFouri %>%
  mutate(period = ifelse(bathlevelmin2019B1.dfFouri$amount >= 1,
                         paste(round(1/bathlevelmin2019B1.dfFouri$freq, 2),"days"),""))
bathlevelmin2019B1.dfFouri[1,3] <- ""
ggplot(bathlevelmin2019B1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,5) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1B Drum Bath Level Fourier Analysis")

# 1B vacuum
vacuummin2019B1.ts <- ts(filtervacuummin2019B1$data) #change to time series
vacuummin2019B1.freq <- (1/(30*60))  #sample frequency in Hz 
vacuummin2019B1.dur <- length(vacuummin2019B1.ts)*30*60 # length of signal in seconds 
vacuummin2019B1.tot <- vacuummin2019B1.freq*vacuummin2019B1.dur #total number of sample

vacuummin2019B1.x <- seq(0, vacuummin2019B1.dur, length.out = vacuummin2019B1.tot)
vacuummin2019B1.fourier <- fft(vacuummin2019B1.ts)
vacuummin2019B1.amo <- 2*Mod(vacuummin2019B1.fourier)/(vacuummin2019B1.tot) #amplitude
vacuummin2019B1.freqvec <- 0:(length(vacuummin2019B1.amo)-1) #vector from 0 to end of signal -> new x-axis

vacuummin2019B1.amo[vacuummin2019B1.freqvec == 0] <-
  vacuummin2019B1.amo[vacuummin2019B1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
vacuummin2019B1.freqvec <- vacuummin2019B1.freqvec/vacuummin2019B1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
vacuummin2019B1.dfFouri <- data.frame(freq = vacuummin2019B1.freqvec[1:as.integer(0.5*vacuummin2019B1.freq*vacuummin2019B1.dur)]*60*60*24,
                                      amount = vacuummin2019B1.amo[1:as.integer(0.5*vacuummin2019B1.freq*vacuummin2019B1.dur)])
vacuummin2019B1.dfFouri <- vacuummin2019B1.dfFouri %>%
  mutate(period = ifelse(vacuummin2019B1.dfFouri$amount >= 0.5,
                         paste(round(1/vacuummin2019B1.dfFouri$freq, 2),"days"),""))
vacuummin2019B1.dfFouri[1,3] <- ""
ggplot(vacuummin2019B1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,2) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1B Drum Vacuum Pressure Fourier Analysis")


# 1B cake wash
cakewashmin2019B1.ts <- ts(filtercakewashmin2019B1$data) #change to time series
cakewashmin2019B1.freq <- (1/(30*60))  #sample frequency in Hz 
cakewashmin2019B1.dur <- length(cakewashmin2019B1.ts)*30*60 # length of signal in seconds 
cakewashmin2019B1.tot <- cakewashmin2019B1.freq*cakewashmin2019B1.dur #total number of sample

cakewashmin2019B1.x <- seq(0, cakewashmin2019B1.dur, length.out = cakewashmin2019B1.tot)
cakewashmin2019B1.fourier <- fft(cakewashmin2019B1.ts)
cakewashmin2019B1.amo <- 2*Mod(cakewashmin2019B1.fourier)/(cakewashmin2019B1.tot) #amplitude
cakewashmin2019B1.freqvec <- 0:(length(cakewashmin2019B1.amo)-1) #vector from 0 to end of signal -> new x-axis

cakewashmin2019B1.amo[cakewashmin2019B1.freqvec == 0] <-
  cakewashmin2019B1.amo[cakewashmin2019B1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
cakewashmin2019B1.freqvec <- cakewashmin2019B1.freqvec/cakewashmin2019B1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
cakewashmin2019B1.dfFouri <- data.frame(freq = cakewashmin2019B1.freqvec[1:as.integer(0.5*cakewashmin2019B1.freq*cakewashmin2019B1.dur)]*60*60*24,
                                        amount = cakewashmin2019B1.amo[1:as.integer(0.5*cakewashmin2019B1.freq*cakewashmin2019B1.dur)])
cakewashmin2019B1.dfFouri <- cakewashmin2019B1.dfFouri %>%
  mutate(period = ifelse(cakewashmin2019B1.dfFouri$amount >= 0.5,
                         paste(round(1/cakewashmin2019B1.dfFouri$freq, 2),"days"),""))
cakewashmin2019B1.dfFouri[1,3] <- ""
ggplot(cakewashmin2019B1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,2.8) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1B Drum Cake Wash Fourier Analysis")

# 1B soda filtrate conc.
sodafiltratemin2019B1.ts <- ts(filtersodafiltratemin2019B1$data) #change to time series
sodafiltratemin2019B1.freq <- (1/(30*60))  #sample frequency in Hz 
sodafiltratemin2019B1.dur <- length(sodafiltratemin2019B1.ts)*30*60 # length of signal in seconds 
sodafiltratemin2019B1.tot <- sodafiltratemin2019B1.freq*sodafiltratemin2019B1.dur #total number of sample

sodafiltratemin2019B1.x <- seq(0, sodafiltratemin2019B1.dur, length.out = sodafiltratemin2019B1.tot)
sodafiltratemin2019B1.fourier <- fft(sodafiltratemin2019B1.ts)
sodafiltratemin2019B1.amo <- 2*Mod(sodafiltratemin2019B1.fourier)/(sodafiltratemin2019B1.tot) #amplitude
sodafiltratemin2019B1.freqvec <- 0:(length(sodafiltratemin2019B1.amo)-1) #vector from 0 to end of signal -> new x-axis

sodafiltratemin2019B1.amo[sodafiltratemin2019B1.freqvec == 0] <-
  sodafiltratemin2019B1.amo[sodafiltratemin2019B1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
sodafiltratemin2019B1.freqvec <- sodafiltratemin2019B1.freqvec/sodafiltratemin2019B1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
sodafiltratemin2019B1.dfFouri <- data.frame(freq = sodafiltratemin2019B1.freqvec[1:as.integer(0.5*sodafiltratemin2019B1.freq*sodafiltratemin2019B1.dur)]*60*60*24,
                                            amount = sodafiltratemin2019B1.amo[1:as.integer(0.5*sodafiltratemin2019B1.freq*sodafiltratemin2019B1.dur)])
sodafiltratemin2019B1.dfFouri <- sodafiltratemin2019B1.dfFouri %>%
  mutate(period = ifelse(sodafiltratemin2019B1.dfFouri$amount >= 1,
                         paste(round(1/sodafiltratemin2019B1.dfFouri$freq, 2),"days"),""))
sodafiltratemin2019B1.dfFouri[1,3] <- ""
ggplot(sodafiltratemin2019B1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,2.8) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1B Drum Soda Filtrate Fourier Analysis")


# 1B oxalate filtrate
oxfiltratemin2019B1.ts <- ts(filteroxfiltratemin2019B1$data) #change to time series
oxfiltratemin2019B1.freq <- (1/(30*60))  #sample frequency in Hz 
oxfiltratemin2019B1.dur <- length(oxfiltratemin2019B1.ts)*30*60 # length of signal in seconds 
oxfiltratemin2019B1.tot <- oxfiltratemin2019B1.freq*oxfiltratemin2019B1.dur #total number of sample

oxfiltratemin2019B1.x <- seq(0, oxfiltratemin2019B1.dur, length.out = oxfiltratemin2019B1.tot)
oxfiltratemin2019B1.fourier <- fft(oxfiltratemin2019B1.ts)
oxfiltratemin2019B1.amo <- 2*Mod(oxfiltratemin2019B1.fourier)/(oxfiltratemin2019B1.tot) #amplitude
oxfiltratemin2019B1.freqvec <- 0:(length(oxfiltratemin2019B1.amo)-1) #vector from 0 to end of signal -> new x-axis

oxfiltratemin2019B1.amo[oxfiltratemin2019B1.freqvec == 0] <-
  oxfiltratemin2019B1.amo[oxfiltratemin2019B1.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
oxfiltratemin2019B1.freqvec <- oxfiltratemin2019B1.freqvec/oxfiltratemin2019B1.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
oxfiltratemin2019B1.dfFouri <- data.frame(freq = oxfiltratemin2019B1.freqvec[1:as.integer(0.5*oxfiltratemin2019B1.freq*oxfiltratemin2019B1.dur)]*60*60*24,
                                          amount = oxfiltratemin2019B1.amo[1:as.integer(0.5*oxfiltratemin2019B1.freq*oxfiltratemin2019B1.dur)])
oxfiltratemin2019B1.dfFouri <- oxfiltratemin2019B1.dfFouri %>%
  mutate(period = ifelse(oxfiltratemin2019B1.dfFouri$amount >= 1,
                         paste(round(1/oxfiltratemin2019B1.dfFouri$freq, 2),"days"),""))
oxfiltratemin2019B1.dfFouri[1,3] <- ""
ggplot(oxfiltratemin2019B1.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="1B Drum Oxalate Filtrate Fourier Analysis")


# 2B Drum----------------------------------------------------------------------------------------
# 2B feed flow
feedflowmin2019B2.ts <- ts(filterfeedflowmin2019B2$data) #change to time series
feedflowmin2019B2.freq <- (1/(30*60))  #sample frequency in Hz 
feedflowmin2019B2.dur <- length(feedflowmin2019B2.ts)*30*60 # length of signal in seconds 
feedflowmin2019B2.tot <- feedflowmin2019B2.freq*feedflowmin2019B2.dur #total number of sample

feedflowmin2019B2.x <- seq(0, feedflowmin2019B2.dur, length.out = feedflowmin2019B2.tot)
feedflowmin2019B2.fourier <- fft(feedflowmin2019B2.ts)
feedflowmin2019B2.amo <- 2*Mod(feedflowmin2019B2.fourier)/(feedflowmin2019B2.tot) #amplitude
feedflowmin2019B2.freqvec <- 0:(length(feedflowmin2019B2.amo)-1) #vector from 0 to end of signal -> new x-axis

feedflowmin2019B2.amo[feedflowmin2019B2.freqvec == 0] <-
  feedflowmin2019B2.amo[feedflowmin2019B2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
feedflowmin2019B2.freqvec <- feedflowmin2019B2.freqvec/feedflowmin2019B2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
feedflowmin2019B2.dfFouri <- data.frame(freq = feedflowmin2019B2.freqvec[1:as.integer(0.5*feedflowmin2019B2.freq*feedflowmin2019B2.dur)]*60*60*24,
                                        amount = feedflowmin2019B2.amo[1:as.integer(0.5*feedflowmin2019B2.freq*feedflowmin2019B2.dur)])
feedflowmin2019B2.dfFouri <- feedflowmin2019B2.dfFouri %>%
  mutate(period = ifelse(feedflowmin2019B2.dfFouri$amount >= 1,
                         paste(round(1/feedflowmin2019B2.dfFouri$freq, 2),"days"),""))
feedflowmin2019B2.dfFouri[1,3] <- ""
ggplot(feedflowmin2019B2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2B Drum Feed Flow Fourier Analysis")

# 2B drum speed
drumspeedmin2019B2.ts <- ts(filterdrumspeedmin2019B2$data) #change to time series
drumspeedmin2019B2.freq <- (1/(30*60))  #sample frequency in Hz 
drumspeedmin2019B2.dur <- length(drumspeedmin2019B2.ts)*30*60 # length of signal in seconds 
drumspeedmin2019B2.tot <- drumspeedmin2019B2.freq*drumspeedmin2019B2.dur #total number of sample

drumspeedmin2019B2.x <- seq(0, drumspeedmin2019B2.dur, length.out = drumspeedmin2019B2.tot)
drumspeedmin2019B2.fourier <- fft(drumspeedmin2019B2.ts)
drumspeedmin2019B2.amo <- 2*Mod(drumspeedmin2019B2.fourier)/(drumspeedmin2019B2.tot) #amplitude
drumspeedmin2019B2.freqvec <- 0:(length(drumspeedmin2019B2.amo)-1) #vector from 0 to end of signal -> new x-axis

drumspeedmin2019B2.amo[drumspeedmin2019B2.freqvec == 0] <-
  drumspeedmin2019B2.amo[drumspeedmin2019B2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
drumspeedmin2019B2.freqvec <- drumspeedmin2019B2.freqvec/drumspeedmin2019B2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
drumspeedmin2019B2.dfFouri <- data.frame(freq = drumspeedmin2019B2.freqvec[1:as.integer(0.5*drumspeedmin2019B2.freq*drumspeedmin2019B2.dur)]*60*60*24,
                                         amount = drumspeedmin2019B2.amo[1:as.integer(0.5*drumspeedmin2019B2.freq*drumspeedmin2019B2.dur)])
drumspeedmin2019B2.dfFouri <- drumspeedmin2019B2.dfFouri %>%
  mutate(period = ifelse(drumspeedmin2019B2.dfFouri$amount >= 0.05,
                         paste(round(1/drumspeedmin2019B2.dfFouri$freq, 2),"days"),""))
drumspeedmin2019B2.dfFouri[1,3] <- ""
ggplot(drumspeedmin2019B2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,0.4) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2B Drum Speed Fourier Analysis")

# 2B bath level
bathlevelmin2019B2.ts <- ts(filterbathlevelmin2019B2$data) #change to time series
bathlevelmin2019B2.freq <- (1/(30*60))  #sample frequency in Hz 
bathlevelmin2019B2.dur <- length(bathlevelmin2019B2.ts)*30*60 # length of signal in seconds 
bathlevelmin2019B2.tot <- bathlevelmin2019B2.freq*bathlevelmin2019B2.dur #total number of sample

bathlevelmin2019B2.x <- seq(0, bathlevelmin2019B2.dur, length.out = bathlevelmin2019B2.tot)
bathlevelmin2019B2.fourier <- fft(bathlevelmin2019B2.ts)
bathlevelmin2019B2.amo <- 2*Mod(bathlevelmin2019B2.fourier)/(bathlevelmin2019B2.tot) #amplitude
bathlevelmin2019B2.freqvec <- 0:(length(bathlevelmin2019B2.amo)-1) #vector from 0 to end of signal -> new x-axis

bathlevelmin2019B2.amo[bathlevelmin2019B2.freqvec == 0] <-
  bathlevelmin2019B2.amo[bathlevelmin2019B2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
bathlevelmin2019B2.freqvec <- bathlevelmin2019B2.freqvec/bathlevelmin2019B2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
bathlevelmin2019B2.dfFouri <- data.frame(freq = bathlevelmin2019B2.freqvec[1:as.integer(0.5*bathlevelmin2019B2.freq*bathlevelmin2019B2.dur)]*60*60*24,
                                         amount = bathlevelmin2019B2.amo[1:as.integer(0.5*bathlevelmin2019B2.freq*bathlevelmin2019B2.dur)])
bathlevelmin2019B2.dfFouri <- bathlevelmin2019B2.dfFouri %>%
  mutate(period = ifelse(bathlevelmin2019B2.dfFouri$amount >= 1,
                         paste(round(1/bathlevelmin2019B2.dfFouri$freq, 2),"days"),""))
bathlevelmin2019B2.dfFouri[1,3] <- ""
ggplot(bathlevelmin2019B2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2B Drum Bath Level Fourier Analysis")

# 2B vacuum
vacuummin2019B2.ts <- ts(filtervacuummin2019B2$data) #change to time series
vacuummin2019B2.freq <- (1/(30*60))  #sample frequency in Hz 
vacuummin2019B2.dur <- length(vacuummin2019B2.ts)*30*60 # length of signal in seconds 
vacuummin2019B2.tot <- vacuummin2019B2.freq*vacuummin2019B2.dur #total number of sample

vacuummin2019B2.x <- seq(0, vacuummin2019B2.dur, length.out = vacuummin2019B2.tot)
vacuummin2019B2.fourier <- fft(vacuummin2019B2.ts)
vacuummin2019B2.amo <- 2*Mod(vacuummin2019B2.fourier)/(vacuummin2019B2.tot) #amplitude
vacuummin2019B2.freqvec <- 0:(length(vacuummin2019B2.amo)-1) #vector from 0 to end of signal -> new x-axis

vacuummin2019B2.amo[vacuummin2019B2.freqvec == 0] <-
  vacuummin2019B2.amo[vacuummin2019B2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
vacuummin2019B2.freqvec <- vacuummin2019B2.freqvec/vacuummin2019B2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
vacuummin2019B2.dfFouri <- data.frame(freq = vacuummin2019B2.freqvec[1:as.integer(0.5*vacuummin2019B2.freq*vacuummin2019B2.dur)]*60*60*24,
                                      amount = vacuummin2019B2.amo[1:as.integer(0.5*vacuummin2019B2.freq*vacuummin2019B2.dur)])
vacuummin2019B2.dfFouri <- vacuummin2019B2.dfFouri %>%
  mutate(period = ifelse(vacuummin2019B2.dfFouri$amount >= 0.5,
                         paste(round(1/vacuummin2019B2.dfFouri$freq, 2),"days"),""))
vacuummin2019B2.dfFouri[1,3] <- ""
ggplot(vacuummin2019B2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,1.5) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2B Drum Vacuum Pressure Fourier Analysis")


# 2B cake wash
cakewashmin2019B2.ts <- ts(filtercakewashmin2019B2$data) #change to time series
cakewashmin2019B2.freq <- (1/(30*60))  #sample frequency in Hz 
cakewashmin2019B2.dur <- length(cakewashmin2019B2.ts)*30*60 # length of signal in seconds 
cakewashmin2019B2.tot <- cakewashmin2019B2.freq*cakewashmin2019B2.dur #total number of sample

cakewashmin2019B2.x <- seq(0, cakewashmin2019B2.dur, length.out = cakewashmin2019B2.tot)
cakewashmin2019B2.fourier <- fft(cakewashmin2019B2.ts)
cakewashmin2019B2.amo <- 2*Mod(cakewashmin2019B2.fourier)/(cakewashmin2019B2.tot) #amplitude
cakewashmin2019B2.freqvec <- 0:(length(cakewashmin2019B2.amo)-1) #vector from 0 to end of signal -> new x-axis

cakewashmin2019B2.amo[cakewashmin2019B2.freqvec == 0] <-
  cakewashmin2019B2.amo[cakewashmin2019B2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
cakewashmin2019B2.freqvec <- cakewashmin2019B2.freqvec/cakewashmin2019B2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
cakewashmin2019B2.dfFouri <- data.frame(freq = cakewashmin2019B2.freqvec[1:as.integer(0.5*cakewashmin2019B2.freq*cakewashmin2019B2.dur)]*60*60*24,
                                        amount = cakewashmin2019B2.amo[1:as.integer(0.5*cakewashmin2019B2.freq*cakewashmin2019B2.dur)])
cakewashmin2019B2.dfFouri <- cakewashmin2019B2.dfFouri %>%
  mutate(period = ifelse(cakewashmin2019B2.dfFouri$amount >= 0.3,
                         paste(round(1/cakewashmin2019B2.dfFouri$freq, 2),"days"),""))
cakewashmin2019B2.dfFouri[1,3] <- ""
ggplot(cakewashmin2019B2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,2.8) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2B Drum Cake Wash Fourier Analysis")

# 2B soda filtrate conc.
sodafiltratemin2019B2.ts <- ts(filtersodafiltratemin2019B2$data) #change to time series
sodafiltratemin2019B2.freq <- (1/(30*60))  #sample frequency in Hz 
sodafiltratemin2019B2.dur <- length(sodafiltratemin2019B2.ts)*30*60 # length of signal in seconds 
sodafiltratemin2019B2.tot <- sodafiltratemin2019B2.freq*sodafiltratemin2019B2.dur #total number of sample

sodafiltratemin2019B2.x <- seq(0, sodafiltratemin2019B2.dur, length.out = sodafiltratemin2019B2.tot)
sodafiltratemin2019B2.fourier <- fft(sodafiltratemin2019B2.ts)
sodafiltratemin2019B2.amo <- 2*Mod(sodafiltratemin2019B2.fourier)/(sodafiltratemin2019B2.tot) #amplitude
sodafiltratemin2019B2.freqvec <- 0:(length(sodafiltratemin2019B2.amo)-1) #vector from 0 to end of signal -> new x-axis

sodafiltratemin2019B2.amo[sodafiltratemin2019B2.freqvec == 0] <-
  sodafiltratemin2019B2.amo[sodafiltratemin2019B2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
sodafiltratemin2019B2.freqvec <- sodafiltratemin2019B2.freqvec/sodafiltratemin2019B2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
sodafiltratemin2019B2.dfFouri <- data.frame(freq = sodafiltratemin2019B2.freqvec[1:as.integer(0.5*sodafiltratemin2019B2.freq*sodafiltratemin2019B2.dur)]*60*60*24,
                                            amount = sodafiltratemin2019B2.amo[1:as.integer(0.5*sodafiltratemin2019B2.freq*sodafiltratemin2019B2.dur)])
sodafiltratemin2019B2.dfFouri <- sodafiltratemin2019B2.dfFouri %>%
  mutate(period = ifelse(sodafiltratemin2019B2.dfFouri$amount >= 1,
                         paste(round(1/sodafiltratemin2019B2.dfFouri$freq, 2),"days"),""))
sodafiltratemin2019B2.dfFouri[1,3] <- ""
ggplot(sodafiltratemin2019B2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,8) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2B Drum Soda Filtrate Fourier Analysis")


# 2B oxalate filtrate
oxfiltratemin2019B2.ts <- ts(filteroxfiltratemin2019B2$data) #change to time series
oxfiltratemin2019B2.freq <- (1/(30*60))  #sample frequency in Hz 
oxfiltratemin2019B2.dur <- length(oxfiltratemin2019B2.ts)*30*60 # length of signal in seconds 
oxfiltratemin2019B2.tot <- oxfiltratemin2019B2.freq*oxfiltratemin2019B2.dur #total number of sample

oxfiltratemin2019B2.x <- seq(0, oxfiltratemin2019B2.dur, length.out = oxfiltratemin2019B2.tot)
oxfiltratemin2019B2.fourier <- fft(oxfiltratemin2019B2.ts)
oxfiltratemin2019B2.amo <- 2*Mod(oxfiltratemin2019B2.fourier)/(oxfiltratemin2019B2.tot) #amplitude
oxfiltratemin2019B2.freqvec <- 0:(length(oxfiltratemin2019B2.amo)-1) #vector from 0 to end of signal -> new x-axis

oxfiltratemin2019B2.amo[oxfiltratemin2019B2.freqvec == 0] <-
  oxfiltratemin2019B2.amo[oxfiltratemin2019B2.freqvec == 0]/2 #amplitude at the zero frequency has to be halfed at freq=0  
oxfiltratemin2019B2.freqvec <- oxfiltratemin2019B2.freqvec/oxfiltratemin2019B2.dur #vector divided by duration of the signal -> frequency vector -> new x-axis
oxfiltratemin2019B2.dfFouri <- data.frame(freq = oxfiltratemin2019B2.freqvec[1:as.integer(0.5*oxfiltratemin2019B2.freq*oxfiltratemin2019B2.dur)]*60*60*24,
                                          amount = oxfiltratemin2019B2.amo[1:as.integer(0.5*oxfiltratemin2019B2.freq*oxfiltratemin2019B2.dur)])
oxfiltratemin2019B2.dfFouri <- oxfiltratemin2019B2.dfFouri %>%
  mutate(period = ifelse(oxfiltratemin2019B2.dfFouri$amount >= 1,
                         paste(round(1/oxfiltratemin2019B2.dfFouri$freq, 2),"days"),""))
oxfiltratemin2019B2.dfFouri[1,3] <- ""
ggplot(oxfiltratemin2019B2.dfFouri, aes(x = freq,y = amount)) +
  geom_bar(stat = "identity", color = "black", fill = "white")+
  geom_text(aes(label=period), hjust = -0.1, size = 3, angle = 90)+
  xlim(0,2) + ylim(0,10) + labs(x ="Freq, 1/day",y ="Magnitude", 
                                title ="2B Drum Oxalate Filtrate Fourier Analysis")
