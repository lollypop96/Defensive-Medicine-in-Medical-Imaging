library(ggplot2)
library(plyr)
library(dplyr)
library(QRM)
library(tibble)
library(qrmtools)
library(nvmix)
library(readxl)
library(pracma)
library(fit.models)
library(sqldf)
library(writexl)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(readr)
library(data.table)

####################################################################################################
                    ### Delayed_Transfers_of_Care_18_19 ###

dtc18_19 <- read.csv(file = "C:/Users/liry9/Desktop/Delayed Transfers of Care Data 2018-19/Delayed_Transfers_of_Care_18_19.csv", header = TRUE)
dtc18_19 <- dtc18_19[, -c(2:4)]
dtc18_19 <- dtc18_19[, -c(3:4)]

dtc_18_19<- sqldf ("SELECT dtc18_19.'Provider.Org.Name',
            dtc18_19.'Reason.For.Delay',
            sum(dtc18_19.'NHS'),
            sum(dtc18_19.'Social.Care'),
            sum(dtc18_19.'Both.B')
            FROM dtc18_19
            GROUP BY dtc18_19.'Provider.Org.Name', dtc18_19.'Reason.For.Delay'")

View(dtc_18_19)
rm(dtc18_19)

#rm(dtc_18_19)
#AND Delayed_Transfers_of_Care_18_19.'Reason.For.Delay'

#####################################################################################################
                   ### Attendances_and_Emergency_Admissions_18_19 ###

aem18_19 <- read.csv(file = "C:/Users/liry9/Desktop/A&E Attendances and Emergency Admissions 2018-19/Attendances_and_Emergency_Admissions_18_19.csv", header = TRUE)
View(aem18_19)
aem18_19 <- aem18_19[, -c(2:3)]

aea_18_19<- sqldf ("SELECT aem18_19.'Org.Name',
            sum(aem18_19.'Number.of.A.E.attendances.Type.1'),
            sum(aem18_19.'Number.of.A.E.attendances.Type.2'),
            sum(aem18_19.'Number.of.A.E.attendances.Other.A.E.Department') AS Number_of_AE_attendances_other_AE_dep,
            sum(aem18_19.'Number.of.attendances.over.4hrs.Type.1'),
            sum(aem18_19.'Number.of.attendances.over.4hrs.Type.2'),
            sum(aem18_19.'Number.of.attendances.over.4hrs.Other.A.E.Department') AS Number_of_attendances_over_4hrs_other_AE_dep,
            sum(aem18_19.'Patients.who.have.waited.4.12.hs.from.DTA.to.admission'),
            sum(aem18_19.'Patients.who.have.waited.12..hrs.from.DTA.to.admission'),
            sum(aem18_19.'Emergency.admissions.via.A.E...Type.1') AS Emergency_admissions_via_AE_Type1,
            sum(aem18_19.'Emergency.admissions.via.A.E...Type.2') AS Emergency_admissions_via_AE_Type2,
            sum(aem18_19.'Emergency.admissions.via.A.E...Other.A.E.department') AS Emergency_admissions_via_AE_other_AE_dep,
            sum(aem18_19.'Other.emergency.admissions')
            FROM aem18_19
            GROUP BY aem18_19.'Org.Name'")

View(aea_18_19)
rm(aem18_19)

aea_18_19[c(2:13)] <- lapply(aea_18_19[c(2:13)], function(x) c(scale(x)))
#######################################################################################################################
                        ### Critical_Care_Bed_Capacity_and_Urgent_Operations_Cancelled_18_19 ###

ccbc_uoc18_19 <- read.csv(file = "C:/Users/liry9/Desktop/Critical Care Bed Capacity and Urgent Operations Cancelled 2018-19 Data/Critical_Care_Bed_Capacity_and_Urgent_Operations_Cancelled_18_19.csv", header = TRUE)
ccbc_uoc18_19 <- ccbc_uoc18_19[, -c(2:4)]
View(ccbc_uoc18_19)

ccbc_uoc_18_19<- sqldf ("SELECT ccbc_uoc18_19.'Org.Name',
                        sum(ccbc_uoc18_19.'Urgent.operations.cancelled') AS 'uo_canced',
                        sum(ccbc_uoc18_19.'Urgent.operations.cancelled.for.the.2nd.or.more.time') AS 'uo_canced_2times_or_more',
                        avg(ccbc_uoc18_19.'Number.of.adult.critical.care.beds.open'),
                        avg(ccbc_uoc18_19.'Number.of.adult.critical.care.beds.occupied'),
                        avg(ccbc_uoc18_19.'Number.of.paediatric.intensive.care.beds.open'),
                        avg(ccbc_uoc18_19.'Number.of.paediatric.intensive.care.beds.occupied'),
                        avg(ccbc_uoc18_19.'Number.of.neonatal.critical.care.cots..or.beds..open') AS Number_of_neonatal_critical_care_cots_or_beds_open,
                        avg(ccbc_uoc18_19.'Number.of.neonatal.critical.care.cots..or.beds..occupied') AS Number_of_neonatal_critical_care_cots_or_beds_occupied,
                        avg(ccbc_uoc18_19.'Non.medical.critical.care.transfers')
                        FROM ccbc_uoc18_19
                        GROUP BY ccbc_uoc18_19.'Org.Name'")

View(ccbc_uoc_18_19)
rm(ccbc_uoc18_19)

ccbc_uoc_18_19[c(2:10)] <- lapply(ccbc_uoc_18_19[c(2:10)], function(x) c(scale(x)))
################################################################################################################################################
                        ### Summary Hospital-level Mortality Indicator (SHMI) 18-19 ###
shmi18_19 <- read.csv(file = "C:/Users/liry9/Desktop/SHMI_18_19/SHMI_18_19.csv", header = TRUE)
#rm(shmi18_19)
shmi18_19 <- shmi18_19[, -c(2:2)]
deleterows <- which(shmi18_19$SITE_NAME== "NOT PROVIDED")
shmi18_19 <- shmi18_19[-deleterows,]
shmi18_19_2 = shmi18_19

shmi18_19 <- shmi18_19[!is.na(shmi18_19$SHMI_VALUE), ]
shmi18_19 <- shmi18_19[, -c(15:15)]

shmi18_19 <- sqldf ("SELECT PROVIDER_NAME,
                    avg(SHMI_VALUE),
                    avg(SPELLS),
                    avg(OBSERVED),
                    avg(EXPECTED)
                    FROM shmi18_19
                    GROUP BY shmi18_19.'PROVIDER_NAME'")

shmi18_19[c(2:5)] <- lapply(shmi18_19[c(2:5)], function(x) c(scale(x)))

################################################################################################################################################
### aea_18_19 JOIN shmi18_19 = prova ###
prova <- sqldf("SELECT *
               FROM aea_18_19 JOIN shmi18_19 on aea_18_19.'Org.name'=shmi18_19.'PROVIDER_NAME'")
prova <- prova[, -c(14:14)]

### prova JOIN ccbc_uoc_18_19 = prova2 ###
prova2 <- sqldf("SELECT *
                 FROM prova JOIN ccbc_uoc_18_19 on prova.'Org.name'=ccbc_uoc_18_19.'Org.name'")
prova2 <- prova2[, -c(18:18)]
prova2<- data.frame(prova2)


#####

#rownames(prova2) <- prova2$Org.name
#prova2 <- prova2[, -c(1:1)]

distance <- get_dist(prova2[-1])
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

d2 <- dist(prova2[-1])
d2<- as.matrix(d2)
d2

k2 <- kmeans(d2, centers = 2)
str(k2)

plot<-fviz_cluster(k2, data = prova2[-1], labelsize= 6, pointsize = 0.6)

prova2 %>% 
  select(sum.aem18_19..Patients.who.have.waited.4.12.hs.from.DTA.to.admission.., avg.SHMI_VALUE., avg.ccbc_uoc18_19..Number.of.adult.critical.care.beds.occupied.. ) %>% 
  dist() %>% 
  hclust() %>% 
  as.dendrogram() -> dend

# Plot
par(mar=c(8,2,2,1), mfrow=c(1,1), cex=0.45)  # Increase bottom margin to have the complete label
plot(dend)
