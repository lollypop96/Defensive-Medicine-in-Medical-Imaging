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
library(dygraphs)
library(xts)
library(lubridate)
library(plotly)
library(hrbrthemes)
library(forecast)

####################################################################################################
####################################################################################################
                                      ### England: Total cases ### 
England <- read_excel("C:/Users/liry9/Desktop/Patients_needs/England/England.xlsx", sheet = "England", col_names = TRUE)

# Time series of Computerized Axial Tomography
cat <- xts(x= England$`Computerized Axial Tomography`, order.by = England$`mm/yy` )

CAT_plot <- dygraph(cat) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

CAT_plot

# Time series of Diagnostic Ultrasonography
du <- xts(x= England$`Diagnostic Ultrasonography`, order.by = England$`mm/yy` )

DU_plot <- dygraph(du) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

DU_plot

# Time series of Fluoroscopy
fluo <- xts(x= England$`Computerized Axial Tomography`, order.by = England$`mm/yy` )

FLUO_plot <- dygraph(fluo) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

FLUO_plot

# Time series of Magnetic Resonance Imaging
mri <- xts(x= England$`Magnetic Resonance Imaging`, order.by = England$`mm/yy` )

MRI_plot <- dygraph(mri) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

MRI_plot

# Time series of Medical Photography
names(England)[6] <- "Medical Photography"

mp <- xts(x= England$`Medical Photography`, order.by = England$`mm/yy` )

MP_plot <- dygraph(mp) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

MP_plot

# Alternative Medical Photography

mp2<- England %>%
  ggplot( aes(x=England$`mm/yy`, y=England$`Medical Photography`)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  xlab("Year") +
  ylab("Medical Photography") +
  theme_ipsum()

mp2 <- ggplotly(mp2)
mp2


# Time series of Nuclear Medicine
names(England)[7] <- "Nuclear Medicine"
nm <- xts(x= England$`Nuclear Medicine`, order.by = England$`mm/yy` )

NM_plot <- dygraph(nm) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

NM_plot

## Time series for Plain Radiography
names(England)[8] <- "Plain Radiography"
pr <- xts(x= England$`Plain Radiography`, order.by = England$`mm/yy` )

PR_plot <- dygraph(pr) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

PR_plot

## Time series for Positron Emission Tomography
names(England)[9] <- "Positron Emission Tomography"
pet <- xts(x= England$`Positron Emission Tomography`, order.by = England$`mm/yy` )

PET_plot <- dygraph(pet) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

PET_plot

## Time series for Single Photon Emission Computerized Tomography
names(England)[10] <- "Single Photon Emission Computerized Tomography"
spect <- xts(x= England$`Single Photon Emission Computerized Tomography`, order.by = England$`mm/yy` )

SPECT_plot <- dygraph(spect) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.2, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 10, highlightSeriesBackgroundAlpha = 0.5, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)

SPECT_plot

####################################################################################################
#### Trend and seasonality ####

### Computerized Axial Tomography ###
cat_ts <- ts(as.numeric(England$`Computerized Axial Tomography`), frequency=12, start=c(2012, 4))
cat_ts
plot.ts(cat_ts)

log_cat<- log(cat_ts)
plot.ts(log_cat)
cat_components <- decompose(log_cat)
cat_components$seasonal

plot(cat_components)
cat_seasonally_adjusted <- log_cat - cat_components$seasonal
plot(cat_seasonally_adjusted)

cat_forecast <- HoltWinters(log_cat)
cat_forecast
cat_forecast$SSE
plot(cat_forecast)


### Diagnostic Ultrasonography ###

du_ts <- ts(as.numeric(England$`Diagnostic Ultrasonography`), frequency=12, start=c(2012, 4))
du_ts
plot.ts(du_ts)

log_du<- log(du_ts)
plot.ts(log_du)
du_components <- decompose(log_du)
du_components$seasonal

plot(du_components)
du_seasonally_adjusted <- log_du - du_components$seasonal
plot(du_seasonally_adjusted)

du_forecast <- HoltWinters(log_du)
du_forecast
du_forecast$SSE
plot(du_forecast)

### Fluoroscopy
fluo_ts <- ts(as.numeric(England$Fluoroscopy), frequency=12, start=c(2012, 4))
fluo_ts
plot.ts(fluo_ts)

log_fluo<- log(fluo_ts)
plot.ts(log_fluo)
fluo_components <- decompose(log_fluo)
fluo_components$seasonal

plot(fluo_components)
fluo_seasonally_adjusted <- log_fluo - fluo_components$seasonal
plot(fluo_seasonally_adjusted)

fluo_forecast <- HoltWinters(log_fluo)
fluo_forecast
fluo_forecast$SSE
plot(fluo_forecast)

### Magnetic Resonance Imaging
mri_ts <- ts(as.numeric(England$`Magnetic Resonance Imaging`), frequency=12, start=c(2012, 4))
mri_ts
plot.ts(mri_ts)

log_mri<- log(mri_ts)
plot.ts(log_mri)
mri_components <- decompose(log_mri)
mri_components$seasonal

plot(mri_components)
mri_seasonally_adjusted <- log_mri - mri_components$seasonal
plot(mri_seasonally_adjusted)

mri_forecast <- HoltWinters(log_mri)
mri_forecast
mri_forecast$SSE
plot(mri_forecast)

### Medical Photography
England
mp_ts <- ts(as.numeric(England$`Medical Photography`), frequency=12, start=c(2012, 4))
mp_ts
plot.ts(mp_ts)

log_mp<- log(mp_ts)
plot.ts(log_mp)
mp_components <- decompose(log_mp)
mp_components$seasonal

plot(mp_components)
mp_seasonally_adjusted <- log_mp - mp_components$seasonal
plot(mp_seasonally_adjusted)

mp_forecast <- HoltWinters(log_mp)
mp_forecast
mp_forecast$SSE
plot(mp_forecast)

### Nuclear Medicine
nm_ts <- ts(as.numeric(England$`Nuclear Medicine`), frequency=12, start=c(2012, 4))
nm_ts
plot.ts(nm_ts)

log_nm<- log(nm_ts)
plot.ts(log_nm)
nm_components <- decompose(log_nm)
nm_components$seasonal

plot(nm_components)
nm_seasonally_adjusted <- log_nm - nm_components$seasonal
plot(nm_seasonally_adjusted)

nm_trend_adjusted <- log_nm - nm_components$trend
plot(nm_trend_adjusted)

nm_forecast <- HoltWinters(log_nm)
nm_forecast
nm_forecast$SSE
plot(nm_forecast)

### Plain Radiography
pr_ts <- ts(as.numeric(England$`Plain Radiography`), frequency=12, start=c(2012, 4))
pr_ts
plot.ts(pr_ts)

log_pr<- log(pr_ts)
plot.ts(log_pr)
pr_components <- decompose(log_pr)
pr_components$seasonal

plot(pr_components)
pr_seasonally_adjusted <- log_pr - pr_components$seasonal
plot(pr_seasonally_adjusted)

pr_trend_adjusted <- log_pr - pr_components$trend
plot(pr_trend_adjusted)

pr_forecast <- HoltWinters(log_pr)
pr_forecast
pr_forecast$SSE
plot(pr_forecast)

### Positron Emission Tomography
pet_ts <- ts(as.numeric(England$`Positron Emission Tomography`), frequency=12, start=c(2012, 4))
pet_ts
plot.ts(pet_ts)

log_pet<- log(pet_ts)
plot.ts(log_pet)
pet_components <- decompose(log_pet)
pet_components$seasonal

plot(pet_components)
pet_seasonally_adjusted <- log_pet - pet_components$seasonal
plot(pet_seasonally_adjusted)

pet_trend_adjusted <- log_pet - pet_components$trend
plot(pet_trend_adjusted)

pet_forecast <- HoltWinters(log_pet)
pet_forecast
pet_forecast$SSE
plot(pet_forecast)

### Single Photon Emission Computerized Tomography
spect_ts <- ts(as.numeric(England$`Single Photon Emission Computerized Tomography`), frequency=12, start=c(2012, 4))
spect_ts
plot.ts(spect_ts)

log_spect<- log(spect_ts)
plot.ts(log_spect)
spect_components <- decompose(log_spect)
spect_components$seasonal

plot(spect_components)
spect_seasonally_adjusted <- log_spect - spect_components$seasonal
plot(spect_seasonally_adjusted)

spect_trend_adjusted <- log_spect - spect_components$trend
plot(spect_trend_adjusted)

spect_forecast <- HoltWinters(log_spect)
spect_forecast
spect_forecast$SSE
plot(spect_forecast)


###################################################################################################
###################################################################################################
                          ###### PREPROCESSING ######
# RUN ONLY ONCE!!!!!!!          ### 2012-2018 ###

#sheets=c('04', '05', '06', '07', '08', '09', '10', '11', '12', '01', '02', '03')
#years=c("12_13","13_14","14_15","15_16","16_17","17_18")
#for(i in 1:length(years)){
#  yearsii=strsplit(years[i], "_")
#  for(k in 1:12){
#    cursheet=sheets[k]
#    if(k<=9){
#      tabletemp<-read_excel(paste("C:/Users/liry9/Desktop/Project PNHM_Lirida Papallazi/Tables/",years[i],".xlsx",sep=""), sheet = paste(cursheet,yearsii[[1]][1],sep="-"), col_names = TRUE)
#      tabletemp["MY"]=paste(cursheet,yearsii[[1]][1],sep="-")
#    }else{
#      tabletemp<-read_excel(paste("C:/Users/liry9/Desktop/Project PNHM_Lirida Papallazi/Tables/",years[i],".xlsx",sep=""), sheet = paste(cursheet,yearsii[[1]][2],sep="-"), col_names = TRUE)
#      tabletemp["MY"]=paste(cursheet,yearsii[[1]][2],sep="-")
#    }
#    if(k==1){
#      table=tabletemp
#    }else{
#      table<-rbind(table,tabletemp)
#    }
#  }
#  write_xlsx(table, path = paste("C:/Users/liry9/Desktop/Project PNHM_Lirida Papallazi/Tables/",years[i],"_new.xlsx",sep="")) #salva in excel
#}


####################################################################################################
####################################################################################################
### Import datasets obtained from the previous operation

new12_13 <- read_excel("C:/Users/liry9/Desktop/Project PNHM_Lirida Papallazi/Tables/12_13_new.xlsx", col_names = TRUE)
new13_14 <- read_excel("C:/Users/liry9/Desktop/Project PNHM_Lirida Papallazi/Tables/13_14_new.xlsx", col_names = TRUE)
new14_15 <- read_excel("C:/Users/liry9/Desktop/Project PNHM_Lirida Papallazi/Tables/14_15_new.xlsx", col_names = TRUE)
new15_16 <- read_excel("C:/Users/liry9/Desktop/Project PNHM_Lirida Papallazi/Tables/15_16_new.xlsx", col_names = TRUE)
new16_17 <- read_excel("C:/Users/liry9/Desktop/Project PNHM_Lirida Papallazi/Tables/16_17_new.xlsx", col_names = TRUE)
new17_18 <- read_excel("C:/Users/liry9/Desktop/Project PNHM_Lirida Papallazi/Tables/17_18_new.xlsx", col_names = TRUE)


### 2018-2019 ###
hospitals_18_19 <- read_excel("C:/Users/liry9/Desktop/Project PNHM_Lirida Papallazi/Tables/18_19.xlsx", sheet = "18_19", col_names = TRUE)
hospitals_18_19 <- hospitals_18_19[!(is.na(hospitals_18_19$`Provider name`)),]
months=c('Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','Jan','Feb','Mar')
monthsnum=c('04', '05', '06', '07', '08', '09', '10', '11', '12', '01', '02', '03')
for(i in 5:16){
  month <- hospitals_18_19[,c(3,4,i)]
  monthsum <- sqldf(paste("SELECT month.'Provider name', Modality, sum(",months[i-4],") as tot
              FROM month
              GROUP BY month.'Provider name', Modality",sep=''))
  transposed <- monthsum %>% spread(Modality, tot)
  if(i-4<=9){
    transposed["MY"]=paste(monthsnum[i-4],"18",sep="-")
  }else{
    transposed["MY"]=paste(monthsnum[i-4],"19",sep="-")
  }
  if(i==5){
    table=transposed
  }else{
    table<-rbind(table,transposed)
  }
}

new18_19 <- table[,-c(6:6)]
rm(table)

                                    ###### END OF PREPROCESSING ########
####################################################################################################
####################################################################################################


provider12_13<- sqldf("SELECT distinct(new12_13.'Provider name')
                      FROM new12_13")
provider13_14<- sqldf("SELECT distinct(new13_14.'Provider name')
                      FROM new13_14")
provider14_15<- sqldf("SELECT distinct(new14_15.'Provider name')
                      FROM new14_15")
provider15_16<- sqldf("SELECT distinct(new15_16.'Provider name')
                      FROM new15_16")
provider16_17<- sqldf("SELECT distinct(new16_17.'Provider name')
                      FROM new16_17")
provider17_18<- sqldf("SELECT distinct(new17_18.'Provider name')
                      FROM new17_18")

provider18_19<- sqldf("SELECT distinct(new18_19.'Provider name')
                      FROM new18_19")

boh1 <- sqldf("SELECT *
              FROM provider12_13
              WHERE provider12_13.'Provider Name' IN 
              (SELECT *
              FROM provider13_14)
              ")

boh2 <- sqldf("SELECT *
              FROM provider14_15
              WHERE provider14_15.'Provider Name' IN 
              (SELECT *
              FROM boh1)
              ")

boh3 <- sqldf("SELECT *
              FROM provider15_16
              WHERE provider15_16.'Provider Name' IN 
              (SELECT *
              FROM boh2)")


boh4 <- sqldf("SELECT *
              FROM provider16_17
              WHERE provider16_17.'Provider Name' IN 
              (SELECT *
              FROM boh3)")

boh5<- sqldf("SELECT *
              FROM provider17_18
              WHERE provider17_18.'Provider Name' IN 
              (SELECT *
              FROM boh4)")

boh6<- sqldf("SELECT *
              FROM provider18_19
              WHERE provider18_19.'Provider Name' IN 
              (SELECT *
              FROM boh5)")

final_providers <- boh6

rm(boh1, boh2, boh3, boh4, boh5, boh6, provider12_13, provider13_14, provider14_15, provider15_16, provider16_17, provider17_18, provider18_19)

# <- merge(new12_13, new13_14, new14_15, new15_16, new17_18, by=('Area Team Code', 'Org Code', 'Provider Name', 'Computerized Axial Tomography','Diagnostic Ultrasonography','Fluoroscopy',	'Magnetic Resonance Imaging',	'Nuclear
#Medicine', 'Plain Radiography',	'Positron Emission Tomography', 'Single Photon Emission Computerized Tomography',	'MY'))

new12_18 <- rbind(new12_13, new13_14, new14_15, new15_16, new16_17, new17_18)

rm(new12_13, new13_14, new14_15, new15_16, new16_17, new17_18)
new12_18 <- new12_18[!(is.na(new12_18$`Provider Name`)),]
new12_18 <- new12_18[,-c(1:2)]

new12_18 <- sqldf("SELECT *
                  FROM new12_18
                  WHERE new12_18.'Provider Name' in (
                      SELECT *
                      FROM final_providers)")

names(new18_19)[1] <- "Provider Name"

new18_19 <- sqldf("SELECT *
                  FROM new18_19
                  WHERE new18_19.'Provider Name' in (
                      SELECT *
                      FROM final_providers)")


names(new18_19)[6] <- "Nuclear Medicine"
names(new12_18)[6] <- "Nuclear Medicine"
new12_19 <- rbind(new12_18, new18_19)
rm(new12_18, new18_19)

colSums(is.na(new12_19))
new12_19 <- new12_19[,-c(8:9)]
new12_19 <- new12_19[,-c(6:6)]

####################################################################################
### Check if all the hospitals in the list have a value for every period ###

sqldf("SELECT new12_19.'Provider Name', count(*) as tot
      FROM new12_19
      group by new12_19.'Provider Name'
      having count(*)>84
          ")

rm(final_providers, hospitals_18_19, month, monthsum, transposed)

####################################################################################################
### Computerized Axial Tomography ###

cat <- sqldf("SELECT new12_19.'Provider Name', new12_19.'Computerized Axial Tomography', MY
                FROM new12_19")

cat <- sqldf("SELECT cat.'Provider name',
              sum(cat.'Computerized Axial Tomography') as tot_cat
              From cat
              Group by cat.'Provider Name'
             ")
cat = cat[order(-cat$tot_cat),]

#Add Rank Column
cat = add_column(cat, Rank=1:nrow(cat), .before="Provider Name")

#Add LogRank Column and LogCO2 Column
cat = add_column(cat, LogRank=log(cat$Rank), .before="Provider Name")

#Mean excess function plot
MEplot(cat$tot_cat)        
plot(x=cat$LogRank, y=cat$Rank, col="blue")


### Diagnostic Ultrasonography ###

du <- sqldf("SELECT new12_19.'Provider Name', new12_19.'Diagnostic Ultrasonography', MY
                FROM new12_19")

du <- sqldf("SELECT du.'Provider Name',
              sum(du.'Diagnostic Ultrasonography') as tot_du
              From du
              Group by du.'Provider Name'
             ")
du = du[order(-du$tot_du),]

#Add Rank Column
du = add_column(du, Rank=1:nrow(du), .before="Provider Name")

#Add LogRank Column and LogCO2 Column
du = add_column(du, LogRank=log(du$Rank), .before="Provider Name")

#Mean excess function plot
MEplot(du$tot_du)        
plot(x=du$LogRank, y=du$Rank, col="blue")

### Fluoroscopy

fluo <- sqldf("SELECT new12_19.'Provider Name', new12_19.'Fluoroscopy', MY
                FROM new12_19")

fluo <- sqldf("SELECT fluo.'Provider Name',
              sum(fluo.'Fluoroscopy') as tot_fluo
              From fluo
              Group by fluo.'Provider Name'
             ")

fluo = fluo[order(-fluo$tot_fluo),]

#Add Rank Column
fluo = add_column(fluo, Rank=1:nrow(fluo), .before="Provider Name")

#Add LogRank Column and LogCO2 Column
fluo = add_column(fluo, LogRank=log(fluo$Rank), .before="Provider Name")

#Mean excess function plot
MEplot(fluo$tot_fluo)     
plot(x=fluo$LogRank, y=fluo$Rank, col="blue")

### Magnetic Resonance Imaging

mri <- sqldf("SELECT new12_19.'Provider Name', new12_19.'Magnetic Resonance Imaging', MY
                FROM new12_19")

mri <- sqldf("SELECT mri.'Provider Name',
              sum(mri.'Magnetic Resonance Imaging') as tot_mri
              From mri
              Group by mri.'Provider Name'
             ")

mri = mri[order(-mri$tot_mri),]

#Add Rank Column
mri = add_column(mri, Rank=1:nrow(mri), .before="Provider Name")

#Add LogRank Column and LogCO2 Column
mri = add_column(mri, LogRank=log(mri$Rank), .before="Provider Name")

#Mean excess function plot
MEplot(mri$tot_mri)        
plot(x=mri$LogRank, y=mri$Rank, col="blue")


### Plain Radiography

pr <- sqldf("SELECT new12_19.'Provider Name', new12_19.'Plain Radiography', MY
                FROM new12_19")

pr <- sqldf("SELECT pr.'Provider Name',
              sum(pr.'Plain Radiography') as tot_pr
              From pr
              Group by pr.'Provider Name'
             ")

pr = pr[order(-pr$tot_pr),]

#Add Rank Column
pr = add_column(pr, Rank=1:nrow(pr), .before="Provider Name")

#Add LogRank Column and LogCO2 Column
pr = add_column(pr, LogRank=log(pr$Rank), .before="Provider Name")

#Mean excess function plot
MEplot(pr$tot_pr)        
plot(x=pr$LogRank, y=pr$Rank, col="blue")

#################################################################################
#################################################################################
### We plot now the time series for the 4 hospitals we are considering
# Thornton v Homerton University Hospital NHS Trust [2017] -> https://medicalnegligenceteam.com/post/thornton-v-homerton-university-hospital-nhs-trust-2017-ewhc-3244-qb-15-december-2017
# Harding v Buckinghamshire Healthcare NHS Trust -> https://medicalnegligenceteam.com/post/harding-v-buckinghamshire-healthcare-nhs-trust-13-september-2017
# Barnett v Medway NHS Foundation Trust [2017] -> https://medicalnegligenceteam.com/post/barnett-v-medway-nhs-foundation-trust-2017-ewca-civ-235-06-april-2017
# AB v Royal Devon & Exeter NHS Foundation Trust [2016] -> https://www.medicalnegligenceteam.com/post/ab-v-royal-devon--exeter-nhs-foundation-trust-4-may-2016

## !!!! CODE FOR THE CONSTRUCTION OF THE EXCEL DATASET!!!!!
## !!!! DON'T RUN THE LAST LINE OF CODE: I WILL PROVIDE YOU THE DATASETS!!!!

################################
#### Medway NHS Foundation Trust
Medway <- sqldf("SELECT *
                   FROM new12_19
                   WHERE new12_19.'Provider Name'= 'Medway NHS Foundation Trust'")


med_ts_cat <- ts(as.numeric(Medway$`Computerized Axial Tomography`), frequency=12, start=c(2012, 4))
med_ts_cat
plot.ts(med_ts_cat)

log_med_ts_cat <- log(med_ts_cat)
plot.ts(log_med_ts_cat)
med_components_cat <- decompose(log_med_ts_cat)

plot(med_components_cat)
med_seasonally_adjusted_cat <- log_med_ts_cat - med_components_cat$seasonal
plot(med_seasonally_adjusted_cat)

med_forecast <- HoltWinters(log_med_ts_cat)
med_forecast
med_forecast$SSE
plot(med_forecast)

Medway=add_column(Medway, med_components_cat$seasonal, .after="Computerized Axial Tomography")
Medway = add_column(Medway, med_components_cat$trend, .after="med_components_cat$seasonal")
Medway = add_column(Medway, med_components_cat$random, .after="med_components_cat$trend")


# Diagnostic Ultrasonography
med_ts_du <- ts(as.numeric(Medway$`Diagnostic Ultrasonography`), frequency=12, start=c(2012, 4))
med_ts_du
plot.ts(med_ts_du)

log_med_ts_du<- log(med_ts_du)
plot.ts(log_med_ts_du)
med_components_du <- decompose(log_med_ts_du)

plot(med_components_du)
med_du_seasonally_adjusted <- log_med_ts_du - med_components_du$seasonal
plot(med_du_seasonally_adjusted)

Medway=add_column(Medway, med_components_du$seasonal, .after="Diagnostic Ultrasonography")
Medway = add_column(Medway, med_components_du$trend, .after="med_components_du$seasonal")
Medway = add_column(Medway, med_components_du$random, .after="med_components_du$trend")

# Fluoroscopy
med_ts_fluo <- ts(as.numeric(Medway$Fluoroscopy), frequency=12, start=c(2012, 4))
med_ts_fluo
plot.ts(med_ts_fluo)

log_med_ts_fluo <- log(med_ts_fluo)
plot.ts(log_med_ts_fluo)
med_components_fluo <- decompose(log_med_ts_fluo)

plot(med_components_fluo)
med_fluo_seasonally_adjusted <- log_med_ts_fluo - med_components_fluo$seasonal
plot(med_fluo_seasonally_adjusted)

Medway = add_column(Medway, med_components_fluo$seasonal, .after="Fluoroscopy")
Medway = add_column(Medway, med_components_fluo$trend, .after="med_components_fluo$seasonal")
Medway = add_column(Medway, med_components_fluo$random, .after="med_components_fluo$trend")

# Magnetic Resonance Imaging
med_ts_mri <- ts(as.numeric(Medway$`Magnetic Resonance Imaging`), frequency=12, start=c(2012, 4))
med_ts_mri
plot.ts(med_ts_mri)

log_med_ts_mri<- log(med_ts_mri)
plot.ts(log_med_ts_mri)
med_components_mri <- decompose(log_med_ts_mri)


plot(med_components_mri)
med_mri_seasonally_adjusted <- log_med_ts_mri - med_components_mri$seasonal
plot(med_mri_seasonally_adjusted)

Medway=add_column(Medway, med_components_mri$seasonal, .after="Magnetic Resonance Imaging")
Medway = add_column(Medway, med_components_mri$trend, .after="med_components_mri$seasonal")
Medway = add_column(Medway, med_components_mri$random, .after="med_components_mri$trend")

# Plain Radiography
med_ts_pr <- ts(as.numeric(Medway$`Plain Radiography`), frequency=12, start=c(2012, 4))
med_ts_pr
plot.ts(med_ts_pr)

log_med_ts_pr<- log(med_ts_pr)
plot.ts(log_med_ts_pr)
med_components_pr <- decompose(log_med_ts_pr)

plot(med_components_pr)
med_pr_seasonally_adjusted <- log_med_ts_pr - med_components_pr$seasonal
plot(med_pr_seasonally_adjusted)


Medway=add_column(Medway, med_components_pr$seasonal, .after="Plain Radiography")
Medway = add_column(Medway, med_components_pr$trend, .after="med_components_pr$seasonal")
Medway = add_column(Medway, med_components_pr$random, .after="med_components_pr$trend")

names(Medway)[3] <- "CAT Seasonality"
names(Medway)[4] <- "CAT Trend"
names(Medway)[5] <- "CAT Random"
names(Medway)[7] <- "DU Seasonality"
names(Medway)[8] <- "DU Trend"
names(Medway)[9] <- "DU Random"
names(Medway)[11] <- "Fluo Seasonality"
names(Medway)[12] <- "Fluo Trend"
names(Medway)[13] <- "Fluo Random"
names(Medway)[15] <- "MRI Seasonality"
names(Medway)[16] <- "MRI Trend"
names(Medway)[17] <- "MRI Random"
names(Medway)[19] <- "PR Seasonality"
names(Medway)[20] <- "PR Trend"
names(Medway)[21] <- "PR Random"


# write_xlsx(Medway, path = "C:/Users/liry9/Desktop/Medway.xlsx") #salva in excel

######################################################
#### Homerton University Hospital NHS Foundation Trust
Homerton <- sqldf("SELECT *
                   FROM new12_19
                   WHERE new12_19.'Provider Name'= 'Homerton University Hospital NHS Foundation Trust'")

# Computerized Axial Tomography
homer_ts_cat <- ts(as.numeric(Homerton$`Computerized Axial Tomography`), frequency=12, start=c(2012, 4))
homer_ts_cat
plot.ts(homer_ts_cat)

log_homer_ts_cat<- log(homer_ts_cat)
plot.ts(log_homer_ts_cat)
homer_components_cat <- decompose(log_homer_ts_cat)

plot(homer_components_cat)
homer_cat_seasonally_adjusted <- log_homer_ts_cat - homer_components_cat$seasonal
plot(homer_cat_seasonally_adjusted)

Homerton=add_column(Homerton, homer_components_cat$seasonal, .after="Computerized Axial Tomography")
Homerton = add_column(Homerton, homer_components_cat$trend, .after="homer_components_cat$seasonal")
Homerton = add_column(Homerton, homer_components_cat$random, .after="homer_components_cat$trend")

# Diagnostic Ultrasonography
homer_ts_du <- ts(as.numeric(Homerton$`Diagnostic Ultrasonography`), frequency=12, start=c(2012, 4))
homer_ts_du
plot.ts(homer_ts_du)

log_homer_ts_du<- log(homer_ts_du)
plot.ts(log_homer_ts_du)
homer_components_du <- decompose(log_homer_ts_du)

plot(homer_components_du)
homer_du_seasonally_adjusted <- log_homer_ts_du - homer_components_du$seasonal
plot(homer_du_seasonally_adjusted)

Homerton=add_column(Homerton, homer_components_du$seasonal, .after="Diagnostic Ultrasonography")
Homerton = add_column(Homerton, homer_components_du$trend, .after="homer_components_du$seasonal")
Homerton = add_column(Homerton, homer_components_du$random, .after="homer_components_du$trend")

# Fluoroscopy
homer_ts_fluo <- ts(as.numeric(Homerton$Fluoroscopy), frequency=12, start=c(2012, 4))
homer_ts_fluo
plot.ts(homer_ts_fluo)

log_homer_ts_fluo <- log(homer_ts_fluo)
plot.ts(log_homer_ts_fluo)
homer_components_fluo <- decompose(log_homer_ts_fluo)

plot(homer_components_fluo)
homer_fluo_seasonally_adjusted <- log_homer_ts_fluo - homer_components_fluo$seasonal
plot(homer_fluo_seasonally_adjusted)

Homerton = add_column(Homerton, homer_components_fluo$seasonal, .after="Fluoroscopy")
Homerton = add_column(Homerton, homer_components_fluo$trend, .after="homer_components_fluo$seasonal")
Homerton = add_column(Homerton, homer_components_fluo$random, .after="homer_components_fluo$trend")

# Magnetic Resonance Imaging
homer_ts_mri <- ts(as.numeric(Homerton$`Magnetic Resonance Imaging`), frequency=12, start=c(2012, 4))
homer_ts_mri
plot.ts(homer_ts_mri)

log_homer_ts_mri<- log(homer_ts_mri)
plot.ts(log_homer_ts_mri)
homer_components_mri <- decompose(log_homer_ts_mri)

plot(homer_components_mri)
homer_mri_seasonally_adjusted <- log_homer_ts_mri - homer_components_mri$seasonal
plot(homer_mri_seasonally_adjusted)

Homerton=add_column(Homerton, homer_components_mri$seasonal, .after="Magnetic Resonance Imaging")
Homerton = add_column(Homerton, homer_components_mri$trend, .after="homer_components_mri$seasonal")
Homerton = add_column(Homerton, homer_components_mri$random, .after="homer_components_mri$trend")

# Plain Radiography
homer_ts_pr <- ts(as.numeric(Homerton$`Plain Radiography`), frequency=12, start=c(2012, 4))
homer_ts_pr
plot.ts(homer_ts_pr)

log_homer_ts_pr<- log(homer_ts_pr)
plot.ts(log_homer_ts_pr)
homer_components_pr <- decompose(log_homer_ts_pr)

plot(homer_components_pr)
homer_pr_seasonally_adjusted <- log_homer_ts_pr - homer_components_pr$seasonal
plot(homer_pr_seasonally_adjusted)

Homerton=add_column(Homerton, homer_components_pr$seasonal, .after="Plain Radiography")
Homerton = add_column(Homerton, homer_components_pr$trend, .after="homer_components_pr$seasonal")
Homerton = add_column(Homerton, homer_components_pr$random, .after="homer_components_pr$trend")

names(Homerton)[3] <- "CAT Seasonality"
names(Homerton)[4] <- "CAT Trend"
names(Homerton)[5] <- "CAT Random"
names(Homerton)[7] <- "DU Seasonality"
names(Homerton)[8] <- "DU Trend"
names(Homerton)[9] <- "DU Random"
names(Homerton)[11] <- "Fluo Seasonality"
names(Homerton)[12] <- "Fluo Trend"
names(Homerton)[13] <- "Fluo Random"
names(Homerton)[15] <- "MRI Seasonality"
names(Homerton)[16] <- "MRI Trend"
names(Homerton)[17] <- "MRI Random"
names(Homerton)[19] <- "PR Seasonality"
names(Homerton)[20] <- "PR Trend"
names(Homerton)[21] <- "PR Random"

# write_xlsx(Homerton, path = "C:/Users/liry9/Desktop/Homerton.xlsx") #salva in excel

#########################################
#### Buckinghamshire Healthcare NHS Trust
Buck <- sqldf("SELECT *
                   FROM new12_19
                   WHERE new12_19.'Provider Name'= 'Buckinghamshire Healthcare NHS Trust'")

# Computerized Axial Tomography
buck_ts_cat <- ts(as.numeric(Buck$`Computerized Axial Tomography`), frequency=12, start=c(2012, 4))
buck_ts_cat
plot.ts(buck_ts_cat)

log_buck_ts_cat<- log(buck_ts_cat)
plot.ts(log_buck_ts_cat)
buck_components_cat <- decompose(log_buck_ts_cat)

plot(buck_components_cat)
buck_cat_seasonally_adjusted <- log_buck_ts_cat - buck_components_cat$seasonal
plot(buck_cat_seasonally_adjusted)

Buck=add_column(Buck, buck_components_cat$seasonal, .after="Computerized Axial Tomography")
Buck = add_column(Buck, buck_components_cat$trend, .after="buck_components_cat$seasonal")
Buck = add_column(Buck, buck_components_cat$random, .after="buck_components_cat$trend")

# Diagnostic Ultrasonography
buck_ts_du <- ts(as.numeric(Buck$`Diagnostic Ultrasonography`), frequency=12, start=c(2012, 4))
buck_ts_du
plot.ts(buck_ts_du)

log_buck_ts_du<- log(buck_ts_du)
plot.ts(log_buck_ts_du)
buck_components_du <- decompose(log_buck_ts_du)

plot(buck_components_du)
buck_du_seasonally_adjusted <- log_buck_ts_du - buck_components_du$seasonal
plot(buck_du_seasonally_adjusted)

Buck=add_column(Buck, buck_components_du$seasonal, .after="Diagnostic Ultrasonography")
Buck = add_column(Buck, buck_components_du$trend, .after="buck_components_du$seasonal")
Buck = add_column(Buck, buck_components_du$random, .after="buck_components_du$trend")

# Fluoroscopy
buck_ts_fluo <- ts(as.numeric(Buck$Fluoroscopy), frequency=12, start=c(2012, 4))
buck_ts_fluo
plot.ts(buck_ts_fluo)

log_buck_ts_fluo <- log(buck_ts_fluo)
plot.ts(log_buck_ts_fluo)
buck_components_fluo <- decompose(log_buck_ts_fluo)

plot(buck_components_fluo)
buck_fluo_seasonally_adjusted <- log_buck_ts_fluo - buck_components_fluo$seasonal
plot(buck_fluo_seasonally_adjusted)

Buck = add_column(Buck, buck_components_fluo$seasonal, .after="Fluoroscopy")
Buck = add_column(Buck, buck_components_fluo$trend, .after="buck_components_fluo$seasonal")
Buck = add_column(Buck, buck_components_fluo$random, .after="buck_components_fluo$trend")

# Magnetic Resonance Imaging
buck_ts_mri <- ts(as.numeric(Buck$`Magnetic Resonance Imaging`), frequency=12, start=c(2012, 4))
buck_ts_mri
plot.ts(buck_ts_mri)

log_buck_ts_mri<- log(buck_ts_mri)
plot.ts(log_buck_ts_mri)
buck_components_mri <- decompose(log_buck_ts_mri)

plot(buck_components_mri)
buck_mri_seasonally_adjusted <- log_buck_ts_mri - buck_components_mri$seasonal
plot(buck_mri_seasonally_adjusted)

Buck=add_column(Buck, buck_components_mri$seasonal, .after="Magnetic Resonance Imaging")
Buck = add_column(Buck, buck_components_mri$trend, .after="buck_components_mri$seasonal")
Buck = add_column(Buck, buck_components_mri$random, .after="buck_components_mri$trend")

# Plain Radiography
buck_ts_pr <- ts(as.numeric(Buck$`Plain Radiography`), frequency=12, start=c(2012, 4))
buck_ts_pr
plot.ts(buck_ts_pr)

log_buck_ts_pr<- log(buck_ts_pr)
plot.ts(log_buck_ts_pr)
buck_components_pr <- decompose(log_buck_ts_pr)

plot(buck_components_pr)
buck_pr_seasonally_adjusted <- log_buck_ts_pr - buck_components_pr$seasonal
plot(buck_pr_seasonally_adjusted)

Buck=add_column(Buck, buck_components_pr$seasonal, .after="Plain Radiography")
Buck = add_column(Buck, buck_components_pr$trend, .after="buck_components_pr$seasonal")
Buck = add_column(Buck, buck_components_pr$random, .after="buck_components_pr$trend")

names(Buck)[3] <- "CAT Seasonality"
names(Buck)[4] <- "CAT Trend"
names(Buck)[5] <- "CAT Random"
names(Buck)[7] <- "DU Seasonality"
names(Buck)[8] <- "DU Trend"
names(Buck)[9] <- "DU Random"
names(Buck)[11] <- "Fluo Seasonality"
names(Buck)[12] <- "Fluo Trend"
names(Buck)[13] <- "Fluo Random"
names(Buck)[15] <- "MRI Seasonality"
names(Buck)[16] <- "MRI Trend"
names(Buck)[17] <- "MRI Random"
names(Buck)[19] <- "PR Seasonality"
names(Buck)[20] <- "PR Trend"
names(Buck)[21] <- "PR Random"


# write_xlsx(Buck, path = "C:/Users/liry9/Desktop/Buck.xlsx") #salva in excel


################################################
#### Royal Devon and Exeter NHS Foundation Trust
Royal <- sqldf("SELECT *
                   FROM new12_19
                   WHERE new12_19.'Provider Name'= 'Royal Devon and Exeter NHS Foundation Trust'")

# Computerized Axial Tomography
royal_ts_cat <- ts(as.numeric(Royal$`Computerized Axial Tomography`), frequency=12, start=c(2012, 4))
royal_ts_cat
plot.ts(royal_ts_cat)

log_royal_ts_cat<- log(royal_ts_cat)
plot.ts(log_royal_ts_cat)
royal_components_cat <- decompose(log_royal_ts_cat)

plot(royal_components_cat)
royal_cat_seasonally_adjusted <- log_royal_ts_cat - royal_components_cat$seasonal
plot(royal_cat_seasonally_adjusted)

Royal=add_column(Royal, royal_components_cat$seasonal, .after="Computerized Axial Tomography")
Royal = add_column(Royal, royal_components_cat$trend, .after="royal_components_cat$seasonal")
Royal = add_column(Royal, royal_components_cat$random, .after="royal_components_cat$trend")

# Diagnostic Ultrasonography
royal_ts_du <- ts(as.numeric(Royal$`Diagnostic Ultrasonography`), frequency=12, start=c(2012, 4))
royal_ts_du
plot.ts(royal_ts_du)

log_royal_ts_du<- log(royal_ts_du)
plot.ts(log_royal_ts_du)
royal_components_du <- decompose(log_royal_ts_du)

plot(royal_components_du)
royal_du_seasonally_adjusted <- log_royal_ts_du - royal_components_du$seasonal
plot(royal_du_seasonally_adjusted)

Royal=add_column(Royal, royal_components_du$seasonal, .after="Diagnostic Ultrasonography")
Royal = add_column(Royal, royal_components_du$trend, .after="royal_components_du$seasonal")
Royal = add_column(Royal, royal_components_du$random, .after="royal_components_du$trend")

# Fluoroscopy
royal_ts_fluo <- ts(as.numeric(Royal$Fluoroscopy), frequency=12, start=c(2012, 4))
royal_ts_fluo
plot.ts(royal_ts_fluo)

log_royal_ts_fluo <- log(royal_ts_fluo)
plot.ts(log_royal_ts_fluo)
royal_components_fluo <- decompose(log_royal_ts_fluo)

plot(royal_components_fluo)
royal_fluo_seasonally_adjusted <- log_royal_ts_fluo - royal_components_fluo$seasonal
plot(royal_fluo_seasonally_adjusted)

Royal = add_column(Royal, royal_components_fluo$seasonal, .after="Fluoroscopy")
Royal = add_column(Royal, royal_components_fluo$trend, .after="royal_components_fluo$seasonal")
Royal = add_column(Royal, royal_components_fluo$random, .after="royal_components_fluo$trend")

# Magnetic Resonance Imaging
royal_ts_mri <- ts(as.numeric(Royal$`Magnetic Resonance Imaging`), frequency=12, start=c(2012, 4))
royal_ts_mri
plot.ts(royal_ts_mri)

log_royal_ts_mri<- log(royal_ts_mri)
plot.ts(log_royal_ts_mri)
royal_components_mri <- decompose(log_royal_ts_mri)

plot(royal_components_mri)
buck_mri_seasonally_adjusted <- log_royal_ts_mri - royal_components_mri$seasonal

Royal=add_column(Royal, royal_components_mri$seasonal, .after="Magnetic Resonance Imaging")
Royal = add_column(Royal, royal_components_mri$trend, .after="royal_components_mri$seasonal")
Royal = add_column(Royal, royal_components_mri$random, .after="royal_components_mri$trend")

# Plain Radiography
royal_ts_pr <- ts(as.numeric(Royal$`Plain Radiography`), frequency=12, start=c(2012, 4))
royal_ts_pr
plot.ts(royal_ts_pr)

log_royal_ts_pr<- log(royal_ts_pr)
plot.ts(log_royal_ts_pr)
royal_components_pr <- decompose(log_royal_ts_pr)

plot(royal_components_pr)
royal_pr_seasonally_adjusted <- log_royal_ts_pr - royal_components_pr$seasonal
plot(royal_pr_seasonally_adjusted)


Royal=add_column(Royal, royal_components_pr$seasonal, .after="Plain Radiography")
Royal = add_column(Royal, royal_components_pr$trend, .after="royal_components_pr$seasonal")
Royal = add_column(Royal, royal_components_pr$random, .after="royal_components_pr$trend")

names(Royal)[3] <- "CAT Seasonality"
names(Royal)[4] <- "CAT Trend"
names(Royal)[5] <- "CAT Random"
names(Royal)[7] <- "DU Seasonality"
names(Royal)[8] <- "DU Trend"
names(Royal)[9] <- "DU Random"
names(Royal)[11] <- "Fluo Seasonality"
names(Royal)[12] <- "Fluo Trend"
names(Royal)[13] <- "Fluo Random"
names(Royal)[15] <- "MRI Seasonality"
names(Royal)[16] <- "MRI Trend"
names(Royal)[17] <- "MRI Random"
names(Royal)[19] <- "PR Seasonality"
names(Royal)[20] <- "PR Trend"
names(Royal)[21] <- "PR Random"


# write_xlsx(Royal, path = "C:/Users/liry9/Desktop/Royal.xlsx") #salva in excel


############################################################################################
### Comparison hospitals
#####################################################################################
# NORTHUMBRIA HEALTHCARE NHS FOUNDATION TRUST vs BUCKINGHAMSHIRE HEALTHCARE NHS TRUST
North <- sqldf("SELECT *
                   FROM new12_19
                   WHERE new12_19.'Provider Name'= 'Northumbria Healthcare NHS Foundation Trust'")

# Computerized Axial Tomography
north_ts_cat <- ts(as.numeric(North$`Computerized Axial Tomography`), frequency=12, start=c(2012, 4))
north_ts_cat
plot.ts(north_ts_cat)

log_north_ts_cat<- log(north_ts_cat)
plot.ts(log_north_ts_cat)
north_components_cat <- decompose(log_north_ts_cat)

plot(north_components_cat)
north_cat_seasonally_adjusted <- log_north_ts_cat - north_components_cat$seasonal
plot(north_cat_seasonally_adjusted)

North=add_column(North, north_components_cat$seasonal, .after="Computerized Axial Tomography")
North = add_column(North, north_components_cat$trend, .after="north_components_cat$seasonal")
North = add_column(North, north_components_cat$random, .after="north_components_cat$trend")

# Diagnostic Ultrasonography
north_ts_du <- ts(as.numeric(North$`Diagnostic Ultrasonography`), frequency=12, start=c(2012, 4))
north_ts_du
plot.ts(north_ts_du)

log_north_ts_du<- log(north_ts_du)
plot.ts(log_north_ts_du)
north_components_du <- decompose(log_north_ts_du)

plot(north_components_du)
north_du_seasonally_adjusted <- log_north_ts_du - north_components_du$seasonal
plot(north_du_seasonally_adjusted)

North=add_column(North, north_components_du$seasonal, .after="Diagnostic Ultrasonography")
North = add_column(North, north_components_du$trend, .after="north_components_du$seasonal")
North = add_column(North, north_components_du$random, .after="north_components_du$trend")

# Fluoroscopy
north_ts_fluo <- ts(as.numeric(North$Fluoroscopy), frequency=12, start=c(2012, 4))
north_ts_fluo
plot.ts(north_ts_fluo)

log_north_ts_fluo <- log(north_ts_fluo)
plot.ts(log_north_ts_fluo)
north_components_fluo <- decompose(log_north_ts_fluo)

plot(north_components_fluo)
north_fluo_seasonally_adjusted <- log_north_ts_fluo - north_components_fluo$seasonal
plot(north_fluo_seasonally_adjusted)

North = add_column(North, north_components_fluo$seasonal, .after="Fluoroscopy")
North = add_column(North, north_components_fluo$trend, .after="north_components_fluo$seasonal")
North = add_column(North, north_components_fluo$random, .after="north_components_fluo$trend")

# Magnetic Resonance Imaging
north_ts_mri <- ts(as.numeric(North$`Magnetic Resonance Imaging`), frequency=12, start=c(2012, 4))
north_ts_mri
plot.ts(north_ts_mri)

log_north_ts_mri<- log(north_ts_mri)
plot.ts(log_north_ts_mri)
north_components_mri <- decompose(log_north_ts_mri)

plot(north_components_mri)
north_mri_seasonally_adjusted <- log_north_ts_mri - north_components_mri$seasonal

North=add_column(North, north_components_mri$seasonal, .after="Magnetic Resonance Imaging")
North = add_column(North, north_components_mri$trend, .after="north_components_mri$seasonal")
North = add_column(North, north_components_mri$random, .after="north_components_mri$trend")

# Plain Radiography
north_ts_pr <- ts(as.numeric(North$`Plain Radiography`), frequency=12, start=c(2012, 4))
north_ts_pr
plot.ts(north_ts_pr)

log_north_ts_pr<- log(north_ts_pr)
plot.ts(log_north_ts_pr)
north_components_pr <- decompose(log_north_ts_pr)

plot(north_components_pr)
north_pr_seasonally_adjusted <- log_north_ts_pr - north_components_pr$seasonal
plot(north_pr_seasonally_adjusted)


North=add_column(North, north_components_pr$seasonal, .after="Plain Radiography")
North = add_column(North, north_components_pr$trend, .after="north_components_pr$seasonal")
North = add_column(North, north_components_pr$random, .after="north_components_pr$trend")

names(North)[3] <- "CAT Seasonality"
names(North)[4] <- "CAT Trend"
names(North)[5] <- "CAT Random"
names(North)[7] <- "DU Seasonality"
names(North)[8] <- "DU Trend"
names(North)[9] <- "DU Random"
names(North)[11] <- "Fluo Seasonality"
names(North)[12] <- "Fluo Trend"
names(North)[13] <- "Fluo Random"
names(North)[15] <- "MRI Seasonality"
names(North)[16] <- "MRI Trend"
names(North)[17] <- "MRI Random"
names(North)[19] <- "PR Seasonality"
names(North)[20] <- "PR Trend"
names(North)[21] <- "PR Random"


# write_xlsx(North, path = "C:/Users/liry9/Desktop/North.xlsx") #salva in excel

##########################################################################
# Mid Yorkshire Hospitals NHS Trust vs MEDWAY NHS FOUNDATION TRUST
York <- sqldf("SELECT *
                   FROM new12_19
                   WHERE new12_19.'Provider Name'= 'Mid Yorkshire Hospitals NHS Trust'")

# Computerized Axial Tomography
york_ts_cat <- ts(as.numeric(York$`Computerized Axial Tomography`), frequency=12, start=c(2012, 4))
york_ts_cat
plot.ts(york_ts_cat)

log_york_ts_cat<- log(york_ts_cat)
plot.ts(log_york_ts_cat)
york_components_cat <- decompose(log_york_ts_cat)

plot(york_components_cat)
york_cat_seasonally_adjusted <- log_york_ts_cat - york_components_cat$seasonal
plot(york_cat_seasonally_adjusted)

York=add_column(York, york_components_cat$seasonal, .after="Computerized Axial Tomography")
York = add_column(York, york_components_cat$trend, .after="york_components_cat$seasonal")
York = add_column(York, york_components_cat$random, .after="york_components_cat$trend")

# Diagnostic Ultrasonography
york_ts_du <- ts(as.numeric(York$`Diagnostic Ultrasonography`), frequency=12, start=c(2012, 4))
york_ts_du
plot.ts(york_ts_du)

log_york_ts_du<- log(york_ts_du)
plot.ts(log_york_ts_du)
york_components_du <- decompose(log_york_ts_du)

plot(york_components_du)
york_du_seasonally_adjusted <- log_york_ts_du - york_components_du$seasonal
plot(york_du_seasonally_adjusted)

York=add_column(York, york_components_du$seasonal, .after="Diagnostic Ultrasonography")
York = add_column(York, york_components_du$trend, .after="york_components_du$seasonal")
York = add_column(York, york_components_du$random, .after="york_components_du$trend")

# Fluoroscopy
york_ts_fluo <- ts(as.numeric(York$Fluoroscopy), frequency=12, start=c(2012, 4))
york_ts_fluo
plot.ts(york_ts_fluo)

log_york_ts_fluo <- log(york_ts_fluo)
plot.ts(log_york_ts_fluo)
york_components_fluo <- decompose(log_york_ts_fluo)

plot(york_components_fluo)
york_fluo_seasonally_adjusted <- log_york_ts_fluo - york_components_fluo$seasonal
plot(york_fluo_seasonally_adjusted)

York = add_column(York, york_components_fluo$seasonal, .after="Fluoroscopy")
York = add_column(York, york_components_fluo$trend, .after="york_components_fluo$seasonal")
York = add_column(York, york_components_fluo$random, .after="york_components_fluo$trend")

# Magnetic Resonance Imaging
york_ts_mri <- ts(as.numeric(York$`Magnetic Resonance Imaging`), frequency=12, start=c(2012, 4))
york_ts_mri
plot.ts(york_ts_mri)

log_york_ts_mri<- log(york_ts_mri)
plot.ts(log_york_ts_mri)
york_components_mri <- decompose(log_york_ts_mri)

plot(york_components_mri)
york_mri_seasonally_adjusted <- log_york_ts_mri - york_components_mri$seasonal

York=add_column(York, york_components_mri$seasonal, .after="Magnetic Resonance Imaging")
York = add_column(York, york_components_mri$trend, .after="york_components_mri$seasonal")
York = add_column(York, york_components_mri$random, .after="york_components_mri$trend")

# Plain Radiography
york_ts_pr <- ts(as.numeric(York$`Plain Radiography`), frequency=12, start=c(2012, 4))
york_ts_pr
plot.ts(york_ts_pr)

log_york_ts_pr<- log(york_ts_pr)
plot.ts(log_york_ts_pr)
york_components_pr <- decompose(log_york_ts_pr)

plot(york_components_pr)
york_pr_seasonally_adjusted <- log_york_ts_pr - york_components_pr$seasonal
plot(york_pr_seasonally_adjusted)


York=add_column(York, york_components_pr$seasonal, .after="Plain Radiography")
York = add_column(York, york_components_pr$trend, .after="york_components_pr$seasonal")
York = add_column(York, york_components_pr$random, .after="york_components_pr$trend")

names(York)[3] <- "CAT Seasonality"
names(York)[4] <- "CAT Trend"
names(York)[5] <- "CAT Random"
names(York)[7] <- "DU Seasonality"
names(York)[8] <- "DU Trend"
names(York)[9] <- "DU Random"
names(York)[11] <- "Fluo Seasonality"
names(York)[12] <- "Fluo Trend"
names(York)[13] <- "Fluo Random"
names(York)[15] <- "MRI Seasonality"
names(York)[16] <- "MRI Trend"
names(York)[17] <- "MRI Random"
names(York)[19] <- "PR Seasonality"
names(York)[20] <- "PR Trend"
names(York)[21] <- "PR Random"

# write_xlsx(York, path = "C:/Users/liry9/Desktop/York.xlsx") #salva in excel

##########################################################################
# Airedale NHS Foundation Trust vs HRoyal Devon & Exeter NHS Foun-dation Trust

Air <- sqldf("SELECT *
                   FROM new12_19
                   WHERE new12_19.'Provider Name'= 'Airedale NHS Foundation Trust'")

# Computerized Axial Tomography
air_ts_cat <- ts(as.numeric(Air$`Computerized Axial Tomography`), frequency=12, start=c(2012, 4))
air_ts_cat
plot.ts(air_ts_cat)

log_air_ts_cat<- log(air_ts_cat)
plot.ts(log_air_ts_cat)
air_components_cat <- decompose(log_air_ts_cat)

plot(air_components_cat)
air_cat_seasonally_adjusted <- log_air_ts_cat - air_components_cat$seasonal
plot(air_cat_seasonally_adjusted)

Air=add_column(Air, air_components_cat$seasonal, .after="Computerized Axial Tomography")
Air = add_column(Air, air_components_cat$trend, .after="air_components_cat$seasonal")
Air = add_column(Air, air_components_cat$random, .after="air_components_cat$trend")

# Diagnostic Ultrasonography
air_ts_du <- ts(as.numeric(Air$`Diagnostic Ultrasonography`), frequency=12, start=c(2012, 4))
air_ts_du
plot.ts(air_ts_du)

log_air_ts_du<- log(air_ts_du)
plot.ts(log_air_ts_du)
air_components_du <- decompose(log_air_ts_du)

plot(air_components_du)
air_du_seasonally_adjusted <- log_air_ts_du - air_components_du$seasonal
plot(air_du_seasonally_adjusted)

Air=add_column(Air, air_components_du$seasonal, .after="Diagnostic Ultrasonography")
Air = add_column(Air, air_components_du$trend, .after="air_components_du$seasonal")
Air = add_column(Air, air_components_du$random, .after="air_components_du$trend")

# Fluoroscopy
air_ts_fluo <- ts(as.numeric(Air$Fluoroscopy), frequency=12, start=c(2012, 4))
air_ts_fluo
plot.ts(air_ts_fluo)

log_air_ts_fluo <- log(air_ts_fluo)
plot.ts(log_air_ts_fluo)
air_components_fluo <- decompose(log_air_ts_fluo)

plot(air_components_fluo)
air_fluo_seasonally_adjusted <- log_air_ts_fluo - air_components_fluo$seasonal
plot(air_fluo_seasonally_adjusted)

Air = add_column(Air, air_components_fluo$seasonal, .after="Fluoroscopy")
Air = add_column(Air, air_components_fluo$trend, .after="air_components_fluo$seasonal")
Air = add_column(Air, air_components_fluo$random, .after="air_components_fluo$trend")

# Magnetic Resonance Imaging
air_ts_mri <- ts(as.numeric(Air$`Magnetic Resonance Imaging`), frequency=12, start=c(2012, 4))
air_ts_mri
plot.ts(air_ts_mri)

log_air_ts_mri<- log(air_ts_mri)
plot.ts(log_air_ts_mri)
air_components_mri <- decompose(log_air_ts_mri)

plot(air_components_mri)
air_mri_seasonally_adjusted <- log_air_ts_mri - air_components_mri$seasonal

Air=add_column(Air, air_components_mri$seasonal, .after="Magnetic Resonance Imaging")
Air = add_column(Air, air_components_mri$trend, .after="air_components_mri$seasonal")
Air = add_column(Air, air_components_mri$random, .after="air_components_mri$trend")

# Plain Radiography
air_ts_pr <- ts(as.numeric(Air$`Plain Radiography`), frequency=12, start=c(2012, 4))
air_ts_pr
plot.ts(air_ts_pr)

log_air_ts_pr<- log(air_ts_pr)
plot.ts(log_air_ts_pr)
air_components_pr <- decompose(log_air_ts_pr)

plot(air_components_pr)
air_pr_seasonally_adjusted <- log_air_ts_pr - air_components_pr$seasonal
plot(air_pr_seasonally_adjusted)


Air=add_column(Air, air_components_pr$seasonal, .after="Plain Radiography")
Air = add_column(Air, air_components_pr$trend, .after="air_components_pr$seasonal")
Air = add_column(Air, air_components_pr$random, .after="air_components_pr$trend")

names(Air)[3] <- "CAT Seasonality"
names(Air)[4] <- "CAT Trend"
names(Air)[5] <- "CAT Random"
names(Air)[7] <- "DU Seasonality"
names(Air)[8] <- "DU Trend"
names(Air)[9] <- "DU Random"
names(Air)[11] <- "Fluo Seasonality"
names(Air)[12] <- "Fluo Trend"
names(Air)[13] <- "Fluo Random"
names(Air)[15] <- "MRI Seasonality"
names(Air)[16] <- "MRI Trend"
names(Air)[17] <- "MRI Random"
names(Air)[19] <- "PR Seasonality"
names(Air)[20] <- "PR Trend"
names(Air)[21] <- "PR Random"

# write_xlsx(Air, path = "C:/Users/liry9/Desktop/Air.xlsx") #salva in excel
#####################################################################################################
#deleterows <- which($SITE_NAME== "NOT PROVIDED")
#shmi18_19 <- shmi18_19[-deleterows,]