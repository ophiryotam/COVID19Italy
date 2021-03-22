library(foreign)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(tseries)
library(dynlm)
#mobility data unit of analysis: 
dat2 <- read.csv("new_county_data_media_and_nonmedia.csv")
summary(dat2)
#comm1 - politics and economy\
#comm2 - treatment containtment frame
#comm3 - scientific

# remove all italy from data, store only county level
dat2 <- dplyr::filter(dat2, state == "ALLITA")

#Create variables in correct form
dat2$date <- as.character(dat2$date)
dat2$date <- as.Date(dat2$date,"%m/%d/%Y")
dat2$comm1 <- as.numeric(as.character(dat2$comm1))
dat2$comm2 <- as.numeric(as.character(dat2$comm2))
dat2$comm3 <- as.numeric(as.character(dat2$comm3))
dat2$retail <- as.numeric(as.character(dat2$retail_and_recreation_percent_change_from_baseline))
dat2$grocery <- as.numeric(as.character(dat2$grocery_and_pharmacy_percent_change_from_baseline))
dat2$parks <- as.numeric(as.character(dat2$parks_percent_change_from_baseline))
dat2$transit <- as.numeric(as.character(dat2$transit_stations_percent_change_from_baseline))
dat2$work <- as.numeric(as.character(dat2$workplaces_percent_change_from_baseline))
dat2$res <- as.numeric(as.character(dat2$residential_percent_change_from_baseline))
dat2$volume <- as.numeric(as.character(dat2$volume))
dat2$deaths_day <- as.numeric(as.character(dat2$deaths_per_day))

#########################
### VAR models
#########################
library(vars)
#retail VAR Models
dat2_comm1 <- dplyr::select(dat2, retail, deaths_day, comm1, volume, restrict)
dat2_comm1 <- ts(dat2_comm1)
plot(dat2_comm1)

#Test for stationarity
Acf(dat2_comm1[,"comm1"])
Acf(dat2_comm1[,"retail"])
adf.test(dat2_comm1[,"comm1"])
adf.test(dat2_comm1[,"retail"])
adf.test(diff(dat2_comm1[,"comm1"]))
adf.test(diff(dat2_comm1[,"retail"]))
coint1 <- dynlm(comm1~retail, data=dat2_comm1)
ehat1 <- resid(coint1)
adf.test(ehat1)

#VAR Comm1
fitvarcomm1 <- VAR(dat2_comm1, p=3, type="both")
summary(fitvarcomm1) # comm1.l2 128
causality(fitvarcomm1, cause = "comm1")
causality(fitvarcomm1, cause = "deaths_day")
irf_comm1_retail <- irf(fitvarcomm1, impulse = "comm1", response = "retail", boot = TRUE)
plot(irf_comm1_retail)
plot(fevd(fitvarcomm1))

#Comm2
dat2_comm2 <- dplyr::select(dat2, retail, deaths_day, comm2, volume, restrict)
dat2_comm2 <- ts(dat2_comm2)
plot(dat2_comm2)

#Test for stationarity
Acf(dat2_comm2[,"comm2"])
Acf(dat2_comm2[,"retail"])
adf.test(dat2_comm2[,"comm2"])
adf.test(dat2_comm2[,"retail"])
adf.test(diff(dat2_comm2[,"comm2"]))
adf.test(diff(dat2_comm2[,"retail"]))
coint2 <- dynlm(comm2~retail, data=dat2_comm2)
ehat2 <- resid(coint2)
adf.test(ehat2)

#VAR Comm2
fitvarcomm2 <-  VAR(dat2_comm2, p=3, type="both")
summary(fitvarcomm2) #nothing
causality(fitvarcomm2, cause = "comm2")
causality(fitvarcomm2, cause = "deaths_day")
irf_comm2_retail <- irf(fitvarcomm2, impulse = "comm2", response = "retail", boot = TRUE)
plot(irf_comm2_retail)

#Comm3
dat2_comm3 <- dplyr::select(dat2, retail, deaths_day, comm3, volume, restrict)
dat2_comm3 <- ts(dat2_comm3)
plot(dat2_comm3)

#Test for stationarity
Acf(dat2_comm3[,"comm3"])
Acf(dat2_comm3[,"retail"])
adf.test(dat2_comm3[,"comm3"])
adf.test(dat2_comm3[,"retail"])
adf.test(diff(dat2_comm3[,"comm3"]))
adf.test(diff(dat2_comm3[,"retail"]))
coint3 <- dynlm(comm3~retail, data=dat2_comm3)
ehat3 <- resid(coint3)
adf.test(ehat3)

#VAR Comm3
fitvarcomm3 <-  VAR(dat2_comm3, p=3, type="both")
summary(fitvarcomm3) # comm3.l2 -74
causality(fitvarcomm3, cause = "comm3")
causality(fitvarcomm3, cause = "deaths_day")
irf_comm3_retail <- irf(fitvarcomm3, impulse = "comm3", response = "retail", boot = TRUE)
plot(irf_comm3_retail)



#grocery VAR models
dat2_comm1.2 <- dplyr::select(dat2,grocery, deaths_day, comm1, volume, restrict)
dat2_comm1.2 <- ts(dat2_comm1.2)
plot(dat2_comm1.2)

#Test for stationarity
Acf(dat2_comm1.2[,"comm1"])
Acf(dat2_comm1.2[,"grocery"])
adf.test(dat2_comm1.2[,"comm1"])
adf.test(dat2_comm1.2[,"grocery"])
adf.test(diff(dat2_comm1.2[,"comm1"]))
adf.test(diff(dat2_comm1.2[,"grocery"]))
coint1.2 <- dynlm(comm1~grocery, data=dat2_comm1.2)
ehat1.2 <- resid(coint1.2)
adf.test(ehat1.2)

#VAR Comm1
fitvarcomm1.2 <- VAR(dat2_comm1.2, p=3, type="both")
summary(fitvarcomm1.2) #nothing
causality(fitvarcomm1.2, cause = "comm1")
causality(fitvarcomm1.2, cause = "deaths_day")
irf_comm1_groc <- irf(fitvarcomm1.2, impulse = "comm1", response = "grocery", boot = TRUE)
plot(irf_comm1_groc)

#Comm2
dat2_comm2.2 <- dplyr::select(dat2, grocery, deaths_day, comm2, volume, restrict)
dat2_comm2.2 <- ts(dat2_comm2.2)
plot(dat2_comm2.2)

#Test for stationarity
Acf(dat2_comm2.2[,"comm2"])
Acf(dat2_comm2.2[,"grocery"])
adf.test(dat2_comm2.2[,"comm2"])
adf.test(dat2_comm2.2[,"grocery"])
adf.test(diff(dat2_comm2.2[,"comm2"]))
adf.test(diff(dat2_comm2.2[,"grocery"]))
coint2.2 <- dynlm(comm2~grocery, data=dat2_comm2.2)
ehat2.2 <- resid(coint2.2)
adf.test(ehat2.2)

#VAR Comm2
fitvarcomm2.2 <-  VAR(dat2_comm2.2, p=3, type="both")
summary(fitvarcomm2.2) #nothing
causality(fitvarcomm2.2, cause = "comm2")
causality(fitvarcomm2.2, cause = "deaths_day")
irf_comm2_groc <- irf(fitvarcomm2.2, impulse = "comm2", response = "grocery", boot = TRUE)
plot(irf_comm2_groc)

#Comm3
dat2_comm3.2 <- dplyr::select(dat2, grocery, deaths_day, comm3, volume, restrict)
dat2_comm3.2 <- ts(dat2_comm3.2)
plot(dat2_comm3.2)

#Test for stationarity
Acf(dat2_comm3.2[,"comm3"])
Acf(dat2_comm3.2[,"grocery"])
adf.test(dat2_comm3.2[,"comm3"])
adf.test(dat2_comm3.2[,"grocery"])
adf.test(diff(dat2_comm3.2[,"comm3"]))
adf.test(diff(dat2_comm3.2[,"grocery"]))
coint3.2 <- dynlm(comm3~grocery, data=dat2_comm3.2)
ehat3.2 <- resid(coint3.2)
adf.test(ehat3.2)

#VAR Comm3
fitvarcomm3.2 <-  VAR(dat2_comm3.2, p=3, type="both")
summary(fitvarcomm3.2) #nothing
causality(fitvarcomm3.2, cause = "comm3")
causality(fitvarcomm3.2, cause = "deaths_day")
irf_comm3_groc <- irf(fitvarcomm3.2, impulse = "comm3", response = "grocery", boot = TRUE)
plot(irf_comm3_groc)


#parks VAR models
dat2_comm1.3 <- dplyr::select(dat2, parks, deaths_day, comm1, volume, restrict)
dat2_comm1.3 <- ts(dat2_comm1.3)
plot(dat2_comm1.3)

#Test for stationarity
Acf(dat2_comm1.3[,"comm1"])
Acf(dat2_comm1.3[,"parks"])
adf.test(dat2_comm1.3[,"comm1"])
adf.test(dat2_comm1.3[,"parks"])
adf.test(diff(dat2_comm1.3[,"comm1"]))
adf.test(diff(dat2_comm1.3[,"parks"]))
coint1.3 <- dynlm(comm1~parks, data=dat2_comm1.3)
ehat1.3 <- resid(coint1.3)
adf.test(ehat1.3)

#VAR Comm1
fitvarcomm1.3 <- VAR(dat2_comm1.3, p=3, type="both")
summary(fitvarcomm1.3) # comm1.l2 157
causality(fitvarcomm1.3, cause = "comm1")
causality(fitvarcomm1.3, cause = "deaths_day")
irf_comm1_parks <- irf(fitvarcomm1.3, impulse = "comm1", response = "parks", boot = TRUE)
plot(irf_comm1_parks)


#Comm2
dat2_comm2.3 <- dplyr::select(dat2, parks, deaths_day, comm2, volume, restrict)
dat2_comm2.3 <- ts(dat2_comm2.3)
plot(dat2_comm2.3)

#Test for stationarity
Acf(dat2_comm2.3[,"comm2"])
Acf(dat2_comm2.3[,"parks"])
adf.test(dat2_comm2.3[,"comm2"])
adf.test(dat2_comm2.3[,"parks"])
adf.test(diff(dat2_comm2.3[,"comm2"]))
adf.test(diff(dat2_comm2.3[,"parks"]))
coint2.3 <- dynlm(comm2~parks, data=dat2_comm2.3)
ehat2.3 <- resid(coint2.3)
adf.test(ehat2.3)

#VAR Comm2
fitvarcomm2.3 <-  VAR(dat2_comm2.3, p=3, type="both")
summary(fitvarcomm2.3) # comm2.l2 -114
causality(fitvarcomm2.3, cause = "comm2")
causality(fitvarcomm2.3, cause = "deaths_day")
irf_comm2_parks <- irf(fitvarcomm2.3, impulse = "comm2", response = "parks", boot = TRUE)
plot(irf_comm2_parks)

#Comm3
dat2_comm3.3 <- dplyr::select(dat2, parks, deaths_day, comm3, volume, restrict)
dat2_comm3.3 <- ts(dat2_comm3.3)
plot(dat2_comm3.3)

#Test for stationarity
Acf(dat2_comm3.3[,"comm3"])
Acf(dat2_comm3.3[,"parks"])
adf.test(dat2_comm3.3[,"comm3"])
adf.test(dat2_comm3.3[,"parks"])
adf.test(diff(dat2_comm3.3[,"comm3"]))
adf.test(diff(dat2_comm3.3[,"parks"]))
coint3.3 <- dynlm(comm3~parks, data=dat2_comm3.3)
ehat3.3 <- resid(coint3.3)
adf.test(ehat3.3)

#VAR Comm3
fitvarcomm3.3 <-  VAR(dat2_comm3.3, p=3, type="both")
summary(fitvarcomm3.3) # comm3.l2 -43
causality(fitvarcomm3.3, cause = "comm3")
causality(fitvarcomm3.3, cause = "deaths_day")
irf_comm3_parks <- irf(fitvarcomm3.3, impulse = "comm3", response = "parks", boot = TRUE)
plot(irf_comm3_parks)


#transit VAR models
dat2_comm1.4 <- dplyr::select(dat2, transit, deaths_day, comm1, volume, restrict)
dat2_comm1.4 <- ts(dat2_comm1.4)
plot(dat2_comm1.4)
#Test for stationarity
Acf(dat2_comm1.4[,"comm1"])
Acf(dat2_comm1.4[,"transit"])
adf.test(dat2_comm1.4[,"comm1"])
adf.test(dat2_comm1.4[,"transit"])
adf.test(diff(dat2_comm1.4[,"comm1"]))
adf.test(diff(dat2_comm1.4[,"transit"]))
coint1.4 <- dynlm(comm1~transit, data=dat2_comm1.4)
ehat1.4 <- resid(coint1.4)
adf.test(ehat1.4)

#VAR Comm1
fitvarcomm1.4 <- VAR(dat2_comm1.4, p=3, type="both")
summary(fitvarcomm1.4) # comm1.l2 67
causality(fitvarcomm1.4, cause = "comm1")
causality(fitvarcomm1.4, cause = "deaths_day")
irf_comm1_transit <- irf(fitvarcomm1.4, impulse = "comm1", response = "transit", boot = TRUE)
plot(irf_comm1_transit)

#Comm2
dat2_comm2.4 <- dplyr::select(dat2,  transit, deaths_day,  comm2, volume, restrict)
dat2_comm2.4 <- ts(dat2_comm2.4)
plot(dat2_comm2.4)
#Test for stationarity
Acf(dat2_comm2.4[,"comm2"])
Acf(dat2_comm2.4[,"transit"])
adf.test(dat2_comm2.4[,"comm2"])
adf.test(dat2_comm2.4[,"transit"])
adf.test(diff(dat2_comm2.4[,"comm2"]))
adf.test(diff(dat2_comm2.4[,"transit"]))
coint2.4 <- dynlm(comm2~transit, data=dat2_comm2.4)
ehat2.4 <- resid(coint2.4)
adf.test(ehat2.4)

#VAR Comm2
fitvarcomm2.4 <-  VAR(dat2_comm2.4, p=3, type="both")
summary(fitvarcomm2.4) #nothing
causality(fitvarcomm2.4, cause = "comm2")
causality(fitvarcomm2.4, cause = "deaths_day")
irf_comm2_transit <- irf(fitvarcomm2.4, impulse = "comm2", response = "transit", boot = TRUE)
plot(irf_comm2_transit)

#Comm3
dat2_comm3.4 <- dplyr::select(dat2,  transit, deaths_day, comm3, volume, restrict)
dat2_comm3.4 <- ts(dat2_comm3.4)
plot(dat2_comm3.4)

#Test for stationarity
Acf(dat2_comm3.4[,"comm3"])
Acf(dat2_comm3.4[,"transit"])
adf.test(dat2_comm3.4[,"comm3"])
adf.test(dat2_comm3.4[,"transit"])
adf.test(diff(dat2_comm3.4[,"comm3"]))
adf.test(diff(dat2_comm3.4[,"transit"]))
coint3.4 <- dynlm(comm3~transit, data=dat2_comm3.4)
ehat3.4 <- resid(coint3.4)
adf.test(ehat3.4)

#VAR Comm3
fitvarcomm3.4 <-  VAR(dat2_comm3.4, p=3, type="both")
summary(fitvarcomm3.4) # nothing
causality(fitvarcomm3.4, cause = "comm3")
irf_comm3_transit <- irf(fitvarcomm3.4, impulse = "comm3", response = "transit", boot = TRUE)
plot(irf_comm3_transit)

#Work VAR models
dat2_comm1.5 <- dplyr::select(dat2, work, deaths_day, comm1, volume, restrict)
dat2_comm1.5 <- ts(dat2_comm1.5)
plot(dat2_comm1.5)

#Test for stationarity
Acf(dat2_comm1.5[,"comm1"])
Acf(dat2_comm1.5[,"work"])
adf.test(dat2_comm1.5[,"comm1"])
adf.test(dat2_comm1.5[,"work"])
adf.test(diff(dat2_comm1.5[,"comm1"]))
adf.test(diff(dat2_comm1.5[,"work"]))
coint1.5 <- dynlm(comm1~work, data=dat2_comm1.5)
ehat1.5 <- resid(coint1.5)
adf.test(ehat1.5)

#VAR Comm1
fitvarcomm1.5 <- VAR(dat2_comm1.5, p=3, type="both")
summary(fitvarcomm1.5) #nothing
causality(fitvarcomm1.5, cause = "comm1")
causality(fitvarcomm1.5, cause = "deaths_day")
irf_comm1_work <- irf(fitvarcomm1.5, impulse = "comm1", response = "work", boot = TRUE)
plot(irf_comm1_transit)

#Comm2
dat2_comm2.5 <- dplyr::select(dat2,  work, deaths_day, comm2, volume, restrict)
dat2_comm2.5 <- ts(dat2_comm2.5)
plot(dat2_comm2.5)

#Test for stationarity
Acf(dat2_comm2.5[,"comm2"])
Acf(dat2_comm2.5[,"work"])
adf.test(dat2_comm2.5[,"comm2"])
adf.test(dat2_comm2.5[,"work"])
adf.test(diff(dat2_comm2.5[,"comm2"]))
adf.test(diff(dat2_comm2.5[,"work"]))
coint2.5 <- dynlm(comm2~work, data=dat2_comm2.5)
ehat2.5 <- resid(coint2.5)
adf.test(ehat2.5)

#VAR Comm2
fitvarcomm2.5 <-  VAR(dat2_comm2.5, p=3, type="both")
summary(fitvarcomm2.5) #nothing
causality(fitvarcomm2.5, cause = "comm2")
irf_comm2_work <- irf(fitvarcomm2.5, impulse = "comm2", response = "work", boot = TRUE)
plot(irf_comm2_transit)

#Comm3
dat2_comm3.5 <- dplyr::select(dat2,  work, deaths_day, comm3, volume, restrict)
dat2_comm3.5 <- ts(dat2_comm3.5)
plot(dat2_comm3.5)
#Test for stationarity
Acf(dat2_comm3.5[,"comm3"])
Acf(dat2_comm3.5[,"work"])
adf.test(dat2_comm3.5[,"comm3"])
adf.test(dat2_comm3.5[,"work"])
adf.test(diff(dat2_comm3.5[,"comm3"]))
adf.test(diff(dat2_comm3.5[,"work"]))
coint3.5 <- dynlm(comm3~work, data=dat2_comm3.5)
ehat3.5 <- resid(coint3.5)
adf.test(ehat3.5)

#VAR Comm3
fitvarcomm3.5 <-  VAR(dat2_comm3.5, p=3, type="both")
summary(fitvarcomm3.5) #nothing
causality(fitvarcomm3.5, cause = "comm3")
irf_comm3_work <- irf(fitvarcomm3.5, impulse = "comm3", response = "work", boot = TRUE)
plot(irf_comm3_transit)

#residential VAR models
dat2_comm1.6 <- dplyr::select(dat2, res, deaths_day, comm1, volume, restrict)
dat2_comm1.6 <- ts(dat2_comm1.6)
plot(dat2_comm1.6)

#Test for stationarity
Acf(dat2_comm1.6[,"comm1"])
Acf(dat2_comm1.6[,"res"])
adf.test(dat2_comm1.6[,"comm1"])
adf.test(dat2_comm1.6[,"res"])
adf.test(diff(dat2_comm1.6[,"comm1"]))
adf.test(diff(dat2_comm1.6[,"res"]))
coint1.6 <- dynlm(comm1~res, data=dat2_comm1.6)
ehat1.6 <- resid(coint1.6)
adf.test(ehat1.6)

#VAR Comm1
fitvarcomm1.6 <- VAR(dat2_comm1.6, p=3, type="both")
summary(fitvarcomm1.6) # comm1.l1 -44, 
causality(fitvarcomm1.6, cause = "comm1")
causality(fitvarcomm1.6, cause = "deaths_day")
irf_comm1_res <- irf(fitvarcomm1.6, impulse = "comm1", response = "res", boot = TRUE)
plot(irf_comm1_res)

#Comm2
dat2_comm2.6 <- dplyr::select(dat2,  res, deaths_day, comm2, volume, restrict)
dat2_comm2.6 <- ts(dat2_comm2.6)
plot(dat2_comm2.6)

#Test for stationarity
Acf(dat2_comm2.6[,"comm2"])
Acf(dat2_comm2.6[,"res"])
adf.test(dat2_comm2.6[,"comm2"])
adf.test(dat2_comm2.6[,"res"])
adf.test(diff(dat2_comm2.6[,"comm2"]))
adf.test(diff(dat2_comm2.6[,"res"]))
coint2.6 <- dynlm(comm2~res, data=dat2_comm2.6)
ehat2.6 <- resid(coint2.6)
adf.test(ehat2.6)

#VAR COmm2
fitvarcomm2.6 <-  VAR(dat2_comm2.6, p=3, type="both")
summary(fitvarcomm2.6) # comm2.l1 34.24
causality(fitvarcomm2.6, cause = "comm2")
causality(fitvarcomm2.6, cause = "deaths_day")
irf_comm2_res <- irf(fitvarcomm2.6, impulse = "comm2", response = "res", boot = TRUE)
plot(irf_comm2_res)

#Comm3
dat2_comm3.6 <- dplyr::select(dat2,  res, deaths_day, comm3, volume, restrict)
dat2_comm3.6 <- ts(dat2_comm3.6)
plot(dat2_comm3.6)

#Test for stationarity
Acf(dat2_comm3.6[,"comm3"])
Acf(dat2_comm3.6[,"res"])
adf.test(dat2_comm3.6[,"comm3"])
adf.test(dat2_comm3.6[,"res"])
adf.test(diff(dat2_comm3.6[,"comm3"]))
adf.test(diff(dat2_comm3.6[,"res"]))
coint3.6 <- dynlm(comm3~res, data=dat2_comm3.6)
ehat3.6 <- resid(coint3.6)
adf.test(ehat3.6)

#VAR Comm3
fitvarcomm3.6 <-  VAR(dat2_comm3.6, p=3, type="both")
summary(fitvarcomm3.6) # comm2.l1 34.24
causality(fitvarcomm3.6, cause = "comm3")
causality(fitvarcomm3.6, cause = "deaths_day")
irf_comm3_res <- irf(fitvarcomm3.6, impulse = "comm3", response = "res", boot = TRUE)
plot(irf_comm3_res)

#retrieve info from main models for tables
varcomm1G <- fitvarcomm1[["varresult"]]$retail
varcomm1.2G <- fitvarcomm1.2[["varresult"]]$grocery
varcomm1.3G <- fitvarcomm1.3[["varresult"]]$parks
varcomm1.4G <- fitvarcomm1.4[["varresult"]]$transit
varcomm1.5G <- fitvarcomm1.5[["varresult"]]$work
varcomm1.6G <- fitvarcomm1.6[["varresult"]]$res

library(stargazer)
stargazer(varcomm1G, varcomm1.2G, varcomm1.3G, varcomm1.4G, varcomm1.5G, varcomm1.6G, align = T, df = F,
          type = "html", digits = 2, out = "VAR_Table1.html")


varcomm2G <- fitvarcomm2[["varresult"]]$retail
varcomm2.2G <- fitvarcomm2.2[["varresult"]]$grocery
varcomm2.3G <- fitvarcomm2.3[["varresult"]]$parks
varcomm2.4G <- fitvarcomm2.4[["varresult"]]$transit
varcomm2.5G <- fitvarcomm2.5[["varresult"]]$work
varcomm2.6G <- fitvarcomm2.6[["varresult"]]$res


stargazer(varcomm2G, varcomm2.2G, varcomm2.3G, varcomm2.4G, varcomm2.5G, varcomm2.6G, align = T, df = F,
          type = "html", digits = 2, out = "VAR_Table2.html")

varcomm3G <- fitvarcomm3[["varresult"]]$retail
varcomm3.2G <- fitvarcomm3.2[["varresult"]]$grocery
varcomm3.3G <- fitvarcomm3.3[["varresult"]]$parks
varcomm3.4G <- fitvarcomm3.4[["varresult"]]$transit
varcomm3.5G <- fitvarcomm3.5[["varresult"]]$work
varcomm3.6G <- fitvarcomm3.6[["varresult"]]$res


stargazer(varcomm3G, varcomm3.2G, varcomm3.3G, varcomm3.4G, varcomm3.5G, varcomm3.6G, align = T, df = F,
          type = "html", digits = 2, out = "VAR_Table3.html")



#create function to retrieve IRF model info
getIRFPlotData <- function(impulse, response, list) {
  cbind.data.frame(Comm = 0:(nrow(list[[1]][[1]])-1),
                   Lower = list[[2]][names(list[[2]]) == impulse][[1]] %>% as.data.frame() %>% dplyr::select_(response) %>% pull(1),
                   irf = list[[1]][names(list[[1]]) == impulse][[1]] %>% as.data.frame() %>% dplyr::select_(response) %>% pull(1),
                   Upper = list[[3]][names(list[[3]]) == impulse][[1]] %>% as.data.frame() %>% dplyr::select_(response) %>% pull(1),
                   Impulse = impulse,
                   Response = response, stringsAsFactors = FALSE)
}

library(ggplot2)
retail1 <- getIRFPlotData("comm1", "retail", irf_comm1_retail)
grocery1 <- getIRFPlotData("comm1", "grocery", irf_comm1_groc)
parks1 <- getIRFPlotData("comm1", "parks", irf_comm1_parks)
transit1 <-  getIRFPlotData("comm1", "transit", irf_comm1_transit)
work1 <-  getIRFPlotData("comm1", "work", irf_comm1_work)
residential1 <- getIRFPlotData("comm1", "res", irf_comm1_res)

retail2 <- getIRFPlotData("comm2", "retail", irf_comm2_retail)
grocery2 <- getIRFPlotData("comm2", "grocery", irf_comm2_groc)
parks2 <- getIRFPlotData("comm2", "parks", irf_comm2_parks)
transit2 <-  getIRFPlotData("comm2", "transit", irf_comm2_transit)
work2 <-  getIRFPlotData("comm2", "work", irf_comm2_work)
residential2 <- getIRFPlotData("comm2", "res", irf_comm2_res)


retail3 <- getIRFPlotData("comm3", "retail", irf_comm3_retail)
grocery3 <- getIRFPlotData("comm3", "grocery", irf_comm3_groc)
parks3 <- getIRFPlotData("comm3", "parks", irf_comm3_parks)
transit3 <-  getIRFPlotData("comm3", "transit", irf_comm3_transit)
work3 <-  getIRFPlotData("comm3", "work", irf_comm3_work)
residential3 <- getIRFPlotData("comm3", "res", irf_comm3_res)


#Create singular data for all plots for ggplot plotting
plot_all <- rbind(retail1, grocery1, parks1, transit1, work1, residential1, retail2, grocery2, parks2, transit2, work2, residential2,
                  retail3, grocery3, parks3, transit3, work3, residential3)
names(plot_all)
nrow(plot_all)
plot_all <- plot_all %>% mutate(group = paste0(Impulse, Response))
plot_all_comm1 <- plot_all[1:66,]
plot_all_comm2 <- plot_all[67:132,]
plot_all_comm3 <- plot_all[133:198,]
plot_all_long <- gather(plot_all, "key", "n", 2:4) %>% mutate(group = paste0(Impulse, Response))

names(plot_all)
library(ggplot2)

# Main plot for IRF of Comm1
a <- ggplot(plot_all_comm1, aes(x = Comm, y = irf)) 
a + geom_line(aes(x = Comm, y = irf), color = "red2", size = 1.5) + 
  geom_line(aes(x = Comm, y = Upper) , linetype = "dashed")+ 
  geom_line(aes(x = Comm, y = Lower), linetype = "dashed")+ 
  geom_hline(aes(yintercept=0), linetype = "solid", color = "blue2") +
  facet_grid(.~ Response) + labs(x = "Social Frame") +  theme_bw()
ggsave("Comm1_IRF.jpg")


# Main plot for IRF of Comm2
b <- ggplot(plot_all_comm2, aes(x = Comm, y = irf)) 
b + geom_line(aes(x = Comm, y = irf), color = "red2", size = 1.5) + 
  geom_line(aes(x = Comm, y = Upper) , linetype = "dashed")+ 
  geom_line(aes(x = Comm, y = Lower), linetype = "dashed")+ 
  geom_hline(aes(yintercept=0), linetype = "solid", color = "blue2") +
  facet_grid(.~ Response) + labs(x = "Pandemic Frame") + theme_bw()
ggsave("Comm2_IRF.jpg")

# Main plot for IRF of Comm3
c <- ggplot(plot_all_comm3, aes(x = Comm, y = irf)) 
c + geom_line(aes(x = Comm, y = irf), color = "red2", size = 1.5) + 
  geom_line(aes(x = Comm, y = Upper) , linetype = "dashed")+ 
  geom_line(aes(x = Comm, y = Lower), linetype = "dashed")+ 
  geom_hline(aes(yintercept=0), linetype = "solid", color = "blue2") +
  facet_grid(.~ Response) + labs(x = "Scientific Frame") + theme_bw()
ggsave("Comm3_IRF.jpg")
