cat(as.character(Sys.time()), "==","Script Download NPHCDA weekly data for state started successfully\n")

library("httr",quietly = TRUE,warn.conflicts = FALSE)
library("rjson",quietly = TRUE,warn.conflicts = FALSE)
library("plyr",quietly = TRUE,warn.conflicts = FALSE)
library("dplyr",quietly = TRUE,warn.conflicts = FALSE)
library("XML",quietly = TRUE,warn.conflicts = FALSE)
library("RCurl",quietly = TRUE,warn.conflicts = FALSE)
library("tibble", quietly = TRUE, warn.conflicts = FALSE)
first_time <- "yes"

config <- config::get()
username <- config$NPHCDAInstanceUsername
password <- config$NPHCDAInstancePassword
server_version <- config$ServerVersion

if(server_version == "yes") {
  setwd("data")
  source("/srv/shiny-server/e4e-apps/functions/collect_return_period_fxn2.R")
} else if (server_version == "no") {
  setwd("~/R_projects/NHMIS_Monthly_Analysis/data/")
  source("~/R_projects/R programming/DHIS2 data extract/Functions/collect_return_period_fxn2.R")
}
files <- list.files()
no_of_files <- length(files) 

timeout <-600

combined_file_name <- "combined.csv"
new_file_name <- "new.csv"
ind_file_name <- paste0("NPHCDA_Weekly_State_Level","_",(no_of_files + 1),".csv")

ou <- c("s5DPBsdoE8b;LEVEL-2")
dx <- c("Fffl0c9Slo9;aK28v12UgVk.ACTUAL_REPORTS;PGTLyinSaJq;tMdqWN1sTPC;waef1oV31X8;aK28v12UgVk.REPORTING_RATE;xyNYVhzkxlK.REPORTING_RATE;xyNYVhzkxlK.ACTUAL_REPORTS;l7jWN4lOwTn;iEEbvfmCSFX;ZL1hNMypqNB;lteesyAZMei;f4pgzBfPdUw;Zz9DUkrpY40;PBmj2jQ3GuD;HST6pIQ4Bny;Xwm4UxaPsQ4;bnMt44mYOwT;NqCUY7ZClhb;yI6H2LI9EjL;Tszlne4d6eR;Pj8Z2vHypGZ;cIRnZ0GVVDM;waFuEU4u0gS;vX1CfE2Sb4R;hdH92lJKkmo;cE2VNBWGMiK;tPGphFDxKGu")


if(first_time == "yes") {
  pe <- collect_return_period2(sDate = "2017-1-1", eDate = Sys.Date(), oneYeartoOmit = 2018, omitMultipleYears = FALSE, extraction_type = "NPHCDA_WEEKLY")[2]
} else if (first_time == "no") {
  pe <- "LAST_WEEK"
}


dp <- "NAME"
oIS <- "NAME"
tl <- "true"
cols <- "dx"
rows <- "pe;ou"
ft <- ""
agg <- ""
mc <- ""
pamc <- ""
sm <- "true"
sd <- "false"
sr <- "true"
hm <- "false"
il <- "true"
her <- "true"
sh <- "false"
ind <- "false"
iis <- ""
al <- ""
rpd <- ""
uou <- ""

analytics_url <- paste0("http://172.104.147.136:8087/api/26/analytics.json?",
                        if(dx != "") {paste0("&dimension=dx:",dx)},
                        if(ou != "") {paste0("&dimension=ou:",ou)},
                        if(pe != "") {paste0("&dimension=pe:",pe)},
                        if(ft != "") {paste0("&filter=",ft)},
                        if(agg != "") {paste0("&aggregationType=",agg)},
                        if(mc != "") {paste0("&measureCriteria=",mc)},
                        if(pamc != "") {paste0("&preAggregationMeasureCriteria	=",pamc)},
                        if(sm != "") {paste0("&skipMeta=",sm)},
                        if(sd != "") {paste0("&skipData=",sd)},
                        if(sr != "") {paste0("&skipRounding=",sr)},
                        if(hm != "") {paste0("&hierarchyMeta=",hm)},
                        if(il != "") {paste0("&ignoreLimit=",il)},
                        if(her != "") {paste0("&hideEmptyRows=",her)},
                        if(sh != "") {paste0("&showHierarchy=",sh)},
                        if(ind != "") {paste0("&includeNumDen=",ind)},
                        if(iis != "") {paste0("&inputIdScheme=",iis)},
                        if(al != "") {paste0("&approvalLevel=",al)},
                        if(rpd != "") {paste0("&relativePeriodDate=",rpd)},
                        if(uou != "") {paste0("&userOrgUnit=",uou)},
                        if(dp != "") {paste0("&displayProperty=",dp)},
                        if(oIS != "") {paste0("&outputIdScheme=",oIS)},
                        if(tl != "") {paste0("&tableLayout=",tl)},
                        if(cols != "") {paste0("&columns=",cols)},
                        if(rows != "") {paste0("&rows=",rows)})

url <- URLencode(analytics_url)
r <- httr::GET(url, httr::authenticate(username,password),httr::timeout(timeout))
r <- httr::content(r, "text")
d <- jsonlite::fromJSON(r, flatten=TRUE)
Rdata <- as.data.frame(d$rows,stringsAsFactors=FALSE)
names(Rdata) <- d$headers$name

# Try to modularize this script so that the extraction phase, cleaning phases, analytics phase and saving phase are different functions

if("Organisation unit" %in% colnames(Rdata)) {
  Rdata$State <- gsub(" state","", Rdata$`Organisation unit`, ignore.case = TRUE)
  Rdata$State <- gsub("Federal Government","National",gsub("Federal Capital Territory","FCT", substr(Rdata$State,unlist(lapply(gregexpr(" ", Rdata$State, ignore.case = TRUE), function(k) if(k[[1]] == 3) {k[[1]]} else {k[[2]]}))+1, 100)))
  Rdata$`Organisation unit` <- Rdata$State
  Rdata <- Rdata[,!(names(Rdata) %in% c("State"))]
  if(sum(ifelse(grepl("Local Government Area", Rdata$`Organisation unit`), 1,0)) > 0) {
    Rdata$State <- "Nigeria"
    Rdata$`Organisation unit` <- gsub("Local Government Area","LGA", substr(Rdata$`Organisation unit`, unlist(lapply(gregexpr(" ",Rdata$`Organisation unit`, ignore.case = TRUE),function(k) if(k[[1]] == 3) {k[[1]]} else {k[[2]]}))+1,100))
    Rdata <- plyr::rename(Rdata, c("Organisation unit" = "LGA"))}}
if("ou" %in% colnames(Rdata)) { 
  if(sum(ifelse(grepl("State",Rdata$ou),1,0)) > 0) {
    Rdata$State <- gsub(" state","", Rdata$ou, ignore.case = TRUE)
    Rdata$State <- gsub("Federal Government","National",gsub("Federal Capital Territory","FCT", substr(Rdata$State,unlist(lapply(gregexpr(" ", Rdata$State, ignore.case = TRUE), function(k) if(k[[1]] == 3) {k[[1]]} else {k[[2]]}))+1, 100)))
    Rdata$`Organisation unit` <- Rdata$State}
  if(sum(ifelse(grepl("Local Government Area", Rdata$ou), 1,0)) > 0) {
    Rdata$State <- sheet_name
    Rdata$ou <- gsub("Local Government Area","LGA", substr(Rdata$ou, unlist(lapply(gregexpr(" ",Rdata$ou, ignore.case = TRUE),function(k) if(k[[1]] == 3) {k[[1]]} else {k[[2]]}))+1,100))
    Rdata <- plyr::rename(Rdata, c("ou" = "LGA"))}}
first_cols <- c("Period")
Rdata <- Rdata[,c(first_cols,setdiff(colnames(Rdata),first_cols))]
drops <- c("Period ID", "Period code", "Period description", "Organisation unit ID",
           "Organisation unit code", "Organisation unit description","ou")
Rdata <- Rdata[, !(names(Rdata) %in% drops)]

Rdata <- Rdata[,!(names(Rdata) %in% drops)]
# Convert data to numberic class so as to do arithmetic operations ----
Rdata[4:ncol(Rdata)] <-
  lapply(Rdata[4:ncol(Rdata)], FUN = as.numeric)
TempRdata <- Rdata
TempRdata[is.na(TempRdata)] <- 0
# Calculate derived indicators ====
Rdata$`Total BCG doses opened for fixed and outreach sessions` <-
  TempRdata$`FS_BCG doses opened for session` + TempRdata$`OS_BCG doses opened for session`
Rdata$`Total Number of children given BCG in fixed and outreach sessions` <-
  TempRdata$`FS_Number of children given BCG` + TempRdata$`OS_Number of children given BCG`
Rdata$`Total Measles doses utilized in fixed and outreach sessions` <-
  TempRdata$`OS_Doses opened for session_Measles` + TempRdata$`FS_Doses opened  for  session_Measles`
Rdata$`Total Number of children vaccinated with Measles in fixed and outreach sessions` <-
  TempRdata$`OS_Number vaccinated_Measles` + TempRdata$`FS_Number vaccinated _Measles`
Rdata$`Total Penta doses utilized in fixed and outreach sessions` <-
  TempRdata$`FS_Doses opened for session_Penta` + TempRdata$`OS_Doses opened for session_Penta`
Rdata$`Total Number of children vaccinated with Penta (1, 2 or 3) in outreach sessions` <-
  TempRdata$`OS_Number vaccinated_Penta1` + TempRdata$`OS_Number vaccinated_Penta2` + TempRdata$`OS_Number vaccinated_Penta3`
Rdata$`Total Number of children vaccinated with Penta (1, 2 or 3) in fixed sessions` <-
  TempRdata$`FS_Number vaccinated _Penta 1` + TempRdata$`FS_Number vaccinated _Penta2` + TempRdata$`FS_Number vaccinated _Penta3`
Rdata$`Total Number of children vaccinated with Penta (1, 2 or 3) in fixed and outreach sessions` <-
  Rdata$`Total Number of children vaccinated with Penta (1, 2 or 3) in outreach sessions` + Rdata$`Total Number of children vaccinated with Penta (1, 2 or 3) in fixed sessions`
Rdata$`Total IPV doses utilized in fixed and outreach sessions` <-
  TempRdata$`FS_IPV doses opened for session` + TempRdata$`OS_IPV doses opened for session`
Rdata$`Total Number of children vaccinated with IPV in fixed and outreach sessions` <-
  TempRdata$`FS_Number of children given IPV` + TempRdata$`OS_Number of children given IPV`
# Rename some columns to be more descriptive ----
Rdata <-
  plyr::rename(
    Rdata,
    c(
      "Daily Fixed Sessions Actual reports" = "Daily fixed sessions conducted",
      "Daily Outreach Sessions Actual reports" = "Daily outreach sessions conducted"
    )
  )
Rdata_sort <- Rdata
# Rdata_sort <- Rdata_sort %>% arrange(Period,desc(`Organisation unit`))
chr <- apply(X = Rdata_sort,MARGIN = 2, nchar)[,1]
Rdata_sort$Period[chr == 6] <- gsub(pattern = "W",replacement = "W0",x = Rdata_sort$Period)[chr == 6]
Rdata_sort <- Rdata_sort %>% arrange(Period,`Organisation unit`)

#Finalise the storage ####
RRData <- add_column(.data = Rdata_sort, Date_exported = as.character(Sys.time()), .before = 1)
write.csv(RRData,new_file_name, row.names = FALSE)

newData <- read.csv(file = new_file_name, as.is = TRUE, check.names = FALSE)
if(first_time == "yes") {
  write.csv(as.data.frame(RRData),combined_file_name, row.names = FALSE)
} else if (first_time == "no") {
  initialCombinedData <- read.csv(file = combined_file_name, as.is = TRUE, check.names = FALSE)
  combinedData <- bind_rows(initialCombinedData,newData)
  write.csv(combinedData,combined_file_name, row.names = FALSE)
}

write.csv(as.data.frame(RRData),file = ind_file_name,row.names = FALSE)
setwd("..")
cat(as.character(Sys.time()), "==","Script Download NPHCDA weekly states data completed successfully\n\n----------\n")