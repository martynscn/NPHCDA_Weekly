collect_return_period <- function(useToday,sDate = 2017-1-1, return = "weekly",oneYeartoOmit = "2018", omitMultipleYears = FALSE, subtractMonths = TRUE){
  library(lubridate)
  if(useToday == TRUE) {
    d2 <- Sys.Date()
    if(subtractMonths == TRUE) {
      monthsToSubtract <- 2
    } else {
      monthsToSubtract <- 1
    }
    
    if(missing(sDate) == TRUE) {
      sDate <- readline("Input start date only in this format e.g. 2017-monthNo-dayNo.: ")
    }
    
  } else {
    monthsToSubtract <- 0
    if(missing(sDate) == FALSE) {
      eDate <- readline("Input end date only in this format e.g. 2017-monthNo-dayNo.: ")
    } else {
      fullDate <- readline("Input start and end dates seperated by a comma: ")
      Dates1 <- unlist(strsplit(x = fullDate, split = ","))
      sDate <- Dates1[1]
      eDate <- Dates1[2]
    }

    d2 <- as.Date.character(x = eDate)
  }
  d1 <- as.Date.character(x = sDate)
  dateSeq <- sort(unique(strftime(x = seq(d1,d2,"weeks"), format = "%YW%V")))
  joinedDateSeq <- paste0(... = dateSeq, collapse = ";")
  joinedDateSeqCleaned <- gsub("W0","W",joinedDateSeq)
  

  monthsd2 <- ceiling_date((as.Date(d2) %m-% months(monthsToSubtract)), unit = "month") - days(1)
  monthSeq <- sort(unique(strftime(x = seq(d1, monthsd2, "months"), format = "%Y%m")))
  joinedmonthSeq <- paste0(... = monthSeq, collapse = ";")
  yearSeq <- sort(unique(strftime(x = seq(d1, monthsd2, "years"), format = "%Y")))
  yearstoOmit <- "None"
  if(missing(oneYeartoOmit) == FALSE) {
    yearstoOmit <- oneYeartoOmit
  }
  if(omitMultipleYears == TRUE) {
      yeartoOmitInput <- readline(prompt = "Enter year(s) to omit seperated by comma: ")
      yearstoOmit <- unlist(strsplit(x = yeartoOmitInput, split = ","))
    }
  p <- lapply(X = yearstoOmit, FUN = function(K) {grepl(pattern = K,x = yearSeq, ignore.case = TRUE)})
  k <- as.data.frame(p)
  names(k) <- paste0("Col ", seq(1:length(k)))
  yearlog <- Reduce(`|`, k)
  yearSeq <- yearSeq[!yearlog]

  joinedyearSeq <- paste0(... = yearSeq, collapse = ";")
  myresult <- paste(joinedyearSeq,joinedmonthSeq, sep = ";")
  if(return =="weekly") {
    return(list(joinedDateSeq,joinedDateSeqCleaned))
  } else if (return == "monthly") {
    return(myresult)
  }
}
