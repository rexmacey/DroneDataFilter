# Functions for DroneTagging

# massageData is first pass at choosing which flight observations to ignore.
massageDataPass1 <- function(data, 
                        ignoreNAs = TRUE,
                        ignoreTestChips = TRUE,
                        ignoreDNAChips = TRUE,
                        ignoreDuplicates = TRUE,
                        ignoreMar21 = TRUE,
                        ignoreBadTimeStamps = TRUE,
                        ignoreApr07 = TRUE,
                        convertTimeStamps = TRUE,
                        adjustTimeStamps = TRUE) {
  out <- data %>% mutate(Ignore = FALSE, OrigTimestamp.UTC = Timestamp.UTC, OrigFirstTimeFound = FirstTimeFound,
                         OrigLastTimeFound = LastTimeFound) 
  if (ignoreNAs) {
    out <- out %>% mutate(Ignore = Ignore | (is.na(Timestamp.UTC) | is.na(UID) | is.na(ReaderID)))
  }
  if (ignoreTestChips) {
    out <- out %>% mutate(Ignore = Ignore | (UID == "25SQCMICSBBF0360056C4" |
                                               UID == "25SQCMICSBB49C80056CD"))
  }
  if (ignoreDNAChips) {
    out <- out %>% mutate(Ignore = Ignore |
                            (UID == "25SQCMICSBBCF5D005ADF" |
                               UID == "25SQCMICSBB6B2A005B08" |
                               UID == "25SQCMICSBB8AE2005AE" |
                               UID == "25SQCMICSBB3762005B0" |
                               UID == "25SQCMICSBB0B45005B09" |
                               UID == "25SQCMICSBB0825005A" |
                               UID == "25SQCMICSBB0E68005AB" |
                               UID == "25SQCMICSBB9246005AC" |
                               UID == "25SQCMICSBBAB48005A" |
                               UID == "25SQCMICSBB4A3C00" |
                               UID == "25SQCMICSBBEA8D005AE0" |
                               UID == "25SQCMICSBB6E07005AB" |
                               UID == "25SQCMICSBB684A005AF" |
                               UID == "25SQCMICSBBAED9005AB" |
                               UID == "25SQCMICSBB4D9E005B0" |
                               UID == "25SQCMICSBBC8FB005AF" |
                               UID == "25SQCMICSBB5298005" |
                               UID == "25SQCMICSBB2A53005AE" |
                               UID == "25SQCMICSBB92FE005A" |
                               UID == "25SQCMICSBBF1F5005AC"))
  }
  if (ignoreDuplicates) {
    out <- out %>% mutate(Ignore = Ignore | duplicated(subset(., select = -c(SourceFile))))
  }
  if (ignoreMar21) {out <- out %>% mutate(Ignore = Ignore | (substr(Timestamp.UTC, 1, 7) == "2021-03"))}
  if (ignoreBadTimeStamps) {
    out <- out %>% mutate(Ignore = Ignore | (nchar(Timestamp.UTC) != 23 |
                                               nchar(FirstTimeFound) != 23 |
                                               nchar(LastTimeFound) != 23))
  }
  if (convertTimeStamps | ignoreApr07 | adjustTimeStamps) {
    out <- out %>% mutate(Timestamp.UTC = sapply(Timestamp.UTC, as.POSIXct, tz = "UTC", optional = TRUE),
                          FirstTimeFound = sapply(FirstTimeFound, as.POSIXct, tz = "UTC", optional = TRUE),
                          LastTimeFound = sapply(LastTimeFound, as.POSIXct, tz = "UTC", optional = TRUE)) %>%
      mutate(Timestamp.UTC = as.POSIXct(Timestamp.UTC, tz = "UTC", origin = "1970-01-01"),
             FirstTimeFound = as.POSIXct(FirstTimeFound, tz = "UTC", origin = "1970-01-01"),
             LastTimeFound = as.POSIXct(LastTimeFound, tz = "UTC", origin = "1970-01-01"))
    out <- out %>% mutate(Ignore = Ignore | is.na(Timestamp.UTC) | is.na(FirstTimeFound) | is.na(LastTimeFound))
  }
  if (ignoreApr07) {
    Apr07StartTime <- as.POSIXct("2021-04-07 20:13:42.037", tz = "UTC")
    Apr07EndTime <- as.POSIXct("2021-04-07 20:31:00.927", tz = "UTC")
    out <- out %>% mutate(Ignore = Ignore | (Timestamp.UTC >= Apr07StartTime & Timestamp.UTC <= Apr07EndTime))
  }
  if (adjustTimeStamps) {
    out <- out %>%  mutate(Timestamp.UTC = Timestamp.UTC - hours(4),
                           FirstTimeFound = FirstTimeFound - hours(4),
                           LastTimeFound = LastTimeFound - hours(4))
  }
  return(out)
}

loadDataFiles <- function(computeFlag = FALSE, dataPath) {
  if (computeFlag) { # load raw data
    data1 <- data.table::data.table(readRDS(paste0(dataPath, "data.rds"))$data) %>% select(-c("Collection"))
    assign(data1, data1, envir = globalenv())
    keyfordronetags <- read_excel(paste0(dataPath, "keyfordronetags.xlsx"), range = "A1:C3000")[, 1:3]                                
    keyfordronetags <- keyfordronetags[complete.cases(keyfordronetags),] %>% 
      mutate(hive = factor(hive), OriginalUID = UID)
    
  } else {  # load RDS files
    cleanData <- readRDS(paste0(dataPath, "cleanData.rds"))
    ignoreData <- readRDS(paste0(dataPath, "ignoreData.rds"))
    assign(cleanData, cleanData, envir = globalenv())
    assign(ignoreData, ignoreData, envir = globalenv())
    keyfordronetags <- readRDS(paste0(dataPath, "keyfordronetags.rds"))
  }
  assign(keyfordronetags, keyfordronetags, envir = globalenv())
}

# setDataPath is mainly to allow for debugging (running local)
setDataPath <- function(isLocal, localPath = "DroneDataFilter/data/", shinyPath = "data\\") {
  if (isLocal) {
    return(localPath)
  } else {
    return(shinyPath)
  }
}

# Short UIDs in tag file
# Let's see if we can fix these.  Starting with the UIDs that are too short. If we can find one and only UID in the main data file whose
# first n characters match the short tag UID (where n is the length of the short tag UID), then we assume the short UID should be the UID found
# in the main data file.
# UniqueUIDMatches is the number of unique UIDs in the data file whose n leftmost characters that match the short UID from the Tag key file where
# n is the length of the short UID from the tag key file whose n leftmost characters matches the short UID from the tag file if UniqueIDMatches = 1.
# nUIDMatchesData is the number of observations from the data file with UIDs that match 
fixShortTagUIDs <- function(keyfordronetags, data) {
  out <- keyfordronetags
  uniqueDataUID <- unique(data$UID)
  uniqueTagUID <- unique(keyfordronetags$UID)
  missingTagUIDs <- setdiff(uniqueTagUID, uniqueDataUID) # UIDs in tag file not in main data
  for (i in 1:length(missingTagUIDs)) {
    badTagUID <- missingTagUIDs[i]
    ncharTemp <- nchar(badTagUID)
    if (ncharTemp < 21) {
      shortDataUID <- substr(data$UID, 1, ncharTemp)
      matchingUID <- data$UID[shortDataUID == badTagUID]
      uniqueMatching <- unique(matchingUID)
      if (length(uniqueMatching) == 1) {
        out[keyfordronetags$UID == badTagUID, "UID"] <- uniqueMatching
      }  
    }
  }
  return(out)
}

# For long UIDs (> 21 characters) in tag file
# If we can find one and only UID in the main data file whose 21 characters match the first 21 characters 
# of a long tag UIDthen we assume the first 21 characters of the tag file's UID should be the UID
# in the main data file.

# UniqueUIDMatches is the number of unique UIDs in the data file that match the first 21 characters of the UID from the Tag key file
# if UniqueIDMatches = 1,
# nUIDMatchesData is the number of observations from the data file with UIDs that match 
fixLongTagUIDs <- function(keyfordronetags, data) {
  out <- keyfordronetags
  uniqueDataUID <- unique(data$UID)
  uniqueTagUID <- unique(keyfordronetags$UID)
  missingTagUIDs <- setdiff(uniqueTagUID, uniqueDataUID) # UIDs in tag file not in main data
  for (i in 1:length(missingTagUIDs)) {
    badTagUID <- missingTagUIDs[i]
    ncharTemp <- nchar(badTagUID)
    if (ncharTemp > 21) {
      shortBadTagUID <- substr(badTagUID, 1, 21)
      matchingUID <- data$UID[data$UID == shortBadTagUID]
      uniqueMatching <- unique(matchingUID)
      if (length(uniqueMatching) == 1) {
        out[keyfordronetags$UID == badTagUID, "UID"] <- uniqueMatching
      }  
    }
  }
  return(out)
}

tagToDate <- function(tag, y) {
  underscoreLoc <- grep('_', strsplit(tag, '')[[1]])
  m <- substr(tag, 0, underscoreLoc[1] - 1)
  d <- substr(tag, underscoreLoc[1] + 1, underscoreLoc[2] - 1)
  return(as.Date(paste0(as.character(y), "-", m, "-", d), "%Y-%m-%d"))
}

lookupTagForUID <- function(dataRec, keyfordronetags) {
  temp <- keyfordronetags %>% filter(UID == dataRec$UID)
  dataDate <- as.Date(dataRec$FirstTimeFound)
  if (nrow(temp) == 1) {
    return(temp %>% pull(TAG))
  }
  if (nrow(temp) == 0) {
    return("NoTagRecordFound")
  }
  temp <- temp %>% filter(Birthday <= dataDate)
  if (nrow(temp) == 0) {
    return("NoTagBeforeFirstTimeDate")
  }
  return(as.character(temp[nrow(temp), "TAG"]))
}

AddTagToData <- function(data, keyfordronetags) {
  out <- data %>% mutate(TAG = sapply(1:nrow(data), function(x) lookupTagForUID(data[x, c("UID", "FirstTimeFound")], keyfordronetags)))
  return(out)
}

massageData2 <- function(data, keyfordronetags) {
  data1 <- massageData(data)
  ignoreData <- data1 %>% filter(Ignore) %>% select(-c("Ignore"))
  cleanData <- data1 %>% filter(!Ignore) %>% select(-c("Ignore", "OrigTimestamp.UTC",  "OrigFirstTimeFound",
                                                       "OrigLastTimeFound"))
  keyfordronetags <- fixShortTagUIDs(keyfordronetags, cleanData)
  keyfordronetags <- fixLongTagUIDs(keyfordronetags, cleanData)
  keyfordronetags <- keyfordronetags %>% 
    mutate(Birthday = tagToDate(TAG, 2021))
  cleanData <- AddTagToData(cleanData, keyfordronetags)
  cleanData[is.na(cleanData$Ant2), "Ant2"] <- "" # not sure where to put this
  summaryByBee <- summarizeByBee(cleanData, keyfordronetags)
  return(list(ignoreData = ignoreData, 
              cleanData = cleanData, 
              keyfordronetags = keyfordronetags,
              summaryByBee = summaryByBee))
}

BeeCalc <- function(BeeTAG, data, keyfordronetags) {
  out <- list()
  out$TAG = BeeTAG
  dataForTAG <- data %>% filter(TAG == BeeTAG) %>% arrange(FirstTimeFound) %>%
    mutate(FlightDate = as.Date(FirstTimeFound))
  out$nObs <- nrow(dataForTAG)
  out$FirstObsDate <- pull(dataForTAG, "FlightDate")[1]
  out$LastObsDate <- pull(dataForTAG, "FlightDate")[nrow(dataForTAG)]
  out$FirstObsReader <- pull(dataForTAG, "ReaderID")[1]
  out$FirstObsDirection <- pull(dataForTAG, "Direction")[1]
  out$FirstObsAnt1 <- pull(dataForTAG, "Ant1")[1]
  out$FirstObsAnt2 <- pull(dataForTAG, "Ant2")[1]
  keyData <- keyfordronetags %>% filter(TAG == BeeTAG)
  if (nrow(keyData) == 0) {
    out$Birthday <- as.Date(NA)  
    out$AgeFirstObs <- as.numeric(NA)
    out$AgeLastObs <- as.numeric(NA)
    out$Hive = NA
  } else {
    out$Birthday <- pull(keyData, "Birthday")[1]
    out$AgeFirstObs <- out$FirstObsDate - out$Birthday
    out$AgeLastObs <- out$LastObsDate - out$Birthday
    out$Hive <- keyData$hive  
  }
  return(out)
}

summarizeByBee <- function(data, keyfordronetags){
  temp <- lapply(unique(data$TAG), BeeCalc, data = data, keyfordronetags = keyfordronetags)
  
  out <- data.frame(cbind(unlist(purrr::map(temp, "TAG")), 
                           unlist(purrr::map(temp, "nObs")),
                           unlist(purrr::map(temp, "AgeFirstObs")), 
                           unlist(purrr::map(temp, "AgeLastObs")),
                           unlist(purrr::map(temp, "Hive")),
                           unlist(purrr::map(temp, "FirstObsReader")),
                           unlist(purrr::map(temp, "FirstObsDirection")),
                           unlist(purrr::map(temp, "FirstObsAnt1")),
                           unlist(purrr::map(temp, "FirstObsAnt2"))
                    )) %>% 
          rename(TAG = X1, nObs = X2, AgeFirst = X3, AgeLast = X4, Hive = X5, FirstReader = X6, FirstDirection = X7, FirstAnt1 = X8, FirstAnt2 = X9) %>%
          mutate(AgeFirst = as.numeric(AgeFirst), nObs = as.numeric(nObs), AgeLast = as.numeric(AgeLast))
  return(out)
}

# Resolving Unknowns
# Combine Unknowns with departing
# If unknown/Ant2 is within a time interval of the FirstTimeFound for a Departing obs, then combine them.
# Multiple unknowns/Ant2 might be combined into one (took time to leave)
# Likewise unknown/Ant1 might be combined if the LastTimeFound for the Departing obs is close to the 
# FirstTimeFound of the unknown/Ant1 (didn't quite leave until the last close unknown/Ant1)

# Similar for Unknowns and Arriving
# Unknown/Ant1 before Arriving FirstTimeFound would be combined as would 
# Unknown/Ant2 soon after Arrival LastTimeFound.

# find consecutive unknown obs of same bee, same day, same ant within a given time
# interval.  Interval resets for each obs.  So if the interval is 3 sec, and obs 1
# lasttimefound is at 1 sec, and obs 2 firsttimefound is at 2 sec and lasttimefound
# is at 2.5 sec and obs 3's first time found is at 5.5 sec, its last time is 6 sec 
# and obs 4's firsttimefound is at 9 sec, then all these observations might be
# collapsed into 1

# All of the above can happen if the readers never miss because these conditions imagine a bee hanging out
# near an antenna and being read multiple times.  But what if the bees can avoid antenna 
# (an antenna misses a bee)?  I don't have an answer.
#
# Let's say there was a departure, and then an unknown reading after a specified interval which would indicate a 
# flight.  A unknown/Ant1 would suggest an arrival.  But if the readers are imperfect and can miss, then 
# Ant1 might miss and an unknown/Ant2 would indicate an Arrival.  
#
# Let's say there was an arrival and then a reading after a specified rest interval (does this depend 
# on the duration of the flight?).  This might be a departure or just a bee hanging out.  If there's a 
# subsequent arrival, if would indicate a true departure.  If not, if could be a departure but the bee 
# did not return.  
#
# It seems to me readers are more likely to miss a bee (one type of error) than to record a bee that
# wasn't actually there (another type of error).  Do you agree?
# then Ant1 and Ant2 
#
# I wonder if there are readings of a bee quickly going between hives.

AnalyzeData <- function(data, interval = 5) {
  data <- data %>% filter(TAG != "NoTagRecordFound" &
                          TAG != "NoTagBeforeFirstTimeDate") %>%
                   arrange(TAG, FirstTimeFound) %>% 
                   mutate(FlightDate = as.Date(FirstTimeFound), 
                          Birthday = tagToDate(TAG, 2021))
  data <- data %>% mutate(RecNo = seq.int(nrow(data)),
                          Treatment = as.character(0)) 
  beeTags <- unique(data$TAG)
  out$flights <- data.frame(TAG = character(), DepObs = numeric(), ArrObs = numeric())
  out <- list()
  out$nObs <- nrow(data)
  out$nTags <- length(beeTags)
  out$nOneObs <-  nrow(unique(data %>% select(TAG, FlightDate)))
  out$CountByTag  <- data %>% group_by(TAG) %>% summarise(Count = n())
  out$CountByDate  <- data %>% group_by(FlightDate) %>% summarise(Count = n())
  out$CountByDateHive  <- data %>% group_by(FlightDate, HexAddress) %>% summarise(Count = n())
  for (beeTag in beeTags) {
    uniqueDays <- unique(data %>% filter(TAG == beeTag) %>% pull(FlightDate))
    for (uniqueDay in uniqueDays) {
      beeLocation <- "Hive"
      temp <- data %>% filter(TAG == beeTag & FlightDate == uniqueDay) %>% 
                       mutate(lagDirection = lag(Direction), lagRecNo = lag(RecNo))
      if (nrow(temp) == 1) {
        data[temp$RecNo, "Treatment"] <- "Ignore - 1Obs"
      } else {
        temp1 <- temp %>% filter(Direction == "Arriving" & lagDirection == "Departing") %>%
          mutate(DepObs = lagRecNo, ArrObs = RecNo) %>% select(TAG, DepObs, ArrObs)
        out$flights <- rbind(out$flights, temp1)
      }
      
    }
  }
  out$data <- data
  out$flights <- out$flights %>% 
                 mutate(Departure = data[out$flights$DepObs, "FirstTimeFound"] %>% pull(FirstTimeFound),
                        Arrival = data[out$flights$ArrObs, "FirstTimeFound"] %>% pull(FirstTimeFound),
                        Birthday = data[out$flights$DepObs, "Birthday"] %>% pull(Birthday),
                        DepHive = data[out$flights$DepObs, "HexAddress"] %>% pull(HexAddress),
                        ArrHive = data[out$flights$ArrObs, "HexAddress"] %>% pull(HexAddress)) %>%
                 mutate(Age = as.Date(Departure) - Birthday,
                        Duration = Arrival - Departure)
  return(out)
}
