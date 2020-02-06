
# load packages 
library(data.table)
library(ggplot2)

# load in files now
binarySensor <- fread("*/files/QJQ-823_121317.csv")
logServer <- fread("*/files/results-20171214-145849.csv")
eventServer <- fread("*/files/results-20171214-145922.csv")

# we need to clean the data now (convert time column to date object, check for NAs, etc.)
eventServer <-  eventServer[, -c("V4", "V5", "V6")] # delete extra, empty columns loaded in
eventServer[, messageType := "jsonPayload_event"]
setnames(eventServer, "jsonPayload_event", "message")

logServer[, messageType := "jsonPayload_log"]
setnames(logServer, "jsonPayload_log", "message")

messageServer <- dplyr::bind_rows(eventServer, logServer)
messageServer[, time := substr(messageServer$time, 1, nchar(messageServer$time)-3)] # strip UTC from end of time
options(digits.secs=6) # so we can get enough digits with milliseconds for our data
messageServer[, time := strptime(time, "%Y-%m-%d %H:%M:%OS")] # converting to date object
messageServer[, monthDay := format(time, "%m-%d")] # 
messageServer <- messageServer[order(time),]  # now need to make reorder time column



########
# How often does the printer connect and disconnect? 
connect <- messageServer[message %like% "ws:",]


########3
# What is the average connection time?
huntReceived <- headHunts[message == "hunt:received"] 
huntReceived[, lag_elapsed := c(NA,diff(time)), by = "monthDay"]

############
# How often (over what period) does the printer perform head-hunt? Is the head-hunt result always the same? Does it always require the same amount of time to complete a hunt?
headHunts <- messageServer[jsonPayload_event %like% "hunt",] # subset to head-hunt data

############
# attempts to perform a head-hunt an average of XX seconds between head-hunts per day 
huntReceived <- headHunts[message == "hunt:received"] 
huntReceived[, lag_elapsed := c(NA,diff(time)), by = "monthDay"]

# check for outliers statistically 

# plot time elapsed between head-hunt attempts a day
# add mean, median, standard deviation, make look pretty
ggplot(huntReceived[lag_elapsed < 9000,], aes(lag_elapsed)) + geom_histogram()

# the head hunt results is not always the same
# make histogram of distribution of 
unique(headHunts$jsonPayload_event)


###############
# does it always require the same amount of time to complete a hunt...no
# time elapsed between completed head-hunts
huntSuccess <- headHunts[jsonPayload_event %in% c("hunt:finished:succeeded", 
                                                  "hunt:received", "hunt:reset")] 

# want to keep only rows that state hunt:finished:succeeded and the row directly above!
rowsFinish <- huntSuccess[jsonPayload_event == "hunt:finished:succeeded", which = TRUE]
rows <- append(rowsFinish, rowsFinish - 1)
huntSuccess <- huntSuccess[rows,] # subset to rows with only succesful head-hunts 
huntSuccess <- huntSuccess[order(time),]  # now need to make reorder time column
huntSuccess$pair <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
time <- as.numeric(na.omit(huntSuccess[, .(lag_elapsed = c(NA,diff(time))), by = "pair"]$lag_elapsed))

# does it always require the same amount of time to complete a hunt...no
# time elapsed between  head-hunts
huntSuccess <- headHunts[jsonPayload_event %in% c("hunt:finished:succeeded", 
                                                  "hunt:received")] 

# 
rowsFinish <- huntSuccess[jsonPayload_event == "hunt:finished:succeeded", which = TRUE]
rows <- append(rowsFinish, rowsFinish[1:4]+1)
rows <- append(rows, 1)
huntSuccess <- huntSuccess[rows,] # subset to rows with only succesful head-hunts 
huntSuccess <- huntSuccess[order(time),]  # now need to make reorder time column
huntSuccess$pair <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
time <- as.numeric(na.omit(huntSuccess[, .(lag_elapsed = c(NA,diff(time))), by = "pair"]$lag_elapsed))






###########
# What is the rate of change of the "machine settings" over time? 
settings <- messageServer[message == "settings:completed"] 
settings[, lag_elapsed := c(NA,diff(time)), by = "monthDay"]
mean(na.omit(settings$lag_elapsed))

############
#How often does the server request "machine settings", and how does that compare with how often the printer updates them?
settings <- messageServer[message == "settings:completed"] 
settings[, lag_elapsed := c(NA,diff(time)), by = "monthDay"]
mean(na.omit(settings$lag_elapsed))

