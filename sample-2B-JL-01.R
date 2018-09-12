# house keeping -----------------------------------------------------------

rm(list = ls())

options(warn = -1)
options(digits.secs = 6) # if options('digits.sec') is set, up to the specified number of digits will be printed for record.


# packages ----------------------------------------------------------------

if(!require(pacman)) install.packages("pacman"); require(pacman)

p_load(data.table, scales, ggplot2, rstudioapi, fasttime)


# setting up path ---------------------------------------------------------

code_path <- setwd(dirname(getActiveDocumentContext()$path))

rootpath  <- gsub(basename(code_path), "", code_path)


# data cleaning -----------------------------------------------------------

# preset the type of each column to save memory and accelarate the loading
colclass = c('SYMBOL' = 'factor',
             'DATE'   = 'character',
             'TIME'   = 'character',
             'BID'    = 'double',
             'OFR'    = 'double',
             'BIDSIZ' = 'integer',
             'OFRSIZ' = 'integer',
             'MODE'   = 'integer',
             'EX'     = 'factor')

# read the data into memory
quotes <- fread(paste0(rootpath, 'data/quotes-1994.csv'), colClasses = colclass)

# convert uppercase column names into lowercase
setnames(quotes, tolower(colnames(quotes))) 
str(quotes)

# check for NA entries except mmid
print(any(is.na(quotes)))    

# check for NULL entries
print(any(is.null(quotes)))  


# fastPOSIXct -------------------------------------------------------------

EST_tz_data <- cbind(
  start = fastPOSIXct(
    c("2010-03-14 07:00:00", "2011-03-13 07:00:00", "2012-03-11 07:00:00", "2013-03-10 07:00:00")
  ),
  end   = fastPOSIXct(
    c("2010-11-07 06:00:00", "2011-11-06 06:00:00", "2012-11-04 06:00:00", "2013-11-03 06:00:00")
  )
)

to_EST_tz <- function(x)
{
  x <- as.numeric(x)
  is_dst <- logical(length(x))  #initialise as FALSE
  #Loop over DST periods in each year
  for(row in seq_len(nrow(EST_tz_data)))
  {
    is_dst[x > EST_tz_data[row, 1] & x < EST_tz_data[row, 2]] <- TRUE
  }
  #Hard-coded numbers are 4/5 hours in seconds
  ans <- ifelse(is_dst, x + 14400, x + 18000)
  class(ans) <- c("POSIXct", "POSIXt")
  ans
}

to_EST_tz(fastPOSIXct(ch))


# POSIXct -----------------------------------------------------------------

# deal with time formatting
quotes[, datetime := fastPOSIXct(paste(date, time), format='%Y%m%d %H:%M:%S')]
quotes[, datetime := as.POSIXct(paste(date, time), format='%Y%m%d %H:%M:%S', tz = 'EST')]
quotes[, ':=' (date = strftime(datetime, '%Y-%m-%d'), 
               time = strftime(datetime, '%H:%M:%S'))]
# strftime is a wrapper for formate and changes only the formating of the time

# only keep the quotes in normal trading time and mode which is not 0
quotes <- quotes[time >= '09:30:00' & time <= '16:00:00' & mode != 0, ]

# check the sequence of timestamps
quotes[, false := any(shift(datetime, type = 'lead') < datetime), by = symbol]


# distinguish outliers ----------------------------------------------------

# here, I simply defined outliers as quotes which have more than 100% intraday absolute return
is.out <- function(x) {abs(x-mean(x))/mean(x) > 1}
quotes[, outliers := (is.out(bid)|is.out(ofr)), by = .(symbol, date)]

# have a look at all the outliers, you will see some of them are defintely errors but some are still not clear.
quotes[outliers==T, ]

# calculate time duration of each quote
quotes[, duration := shift(datetime, type = 'lead') - datetime, by=.(symbol, date)]
quotes[, duration:=ifelse(is.na(duration), 0, duration)]





