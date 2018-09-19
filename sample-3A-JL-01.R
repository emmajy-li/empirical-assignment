
# housekeeping -----------------------------------------------------------

rm(list = ls())

options(digits.secs = 6)
options(warn        = -1)


# packages ----------------------------------------------------------------

if(!require(pacman)) install.packages("pacman"); require(pacman)

p_load(data.table, scales, ggplot2, rstudioapi, ModelMetrics, lubridate, 
       leaps, car, MASS)


# setting up path ---------------------------------------------------------

code_path <- setwd(dirname(getActiveDocumentContext()$path))

rootpath  <- gsub(basename(code_path), "", code_path)


# importing data -------------------------------------------------------------

# garbage collection
gc()

# Trade Data

# DATE	     DATE	Date of trade
# EX	       CHAR	Exchange that issued the trade
# PRICE	     NUM	Price of trade
# SIZE	     NUM	Volume of trade
# SYM_ROOT	 CHAR	Security symbol root
# SYM_SUFFIX CHAR	Security symbol suffix
# TIME_M	   NUM	Time of Trade or Quote with milliseconds (HHMMSSXXX)
# TR_CORR	   CHAR	Trade Correction Indicator
# TR_RF	     CHAR	Trade Reporting Facility
# TR_SCOND	 CHAR	Trade Sale Condition (up to 4 codes)
# TR_SEQNUM	 NUM	Trade Sequence Number
# TR_SOURCE	 CHAR	Source of Trade
# TR_STOPIND CHAR	Trade Stop Stock Indicator (NYSE Only)

colclassT = c('DATE'       = 'character',
              'TIME_M'     = 'character',
              'EX'         = 'factor',
              'SYM_ROOT'   = 'factor',
              'SYM_SUFFIX' = 'factor',
              'TR_SCOND'   = 'factor',
              'SIZE'       = 'integer',
              'PRICE'      = 'double',
              'TR_STOPIND' = 'factor',
              'TR_CORR'    = 'factor',
              'TR_SEQNUM'  = 'integer',
              'TR_SOURCE'  = 'factor',
              'TR_RF'      = 'factor')

trades <- fread(paste0(rootpath, 'data/BABA-trades-201505.csv'),
                header = T, sep = ',', colClasses = colclassT)

# structure of trades
str(trades)

# research levels of columns to decide whether or not to delete them
levels(trades$SYM_SUFFIX)
levels(trades$SYM_ROOT)
levels(trades$TR_SCOND)
levels(trades$TR_STOPIND)
levels(trades$TR_CORR)
levels(trades$TR_SEQNUM)
levels(trades$TR_SOURCE)

# eliminate SYM_ROOT, SYM_SUFFIX, TR_RF, TR_STOPIND, TR_SEQNUM, TR_SOURCE, TR_CORR
trades[, c('SYM_SUFFIX', 'SYM_ROOT', 'TR_RF', 'TR_STOPIND', 'TR_SEQNUM', 'TR_SOURCE','TR_CORR'):= NULL]

# trades per exchange
ex_sym <- data.frame(EX = c('A', 'B', 'C', 'D', 'I', 'J', 'K', 'M', 'N', 'P', 'S', 'T', 'Q', 'V', 'W', 'X', 'Y', 'Z'),
                     NAME = c('NYSE MKT LLC',
                              'NASDAQ OMX BX, Inc.',
                              'National Stock Exchange Inc. (NSX)',
                              'Financial Industry Regulatory Authority, Inc. (FINRA ADF)',
                              'International Securities Exchange, LLC (ISE)',
                              'Bats EDGA Exchange, INC',
                              'Bats EDGX Exchange, Inc.',
                              'Chicago Stock Exchange, Inc. (CHX)',
                              'New York Stock Exchange LLC',
                              'NYSE Arca, Inc.',
                              'Consolidated Tape System',
                              'NASDAQ Stock Exchange, LLC (in Tape A, B securities)',
                              'NASDAQ Stock Exchange, LLC (in Tape C securities)',
                              'The Investors’ Exchange, LLC (IEX)',
                              'Chicago Broad Options Exchange, Inc.(CBOE)',
                              'NASDAQ OMX PSX, Inc. LLC',
                              'Bats BYX Exchange, Inc.',
                              'Bats BZX Exchange, Inc.')
)

exchange_t    <- trades[, .N,by = EX]

exchange_sym_t <- merge(ex_sym, exchange_t, by = 'EX', all.y = T)

exchange_sym_t[order(-rank(exchange_sym_t$N)), ]

# Quote Data

# ASK	        NUM	  Ask price
# ASKEX	      CHAR	Ask exchange
# ASKSIZ      NUM	  Ask size in units of trade
# BID	        NUM	  Bid price
# BIDEX	      CHAR	Bid exchange
# BIDSIZ	    NUM	  Bid size in units of trade
# DATE	      DATE	Date of quote
# EX	        CHAR	Exchange that issued the quote
# NASDBBO_IND	CHAR	NASD BBO Indicator
# NATBBO_IND	CHAR	National BBO Indicator
# QU_CANCEL	  CHAR	Quote Cancel/Correction
# QU_COND	    CHAR	Condition of quote issued
# QU_SEQNUM	  NUM	  Quote Sequence Number
# QU_SOURCE	  CHAR	Source of Quote
# SYM_ROOT	  CHAR	Security symbol root
# SYM_SUFFIX	CHAR	Security symbol suffix
# TIME_M	    NUM	  Time of Trade or Quote with milliseconds (HHMMSSXXX)

colclassQ = c('DATE'       = 'character',
              'TIME_M'     = 'character',
              'EX'         = 'factor',
              'SYM_ROOT'   = 'factor',
              'SYM_SUFFIX' = 'factor',
              'BID'        = 'double',
              'BIDSIZ'     = 'integer',
              'ASK'        = 'double',
              'ASKSIZ'     = 'integer',
              'QU_COND'    = 'factor',
              'BIDEX'      = 'factor',
              'ASKEX'      = 'factor',
              'QU_SEQNUM'  = 'factor',
              'NATBBO_IND' = 'factor',
              'NASDBBO_IND'= 'factor',
              'QU_CANCEL'  = 'factor',
              'QU_SOURCE'  = 'factor')

quotes <- fread(paste0(rootpath, 'data/BABA-quotes-201505.csv'), header = T, sep = ',', colClasses = colclassQ)

# structure of quotes
str(quotes)

# research levels of columns to decide whether or not to delete them
levels(quotes$SYM_ROOT)
levels(quotes$SYM_SUFFIX)
levels(quotes$NATBBO_IND)
levels(quotes$NASDBBO_IND)
levels(quotes$QU_CANCEL)
levels(quotes$QU_SOURCE)

# learning point:
# QU_COND:
# ___C___ = Closing;
# ___O___ = Opening Quote;
# ___R___ = Regular, two-sided open quote.
levels(quotes$QU_COND)

# eliminate SYM_ROOT, SYM_SUFFIX, QU_COND, NATBBO_IND, NASDBBO_IND, QU_CANCEL, QU_SOURCE
quotes[, c('SYM_ROOT', 'SYM_SUFFIX', 'QU_COND', 'NATBBO_IND', 'NASDBBO_IND', 'QU_CANCEL', 'QU_SOURCE') := NULL]

# quotes per exchange

exchange_q <- quotes[, .N, by = EX]

exchange_sym_q <- merge(ex_sym, exchange_q, by = 'EX', all.y = T)

exchange_sym_q[order(-rank(exchange_sym_q$N)), ]

gc()



# data cleansing ----------------------------------------------------------

# convert uppercase column names into lowercase to be consistent
setnames(trades,tolower(colnames(trades)))
setnames(quotes,tolower(colnames(quotes)))

# exclude nagetive bid/ask price and size
quotes <- quotes[bid > 0 & ask > 0 & bidsiz > 0 & asksiz > 0 & ask > bid, ]

# deal with time formatting

# # EXPERIMENT
# # way 1: base::as.POSIXct
# # time the code
# ptm <- proc.time()
# 
# trades[, datetime := as.POSIXct(paste(date, time_m), format = '%Y%m%d %H:%M:%OS')]
# 
# proc.time() - ptm # running time
# 
# # way2: lubridate::parse_date_time()
# ptm <- proc.time()
# 
# trades[, datetime_ps := parse_date_time(paste(date,time_m), '%Y%m%d %H:%M:%OS', tz = 'America/New_York')]
# 
# proc.time() - ptm # running time
# 
# # check if they are identical
# mod <- lm(trades$datetime ~ trades$datetime_ps)
# trades[, 'datetime_ps' := NULL]

# trades
trades[, datetime := parse_date_time(paste(date,time_m), '%Y%m%d %H:%M:%OS', tz = 'America/New_York')]

trades[, ':='(date   = strftime(datetime,'%Y-%m-%d'),
              time_m = strftime(datetime,'%H:%M:%S'))] # convert time back to string

# quotes
quotes[, datetime := parse_date_time(paste(date,time_m), '%Y%m%d %H:%M:%OS', tz = 'America/New_York')]

quotes[, ':='(date   = strftime(datetime,'%Y-%m-%d'),
              time_m = strftime(datetime,'%H:%M:%S'))]


# only keep the trades and quotes that happened in normal trading time, with 15min's adjustment
trades <- trades[time_m >= '09:45:00' & time_m <= '15:45:00']
quotes <- quotes[time_m >= '09:45:00' & time_m <= '15:45:00']

# check the sequence of timestamps and set datetime as key
any(shift(trades$datetime, type = 'lead') < trades$datetime, na.rm = T)
any(shift(quotes$datetime, type = 'lead') < quotes$datetime, na.rm = T)

# set datetime as key in trades
setkey(trades, datetime)
setkey(quotes, datetime)

# We can also use quotes' sequence number to check the sequence of datetime
any(shift(quotes$qu_seqnum, type = "lead") < quotes$qu_seqnum, na.rm = T)

# get 1 min intervals
trades[, mins := cut(datetime, '1 min')]
quotes[, mins := cut(datetime, '1 min')]

# trade price outlier detection
# How do we define outlier?

# way1: absolute value of intraminute return greater than 0.1
trades[, ':=' (priceAvg = mean(price), 
               priceSd  = sd(price)), 
       by = mins]

trades[, price_dev := abs(price - priceAvg) / priceAvg]

# trades_outlier_threshold <- function(sequence) {
#   df <- data.frame(threshold = integer(),
#                    rownum    = integer(),
#                    percent   = integer())
#   j     <- 1
#   row_t <- nrow(trades)
#   for(i in sequence) {
#     trades[, outliers := ifelse(price_dev > i, T, F)]
#     row_o   <- nrow(trades[outliers == T, ])
#     df[j, ] <- c(i, row_o, percent(row_o / row_t, accuracy = .0001))
#     j <- j + 1
#   }
#   return(df)
# }
# 
# trades_outlier_threshold(seq(0, 0.1, 0.01))

# defind threshold for outliers
trades[, outliers := ifelse(price_dev > 0.01, T, F)]

# take a look at outliers
trades[outliers == T, c('datetime', 'ex', 'tr_scond', 'size', 'price', 'priceAvg')]

# keep only non-outlier to trades
trades <- trades[outliers == F, ]

# quotes price outlier detection
quotes[, ':='(bidAvg = mean(bid),
              askAvg = mean(ask),
              bidSd  = sd(bid),
              askSd  = sd(ask)),
       by = mins]

# test the threshold of deciding outliers
quotes[, ':=' (bid_dev = abs(bid - bidAvg) / bidAvg,
               ask_dev = abs(ask - askAvg) / askAvg)]

# quotes_outlier_threshold <- function(sequence) {
#   df <- data.frame(threshold = integer(),
#                    rownum    = integer(),
#                    percent   = integer())
#   j     <- 1
#   row_q <- nrow(quotes)
#   for(i in sequence) {
#     quotes[, outliers := ifelse(bid_dev > i | ask_dev > i, T, F)]
#     row_o   <- nrow(quotes[outliers == T, ])
#     df[j, ] <- c(i, row_o, percent(row_o / row_q, accuracy = .0001))
#     j <- j + 1
#   }
#   return(df)
# }
# 
# quotes_outlier_threshold(seq(0, 0.3, 0.05))
# quotes_outlier_threshold(seq(0, 0.05, 0.01))

# defind threshold for outliers
quotes[, outliers := ifelse(bid_dev > 0.01 | ask_dev > 0.01, T, F)]

# take a look at outliers
quotes[outliers == T, c('datetime', 'ex', 'bid', 'bidAvg', 'bidsiz', 'ask', 'askAvg', 'asksiz', 'bid_dev', 'ask_dev')]

# keep only non-outlier to quotes
quotes <- quotes[outliers == F, ]

# way 2: Use different time intervals and standard deviation

# standard deviation of bid and ask over 20 trading days
sd(quotes$bid)
sd(quotes$ask)

# feature engineering -----------------------------------------------------

lags = 4

# trades
# create summary characteristics by minutes in trades
Tmin <- trades[, .(priceAvg   = mean(price),
                   volume     = sum(size),
                   firstPrice = head(price, 1),
                   lastPrice  = tail(price, 1)),
               by = mins]

# take 1-min leads and lags
Tmin[, ':=' (lastPrice_1lead  = shift(lastPrice,   type = 'lead', n = 1),
             lastPrice_1lag   = shift(lastPrice,   type = 'lag',  n = 1),
             lastPrice_2lag   = shift(lastPrice,   type = 'lag',  n = 2),
             lastPrice_3lag   = shift(lastPrice,   type = 'lag',  n = 3),
             lastPrice_4lag   = shift(lastPrice,   type = 'lag',  n = 4),
             priceAvg_1lag    = shift(priceAvg,    type = 'lag' , n = 1),
             priceAvg_2lag    = shift(priceAvg,    type = 'lag' , n = 2),
             priceAvg_3lag    = shift(priceAvg,    type = 'lag' , n = 3),
             priceAvg_4lag    = shift(priceAvg,    type = 'lag' , n = 4),
             volumn_1lag      = shift(volume,      type = 'lag' , n = 1))]

# set mins as key in Tmin
setkey(Tmin, mins)

# get 1 day intervals
Tmin[, days := cut(as.POSIXct(mins), '1 day')]

# mark the first and the last minutes of the day
Tmin[, ':=' (last_min  = ifelse(mins %in% tail(mins, lags), T, F),
             first_min = ifelse(mins %in% head(mins, lags), T, F)),
     by = days]

# clear the first and last trades of the day, to deal with leads and lags
Tmin <- Tmin[first_min == F & last_min == F, ]

# caclulate RHS variables
Tmin[, ':='(LPminuspriceAvg = lastPrice - priceAvg,
            Volume1min_past = volume    - volumn_1lag,
            R1min_future    = (Tmin$lastPrice_1lead - lastPrice)      / lastPrice,
            R1min_past      = (Tmin$lastPrice       - lastPrice_1lag) / lastPrice_1lag,
            R2min_past      = (Tmin$lastPrice_1lag  - lastPrice_2lag) / lastPrice_2lag,
            R3min_past      = (Tmin$lastPrice_2lag  - lastPrice_3lag) / lastPrice_3lag,
            R4min_past      = (Tmin$lastPrice_3lag  - lastPrice_4lag) / lastPrice_4lag,
            RAvg1min_past   = (Tmin$priceAvg        - priceAvg_1lag)  / priceAvg_1lag,
            RAvg2min_past   = (Tmin$priceAvg_1lag   - priceAvg_2lag)  / priceAvg_2lag,
            RAvg3min_past   = (Tmin$priceAvg_2lag   - priceAvg_3lag)  / priceAvg_3lag,
            RAvg4min_past   = (Tmin$priceAvg_3lag   - priceAvg_4lag)  / priceAvg_4lag),]

# quotes
# create summary characteristics by minutes in quotes
Qmin <- quotes[, .(bidaskMid    = (mean(bid) + mean(ask))/2,
                   spreadAvg    = mean(ask) - mean(bid),
                   bidVolume    = sum(bidsiz),
                   askVolume    = sum(asksiz),
                   bidVolumeAvg = mean(bidsiz),
                   askVolumeAvg = mean(asksiz),
                   lastbid      = tail(bid, 1),
                   lastask      = tail(ask, 1)),
               by = mins]

Qmin[, MidVweighted := (askVolume * lastbid + bidVolume * lastask) / (askVolume + bidVolume), ]

# take 1-min leads and lags
Qmin[, ':=' (bidaskMid_1lead    = shift(Qmin$bidaskMid,    type = 'lead', n = 1),
             bidaskMid_1lag     = shift(Qmin$bidaskMid,    type = 'lag' , n = 1),
             bidaskMid_2lag     = shift(Qmin$bidaskMid,    type = 'lag' , n = 2),
             bidaskMid_3lag     = shift(Qmin$bidaskMid,    type = 'lag' , n = 3),
             bidaskMid_4lag     = shift(Qmin$bidaskMid,    type = 'lag' , n = 4),
             spreadAvg_1lag     = shift(Qmin$spreadAvg,    type = 'lag',  n = 1),
             bidVolume_1lag     = shift(Qmin$bidVolume,    type = 'lag',  n = 1),
             bidVolume_2lag     = shift(Qmin$bidVolume,    type = 'lag',  n = 2),
             bidVolume_3lag     = shift(Qmin$bidVolume,    type = 'lag',  n = 3),
             bidVolume_4lag     = shift(Qmin$bidVolume,    type = 'lag',  n = 4),
             askVolume_1lag     = shift(Qmin$askVolume,    type = 'lag' , n = 1),
             askVolume_2lag     = shift(Qmin$askVolume,    type = 'lag' , n = 2),
             askVolume_3lag     = shift(Qmin$askVolume,    type = 'lag' , n = 3),
             askVolume_4lag     = shift(Qmin$askVolume,    type = 'lag' , n = 4),
             bidVolumeAvg_1lag  = shift(Qmin$bidVolumeAvg, type = 'lag',  n = 1),
             bidVolumeAvg_2lag  = shift(Qmin$bidVolumeAvg, type = 'lag',  n = 2),
             bidVolumeAvg_3lag  = shift(Qmin$bidVolumeAvg, type = 'lag',  n = 3),
             bidVolumeAvg_4lag  = shift(Qmin$bidVolumeAvg, type = 'lag',  n = 4),
             askVolumeAvg_1lag  = shift(Qmin$askVolumeAvg, type = 'lag',  n = 1),
             askVolumeAvg_2lag  = shift(Qmin$askVolumeAvg, type = 'lag',  n = 2),
             askVolumeAvg_3lag  = shift(Qmin$askVolumeAvg, type = 'lag',  n = 3),
             askVolumeAvg_4lag  = shift(Qmin$askVolumeAvg, type = 'lag',  n = 4),
             MidVweighted_1lag  = shift(Qmin$MidVweighted, type = 'lag',  n = 1)),]

# set mins as key in Qmin
setkey(Qmin, mins)

# get 1 day intervals
Qmin[, days := cut(as.POSIXct(mins), '1 day')]

# mark the first and the last two minutes of the day
Qmin[, ':=' (last_min  = ifelse(mins %in% tail(mins, lags), T, F),
             first_min = ifelse(mins %in% head(mins, lags), T, F)),
     by = days]

# clear the first and last trades of the day, to deal with leads and lags
Qmin <- Qmin[first_min == F & last_min == F, ]

# caclulate RHS variables
Qmin[, ':='(askVolumeDiff1     = (askVolumeAvg_1lag      - askVolumeAvg_2lag)  / (askVolumeAvg_1lag + askVolumeAvg),
            askVolumeDiff2     = (askVolumeAvg_2lag      - askVolumeAvg_3lag)  / (askVolumeAvg_2lag + askVolumeAvg_1lag),
            askVolumeDiff3     = (askVolumeAvg_3lag      - askVolumeAvg_4lag)  / (askVolumeAvg_3lag + askVolumeAvg_2lag),
            bidVolumeDiff1     = (Qmin$bidVolumeAvg_1lag - Qmin$bidVolumeAvg_2lag) / (Qmin$bidVolumeAvg_1lag + Qmin$bidVolumeAvg),
            bidVolumeDiff2     = (Qmin$bidVolumeAvg_2lag - Qmin$bidVolumeAvg_3lag) / (Qmin$bidVolumeAvg_2lag + Qmin$bidVolumeAvg_1lag),
            bidVolumeDiff3     = (Qmin$bidVolumeAvg_3lag - Qmin$bidVolumeAvg_4lag) / (Qmin$bidVolumeAvg_3lag + Qmin$bidVolumeAvg_2lag),
            MidoverSpread      = (Qmin$bidaskMid         - Qmin$lastbid)        / Qmin$spreadAvg - 1/2,
            VolumeRatio1       = (Qmin$bidVolume         - Qmin$bidVolume_1lag) / (Qmin$askVolume - Qmin$askVolume_1lag),
            VolumeRatio2       = (Qmin$bidVolume_1lag    - Qmin$bidVolume_2lag) / (Qmin$askVolume_1lag - Qmin$askVolume_2lag),
            VolumeRatio3       = (Qmin$bidVolume_2lag    - Qmin$bidVolume_3lag) / (Qmin$askVolume_2lag - Qmin$askVolume_3lag),
            VolumeRatio4       = (Qmin$bidVolume_3lag    - Qmin$bidVolume_4lag) / (Qmin$askVolume_3lag - Qmin$askVolume_4lag),
            MidR1min_future    = (Qmin$bidaskMid_1lead   - Qmin$bidaskMid)      / Qmin$bidaskMid,
            MidR1min_past      = (Qmin$bidaskMid         - Qmin$bidaskMid_1lag) / Qmin$bidaskMid_1lag,
            MidR2min_past      = (Qmin$bidaskMid_1lag    - Qmin$bidaskMid_2lag) / Qmin$bidaskMid_2lag,
            MidR3min_past      = (Qmin$bidaskMid_2lag    - Qmin$bidaskMid_3lag) / Qmin$bidaskMid_3lag,
            MidR4min_past      = (Qmin$bidaskMid_3lag    - Qmin$bidaskMid_4lag) / Qmin$bidaskMid_4lag,
            MidR1min_past_c    = ((Qmin$bidaskMid        - Qmin$bidaskMid_1lag) / Qmin$bidaskMid_1lag) ^ 3,
            MidR2min_past_c    = ((Qmin$bidaskMid_1lag   - Qmin$bidaskMid_2lag) / Qmin$bidaskMid_2lag) ^ 3,
            MidR3min_past_c    = ((Qmin$bidaskMid_2lag   - Qmin$bidaskMid_3lag) / Qmin$bidaskMid_3lag) ^ 3,
            MidR4min_past_c    = ((Qmin$bidaskMid_3lag   - Qmin$bidaskMid_4lag) / Qmin$bidaskMid_4lag) ^ 3,
            MidVweightedDiff   = (Qmin$MidVweighted      - Qmin$MidVweighted_1lag) / Qmin$MidVweighted_1lag), ]

Qmin[, ':='(askVolumeR1 = askVolumeDiff1 * MidR1min_past,
            askVolumeR2 = askVolumeDiff2 * MidR2min_past,
            askVolumeR3 = askVolumeDiff3 * MidR3min_past,
            bidVolumeR1 = bidVolumeDiff1 * MidR1min_past,
            bidVolumeR2 = bidVolumeDiff2 * MidR2min_past,
            bidVolumeR3 = bidVolumeDiff3 * MidR3min_past)]
# merge
Panel <- merge(Tmin, Qmin, by = "mins")

# Tmin 6980 obs
# Qmin 7060 obs
# Panel 6980 obs

# modeling ----------------------------------------------------------------

# split dataset to 80 training set, 20 out of fold set
oof   <- Panel[(nrow(Panel)*0.8 + 1):nrow(Panel), ]
train <- Panel[1:(nrow(Panel)*0.8)              , ]

# train a model to predict outcome
# simple
mod1 <- lm(MidR1min_future ~ askVolumeDiff1 + askVolumeDiff2 + askVolumeDiff3 + 
             bidVolumeDiff1 + bidVolumeDiff2 + bidVolumeDiff3 +
             MidoverSpread + MidVweightedDiff + 
             VolumeRatio1 + VolumeRatio2 + VolumeRatio3 + VolumeRatio4 +
             MidR1min_past + MidR2min_past + MidR3min_past + MidR4min_past + 
             MidR1min_past_c + MidR2min_past_c + MidR3min_past_c + MidR4min_past_c + 
             R1min_past + R2min_past + R3min_past + R4min_past + 
             RAvg1min_past + RAvg2min_past + RAvg3min_past + RAvg4min_past + 
             askVolumeR1 + askVolumeR2 + askVolumeR3 +
             bidVolumeR1 + bidVolumeR2 + bidVolumeR3 +
             LPminuspriceAvg + Volume1min_past,
           data = train)

mod2 <- lm(R1min_future ~ askVolumeDiff1 + askVolumeDiff2 + askVolumeDiff3 + 
             bidVolumeDiff1 + bidVolumeDiff2 + bidVolumeDiff3 +
             MidoverSpread + MidVweightedDiff + 
             VolumeRatio1 + VolumeRatio2 + VolumeRatio3 + VolumeRatio4 +
             MidR1min_past + MidR2min_past + MidR3min_past + MidR4min_past + 
             MidR1min_past_c + MidR2min_past_c + MidR3min_past_c + MidR4min_past_c + 
             R1min_past + R2min_past + R3min_past + R4min_past + 
             RAvg1min_past + RAvg2min_past + RAvg3min_past + RAvg4min_past + 
             askVolumeR1 + askVolumeR2 + askVolumeR3 +
             bidVolumeR1 + bidVolumeR2 + bidVolumeR3 +
             LPminuspriceAvg + Volume1min_past,
           data = train)

summary(mod1)
# r^2:   0.371 
# adjr2: 0.367

summary(mod2)
# r^2:   0.01533 
# adjr2: 0.00901

# all subset regression
mod3 <- regsubsets(MidR1min_future ~ askVolumeDiff1 + askVolumeDiff2 + askVolumeDiff3 + 
                     bidVolumeDiff1 + bidVolumeDiff2 + bidVolumeDiff3 +
                     MidoverSpread + MidVweightedDiff + 
                     VolumeRatio1 + VolumeRatio2 + VolumeRatio3 + VolumeRatio4 +
                     MidR1min_past + MidR2min_past + MidR3min_past + MidR4min_past + 
                     MidR1min_past_c + MidR2min_past_c + MidR3min_past_c + MidR4min_past_c + 
                     R1min_past + R2min_past + R3min_past + R4min_past + 
                     RAvg1min_past + RAvg2min_past + RAvg3min_past + RAvg4min_past + 
                     askVolumeR1 + askVolumeR2 + askVolumeR3 +
                     bidVolumeR1 + bidVolumeR2 + bidVolumeR3 +
                     LPminuspriceAvg + Volume1min_past,
                   data = train,
                   nbest = 1,       # 1 best model for each number of predictors
                   nvmax = NULL,    # NULL for no limit on number of variables
                   force.in = NULL, force.out = NULL,
                   method = "exhaustive")

mod4 <- regsubsets(R1min_future ~ askVolumeDiff1 + askVolumeDiff2 + askVolumeDiff3 + 
                     bidVolumeDiff1 + bidVolumeDiff2 + bidVolumeDiff3 +
                     MidoverSpread + MidVweightedDiff + 
                     VolumeRatio1 + VolumeRatio2 + VolumeRatio3 + VolumeRatio4 +
                     MidR1min_past + MidR2min_past + MidR3min_past + MidR4min_past + 
                     MidR1min_past_c + MidR2min_past_c + MidR3min_past_c + MidR4min_past_c + 
                     R1min_past + R2min_past + R3min_past + R4min_past + 
                     RAvg1min_past + RAvg2min_past + RAvg3min_past + RAvg4min_past + 
                     askVolumeR1 + askVolumeR2 + askVolumeR3 +
                     bidVolumeR1 + bidVolumeR2 + bidVolumeR3 +
                     LPminuspriceAvg + Volume1min_past,
                   data = train,
                   nbest = 1,       # 1 best model for each number of predictors
                   nvmax = NULL,    # NULL for no limit on number of variables
                   force.in = NULL, force.out = NULL,
                   method = "exhaustive")

# best model with highest adjusted r^2
mod3bestsiz <- which.max(summary(mod3)$adjr2)
mod3LHS     <- as.data.frame(summary(mod3)$which[mod3bestsiz, ])
mod4bestsiz <- which.max(summary(mod4)$adjr2)
mod4LHS     <- as.data.frame(summary(mod4)$which[mod4bestsiz, ])

mod3_b <- lm(MidR1min_future ~ askVolumeDiff1 + 
               bidVolumeDiff2 + bidVolumeDiff3 +
               MidoverSpread + MidVweightedDiff + 
               VolumeRatio1 + VolumeRatio3 + 
               MidR1min_past + MidR2min_past + MidR3min_past + MidR4min_past + 
               MidR1min_past_c + MidR2min_past_c + MidR4min_past_c + 
               R1min_past + R4min_past + 
               RAvg1min_past + RAvg2min_past + RAvg3min_past + RAvg4min_past + 
               askVolumeR2 + askVolumeR3 +
               bidVolumeR1 + 
               LPminuspriceAvg + Volume1min_past,
             data = train)

summary(mod3_b)
# r^2:   0.3708 
# adjr2: 0.368

mod4_b <- lm(R1min_future ~ 
               bidVolumeDiff2 + bidVolumeDiff3 +
               MidVweightedDiff + 
               MidR2min_past_c + MidR3min_past_c + MidR4min_past_c + 
               R2min_past + R3min_past +
               RAvg4min_past + 
               askVolumeR2 + askVolumeR3 +
               bidVolumeR1 + bidVolumeR2 + bidVolumeR3 +
               LPminuspriceAvg,
             data = train)

summary(mod4_b)
# r^2:   0.01415 
# adjr2: 0.01153

# predict outcome on OOF data
oof$MPvalidation    <- predict(mod1,   oof, type = "response")
oof$LPvalidation    <- predict(mod2,   oof, type = "response")
oof$MPvalidation_b  <- predict(mod3_b, oof, type = "response")
oof$LPvalidation_b  <- predict(mod4_b, oof, type = "response")
oof$mins            <- as.POSIXct(oof$mins)


# plotting ----------------------------------------------------------------

model1fit1 <- 
  ggplot(oof, aes(x = MidR1min_future, y = MPvalidation)) + 
  geom_point(color = 'dark blue') +
  stat_smooth(method = "lm", col = "light yellow") +
  geom_line(data = data.frame("x-axis" = c(0.0015,-0.0015), 
                              "y-axis" = c(0.0015, -0.0015)), 
            aes(x = x.axis , y = y.axis), color = 'dark red', size = 1) +
  labs(
    title = "BABA Future 1-min Bid-Ask Midpoint Return Prediction versus Real Return 36LHS\n(Yellow: Fitted; Dark Red: 45° line)",
    x = "Future 1-Min Return", y = "36LHS Prediction") +
  theme(
    panel.background = element_rect(fill = "white", size     = 0.1,     linetype = "solid"),
    panel.grid.major = element_line(size = 0.1,     linetype = 'solid', colour   = "grey")) +
  coord_fixed()

model1fit2 <-
  ggplot() +
  geom_step(data = oof, aes(x = mins, y = MidR1min_future), color = 'blue') +
  geom_step(data = oof, aes(x = mins, y = MPvalidation),   color = 'red') +
  labs(
    title = "BABA Future 1-Min Bid-Ask Midpoint Prediction Return and Real Return Match 36LHS\n(Blue: Real; Red: Prediction)",
    x = "Time", y = "Future 1-Min Return") +
  theme(
    panel.background = element_rect(fill = "white", size     = 0.1,     linetype = "solid"),
    panel.grid.major = element_line(size = 0.1,     linetype = 'solid', colour   = "grey"))

model2fit1 <- 
  ggplot(oof, aes(x = R1min_future, y = LPvalidation)) + 
  geom_point(color = 'sky blue') +
  stat_smooth(method = "lm", col = "light yellow") +
  geom_line(data = data.frame("x-axis" = c(0.0015,-0.0015), 
                              "y-axis" = c(0.0015, -0.0015)), 
            aes(x = x.axis , y = y.axis), color = 'dark blue', size = 1) +
  labs(
    title = "BABA Future 1-min Last Price Return Prediction versus Real Return 36LHS\n(Yellow: Fitted; Dark blue: 45° line)",
    x = "Future 1-Min Return", y = "36LHS Prediction") +
  theme(
    panel.background = element_rect(fill = "brown", size     = 0.1,     linetype = "solid"),
    panel.grid.major = element_line(size = 0.1,     linetype = 'solid', colour   = "grey")) +
  coord_fixed()

model2fit2 <-
  ggplot() +
  geom_step(data = oof, aes(x = mins, y = R1min_future), color = 'blue') +
  geom_step(data = oof, aes(x = mins, y = LPvalidation),   color = 'red') +
  labs(
    title = "BABA Future 1-Min Last Price Prediction Return and Real Return Match 36LHS\n(Blue: Real; Red: Prediction)",
    x = "Time", y = "Future 1-Min Return") +
  theme(
    panel.background = element_rect(fill = "white", size     = 0.1,     linetype = "solid"),
    panel.grid.major = element_line(size = 0.1,     linetype = 'solid', colour   = "grey"))

model3fit1 <- 
  ggplot(oof, aes(x = MidR1min_future, y = MPvalidation_b)) + 
  geom_point(color = 'dark blue') +
  stat_smooth(method = "lm", col = "light yellow") +
  geom_line(data = data.frame("x-axis" = c(0.0015,-0.0015), 
                              "y-axis" = c(0.0015, -0.0015)), 
            aes(x = x.axis , y = y.axis), color = 'dark red', size = 1) +
  labs(
    title = "BABA Future 1-min Bid-Ask Midpoint Return Prediction versus Real Return 25LHS\n(Yellow: Fitted; Dark Red: 45° line)",
    x = "Future 1-Min Return", y = "25LHS Prediction") +
  theme(
    panel.background = element_rect(fill = "white", size     = 0.1,     linetype = "solid"),
    panel.grid.major = element_line(size = 0.1,     linetype = 'solid', colour   = "grey")) +
  coord_fixed()

model4fit1 <- 
  ggplot(oof, aes(x = R1min_future, y = LPvalidation_b)) + 
  geom_point(color = 'sky blue') +
  stat_smooth(method = "lm", col = "light yellow") +
  geom_line(data = data.frame("x-axis" = c(0.0015,-0.0015), 
                              "y-axis" = c(0.0015, -0.0015)), 
            aes(x = x.axis , y = y.axis), color = 'dark blue', size = 1) +
  labs(
    title = "BABA Future 1-min Last Price Return Prediction versus Real Return 15LHS\n(Yellow: Fitted; Dark blue: 45° line)",
    x = "Future 1-Min Return", y = "15LHS Prediction") +
  theme(
    panel.background = element_rect(fill = "brown", size     = 0.1,     linetype = "solid"),
    panel.grid.major = element_line(size = 0.1,     linetype = 'solid', colour   = "grey")) +
  coord_fixed()

# save graph to output
ggsave("BABA Future 1-Min Mid Point Prediction Return versus Real Return 36LHS.pdf",
       path = paste0(rootpath, 'output'),
       plot = model1fit1)

ggsave("BABA Future 1-Min Mid Point Prediction Return versus Real Return 25LHS.pdf",
       path = paste0(rootpath, 'output'),
       plot = model3fit1)

ggsave("BABA Future 1-Min Mid Point Prediction Return and Real Return Match 36LHS.pdf",
       path = paste0(rootpath, 'output'),
       plot = model1fit2)

ggsave("BABA Future 1-Min Last Price Prediction Return versus Real Return 36LHS.pdf",
       path = paste0(rootpath, 'output'),
       plot = model2fit1)

ggsave("BABA Future 1-Min Last Price Prediction Return versus Real Return 15LHS.pdf",
       path = paste0(rootpath, 'output'),
       plot = model4fit1)

ggsave("BABA Future 1-Min Last Price Prediction Return and Real Return Match 36LHS.pdf",
       path = paste0(rootpath, 'output'),
       plot = model2fit2)