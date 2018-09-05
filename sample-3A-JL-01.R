
# house keeping -----------------------------------------------------------

rm(list = ls())

options(digits.secs=6)
options(warn = -1)


# packages ----------------------------------------------------------------

if(!require(pacman)) install.packages("pacman"); require(pacman)

p_load(data.table, scales, ggplot2, rstudioapi, ModelMetrics, lubridate)


# setting up path ---------------------------------------------------------

code_path <- setwd(dirname(getActiveDocumentContext()$path))

rootpath  <- gsub(basename(code_path), "", code_path)


# import data -------------------------------------------------------------

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

# eliminate SYM_ROOT, SYM_SUFFIX, TR_RF
trades[, c('SYM_SUFFIX', 'SYM_ROOT', 'TR_RF'):= NULL]

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
                              'Bats BZX Exchange, Inc.'
                              ))

exchange     <- trades[, .N, by = EX]
exchange_sym <- merge(ex_sym, exchange, by = 'EX', all.y = T)

exchange_sym[order(-rank(exchange_sym$N)), ]

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

colclassQ = c('DATE'       = 'factor',
              'TIME_M'     = 'factor',
              'EX'         = 'factor',        
              'SYM_ROOT'   = 'factor',
              'SYM_SUFFIX' = 'factor', 
              'BID'        = 'factor',        
              'BIDSIZ'     = 'factor',     
              'ASK'        = 'factor',            
              'ASKSIZ'     = 'factor',     
              'QU_COND'    = 'factor',    
              'BIDEX'      = 'factor',    
              'ASKEX'      = 'factor',    
              'QU_SEQNUM'  = 'factor',  
              'NATBBO_IND' = 'factor',  
              'NASDBBO_IND'= 'factor',  
              'QU_CANCEL'  = 'factor',  
              'QU_SOURCE'  = 'factor')

# learning point: 
# QU_COND: 
# ___C___ = Closing; 
# ___O___ = Opening Quote; 
# ___R___ = Regular, two-sided open quote.
# levels(quotes$QU_COND)

gc()


# data cleansing ----------------------------------------------------------

# convert uppercase column names into lowercase to be consistent 
setnames(trades,tolower(colnames(trades))) 

# deal with time formatting
trades[, datetime := as.POSIXct(paste(date,time_m), format='%Y%m%d %H:%M:%OS')]

trades[, ':='(date   = strftime(datetime,'%Y-%m-%d'),
              time_m = strftime(datetime,'%H:%M:%S'))]

# only keep the trades and quotes in normal trading time
trades <- trades[time_m >= '09:30:00' & time_m <= '16:00:00']


# feature engineering -----------------------------------------------------

# check the sequence of timestamps or set key
any(shift(trades$datetime, type = 'lead') < trades$datetime, na.rm = T)
setkey(trades, datetime)

# get 1 min and 1 day intervals
trades[, mins := cut(datetime, '1 min')]
trades[, days := cut(datetime, '1 day')]

# mark the first and the last trades of the day
trades[, ':=' (last_trade_time  = last(datetime),
               first_trade_time = head(datetime)),
       by = days]

trades[, ':=' (last_trade  = ifelse(last_trade_time  == datetime, T, F),
               first_trade = ifelse(first_trade_time == datetime, T, F)),
       by = mins]

# LHS: 1 min return
# RHS: past 1 min return with lags; last trade and mean; volume difference
Tmin[, ':=' (priceAvg   = mean(price),
             volume     = sum(size),
             firstPrice = head(price, 1),
             lastPrice  = tail(price, 1)),
     by = mins]

# take lead and lag of priceAvg and volume
trades[, ':=' (priceAvg_1lead = shift(priceAvg,  type = 'lead'),
               priceAvg_1lag  = shift(priceAvg,  type = 'lag'),
               volumn_1lag    = shift(volume,    type = 'lag'))]

# caclulate RHS variables
trades[, ':='(R1min_future    = (trades$priceAvg_1lead - trades$priceAvg)/trades$priceAvg,
              R1min_past      = (trades$priceAvg_1lag  - trades$priceAvg)/trades$priceAvg_1lag,
              LPminusAvg      = trades$lastPrice       - trades$PriceAvg,
              Volume1min_past = trades$volume          - trades$volumn_1lag),
       by = mins]

# clear first_trade_time and last_trade_time
trades[, c('first_trade_time', 'last_trade_time'):= NULL]

# clear the first and last trades of the day, deal with overnight change, lead and lag
trades <- trades[first_trade == F & last_trade == F, ]

# set mins as keys
setkey(trades, mins)


# modeling ----------------------------------------------------------------

# split dataset to 80 training set, 20 out of fold set
oof   <- trades[(nrow(trades)*0.8):nrow(trades), ]
train <- Tmin[1:(nrow(trades)*0.8), ]

# train a model to predict outcome #1
mod <- lm(R1min_future ~ R1min_past + LPminusAvg + Volume1min_past, 
          data = train) 

summary(mod)

# predict outcome on OOF data
oof$validation <- predict(mod, oof, type = "response")
oof$mins       <- as.POSIXct(oof$mins)

# plot prediction against price
modelfit1 <- 
  ggplot() + 
  geom_step(data = oof, aes(x = mins, y = R1min_future), color = "blue") +
  geom_step(data = oof, aes(x = mins, y = validation), color = "red") +
  xlab('Time') +
  ylab('Next Min Average Price')

# save graph to output
ggsave("BABA 1-min Average Price Prediction.png",
       path = paste0(rootpath, 'output'),
       plot = modelfit1)


