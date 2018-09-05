
# house keeping -----------------------------------------------------------

rm(list = ls())

options(digits.secs = 6)
options(warn        = -1)


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
                              'Bats BZX Exchange, Inc.')
)

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

# check the sequence of timestamps and set datetime as key
any(shift(trades$datetime, type = 'lead') < trades$datetime, na.rm = T)
setkey(trades, datetime)

# get 1 min intervals
trades[, mins := cut(datetime, '1 min')]

# LHS variables: 
# R1min_future: future 1-min return

# RHS variables: 
# LPminuspriceAvg: difference between last price and mean price
# Volume1min_past: difference between current volume sum and past 1-min volume sum
# R1min_past: past 1-min return 

Tmin <- trades[, .(priceAvg   = mean(price),
                   volume     = sum(size),
                   firstPrice = head(price, 1),
                   lastPrice  = tail(price, 1)),
               by = mins]

# take 1-min leads and lags of priceAvg and volume
Tmin[, ':=' (priceAvg_1lead = shift(priceAvg,  type = 'lead', n = 1),
             priceAvg_1lag  = shift(priceAvg,  type = 'lag' , n = 1),
             volumn_1lead   = shift(volume,    type = 'lead', n = 1),
             volumn_1lag    = shift(volume,    type = 'lag' , n = 1))]

# set mins as key in Tmin
setkey(Tmin, mins)

# get 1 day intervals
Tmin[, days := cut(as.POSIXct(mins), '1 day')]

# mark the first and the last minutes of the day
Tmin[, ':=' (last_min_record  = last(mins),
             first_min_record = first(mins)),
     by = days]

Tmin[, ':=' (last_min  = ifelse(last_min_record  == mins, T, F),
             first_min = ifelse(first_min_record == mins, T, F)),
     by = days]

# clear first_min_record and last_min_record
Tmin[, c('first_min_record', 'last_min_record'):= NULL]

# clear the first and last trades of the day, to deal with leads and lags
Tmin <- Tmin[first_min == F & last_min == F, ]

# caclulate RHS variables
Tmin[, ':='(LPminuspriceAvg = lastPrice       - priceAvg,
            Volume1min_past = volume          - volumn_1lag,
            R1min_future    = (priceAvg_1lead - Tmin$priceAvg)/Tmin$priceAvg,
            R1min_past      = (priceAvg_1lag  - Tmin$priceAvg)/priceAvg_1lag),
     by = mins]


# modeling ----------------------------------------------------------------

# split dataset to 80 training set, 20 out of fold set
oof   <- Tmin[(nrow(Tmin)*0.8 + 1):nrow(Tmin), ]
train <- Tmin[1:(nrow(Tmin)*0.8)             , ]

# train a model to predict outcome #1
mod <- lm(R1min_future ~ R1min_past + LPminuspriceAvg + Volume1min_past, 
          data = train) 

summary(mod)

# predict outcome on OOF data
oof$validation <- predict(mod, oof, type = "response")
oof$mins       <- as.POSIXct(oof$mins)

# plot prediction against price
modelfit1 <- 
  ggplot() + 
  geom_step(data = oof, aes(x = mins, y = R1min_future), color = 'blue') +
  geom_step(data = oof, aes(x = mins, y = validation), color = 'red') +
  labs(title = "BABA 1-min Average Price Prediction\n(Blue: Prediction; Red: Real)", x = "Time", y = "Next Min Average Price", color = "Legend Title\n")+
  theme(
    panel.background = element_rect(fill = "white", size = 0.1, linetype = "solid"),
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',colour = "grey")
  )

# save graph to output
ggsave("BABA 1-min Average Price Prediction.png",
       path = paste0(rootpath, 'output'),
       plot = modelfit1)
