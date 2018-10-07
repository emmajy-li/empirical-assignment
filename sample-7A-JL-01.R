
# housekeeping -----------------------------------------------------------

rm(list = ls())

options(warn = -1)
options(digits.secs = 6)


# packages ----------------------------------------------------------------

if(!require(pacman)) install.packages("pacman"); require(pacman)

p_load(data.table, rstudioapi, lubridate, ggplot2, scales)


# setting up path ---------------------------------------------------------

code_path <- setwd(dirname(getActiveDocumentContext()$path))

rootpath  <- gsub(basename(code_path), "", code_path)


# importing data -------------------------------------------------------------

# garbage collection
gc()

# read ten lines of code to take a look at the columns
trace_test <- fread(paste0(rootpath, 'data/TRACE_2017Q4.csv'), header = T, sep = ',', nrows = 10)

str(trace_test)

trace_test <- NULL

# TRACE data: Trade Reporting And Compliance Engine
# cusip_id:          CUSIP (bond indicator, same CUSIP, perfect substitute)
# bond_sym_id:       bond symbol 
# company_symbol:    company symbol
# trd_exctn_dt:      trade execution date
# trd_exctn_tm:      trade execution time
# trc_st:            trade status (M: normal, N: cancelled, O: modified)
# ascii_rptd_vol_tx: reported volume (Max of 5MM for IG bonds and 1MM for HY bonds.)
# rptd_pr:           reported price
# side:              buy-sell indicator
# rptg_party_type:   reporting party
# contra_party_type: contra party

colclass_trace <- c( 'cusip_id'          = 'character',
                     'bond_sym_id'       = 'character',
                     'company_symbol'    = 'character',
                     'trd_exctn_dt'      = 'character',
                     'trd_exctn_tm'      = 'character',
                     'trc_st'            = 'factor',
                     'ascii_rptd_vol_tx' = 'character',
                     'rptd_pr'           = 'double',
                     'side'              = 'factor',
                     'rptg_party_type'   = 'factor',
                     'contra_party_type' = 'factor'
)

trace <- fread(paste0(rootpath, 'data/TRACE_2017Q4.csv'), header = T, sep = ',', colClasses = colclass_trace)

# garbage collection
gc()


# data cleansing ----------------------------------------------------------

# take a look at the data structure
str(trace)

# time coversion
trace[, ':=' (datetime = parse_date_time(paste0(trd_exctn_dt, trd_exctn_tm), orders = "%Y%m%d %H:%M:%S", tz = "America/New_York"),
              date     = parse_date_time(trd_exctn_dt, orders = "%Y%m%d", tz = "America/New_York"),
              start    = parse_date_time(paste0(trd_exctn_dt, "08:00:00"), orders = "%Y%m%d %H:%M:%S", tz = "America/New_York"))]

setkey(trace, datetime)

trace[, hrs := cut(datetime, '1 hour')]

trade_tm_freq   <- trace[, .(num = .N), by = hrs]
trade_tm_freq_p <-  trade_tm_freq[order(rank(hrs)), ]

# plot # of trades against hours
trade_tm_freq_plt <- 
  ggplot(data = trade_tm_freq_p[6:52], aes(x = num, y = hrs)) + 
  geom_point() +
  labs(
    title = "Number of Trades by Half an Hour",
    x = "Number of Trades", y = "Datetime") +
  theme(
    panel.background = element_rect(fill = "white", size     = 0.1,     linetype = "solid"),
    panel.grid.major = element_line(size = 0.1,     linetype = 'solid', colour   = "grey"))

trace[, difftime := datetime - start]

nrow(trace[difftime >= 0 & difftime <= 32400, ])
# 3,345,375

nrow(trace[difftime < 0, ])
# 43,031

nrow(trace[difftime > 32400, ])
# 22,770

# check cancelled trade before cleansing
trace[, .(num = .N), by = trc_st]

# trc_st     num
# 1:      M 3338119
# 2:      N   39816
# 3:      O   33241

# cleanse "T" ATS or alternative trading systems
trace <- trace[!(rptg_party_type == "T" & contra_party_type == "D"), ]
# 3,411,176 -> 3,281,007

# cleanse cancelled trade
# NOTICE: this method of matching cancelled and modified paris is not yet perfect. Please feel free to revise it.

for (num in 1:8) {
  trace[, paste0('cusip_id_lg', num)           := shift(cusip_id,          type = 'lag',  n = num)]
  trace[, paste0('trd_exctn_tm_lg', num)       := shift(trd_exctn_tm,      type = 'lag',  n = num)]
  trace[, paste0('rptd_pr_lg', num)            := shift(rptd_pr,           type = 'lag',  n = num)]
  trace[, paste0('ascii_rptd_vol_tx_lg', num)  := shift(ascii_rptd_vol_tx, type = 'lag',  n = num)]
  trace[, paste0('trc_st_lg', num)             := shift(trc_st,            type = 'lag',  n = num)]
  trace[, paste0('side_lg', num)               := shift(side,              type = 'lag',  n = num)]
  trace[, paste0('rptg_party_type_lg', num)    := shift(rptg_party_type,   type = 'lag',  n = num)]
  trace[, paste0('contra_party_type_lg', num)  := shift(contra_party_type, type = 'lag',  n = num)]
}

# create match variable
trace[             , match1 := ifelse(cusip_id_lg1 == cusip_id & trd_exctn_tm_lg1 == trd_exctn_tm & rptd_pr_lg1  == rptd_pr & ascii_rptd_vol_tx_lg1 == ascii_rptd_vol_tx & 
                           side_lg1 == side & rptg_party_type_lg1 == rptg_party_type & contra_party_type_lg1 == contra_party_type & trc_st_lg1 != trc_st, "T", "F")]
trace[match1 == "F", match2 := ifelse(cusip_id_lg2 == cusip_id & trd_exctn_tm_lg2 == trd_exctn_tm & rptd_pr_lg2  == rptd_pr & ascii_rptd_vol_tx_lg2 == ascii_rptd_vol_tx & 
                           side_lg2 == side & rptg_party_type_lg2 == rptg_party_type & contra_party_type_lg2 == contra_party_type & trc_st_lg2 != trc_st & match1 == "F", "T", "F")]
trace[match1 == "F" & match2 == "F", match3 := ifelse(cusip_id_lg3 == cusip_id & trd_exctn_tm_lg3 == trd_exctn_tm & rptd_pr_lg3  == rptd_pr & ascii_rptd_vol_tx_lg3 == ascii_rptd_vol_tx & 
                           side_lg3 == side & rptg_party_type_lg3 == rptg_party_type & contra_party_type_lg3 == contra_party_type & trc_st_lg3 != trc_st & match2 == "F", "T", "F")]
trace[match1 == "F" & match2 == "F" & match3 == "F", match4 := ifelse(cusip_id_lg4 == cusip_id & trd_exctn_tm_lg4 == trd_exctn_tm & rptd_pr_lg4  == rptd_pr & ascii_rptd_vol_tx_lg4 == ascii_rptd_vol_tx & 
                           side_lg4 == side & rptg_party_type_lg4 == rptg_party_type & contra_party_type_lg4 == contra_party_type & trc_st_lg4 != trc_st & match3 == "F", "T", "F")]
trace[match1 == "F" & match2 == "F" & match3 == "F" & match4 == "F", match5 := ifelse(cusip_id_lg5 == cusip_id & trd_exctn_tm_lg5 == trd_exctn_tm & rptd_pr_lg5  == rptd_pr & ascii_rptd_vol_tx_lg5 == ascii_rptd_vol_tx & 
                           side_lg5 == side & rptg_party_type_lg5 == rptg_party_type & contra_party_type_lg5 == contra_party_type & trc_st_lg5 != trc_st & match4 == "F", "T", "F")]
trace[match1 == "F" & match2 == "F" & match3 == "F" & match4 == "F" & match5 == "F" , match6 := ifelse(cusip_id_lg6 == cusip_id & trd_exctn_tm_lg6 == trd_exctn_tm & rptd_pr_lg6  == rptd_pr & ascii_rptd_vol_tx_lg6 == ascii_rptd_vol_tx & 
                           side_lg6 == side & rptg_party_type_lg6 == rptg_party_type & contra_party_type_lg6 == contra_party_type & trc_st_lg6 != trc_st & match5 == "F", "T", "F")]
trace[match1 == "F" & match2 == "F" & match3 == "F" & match4 == "F" & match5 == "F" & match6 == "F" , match7 := ifelse(cusip_id_lg7 == cusip_id & trd_exctn_tm_lg7 == trd_exctn_tm & rptd_pr_lg7  == rptd_pr & ascii_rptd_vol_tx_lg7 == ascii_rptd_vol_tx & 
                           side_lg7 == side & rptg_party_type_lg7 == rptg_party_type & contra_party_type_lg7 == contra_party_type & trc_st_lg7 != trc_st & match6 == "F", "T", "F")]
trace[match1 == "F" & match2 == "F" & match3 == "F" & match4 == "F" & match5 == "F" & match6 == "F" & match7 == "F" , match8 := ifelse(cusip_id_lg8 == cusip_id & trd_exctn_tm_lg8 == trd_exctn_tm & rptd_pr_lg8  == rptd_pr & ascii_rptd_vol_tx_lg8 == ascii_rptd_vol_tx & 
                           side_lg8 == side & rptg_party_type_lg8 == rptg_party_type & contra_party_type_lg8 == contra_party_type & trc_st_lg8 != trc_st & match7 == "F", "T", "F")]

# cancelled pairs captured by 8 match variables
nrow(trace[(match1 == 'T' | match2 == 'T' | match3 == 'T' | match4 == 'T' | match5 == 'T' | match6 == 'T' | match7 == 'T' | match8 == 'T') & trc_st == "N", ])
# 33,238

trace[, ':=' (match_ld1 = shift(match1, type = 'lead', n = 1),
              match_ld2 = shift(match2, type = 'lead', n = 2),
              match_ld3 = shift(match3, type = 'lead', n = 3),
              match_ld4 = shift(match4, type = 'lead', n = 4),
              match_ld5 = shift(match5, type = 'lead', n = 5),
              match_ld6 = shift(match6, type = 'lead', n = 6),
              match_ld7 = shift(match7, type = 'lead', n = 7),
              match_ld8 = shift(match8, type = 'lead', n = 8)) ]

# matchable entries detected
nrow(trace[(match1 == 'T' | match2 == 'T' | match3 == 'T' | match4 == 'T' | match5 == 'T' | match6 == 'T' | match7 == 'T' | match8 == 'T' | 
              match_ld1 == 'T' | match_ld2 == 'T' | match_ld3 == 'T' | match_ld4 == 'T' | match_ld5 == 'T' | match_ld6 == 'T' | match_ld7 == 'T' | match_ld8 == 'T'), ])
# 87,651

# "all the same except for trc_st" cases:
# 1. match N with O in lag: deleted both N and O 
# 2. match N with M in lag: deleted both N and M
# 3. match O with M in lag: delete M and keep O
# 4. match O with N in lag: further investigation needed
# 5. match M with O in lag: further investigation needed
# 6. match M with N in lag: further investigation needed

# case 1

nrow(trace[((match1 == 'T' | match2 == 'T' | match3 == 'T' | match4 == 'T' | match5 == 'T' | match6 == 'T' | match7 == 'T' | match8 == 'T' | match_ld1 == 'T' | match_ld2 == 'T' | match_ld3 == 'T' | match_ld4 == 'T' | match_ld5 == 'T' | match_ld6 == 'T' | match_ld7 == 'T' | match_ld8 == 'T') & 
        trc_st == "N" & 
        (trc_st_lg1 == 'O' | trc_st_lg2 == 'O' | trc_st_lg3 == 'O' | trc_st_lg4 == 'O' | trc_st_lg5 == 'O' | trc_st_lg6 == 'O' | trc_st_lg7 == 'O' | trc_st_lg8 == 'O')), ])
# 2,854

# case 2
nrow(trace[((match1 == 'T' | match2 == 'T' | match3 == 'T' | match4 == 'T' | match5 == 'T' | match6 == 'T' | match7 == 'T' | match8 == 'T' | match_ld1 == 'T' | match_ld2 == 'T' | match_ld3 == 'T' | match_ld4 == 'T' | match_ld5 == 'T' | match_ld6 == 'T' | match_ld7 == 'T' | match_ld8 == 'T') & 
              trc_st == "N" & 
              (trc_st_lg1 == 'M' | trc_st_lg2 == 'M' | trc_st_lg3 == 'M' | trc_st_lg4 == 'M' | trc_st_lg5 == 'M' | trc_st_lg6 == 'M' | trc_st_lg7 == 'M' | trc_st_lg8 == 'M')), ])
# 37,175

# case 3
nrow(trace[((match1 == 'T' | match2 == 'T' | match3 == 'T' | match4 == 'T' | match5 == 'T' | match6 == 'T' | match7 == 'T' | match8 == 'T' | match_ld1 == 'T' | match_ld2 == 'T' | match_ld3 == 'T' | match_ld4 == 'T' | match_ld5 == 'T' | match_ld6 == 'T' | match_ld7 == 'T' | match_ld8 == 'T') & 
              trc_st == "O" & 
              (trc_st_lg1 == 'M' | trc_st_lg2 == 'M' | trc_st_lg3 == 'M' | trc_st_lg4 == 'M' | trc_st_lg5 == 'M' | trc_st_lg6 == 'M' | trc_st_lg7 == 'M' | trc_st_lg8 == 'M')), ])
# 6,268

# eliminate 1
trace[, delete1 := ifelse((match1 == 'T' | match2 == 'T' | match3 == 'T' | match4 == 'T' | match5 == 'T' | match6 == 'T' | match7 == 'T' | match8 == 'T' | match_ld1 == 'T' | match_ld2 == 'T' | match_ld3 == 'T' | match_ld4 == 'T' | match_ld5 == 'T' | match_ld6 == 'T' | match_ld7 == 'T' | match_ld8 == 'T') & 
                            trc_st == "N" & 
                            (trc_st_lg1 == 'O' | trc_st_lg2 == 'O' | trc_st_lg3 == 'O' | trc_st_lg4 == 'O' | trc_st_lg5 == 'O' | trc_st_lg6 == 'O' | trc_st_lg7 == 'O' | trc_st_lg8 == 'O'),
                          "D", "K"), ]

# eliminate 2
trace[, delete2 := ifelse((match1 == 'T' | match2 == 'T' | match3 == 'T' | match4 == 'T' | match5 == 'T' | match6 == 'T' | match7 == 'T' | match8 == 'T' | match_ld1 == 'T' | match_ld2 == 'T' | match_ld3 == 'T' | match_ld4 == 'T' | match_ld5 == 'T' | match_ld6 == 'T' | match_ld7 == 'T' | match_ld8 == 'T') & 
                            trc_st == "N" & 
                            (trc_st_lg1 == 'M' | trc_st_lg2 == 'M' | trc_st_lg3 == 'M' | trc_st_lg4 == 'M' | trc_st_lg5 == 'M' | trc_st_lg6 == 'M' | trc_st_lg7 == 'M' | trc_st_lg8 == 'M'),
                          "D", "K"), ]

# eliminate 3
trace[, delete3 := ifelse((match1 == 'T' | match2 == 'T' | match3 == 'T' | match4 == 'T' | match5 == 'T' | match6 == 'T' | match7 == 'T' | match8 == 'T' | match_ld1 == 'T' | match_ld2 == 'T' | match_ld3 == 'T' | match_ld4 == 'T' | match_ld5 == 'T' | match_ld6 == 'T' | match_ld7 == 'T' | match_ld8 == 'T') & 
                            trc_st == "O" & 
                            (trc_st_lg1 == 'M' | trc_st_lg2 == 'M' | trc_st_lg3 == 'M' | trc_st_lg4 == 'M' | trc_st_lg5 == 'M' | trc_st_lg6 == 'M' | trc_st_lg7 == 'M' | trc_st_lg8 == 'M'),
                          "D", "K"), ]

trace[, delete3_ := ifelse(delete3 == "D" & trc_st == "O", "K", delete3)]

# elimination
trace <- trace[delete1 == "K" & delete2 == "K" & delete3_ == "K", ]

# 3,281,007 -> 3,241,582

# eliminated 39,425 entries
 
# check # of cancelled trades failed to capture using this method
nrow(trace[match1 == 'F' & match2 == 'F' & match3 == 'F' & match4 == 'F' & match5 == 'F' & match6 == 'F' & match7 == 'F' & match8 == 'F' & trc_st == "N",])
# 1,754