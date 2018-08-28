# house keeping -----------------------------------------------------------

rm(list = ls())

options(warn = -1)

# packages ----------------------------------------------------------------

if(!require(pacman)) install.packages("pacman"); require(pacman)

p_load(data.table, scales, ggplot2, rstudioapi, lubridate)


# setting up path ---------------------------------------------------------

code_path <- setwd(dirname(getActiveDocumentContext()$path))

rootpath  <- gsub(basename(code_path), "", code_path)


# import data -------------------------------------------------------------

ticker <- 'TRR'
date   <- '2011-11-11'

quotes <- fread(file = paste0(rootpath, "data/", sprintf('%s-nbbo-%s.csv', ticker, date)))


# data cleansing ----------------------------------------------------------

# check crossed market quotes
quotes[bestbid > bestask, ]

# cleanse crossed market quotes
quotes <- quotes[bestbid < bestask, ]


# data analysis - best bid ask price and size -----------------------------

# count of best bid size
bestbid_size <- quotes[, .(count = .N), by=bestbidsize]
bestbid_size[order(rank(bestbidsize), count)]

# calculate the mean and standard deviation of best bid price per best bid size
quotes[, .(m_bid = mean(bestbid), std_bid = sd(bestbid)), by = bestbidsize]

# calcualte the mean and standard deviation of best bid price per best bid size 
# which has count greater than N
N <- 10

quotes[bestbidsize %in% bestbid_size[count > N, ]$bestbidsize, 
       .(m_bid = mean(bestbid), std_bid = sd(bestbid)), 
       by = bestbidsize]


# data analysis - bid ask spread ------------------------------------------

# bid-ask spread
quotes$`bid-ask spread` <- round(quotes$bestask - quotes$bestbid, digits = 2)

# count of bid ask spread 
spread <- quotes[, .(count = .N), by = "bid-ask spread"]
spread[order(rank(`bid-ask spread`), count)]

# calculate spread 1-minute average 
quotes$datetime <- as.POSIXct(paste(quotes$date, quotes$time_m), format='%Y%m%d %H:%M')
spread1m        <- quotes[, .(m_spread = mean(`bid-ask spread`)), by = datetime]

# plot out the 1-minute average spread
plot_spread1m <- 
  ggplot(spread1m,aes(x = datetime, y = m_spread)) +
  geom_line() +
  ggtitle(sprintf('%s 1-min average bid-ask spread on %s', ticker, date)) +
  labs(x = 'Time', y = 'USD cents') +
  scale_x_datetime(breaks = date_breaks("30 mins"), date_labels = "%H:%M") +
  scale_y_continuous(breaks = c(seq(0, 1, by = 0.01)))  # attention: adjust for scales per stock's micro behavior

# save plot to result
ggsave(sprintf('%s 1-min average bid-ask spread on %s.', ticker, date), 
       path = paste0(rootpath, "result"), 
       plot = plot_spread1m)  