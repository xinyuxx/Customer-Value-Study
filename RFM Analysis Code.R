# # ===================================================
# GBA464: RFM analysis on CDNOW data
# Author: Xinyu Huang
# Description: Lab on functions and loops
# Data: CDNOW customer data (this time full data)
# Source: provided by Professor Bruce Hardie on
#   http://www.brucehardie.com/datasets/CDNOW_sample.zip
# ===================================================

# ====== CLEAR EVERYTHING ======
rm(list = ls())

# ====== READ TRIAL DATA =======

url <- 'https://dl.dropboxusercontent.com/s/xxfloksp0968mgu/CDNOW_sample.txt'
if (!file.exists('CDNOW_sample.txt')) {     # check whether data exists in local folder (prevents downloading every time)
    download.file(url, 'CDNOW_sample.txt')
}
df.raw <- read.fwf('CDNOW_sample.txt', width = c(6, 5, 9, 3, 8), stringsAsFactors = F)  # load data

# ====== Section 2: loading the data ======

df.raw[[1]] <- NULL # drop old id
names(df.raw) <- c("id", "date", "qty", "expd")

# a) generate year and month

# df.raw[[2]] = as_datetime(df.raw[[2]]) ...

year = df.raw[[2]] %/% 10000
month = df.raw[[2]] %/% 100 %%100
df.raw[[2]] <- NULL
df.raw = cbind(df.raw, year, month)


#     ****************df.raw = df.raw[, c('a', 'b', 'd', 'c')]

# b) aggregate into monthly data with number of trips and total expenditure

trips = 1
df.raw = cbind(df.raw, trips)
df.new <- aggregate(cbind(qty,expd,trips) ~ id + year + month, data = df.raw, FUN = sum)


# c) generate a table of year-months, merge, replace no trip to zero.
# Hint: how do you deal with year-months with no trip? These periods are not in the original data,
#   but you might need to have these periods when you calcualte RFM, right?
# Consider expanding the time frame using expand.grid() but you do not have to.

# max(df.raw$id) = 1000
# length(unique(df.raw$id)) = 1000
# there are 1000 id
id = c(1:1000)
y = c(1997)
m = c(1:12)
expand_1 = expand.grid(year=y,month=m,id=id, KEEP.OUT.ATTRS = TRUE, stringsAsFactors = TRUE)
y = c(1998)
m = c(1:6)
expand_2 = expand.grid(year=y,month=m,id=id, KEEP.OUT.ATTRS = TRUE, stringsAsFactors = TRUE)
expand = rbind(expand_1, expand_2)
df.new = merge(df.new, expand, by = c('year', 'month', 'id'), all = TRUE)
df.new[is.na(df$trips),6]=0  # ********** 4:6???

df.new = df.new[order(df.new$id), ] # ********** order

# now we should have the dataset we need; double check to make sure that every consumer is in every period

# ====== Section 3.1: recency ======
# use repetition statement, such as a "for-loop", to generate a recency measure for each consumer 
#   in each period. Hint: if you get stuck here, take a look at Example 3 when we talked about "for-loops"
#   call it df$recency

df.new$recency = NA

for (i in 1:1000){
  for (y in 1997:1998){
    if (y==1997){
      lastmonth = NA
      for (m in 1:12){
        df.new$recency[df.new$id == i & df.new$year == y & df.new$month == m] = 
          df.new$month[df.new$id == i & df.new$year == y & df.new$month == m]-lastmonth
        if (!is.na(df.new$qty[df.new$id == i & df.new$year == y & df.new$month == m])){
          lastmonth = df.new$month[df.new$id == i & df.new$year == y & df.new$month == m]
        }
      }
    }else{
      lastmonth = lastmonth - 12
      for (m in 1:6){
        df.new$recency[df.new$id == i & df.new$year == y & df.new$month == m] = 
          (df.new$month[df.new$id == i & df.new$year == y & df.new$month == m]-lastmonth)
        if (!is.na(df.new$qty[df.new$id == i & df.new$year == y & df.new$month == m]) ){
          lastmonth = df.new$month[df.new$id == i & df.new$year == y & df.new$month == m]
        }
      }
    }
  }
}
df.new$lastmonth=NULL

# ====== Section 3.2: frequency ======
# first define quarters and collapse/merge data sets
#   quarters should be e.g. 1 for January-March, 1997, 2 for April-June, 1997, ...
#   and there should be 8 quarters in the two-year period
#   Next, let's define frequency purchase occasions in PAST QUARTER
#   Call this df$frequency
df.new$quarters=NA
for (i in 1:1000){
  for (y in 1997:1998){
    if (y==1997){
      for (m in 1:12){
        df.new$quarters[df.new$id == i & df.new$year == y & df.new$month == m] = 
          switch(df.new$month[df.new$id == i & df.new$year == y & df.new$month == m], 1,1,1,2,2,2,3,3,3,4,4,4) # ********why cant rep(1,3), rep(2,3)
      }
    }else{
      lastmonth = lastmonth - 12
      for (m in 1:6){
        df.new$quarters[df.new$id == i & df.new$year == y & df.new$month == m] = 
          switch(df.new$month[df.new$id == i & df.new$year == y & df.new$month == m], 5,5,5,6,6,6) 
      }
    }
  }
}

# aggregate trips, by quarter&id, sum --merge with df.new
# for loop --match to previous quarter
df.new[is.na(df.new$qty),4]=0
df.new[is.na(df.new$expd),5]=0
df.new[is.na(df.new$trips),6]=0
df = aggregate(formula = trips ~ id + quarters,
                data = df.new,
                FUN = sum
)

df.new$frequency = NA
for (i in 1:1000){
  # frequencysum = 0
  for (q in 2:6){
    df.new$frequency[df.new$id == i & df.new$quarters == q] = 
      # frequencysum + 
      df$trips[df$id == i & df$quarters == q-1]
    # frequencysum = frequencysum + df$trips[df$id == i & df$quarters == q-1]
  }
}
df = df.new
remove(df.new)

# ====== Section 3.3: monetary value ======
# average monthly expenditure in the months with trips (i.e. when expenditure is nonzero)
#   for each individual in each month, find the average expenditure from the beginning to 
#   the PAST MONTH. Call this df$monvalue
df$seq = rep(1:18, 1000)
df$monvalue=NA
for (i in 1:1000){
  spending_month=0
  spending_total=0
  for (n in 2:18){
    if (df$expd[df$id == i & df$seq == n-1] != 0) {
      spending_month = spending_month + 1
      spending_total = spending_total + df$expd[df$id == i & df$seq == n-1]
    }
    df$monvalue[df$id == i & df$seq == n] = spending_total/spending_month
  }
}

# ====== Section 4: Targeting using RFM ======
# now combine these and construct an RFM index
#   You only need to run this section.

b1 <- -0.05
b2 <- 3.5
b3 <- 0.05

df$index <- b1*df$recency + b2*df$frequency + b3*df$monvalue

# validation: check whether the RFM index predict customer purchase patterns
# Order your sample (still defined by keys of consumer-year-month) based on the RFM index. 
#   Split your sample into 10 groups. The first group is top 10% in terms of
#   the RFM index; second group is 10%-20%, etc.
# Make a bar plot on the expected per-trip revenue that these consumers generate and comment on 
#   whether the RFM index help you segment which set of customers are "more valuable"

df=df[order(df$index),]
cut_offs = quantile(df$index, probs = seq(0, 1, 0.1), na.rm = TRUE, names = TRUE)


df$segment = NA
df$segment[df$index>=cut_offs[1] & df$index<cut_offs[2]] = 1
df$segment[df$index>=cut_offs[2] & df$index<cut_offs[3]] = 2
df$segment[df$index>=cut_offs[3] & df$index<cut_offs[4]] = 3
df$segment[df$index>=cut_offs[4] & df$index<cut_offs[5]] = 4
df$segment[df$index>=cut_offs[5] & df$index<cut_offs[6]] = 5
df$segment[df$index>=cut_offs[6] & df$index<cut_offs[7]] = 6
df$segment[df$index>=cut_offs[7] & df$index<cut_offs[8]] = 7
df$segment[df$index>=cut_offs[8] & df$index<cut_offs[9]] = 8
df$segment[df$index>=cut_offs[9] & df$index<cut_offs[10]] = 9
df$segment[df$index>=cut_offs[10] & df$index<cut_offs[11]] = 10

avg_expd = aggregate(formula = expd ~ segment,
               data = df,
               FUN = mean
)

barplot(avg_expd$expd, names.arg = avg_expd$segment, xlab = 'deciles in the RFM index',
        ylab = 'average expenditure', main = 'average expenditure by deciles in the RFM index')

# The RFM index do help to segment which set of customers are "more valuable"



