setwd('/Users/lintzujeng/Documents/GitHub/R_project_test')
library(readxl)
library(tidyverse)
library(janitor)

#資料導入
guava  <- read_excel("Guava.xls" , col_names = FALSE)
pitaya <- read_excel("Pitaya.xls", col_names = FALSE)
banana <- read_excel("Banana.xls", col_names = FALSE)
tomato <- read_excel("Tomato.xls", col_names = FALSE)

####################################第一題######################################
#建立一個function<adjust>做資料整理
adjust <- function(m){
  df <- m[3:nrow(m),]#取需要的資料
  df <- row_to_names(dat = df, row_number = 1)#變更column names by row1
  df$平均價 = as.numeric(df$平均價)#change datatype to numeric
  df$平均價 = round(df$平均價, 2)#四捨五入到小數點以下第二位
  
  return(df)# return dataframe
}

guava.just <- adjust(guava)#adjust df(guava)
guava.mean = round(mean(guava.just$平均價), 2)#求mean
guava.sd = round(sd(guava.just$平均價), 2)#求standard deviation

pitaya.just <- adjust(pitaya)#adjust
pitaya.mean = round(mean(pitaya.just$平均價), 2)#求mean
pitaya.sd = round(sd(pitaya.just$平均價), 2)#求standard deviation

banana.just <- adjust(banana)#adjust
banana.mean = round(mean(banana.just$平均價), 2)#求mean
banana.sd = round(sd(banana.just$平均價), 2)#求standard deviation

tomato.just <- adjust(tomato)#adjust
tomato.mean = round(mean(tomato.just$平均價), 2)#求mean
tomato.sd = round(sd(tomato.just$平均價), 2)#求standard deviation


mean.history = c(banana.mean, guava.mean, pitaya.mean, tomato.mean)#vector存水果mean
price.max <- max(mean.history)#find max
print(paste('第一題：最高價為',price.max,'所以火龍果 紅肉 賣得最高'))#print result




####################################第二題######################################
#建立一個function<sampler>做sampling
sampler <- function(m){
  n = 1000 #sample 次數
  sample.mean <- rep(NA, times = n)#建立一個list存所有sampling的mean
  for (i in 1:n) {#use for loop to sampling
    sample.mean[i] <- mean(sample(m, 500))#把sample出來的mean存入sample.mean
  }
  excepted <- mean(sample.mean)#get expected value
  return(c(excepted, sample.mean))#return expected value
}

guava.excepted <- sampler(guava.just$平均價)[1]
guava.sammean <- sampler(guava.just$平均價)[-1]
plot.gua <- hist(guava.sammean, breaks = 30)

pitaya.excepted <- sampler(pitaya.just$平均價)[1]
pitaya.sammean <- sampler(pitaya.just$平均價)[-1]
plot.pit <- hist(pitaya.sammean, breaks = 30)

banana.excepted <- sampler(banana.just$平均價)[1]
banana.sammean <- sampler(banana.just$平均價)[-1]
plot.ban <- hist(banana.sammean, breaks = 30)

tomato.excepted <- sampler(tomato.just$平均價)[1]
tomato.sammean <- sampler(tomato.just$平均價)[-1]
plot.tom <- hist(tomato.sammean, breaks = 30)

#從左邊可以看出 excepted 約等於 population mean 所以 unbiased

fruit.excepted <- c(guava.excepted, pitaya.excepted, banana.excepted, tomato.excepted)
fruit.max <- round(max(fruit.excepted), 2)
print(paste('第二題：最高價為',fruit.max,'所以火龍果 紅肉 賣得最高'))#print result

####################################第三題######################################

hist(guava.just$平均價,  breaks = 50)#價格介在10~70之間，波動區間較小，但曲線較平緩，表示價格時常波動(max freq < 200，分散)
hist(pitaya.just$平均價, breaks = 50)#價格介在0~400之間，波動區間很大，但價格較不易改變(max freq > 600，集中)
hist(banana.just$平均價, breaks = 50)#價格介在20~80之間，波動區間較小，但曲線較平緩，表示價格時常波動(350 < max freq < 400，比較分散)
hist(tomato.just$平均價, breaks = 50)#價格介在50~150之間，波動區間較大，但價格較不易改變(max freq > 500，較集中)

####################################第四題######################################
#margin of error
#At 95% confidence, alpha = 0.05 and alpha/2 = 0.025
#z(0.025) is based on n = 1000 degrees of freedom.

#建立一個function<margin.error>做margin of error
margin.error <- function(sd){
  z.value = qnorm(0.05/2, lower.tail = F)#求 z-value
  merror = z.value * sd / sqrt(1000)#求 margin of error 
  
  return(merror)#return margin of error
}
#建立一個function<interval.estimate>做interval estimate
interval.estimate <- function(excepted, merror){
  interval <- rep(NA,2)#建立一個list存interval
  interval[1] = excepted - merror#lower interval
  interval[2] = excepted + merror#upper interval
  
  return(interval)#return interval list
}

#guava interval estimation
guava.merror <-  margin.error(guava.sd)
guava.interval <-  interval.estimate(guava.excepted, guava.merror)
#pitaya interval estimation
pitaya.merror <- margin.error(pitaya.sd)
pitaya.interval <- interval.estimate(pitaya.excepted, pitaya.merror)
#banana interval estimation
banana.merror <- margin.error(banana.sd)
banana.interval <- interval.estimate(banana.excepted, banana.merror)
#tomato interval estimation
tomato.merror <- margin.error(tomato.sd)
tomato.interval <- interval.estimate(tomato.excepted, tomato.merror)

####################################第五題######################################
gua.t <- t.test(guava.just$平均價, mu = guava.mean)
gua.t
pit.t <- t.test(pitaya.just$平均價, mu = pitaya.mean)
pit.t
ban.t <- t.test(banana.just$平均價, mu = banana.mean)
ban.t
tom.t <- t.test(tomato.just$平均價, mu = tomato.mean)
tom.t

