###DGP of mortalities###
# rARIMA <- function(AR, MA, I, Periods, white_noise_var = 1, start = 0,
#                    c = -0.015, exp = F){
#   n_AR <- length(AR)
#   n_MA <- length(MA)
#   y <- rep(start, n_AR + 1)
#   epsilon <- rnorm(n_MA + Periods, 0, sqrt(white_noise_var))
#   for (t in 2:Periods){
#     y[t + n_AR] <- c+sum(AR*y[(t+n_AR-1):t])+sum(MA*epsilon[(n_MA+ t- 2):(t-1)])
#   }
#   y <- tail(y, -n_AR)
#   if (I>=1) for (i in 1:I) y <- cumsum(y)
#   if (exp) ts(exp(y)) else ts(y)
# }
# 
# a <- data.frame()
# for (i in 1:9){
#   a <- rbind(a,rARIMA(AR=c(0.3),MA=c(1,0.5), I=1, Periods = 87, exp = TRUE,
#              white_noise_var = 0.0003, start = -0.15), make.row.names = F)}
# a <- t(a)
# a <- as.data.frame(a)
# dir <- "https://raw.githubusercontent.com/asiergs/Mortality-Rates-Prediction-Project-Time-Series-Analysis/main/mortality_and_policies.csv"
# names(a) <- names(read.csv(dir,skip = 1))[-1]
# data <- a
# autoplot(ts(data[,1], start = 1935))
### FINISH DGP ###

rm(list = ls())

library(fpp2)
library(cowplot)
library(tseries)
library(goftest)
#install.packages("openxlsx")
library(openxlsx)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("svglite")
library(svglite)

# 0. Data import ----

### FROM EXCEL ###
#dir <- file.choose()
#data <- read.xlsx(dir, startRow = 2,sheet = "Hoja3")
#policies <- read.xlsx(dir,sheet = "Hoja3", rows = 1, colNames = F)

### FROM CSV GITHUB REPOSITORY ###
dir <- "https://raw.githubusercontent.com/asiergs/Mortality-Rates-Prediction-Project-Time-Series-Analysis/main/mortality_and_policies.csv"
data <- read.csv(dir,skip = 1)
policies <- read.csv(dir, header = F, nrows = 1)


data <- as.data.frame(data)
rownames(data) <- data[,1]
data <- data[,-1]

policies <- policies[-1]
policies <- t(policies)
rownames(policies) <- colnames(data)
colnames(policies) <- "number_of_policies" 

## 0.1 Visual check ----

svg("graphs\\qx_series.svg")
par(mfrow = c(3,3))
for (i in 1:ncol(data)) {
  plot(ts(data[,i], start = 1935), ylab = "prob", main = names(data)[i])  
}
dev.off()

# 1 ARIMA functional form estimation by Box-Jenkins procedure ----

WB = createWorkbook()

bar <- txtProgressBar(0,ncol(data),style=3)
for (m in 1:ncol(data)){
  
  sheet <- names(data)[m]
  addWorksheet(WB, sheetName = sheet)
  summary <- c()
  
  qx <- ts(data[,m], start = 1935)
  
  ggdraw() +
    draw_plot(ggAcf(qx), x = 0, y = .5, width = .5, height = .5) +
    draw_plot(ggPacf(qx), x = .5, y = .5, width = .5, height = .5) +
    draw_plot(autoplot(qx), x = 0, y = 0, width = 1, height = 0.5)
  
  # According to Lee and Carter model (1992), the log-mortatily rates time series
  # are usually a ARIMA model (random walk plus drift)
  
  qx_log <- log(qx)
  
  ggdraw() +
    draw_plot(ggAcf(qx_log), x = 0, y = .5, width = .5, height = .5) +
    draw_plot(ggPacf(qx_log), x = .5, y = .5, width = .5, height = .5) +
    draw_plot(autoplot(qx_log), x = 0, y = 0, width = 1, height = 0.5)
  
  stationary <- function(timeseries, significance = 0.05){
    
    results <- data.frame(c(adf.test(timeseries)$p.value,
                            pp.test(timeseries)$p.value,
                            kpss.test(timeseries)$p.value))
    
    rownames(results) <- c("ADF", "PP", "KPSS")
    
    stationary <- c(results[,][1] < significance,
                    results[,][2] < significance,
                    results[,][3] > significance)
    
    results <- cbind(results, stationary)
    colnames(results) <- c("p.value", paste("stationary",significance))
    results
  }
  
  stationary(qx_log)
  
  qx_log_diff <- diff(qx_log)
  
  ggdraw() +
    draw_plot(ggAcf(qx_log_diff), x = 0, y = .5, width = .5, height = .5) +
    draw_plot(ggPacf(qx_log_diff), x = .5, y = .5, width = .5, height = .5) +
    draw_plot(autoplot(qx_log_diff), x = 0, y = 0, width = 1, height = 0.5)
  
  stationary(qx_log_diff)
  
  Arima(qx_log, c(0,1,0), include.drift = TRUE)
  Arima(qx_log, c(1,1,0), include.drift = TRUE)
  Arima(qx_log, c(0,1,1), include.drift = TRUE)
  Arima(qx_log, c(2,1,0), include.drift = TRUE)
  Arima(qx_log, c(2,1,1), include.drift = TRUE)
  Arima(qx_log, c(1,1,2), include.drift = TRUE)
  Arima(qx_log, c(2,1,2), include.drift = TRUE)
  
  ARIMA_list <- list(c(0,1,0),
                     c(1,1,0),
                     c(0,1,1),
                     c(1,1,1),
                     c(0,1,2),
                     c(1,1,2),
                     c(2,1,0),
                     c(2,1,1),
                     c(2,1,2))
  
  # select k=n depending to the ARIMA model to be tested
  for (k in 1:length(ARIMA_list)){
  
    ARIMA <- ARIMA_list[[k]]
    
    model <- Arima(qx_log, ARIMA, include.drift = TRUE)
    
    ARIMA <- arimaorder(model)
    
    # 2 Diagnosis ----
    
    ## 2.1 Significance of estimators----
    
    I <- ARIMA["d"]
    
    coef <- model$coef
    AR <- coef[paste("ar",1:5, sep = "")]
    AR[is.na(AR)] <- 0
    
    MA <- coef[paste("ma",1:5, sep = "")]
    MA[is.na(MA)] <- 0
    
    c <- coef["drift"]
    
    I <- ARIMA["d"]
    
    res <- resid(model)
    
    #checkresiduals(model)
    
    # since in some cases, if not all, normality is rejected, bootstrap shall
    # be used for the parameters significance estimation, additionally is a more
    # robust method in most cases.
    
    rARIMA.boot <- function(ARIMA,AR, I, MA, c, length, res){
      y <- rep(0,5)
      residuals <- sample(res, 50+length, replace = TRUE)
      for (i in 6:(50+length)){
        y[i] <- sum(AR*y[i-c(1:5)]) + sum(MA*residuals[i-c(1:5)]) + residuals[i] + c
      }
      if (I > 0) for (i in 1:I) y <- cumsum(y)
      # the the first part of the time series (50) is not taken since the first
      # values start in 0 and for an AR could not be realistic
      y <- y[(50-I):(50+length-I)]
      # sometimes the bootstrap creates non stationary series so it returns error
      boot_model <- try(Arima(y, ARIMA, include.drift = TRUE), silent = T)
      if (typeof(boot_model)!="character") boot_model$coef else NA
    }
    
    sim_size <- 1000
    
    # variables to store the bootstraped parameters (UP TO ARMA(5,d,5))
    AR_boot <- matrix(0,nrow = sim_size, ncol = 5,
                      dimnames = list(c(),paste("ar",1:5, sep = "")))
    MA_boot <- matrix(0,nrow = sim_size, ncol = 5,
                      dimnames = list(c(),paste("ma",1:5, sep = "")))
    drift_boot <- c()
    
    length <- length(qx_log_diff)
    
    for(i in 1:sim_size){
      boot_coef <- rARIMA.boot(ARIMA,AR,I,MA,c,length,res)
      AR_boot[i,] <- boot_coef[paste("ar",1:5, sep = "")]
      MA_boot[i,] <- boot_coef[paste("ma",1:5, sep = "")]
      drift_boot[i] <- boot_coef["drift"]
    }
    
    ## 2.2 Residuals normality test ----
    
    # estimated=TRUE has not been used since the sample is so small that
    # the p.value result is so random it does not provide trustful information
    cvm <- cvm.test(res,"pnorm", mean= 0, sd = sd(res))
    ad <- ad.test(res,"pnorm", mean = 0, sd = sd(res))
    jb <- jarque.bera.test(res)
    # H0: normality
    
    ## 2.3 Ljung Box test for residuals incorrelation ----
    
    Ljung_Box <- Box.test(res, lag = 10, c("Ljung-Box"))
    # H0: incorrelation
    
    ## 2.4 Breusch Pagan /Bartlett test for residuals homocedasticity ----
    
    bp_data <- cbind(res^2,lag(qx_log_diff,-1), lag(qx_log_diff,-2),
                  lag(qx_log_diff,-3),lag(qx_log_diff,-4),lag(qx_log_diff,-5),
                  lag(res,-1),lag(res,-2),lag(res,-3),
                  lag(res,-4),lag(res,-5))
    
    colnames(bp_data) <- c("res2", paste("y_",1:5, sep = ""),
                        paste("res_",1:5, sep = ""))
    
    n_AR <- ARIMA["p"]
    n_MA <- ARIMA["q"]
    f_AR <- c()
    f_MA <- c()
    if (n_AR > 0) f_AR <- paste(" + ",colnames(bp_data)[2:(1+n_AR)],
                                        collapse = "", sep = "")
    if (n_MA > 0) f_MA <- paste(" + ",colnames(bp_data)[7:(6+n_MA)],
                                collapse = "", sep = "")
    
    formula <- paste("res2 ~ 1",f_AR,f_MA,sep = "")
    formula <- as.formula(formula)
    
    # BP test will be done for all ARIMA models but ARIMA(0,d,0), for the
    # ARIMA(0,1,0) the 
    if ((n_AR+n_MA)>0){
      fitaux <- lm(formula, data = bp_data)
      df <- n_AR + n_MA
      bp.statistic<-(length(res)-max(n_AR,n_MA)-df)*summary(fitaux)$r.squared  
      bp_b.pvalue<-pchisq(bp.statistic,df)
      
    } else {
      # A Bartlett test will be perfomed for the ARIMA(0,d,0) since the BP test
      # can not be performed for this cases. The sample is divided in halfs
      # since it can be observed a possible change of variance in that level
      div <- 2
      size <- length(res)
      max <- size/div
      y <- seq_along(res)
      chunks <- split(res, ceiling(y/max))
      bp_b.pvalue <- bartlett.test(chunks)$p.value
    }
    # H0: homocedasticity
    
    
    
    ## 2.5 t test for residuals zero mean ----
    t <- t.test(res)
    # H0: zero mean
    
    ## 2.6 Summary ----
    
    ARIMA_name <- paste(ARIMA, collapse = ",")
    ARIMA_name <- paste(c("(",ARIMA_name,")"), collapse = "")
    
    H0_lim <- function(AR_boot, alpha = 0.05){
      limits <- quantile(AR_boot,c(alpha/2,1-alpha/2),na.rm = TRUE)
      H0 <- sum(limits > 0)
      limits[3] <- if (is.na(H0)) NA else
        if (H0 == 1) "NOT REJECTED" else "REJECTED"
      limits
    }
    
    results <- data.frame(time_series = paste(names(data)[m],"_log"),
                          ARIMA = ARIMA_name,
                          AICc = model$aicc,
                          BIC = model$bic,
                          AR1 = AR[1],
                          AR1_H0_inf = as.numeric(H0_lim(AR_boot[,1])[1]),
                          AR1_H0_sup = as.numeric(H0_lim(AR_boot[,1])[2]),
                          AR1_H0 = H0_lim(AR_boot[,1])[3],
                          AR2 = AR[2],
                          AR2_H0_inf = as.numeric(H0_lim(AR_boot[,2])[1]),
                          AR2_H0_sup = as.numeric(H0_lim(AR_boot[,2])[2]),
                          AR2_H0 = H0_lim(AR_boot[,2])[3],
                          MA1 = MA[1],
                          MA1_H0_inf = as.numeric(H0_lim(MA_boot[,1])[1]),
                          MA1_H0_sup = as.numeric(H0_lim(MA_boot[,1])[2]),
                          MA1_H0 = H0_lim(MA_boot[,1])[3],
                          MA2 = MA[2],
                          MA2_H0_inf = as.numeric(H0_lim(MA_boot[,2])[1]),
                          MA2_H0_sup = as.numeric(H0_lim(MA_boot[,2])[2]),
                          MA2_H0 = H0_lim(MA_boot[,2])[3],
                          drift = c,
                          drift_H0_inf = as.numeric(H0_lim(drift_boot)[1]),
                          drift_H0_sup = as.numeric(H0_lim(drift_boot)[2]),
                          drift_H0 = H0_lim(drift_boot)[3],
                          Normality_CVM_pvalue = cvm$p.value,
                          Normality_AD_pvalue = ad$p.value,
                          Normality_JB_pvalue = jb$p.value,
                          Incorrelation_LB = Ljung_Box$p.value,
                          Homocedasticity_BP_B = bp_b.pvalue,
                          Zero_mean = t$p.value, row.names = c())
                        
    summary <- rbind(summary,results)
  }
  
  # write data in xlsx file to export
  writeData(WB, sheet, summary, startCol = 10,startRow = 2, keepNA = TRUE)
  
  stationarity <- stationary(qx_log)
  writeData(WB, sheet, "I(0)", startCol = 2,startRow = 2)
  writeData(WB, sheet, stationarity, startCol = 2,startRow = 3, rowNames = TRUE)
  
  stationarity <- stationary(qx_log_diff)
  writeData(WB, sheet, "I(1)", startCol = 6,startRow = 2)
  writeData(WB, sheet, stationarity, startCol = 6,startRow = 3, rowNames = TRUE)
  
  setTxtProgressBar(bar, m)
}

saveWorkbook(wb = WB, file = 'identification_diagnosis_summary.xlsx',
             overwrite = TRUE)   

# save the graphs for the report

dir.create("graphs")
names <- paste(names(data),"_log_diff.svg",sep = "")
names <- paste("graphs\\",names,sep="")
for (i in 1:ncol(data)){
  qx <- ts(data[,i], start = 1935)
  qx_log_diff <- diff(log(qx))
  ggdraw() +
    draw_plot(ggAcf(qx_log_diff), x = 0, y = .5, width = .5, height = .5) +
    draw_plot(ggPacf(qx_log_diff), x = .5, y = .5, width = .5, height = .5) +
    draw_plot(autoplot(qx_log_diff), x = 0, y = 0, width = 1, height = 0.5)
  ggsave(names[i], width = 8, height = 6)
}

# 3. Prediction by bootstrap ----

# time horizon
h <- 1

## ARIMA models for prediction ----
ARIMAs <- list(q_67 = c(0,1,1),
               q_68 = c(0,1,1),
               q_69 = c(0,1,1),
               q_70 = c(0,1,1),
               q_71 = c(0,1,1),
               q_72 = c(0,1,1),
               q_73 = c(0,1,1),
               q_74 = c(0,1,1),
               q_75 = c(0,1,1))

# bootstrap

sim_size <- 10000

qx_log_boot <- matrix(nrow = sim_size, ncol = length(ARIMAs))

bar <- txtProgressBar(0,length(ARIMAs),style=3)
for(m in 1:length(ARIMAs)){

  # get ts data
  qx <- ts(data[,m], start = 1935)
  qx_log <- log(qx)
  
  ARIMA <- ARIMAs[[1]]
  
  model <- Arima(qx_log,ARIMA,include.drift = TRUE)

  ARIMA <- arimaorder(model)
  
  I <- ARIMA["d"]
  n_remove <- max(ARIMA["p"], ARIMA["q"])+I
  if (n_remove>0) {
    res <- resid(model)[-c(1:n_remove)]
  } else res <- resid(model)[-1]
  
  coef <- model$coef
  AR <- coef[paste("ar",1:5, sep = "")]
  AR[is.na(AR)] <- 0
  
  MA <- coef[paste("ma",1:5, sep = "")]
  MA[is.na(MA)] <- 0
  
  c <- coef["drift"]
  
  I <- ARIMA["d"]
  
  ARIMA.boot.pred <- function(ARIMA,AR, I, MA, c, res, h, qx_log){
    length <- length(qx_log)
    y <- rep(0,5)
    residuals <- sample(res, 50+length, replace = TRUE)
    for (i in 6:(50+length)){
      y[i] <- sum(AR*y[i-c(1:5)]) + sum(MA*residuals[i-c(1:5)]) + residuals[i] + c
    }
    if (I > 0) for (i in 1:I) y <- cumsum(y)
    # the the first part of the time series (50) is not taken since the first
    # values start in 0 and for an AR could not be realistic
    y <- y[(50-I):(50+length-I)]
    # sometimes the bootstrap creates non stationary series so it returns error
    boot_model <- try(Arima(y, ARIMA, include.drift = TRUE), silent = T)
    if (typeof(boot_model)=="character") return(rep(NA,h))
    # calibration of new bootstraped model
    coef <- boot_model$coef
    AR <- coef[paste("ar",1:5, sep = "")]
    AR[is.na(AR)] <- 0
    MA <- coef[paste("ma",1:5, sep = "")]
    MA[is.na(MA)] <- 0
    c <- coef["drift"]
    # prediction for h periods
    y <- qx_log
    if (I > 0) for (i in 1:I) y <- diff(y)
    # last 5 terms of residuals and data are taken to predict up to ARMA(5,d,5)
    resid_boot <- c(tail(res,5),sample(res,h,replace = TRUE))
    y <- tail(y,5)
    y <- as.vector(y)
    for (i in 1:h){
      y[i+5] <- sum(AR*y[c(5:1)+i-1]) + sum(MA*resid_boot[c(5:1)+i-1]) +
        resid_boot[i+5] + c
    }
    if (I > 0) for (i in 1:I) y <- cumsum(y)
    # recover the level lost by differenciation by making equal latest known value
    pred <- y-y[5]+tail(qx_log,1)[1]
    tail(pred,h)
  }
  
  for(j in 1:sim_size){
    qx_log_boot[j,m] <- ARIMA.boot.pred(ARIMA,AR, I, MA, c, res, h, qx_log)
  }
  setTxtProgressBar(bar, m)
}

qx_log_boot <- na.omit(qx_log_boot)
qx_boot <- exp(qx_log_boot)

# create graphs of the residuals
dir.create("graphs\\residuals_meta")
for (i in 1:length(ARIMAs)){
  qx <- ts(data[,i], start = 1935)
  qx_log <- log(qx)
  model <- Arima(qx_log,ARIMAs[[i]],include.drift = TRUE)
  name <- paste("graphs\\residuals_meta\\",
                names(data)[i],"_residuals.xmf",sep = "")
  win.metafile(name, width = 8, height = 6)
  checkresiduals(model)
  dev.off()
}

# create graphs of the predictions
svg("graphs\\predictions.svg")
par(mfrow = c(3,3))
for (i in 1:ncol(data)) {
  hist(qx_boot[,i], prob = TRUE,
       main = paste(names(data)[i],"one period prediction"),
       breaks = 25, xlab = "qx")
  abline(v = tail(data[,i],1), col = "blue")
}
dev.off()

# 4. Bootstrap estimation of the cost ----

## 4.1 By age ----

payment <- 230000
sim_size <- 100000

deaths <- matrix(0, nrow = sim_size, ncol = ncol(data))

summary <- c()
bar <- txtProgressBar(0,ncol(data),style=3)
for (i in 1:ncol(data)){
  n <- policies[i,]
  qx_age <- qx_boot[,i]
  for (j in 1:sim_size){
    qx <- sample(qx_age, size = 1)
    deaths[j,i] <- rbinom(1,n,qx)
  }
  
  # summary
  qx_current <- tail(data[,i],1)
  qx <- qx_boot[,i]
  qx_expected <- mean(qx)
  qx_expected_sd <- sd(qx)
  qx_VaR99 <- quantile(qx,0.99)
  qx_TVaR99 <- mean(qx[qx > qx_VaR99])
  n_policies <- policies[i,]
  cost <- deaths[,i]*payment
  cost_expected <- mean(cost)
  cost_sd <- sd(cost)
  cost_VaR99 <- quantile(cost,0.99)
  cost_TVaR99 <- mean(cost[cost > cost_VaR99])
  economic_capital <- cost_VaR99-cost_expected
  results <- data.frame(qx_current, qx_expected, qx_expected_sd, qx_VaR99,
                        qx_TVaR99, n_policies, cost_expected, cost_sd, cost_VaR99,
                        cost_TVaR99, economic_capital,
                        row.names = names(data)[i])
  summary <- rbind(summary,results)
  setTxtProgressBar(bar, i)
}

## 4.2 Total cost ----

total_cost <- c()

for (i in 1:sim_size){
  pos <- sample(1:sim_size,ncol(data))
  total_cost[i] <- sum(diag(deaths[pos,]))*payment
}

cost_total_expected <- mean(total_cost)
cost_total_sd <- sd(total_cost)
cost_total_VaR99 <- quantile(total_cost,0.99)
cost_total_TVaR99 <- mean(total_cost[total_cost > cost_total_VaR99])
total_economic_capital <- cost_total_VaR99 - cost_total_expected

summary_total <- data.frame(cost_total_expected, cost_total_sd,
                            cost_total_VaR99, cost_total_TVaR99,
                            total_economic_capital)

hist(total_cost, breaks = 40, main = "Histogram of total aggregated cost",
     xlab = "cost")

# data export
WB = createWorkbook()
addWorksheet(WB, sheetName = "summary")
writeData(WB, "summary", summary_total, startCol = 2,startRow = 2)
writeData(WB, "summary", summary, startCol = 2,startRow = 5, rowNames = TRUE)
saveWorkbook(wb = WB, file = 'cost_predictions_summary.xlsx', overwrite = TRUE)  

# graphs residuals check function for any specified time series and ARIMA

check_resid <- function(time_series = "q_67", ARIMA = c(0,1,1)){
  qx <- data[,time_series]
  qx_log <- log(qx)
  qx_log_diff <- diff(qx_log)
  model <- Arima(qx_log_diff, ARIMA)
  windows()
  checkresiduals(model)
}

check_resid("q_75", c(0,1,1))
graphics.off()

# 5. predictions for more than one year period (up to 2050) ----
h <- 29
n <- 50
colors <- rainbow(n)
svg("graphs\\predictions_more_than_one_year.svg")
par(mfrow = c(3,3))
for(i in 1:length(ARIMAs)){
  
  # get ts data
  qx <- ts(data[,i], start = 1935)
  qx_log <- log(qx)
  
  ARIMA <- ARIMAs[[1]]
  
  model <- Arima(qx_log,ARIMA,include.drift = TRUE)
  ARIMA <- arimaorder(model)
  
  I <- ARIMA["d"]
  n_remove <- max(ARIMA["p"], ARIMA["q"])+I
  if (n_remove>0) {
    res <- resid(model)[-c(1:n_remove)]
  } else res <- resid(model)[-1]
  
  coef <- model$coef
  AR <- coef[paste("ar",1:5, sep = "")]
  AR[is.na(AR)] <- 0
  
  MA <- coef[paste("ma",1:5, sep = "")]
  MA[is.na(MA)] <- 0
  
  c <- coef["drift"]
  I <- ARIMA["d"]
  qx_last <- tail(qx,1)
  
  plot(qx, xlim = c(2000, 2050), ylim = c(0.0,0.04), ylab = names(data)[i],
       main = names(data)[i])
  qx_log_boot_pred <- c()
  for(j in 1:n){
    qx_log_boot_pred <- ARIMA.boot.pred(ARIMA,AR, I, MA, c, res, h, qx_log)
    qx_boot_pred <- ts(c(qx_last,exp(qx_log_boot_pred)), start = 2021)
    points(qx_boot_pred, type = "l", col = colors[j])
  }
}
dev.off()

save.image(file = ".RData")
