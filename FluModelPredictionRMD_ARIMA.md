Flu Prediction models for Linkou and Kaohsiung Branch\_ARIMA
================

匯入套件Import Library
----------------------

``` r
library(data.table)
library(forecast)
library(gtools)
```

ARIMA模型週期
-------------

``` r
weekLIST<-list(
MONTHLY_weeknum<-c(1,4,8,13,17,22,26,30,35,39,43,48,52),
SEASON_weeknum<-c(13,26,39,52))
```

林口院區資料Episode Data from Linkou Branch
-------------------------------------------

``` r
Flu_Linkou_313<-readRDS('Flu_Linkou_313.rds')
Flu_Linkou_313$year_week_no<-1:313
```

### 每週更新模型\_林口 Updating Interval: Weekly (Linkou)

``` r
predictNULL_Flu_Linkou_313<-NULL
training_Flu_Linkou_313_model<-NULL
Flu_Linkou_313_model_coefficient<-NULL


for(i in 0:51){
      newtraining_Flu_Linkou_313<-ts(Flu_Linkou_313[c(1:(261+i)),]$N,start =c(2010,01),freq = 365.25/7)
    fit<-auto.arima(newtraining_Flu_Linkou_313)
    mmm<-data.frame(method = forecast(fit)$method)
    #print(i)
  
  newtraining_Flu_Linkou_313<-ts(Flu_Linkou_313[c(1:(261+i)),]$N,start =c(2010,01),freq = 365.25/7)
  fit_arima<-arima(newtraining_Flu_Linkou_313,order = fit$arma[c(1,6,2)],seasonal = list(order = fit$arma[c(3,7,4)],period = fit$arma[5]))
  mmm<-data.frame(method = forecast(fit_arima)$method,week = i)
  nnn<-data.frame(t(fit_arima$coef))
  training_Flu_Linkou_313_model<-rbind(training_Flu_Linkou_313_model,mmm)
  Flu_Linkou_313_model_coefficient<-rbind(Flu_Linkou_313_model_coefficient,nnn)
  ppp<-forecast(fit_arima,1)
  predictNULL_Flu_Linkou_313<-rbind(data.frame(predictNULL_Flu_Linkou_313),data.frame(ppp))
  #print(i)
}


predictNULL_Flu_Linkou_313$year_week_no<-""
predictNULL_Flu_Linkou_313$year_week_no<-262:313
predictNULL_Flu_Linkou_313<-data.table(predictNULL_Flu_Linkou_313)

saveRDS(predictNULL_Flu_Linkou_313[,c('year_week_no','Point.Forecast'),with=F],'Flu_ARIMA_Weekly_Linkou.rds')

#RMSE
round(sqrt(mean((predictNULL_Flu_Linkou_313$Point.Forecast-Flu_Linkou_313[262:313]$N)^2)),digits=1)
```

    ## [1] 20.2

``` r
#MAE
round(sum(abs(predictNULL_Flu_Linkou_313$Point.Forecast-Flu_Linkou_313[262:313]$N))/52,digits=1)
```

    ## [1] 13.9

### 每月更新模型\_林口 Updating Interval: Monthly (Linkou)

``` r
predictNULL_Flu_Linkou_313_Monthly<-NULL
training_Flu_Linkou_313_model_Monthly<-NULL
Flu_Linkou_313_model_coefficient_Monthly<-NULL


for(i in 0:51){
  if ((i+1)%%4==1){
    newtraining_Flu_Linkou_313_Monthly<-ts(Flu_Linkou_313[c(1:(261+i)),]$N,start =c(2010,01),freq = 365.25/7)
    fit_Monthly<-auto.arima(newtraining_Flu_Linkou_313_Monthly)
    mmm_Monthly<-data.frame(method = forecast(fit_Monthly)$method)
    #print(i)
  }
  
  newtraining_Flu_Linkou_313_Monthly<-ts(Flu_Linkou_313[c(1:(261+i)),]$N,start =c(2010,01),freq = 365.25/7)
  fit_Monthly_arima<-arima(newtraining_Flu_Linkou_313_Monthly,order = fit_Monthly$arma[c(1,6,2)],seasonal = list(order = fit_Monthly$arma[c(3,7,4)],period = fit_Monthly$arma[5]))
  mmm_Monthly<-data.frame(method = forecast(fit_Monthly_arima)$method,week = i)
  nnn<-data.frame(t(fit_Monthly_arima$coef))
  training_Flu_Linkou_313_model_Monthly<-rbind(training_Flu_Linkou_313_model_Monthly,mmm_Monthly)
  Flu_Linkou_313_model_coefficient_Monthly<-rbind(Flu_Linkou_313_model_coefficient_Monthly,nnn)
  ppp<-forecast(fit_Monthly_arima,1)
  predictNULL_Flu_Linkou_313_Monthly<-rbind(data.frame(predictNULL_Flu_Linkou_313_Monthly),data.frame(ppp))
  #print(i)
}
predictNULL_Flu_Linkou_313_Monthly$year_week_no<-262:313
predictNULL_Flu_Linkou_313_Monthly<-data.table(predictNULL_Flu_Linkou_313_Monthly)

saveRDS(predictNULL_Flu_Linkou_313_Monthly[,c('year_week_no','Point.Forecast'),with=F],'Flu_ARIMA_Monthly_Linkou.rds')

#RMSE
round(sqrt(mean((predictNULL_Flu_Linkou_313_Monthly$Point.Forecast-Flu_Linkou_313[262:313]$N)^2)),digits = 1)
```

    ## [1] 20.2

``` r
#MAE
round(sum(abs(predictNULL_Flu_Linkou_313_Monthly$Point.Forecast-Flu_Linkou_313[262:313]$N))/52,digits = 1)
```

    ## [1] 13.9

### 每季更新模型\_林口 Updating Interval: Quarterly (Linkou)

``` r
predictNULL_Flu_Linkou_313_Quarterly<-NULL
training_Flu_Linkou_313_model_Quarterly<-NULL
Flu_Linkou_313_model_coefficient_Quarterly<-NULL


for(i in 0:51){
  if ((i+1)%%13==1){
    newtraining_Flu_Linkou_313_Quarterly<-ts(Flu_Linkou_313[c(1:(261+i)),]$N,start =c(2010,01),freq = 365.25/7)
    fit_Quarterly<-auto.arima(newtraining_Flu_Linkou_313_Quarterly)
    mmm_Quarterly<-data.frame(method = forecast(fit_Quarterly)$method)
    #print(i)
  }
  
  newtraining_Flu_Linkou_313_Quarterly<-ts(Flu_Linkou_313[c(1:(261+i)),]$N,start =c(2010,01),freq = 365.25/7)
  fit_Quarterly_arima<-arima(newtraining_Flu_Linkou_313_Quarterly,order = fit_Quarterly$arma[c(1,6,2)],seasonal = list(order = fit_Quarterly$arma[c(3,7,4)],period = fit_Quarterly$arma[5]))
  mmm_Quarterly<-data.frame(method = forecast(fit_Quarterly_arima)$method,week = i)
  nnn<-data.frame(t(fit_Quarterly_arima$coef))
  training_Flu_Linkou_313_model_Quarterly<-rbind(training_Flu_Linkou_313_model_Quarterly,mmm_Quarterly)
  Flu_Linkou_313_model_coefficient_Quarterly<-rbind(Flu_Linkou_313_model_coefficient_Quarterly,nnn)
  ppp<-forecast(fit_Quarterly_arima,1)
  predictNULL_Flu_Linkou_313_Quarterly<-rbind(data.frame(predictNULL_Flu_Linkou_313_Quarterly),data.frame(ppp))
  #print(i)
}

predictNULL_Flu_Linkou_313_Quarterly$year_week_no<-""
predictNULL_Flu_Linkou_313_Quarterly$year_week_no<-262:313
predictNULL_Flu_Linkou_313_Quarterly<-data.table(predictNULL_Flu_Linkou_313_Quarterly)

saveRDS(predictNULL_Flu_Linkou_313_Quarterly[,c('year_week_no','Point.Forecast'),with=F],'Flu_ARIMA_Quarterly_Linkou.rds')

#RMSE
round(sqrt(mean((predictNULL_Flu_Linkou_313_Quarterly$Point.Forecast-Flu_Linkou_313[262:313]$N)^2)),digits=1)
```

    ## [1] 20.2

``` r
#MAE
round(sum(abs(predictNULL_Flu_Linkou_313_Quarterly$Point.Forecast-Flu_Linkou_313[262:313]$N))/52,digits=1)
```

    ## [1] 13.9

### 每年更新模型\_林口 Updating Interval: Yearly (Linkou)

``` r
predictNULL_Flu_Linkou_313_YEARLY<-NULL
training_Flu_Linkou_313_model_YEARLY<-NULL
Flu_Linkou_313_model_coefficient_YEARLY<-NULL

newtraining_Flu_Linkou_313_YEARLY<-ts(Flu_Linkou_313[c(1:261),]$N,start =c(2010,01),freq = 365.25/7)
fit_YEARLY<-auto.arima(newtraining_Flu_Linkou_313_YEARLY)
mmm_YEARLY<-data.frame(method = forecast(fit_YEARLY)$method)

for(i in 0:51){
  
  newtraining_Flu_Linkou_313_YEARLY<-ts(Flu_Linkou_313[c(1:(261+i)),]$N,start =c(2010,01),freq = 365.25/7)
  fit_YEARLY_arima<-arima(newtraining_Flu_Linkou_313_YEARLY,order = fit_YEARLY$arma[c(1,6,2)],seasonal = list(order = fit_YEARLY$arma[c(3,7,4)],period = fit_YEARLY$arma[5]),method="CSS")
  mmm_YEARLY<-data.frame(method = forecast(fit_YEARLY_arima)$method,week = i)
  nnn<-data.frame(t(fit_YEARLY_arima$coef))
  training_Flu_Linkou_313_model_YEARLY<-rbind(training_Flu_Linkou_313_model_YEARLY,mmm_YEARLY)
  Flu_Linkou_313_model_coefficient_YEARLY<-rbind(Flu_Linkou_313_model_coefficient_YEARLY,nnn)
  ppp<-forecast(fit_YEARLY_arima,1)
  predictNULL_Flu_Linkou_313_YEARLY<-rbind(data.frame(predictNULL_Flu_Linkou_313_YEARLY),data.frame(ppp))
  #print(i)
}
predictNULL_Flu_Linkou_313_YEARLY$year_week_no<-262:313
predictNULL_Flu_Linkou_313_YEARLY<-data.table(predictNULL_Flu_Linkou_313_YEARLY)

saveRDS(predictNULL_Flu_Linkou_313_YEARLY[,c('year_week_no','Point.Forecast'),with=F],'Flu_ARIMA_Yearly_Linkou.rds')

#RMSE
round(sqrt(mean((predictNULL_Flu_Linkou_313_YEARLY$Point.Forecast-Flu_Linkou_313[262:313]$N)^2)),digits=1)
```

    ## [1] 20.2

``` r
#MAE
round(sum(abs(predictNULL_Flu_Linkou_313_YEARLY$Point.Forecast-Flu_Linkou_313[262:313]$N))/52,digits=1)
```

    ## [1] 13.8

<hr>
高雄資料
--------

``` r
Flu_Kaohsiung_313<-readRDS('Flu_Kaohsiung_313.rds')
Flu_Kaohsiung_313<-rbind(Flu_Kaohsiung_313,data.table(year_week_CDC="2014_47",N=0),fill=T)
Flu_Kaohsiung_313<-Flu_Kaohsiung_313[order(year_week_CDC)]
Flu_Kaohsiung_313$year_week_no<-1:313
```

### 每週更新模型\_高雄 Updating Interval: Weekly (Kaohsiung)

``` r
training_Flu_Kaohsiung_313_model<-NULL
predictNULL_Flu_Kaohsiung_313<-NULL
Flu_Kaohsiung_313_model_coefficient<-NULL
for(i in 0:51){
  
  newtraining_Flu_Kaohsiung_313<-ts(Flu_Kaohsiung_313[c(1:(261+i)),]$N,start =c(2010,01),freq = 365.25/7)
  fit<-auto.arima(newtraining_Flu_Kaohsiung_313)
  mmmk<-data.frame(method = forecast(fit)$method,week = i)
  nnnk<-data.frame(t(fit$coef))
  training_Flu_Kaohsiung_313_model<-rbind(training_Flu_Kaohsiung_313_model,mmmk)
  Flu_Kaohsiung_313_model_coefficient<-smartbind(Flu_Kaohsiung_313_model_coefficient,nnnk)
  aaa<-forecast(fit,1)
  predictNULL_Flu_Kaohsiung_313<-rbind(data.frame(predictNULL_Flu_Kaohsiung_313),data.frame(aaa))
  #print(i)
}

predictNULL_Flu_Kaohsiung_313$year_week_no<-262:313
predictNULL_Flu_Kaohsiung_313<-data.table(predictNULL_Flu_Kaohsiung_313)
saveRDS(predictNULL_Flu_Kaohsiung_313[,c('year_week_no','Point.Forecast'),with=F],'Flu_ARIMA_Weekly_Kaohsiung.rds')


#RMSE
round(sqrt(mean((predictNULL_Flu_Kaohsiung_313$Point.Forecast-Flu_Kaohsiung_313[262:313]$N)^2)),digits=1)
```

    ## [1] 20.2

``` r
#MAE
round(sum(abs(predictNULL_Flu_Kaohsiung_313$Point.Forecast-Flu_Kaohsiung_313[262:313]$N))/52,digits=1)
```

    ## [1] 14.2

### 每月更新模型\_高雄 Updating Interval: Monthly (Kaohsiung)

``` r
predictNULL_Flu_Kaohsiung_313_Monthly<-NULL
training_Flu_Kaohsiung_313_model_Monthly<-NULL
Flu_Kaohsiung_313_model_coefficient_Monthly<-NULL


for(i in 0:51){
  if ((i+1)%%4==1){
    newtraining_Flu_Kaohsiung_313_Monthly<-ts(Flu_Kaohsiung_313[c(1:(261+i)),]$N,start =c(2010,01),freq = 365.25/7)
    fit_Monthly<-auto.arima(newtraining_Flu_Kaohsiung_313_Monthly)
    mmm_Monthly<-data.frame(method = forecast(fit_Monthly)$method)
    #print(i)
  }
  
  newtraining_Flu_Kaohsiung_313_Monthly<-ts(Flu_Kaohsiung_313[c(1:(261+i)),]$N,start =c(2010,01),freq = 365.25/7)
  fit_Monthly_arima<-arima(newtraining_Flu_Kaohsiung_313_Monthly,order = fit_Monthly$arma[c(1,6,2)],seasonal = list(order = fit_Monthly$arma[c(3,7,4)],period = fit_Monthly$arma[5]),method="CSS")
  mmm_Monthly<-data.frame(method = forecast(fit_Monthly_arima)$method,week = i)
  nnn<-data.frame(t(fit_Monthly_arima$coef))
  training_Flu_Kaohsiung_313_model_Monthly<-rbind(training_Flu_Kaohsiung_313_model_Monthly,mmm_Monthly)
  Flu_Kaohsiung_313_model_coefficient_Monthly<-smartbind(Flu_Kaohsiung_313_model_coefficient_Monthly,nnn)
  ppp<-forecast(fit_Monthly_arima,1)
  predictNULL_Flu_Kaohsiung_313_Monthly<-rbind(data.frame(predictNULL_Flu_Kaohsiung_313_Monthly),data.frame(ppp))
  #print(i)
}

predictNULL_Flu_Kaohsiung_313_Monthly$year_week_no<-262:313
predictNULL_Flu_Kaohsiung_313_Monthly<-data.table(predictNULL_Flu_Kaohsiung_313_Monthly)
saveRDS(predictNULL_Flu_Kaohsiung_313_Monthly[,c('year_week_no','Point.Forecast'),with=F],'Flu_ARIMA_Monthly_Kaohsiung.rds')


#RMSE
round(sqrt(mean((predictNULL_Flu_Kaohsiung_313_Monthly$Point.Forecast-Flu_Kaohsiung_313[262:313]$N)^2)),digits = 1)
```

    ## [1] 16.2

``` r
#MAE
round(sum(abs(predictNULL_Flu_Kaohsiung_313_Monthly$Point.Forecast-Flu_Kaohsiung_313[262:313]$N))/52,digits = 1)
```

    ## [1] 11

### 每季更新模型\_高雄 Updating Interval: Quarterly (Kaohsiung)

``` r
predictNULL_Flu_Kaohsiung_313_Quarterly<-NULL
training_Flu_Kaohsiung_313_model_Quarterly<-NULL
Flu_Kaohsiung_313_model_coefficient_Quarterly<-NULL


for(i in 0:51){
  if ((i+1)%%13==1){
    newtraining_Flu_Kaohsiung_313_Quarterly<-ts(Flu_Kaohsiung_313[c(1:(261+i)),]$N,start =c(2010,01),freq = 365.25/7)
    fit_Quarterly<-auto.arima(newtraining_Flu_Kaohsiung_313_Quarterly)
    mmm_Quarterly<-data.frame(method = forecast(fit_Quarterly)$method)
    #print(i)
  }
  
  newtraining_Flu_Kaohsiung_313_Quarterly<-ts(Flu_Kaohsiung_313[c(1:(261+i)),]$N,start =c(2010,01),freq = 365.25/7)
  fit_Quarterly_arima<-arima(newtraining_Flu_Kaohsiung_313_Quarterly,order = fit_Quarterly$arma[c(1,6,2)],seasonal = list(order = fit_Quarterly$arma[c(3,7,4)],period = fit_Quarterly$arma[5]),method="CSS")
  mmm_Quarterly<-data.frame(method = forecast(fit_Quarterly_arima)$method,week = i)
  nnn<-data.frame(t(fit_Quarterly_arima$coef))
  training_Flu_Kaohsiung_313_model_Quarterly<-rbind(training_Flu_Kaohsiung_313_model_Quarterly,mmm_Quarterly)
  Flu_Kaohsiung_313_model_coefficient_Quarterly<-smartbind(Flu_Kaohsiung_313_model_coefficient_Quarterly,nnn)
  ppp<-forecast(fit_Quarterly_arima,1)
  predictNULL_Flu_Kaohsiung_313_Quarterly<-rbind(data.frame(predictNULL_Flu_Kaohsiung_313_Quarterly),data.frame(ppp))
  #print(i)
}

predictNULL_Flu_Kaohsiung_313_Quarterly$year_week_no<-262:313
predictNULL_Flu_Kaohsiung_313_Quarterly<-data.table(predictNULL_Flu_Kaohsiung_313_Quarterly)
saveRDS(predictNULL_Flu_Kaohsiung_313_Quarterly[,c('year_week_no','Point.Forecast'),with=F],'Flu_ARIMA_Quarterly_Kaohsiung.rds')

#RMSE
round(sqrt(mean((predictNULL_Flu_Kaohsiung_313_Quarterly$Point.Forecast-Flu_Kaohsiung_313[262:313]$N)^2)),digits=1)
```

    ## [1] 18.1

``` r
#MAE
round(sum(abs(predictNULL_Flu_Kaohsiung_313_Quarterly$Point.Forecast-Flu_Kaohsiung_313[262:313]$N))/52,digits=1)
```

    ## [1] 13.1

### 每年更新模型\_高雄 Updating Interval: Yearly (Kaohsiung)

``` r
predictNULL_Flu_Kaohsiung_313_YEARLY<-NULL
training_Flu_Kaohsiung_313_model_YEARLY<-NULL
Flu_Kaohsiung_313_model_coefficient_YEARLY<-NULL


for(i in 0:51){
  if ((i+1)%%52==1){
    newtraining_Flu_Kaohsiung_313_YEARLY<-ts(Flu_Kaohsiung_313[c(1:(261+i)),]$N,start =c(2010,01),freq = 365.25/7)
    fit_YEARLY<-auto.arima(newtraining_Flu_Kaohsiung_313_YEARLY)
    mmm_YEARLY<-data.frame(method = forecast(fit_YEARLY)$method)
    #print(i)
  }
  
  newtraining_Flu_Kaohsiung_313_YEARLY<-ts(Flu_Kaohsiung_313[c(1:(261+i)),]$N,start =c(2010,01),freq = 365.25/7)
  fit_YEARLY_arima<-arima(newtraining_Flu_Kaohsiung_313_YEARLY,order = fit_YEARLY$arma[c(1,6,2)],seasonal = list(order = fit_YEARLY$arma[c(3,7,4)],period = fit_YEARLY$arma[5]),method="CSS")
  mmm_YEARLY<-data.frame(method = forecast(fit_YEARLY_arima)$method,week = i)
  nnn<-data.frame(t(fit_YEARLY_arima$coef))
  training_Flu_Kaohsiung_313_model_YEARLY<-rbind(training_Flu_Kaohsiung_313_model_YEARLY,mmm_YEARLY)
  Flu_Kaohsiung_313_model_coefficient_YEARLY<-smartbind(Flu_Kaohsiung_313_model_coefficient_YEARLY,nnn)
  ppp<-forecast(fit_YEARLY_arima,1)
  predictNULL_Flu_Kaohsiung_313_YEARLY<-rbind(data.frame(predictNULL_Flu_Kaohsiung_313_YEARLY),data.frame(ppp))
  #print(i)
}
predictNULL_Flu_Kaohsiung_313_YEARLY$year_week_no<-262:313
predictNULL_Flu_Kaohsiung_313_YEARLY<-data.table(predictNULL_Flu_Kaohsiung_313_YEARLY)
saveRDS(predictNULL_Flu_Kaohsiung_313_YEARLY[,c('year_week_no','Point.Forecast'),with=F],'Flu_ARIMA_Yearly_Kaohsiung.rds')
#RMSE
round(sqrt(mean((predictNULL_Flu_Kaohsiung_313_YEARLY$Point.Forecast-Flu_Kaohsiung_313[262:313]$N)^2)),digits = 1)
```

    ## [1] 20.8

``` r
#MAE
round(sum(abs(predictNULL_Flu_Kaohsiung_313_YEARLY$Point.Forecast-Flu_Kaohsiung_313[262:313]$N))/52,digits = 1)
```

    ## [1] 14.7
