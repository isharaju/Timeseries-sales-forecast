suppressMessages(library(tidyverse))
suppressMessages(library(TSA))
suppressMessages(library(data.table))
suppressMessages(library(doBy))
suppressMessages(library(formattable))
suppressMessages(library(lubridate))
suppressMessages(library(DT))
suppressMessages(library(timeSeries))

suppressMessages(library(reshape))
suppressMessages(library(stringr))
suppressMessages(library(gridExtra))


suppressMessages(library(wesanderson))
suppressMessages(library(ggplot2))
suppressMessages(library(plotly))
suppressMessages(library(corrplot))
suppressMessages(library(RColorBrewer))
suppressMessages(library(zoo))

suppressMessages(library(forecast))
suppressMessages(library(tseries))

set.seed(42)



train=read.csv("train.csv")
train$date <- as.Date(train$date, "%Y-%m-%d")
sprintf("The training set has %d rows and %d columns", nrow(train), ncol(train) )
str(train)

test <- read.csv("test.csv")
test$date <- as.Date(test $date, "%Y-%m-%d")
sprintf("The test set has %d rows and %d columns", nrow(test ), ncol(test ) )
str(test)



head(train)

train$Year=year(train$date)        
train$Month=as.yearmon(train$date)

head(train)

print("the summary of train sales is:")
summary(train$sales)

options(repr.plot.width=5, repr.plot.height=3)

gbp1<-wes_palette("IsleofDogs1")[1]

ggplot(train, aes(x=sales))+
  geom_histogram(fill="#1122aa", alpha=.4) + labs(x=NULL, y=NULL, title = "Histogram of Sale Price")+
   scale_x_continuous(breaks= seq(0,600000, by=100000)) + theme_minimal() + theme(plot.title=element_text(vjust=3, size=2) )

options(repr.plot.width=6, repr.plot.height=4)

sales_by_date <- aggregate(sales ~date, train, mean)

sl1 <-ggplot(sales_by_date, aes(x=as.factor(date), y=sales)) + geom_line(color='steelblue', aes(group=1), size=1.5) + 
    geom_point(colour='steelblue', size = 3.5, alpha=0.5) + 
    labs(title="The Growth of Sale Prices by date", x=NULL, y="Sale Price") + theme(  aspect.ratio = 0.2 ) + theme_minimal()

sales_by_date$change = c(0, 100*diff(sales_by_date$sales)/sales_by_date[-nrow(sales_by_date),]$sales)

sl2 <-ggplot(sales_by_date, aes(x=as.factor(date), y=change))+
  geom_line(color= "gray50", aes(group=1), size=1)+
  labs(title="Change rate of Sale Price", x="date", y="rate of change")+
  geom_hline(yintercept = 0, color = gbp1 )+
  theme(aspect.ratio = 0.2)+ theme_minimal()

grid.arrange(sl1,sl2)

options(repr.plot.width=8, repr.plot.height=6)

sales_ts<-ts(sales_by_date$sales,frequency = 365,start = c(2013,01), end = c(2017,12))
gglagplot(sales_ts)

options(repr.plot.width=6, repr.plot.height=4)

sales_by_month <- aggregate(sales ~Month, train, mean)

sl1 <-ggplot(sales_by_month, aes(x=as.factor(Month), y=sales)) + geom_line(color='steelblue', aes(group=1), size=1.5)+
  geom_point(colour='steelblue', size = 3.5, alpha=0.5)+
  labs(title="The Growth of Sale Prices by Month of Year", x=NULL, y="Sale Price")+
  theme( plot.title=element_text(vjust=3, size=15) ) + theme_minimal()

sales_by_month$change = c(0, 100*diff(sales_by_month$sales)/sales_by_month[-nrow(sales_by_month),]$sales)

sl2 <-ggplot(sales_by_month, aes(x=as.factor(Month), y=change)) + geom_line(color= "gray50", aes(group=1), size=1)+
  labs(title="Change rate of Sale Price", x="Month", y="rate of change") + geom_hline(yintercept = 0, color = gbp1 )+
  theme(plot.title=element_text(size=15))+ theme_minimal()

grid.arrange(sl1,sl2)

options(repr.plot.width=6, repr.plot.height=4)

sales_by_year <- aggregate(sales ~Year, train, mean)

sl1 <-ggplot(sales_by_year, aes(x=as.factor(Year), y=sales)) + geom_line(color='steelblue', aes(group=1), size=1.5)+
  geom_point(colour='steelblue', size = 3.5, alpha=0.5)+
  labs(title="The Growth of Sale Prices by Year", x=NULL, y="Sale Price")+
  theme( plot.title=element_text(vjust=3, size=15) ) + theme_minimal()

sales_by_year$change = c(0, 100*diff(sales_by_year$sales)/sales_by_year[-nrow(sales_by_year),]$sales)

sl2 <-ggplot(sales_by_year, aes(x=as.factor(Year), y=change)) + geom_line(color= "gray50", aes(group=1), size=1)+
  labs(title="Change rate of Sale Price", x="Year", y="rate of change") + geom_hline(yintercept = 0, color = gbp1 )+
  theme(plot.title=element_text(size=15))+ theme_minimal()

grid.arrange(sl1,sl2)

options(repr.plot.width=8, repr.plot.height=3)

p1 <- train %>%

  mutate(wday = wday(date, label = TRUE),

         month = month(date, label = TRUE)) %>%

  mutate(wday = fct_relevel(wday, c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))) %>%

  group_by(wday, month) %>%

  summarise(mean_sales = mean(sales)) %>%

  ggplot(aes(month, wday, fill = mean_sales)) +

  geom_tile() +

  labs(x = "Month of the year", y = "Day of the week") +

  scale_fill_distiller(palette = "Spectral")

plot(p1)

Year_state<-aggregate(sales ~store+Year, train,mean)
pal<-rep(brewer.pal(10, "PuBuGn"),5)

ggplot(Year_state, aes(group = factor(store) ))+
  geom_line(aes(x=Year,y=sales,color=factor(store)), alpha=0.5, show.legend=TRUE)+
  labs(title="The Growth of Sales Price by Store from 2013 - 2017", x=NULL
  )+
  theme(panel.background=element_rect(fill = "White"), plot.title=element_text(vjust=3, size=15),
        panel.grid.major=element_blank(), axis.line = element_line(colour = "black"))

Year_state<-aggregate(sales ~item+Year, train,mean)
pal<-rep(brewer.pal(10, "PuBuGn"),5)

ggplot(Year_state, aes(y=sales, x=Year, group = factor(item) ))+
  geom_line(aes(x=Year,y=sales,color=factor(item)), alpha=0.5, show.legend=TRUE)+
  labs(title="The Growth of Sales Price by Item from 2013 - 2017", x=NULL
  )+
  theme(panel.background=element_rect(fill = "White"),
        plot.title=element_text(vjust=3, size=15),
        panel.grid.major=element_blank(), axis.line = element_line(colour = "black"))

plotErrors <- function(ferrors)
{
  calcbinsize <- IQR(ferrors,na.rm=TRUE)/4
  calcsd   <- sd(ferrors,na.rm=TRUE)
  calcmin  <- min(ferrors,na.rm=TRUE) - calcsd*5
  calcmax  <- max(ferrors,na.rm=TRUE) + calcsd*3
  calcnorm <- rnorm(10000, mean=0, sd=calcsd)
  calcmin2 <- min(calcnorm)
  calcmax2 <- max(calcnorm)
  if (calcmin2 < calcmin) { calcmin <- calcmin2 }
  if (calcmax2 > calcmax) { calcmax <- calcmax2 }
  calcbins <- seq(calcmin, calcmax, calcbinsize)
  hist(ferrors, col="orange", freq=FALSE, breaks=calcbins)
  calchist <- hist(calcnorm, plot=FALSE, breaks=calcbins)
  points(calchist$mids, calchist$density, type="l", col="red", lwd=2)
}


item_id_4_5 = subset(train, store == 5 & item == 4 )
### creating time series object
item_id_4_5_ts<-ts(item_id_1_1$sales,frequency = 12,start = c(2013,01), end = c(2017,12))

options(repr.plot.width=8, repr.plot.height=3)
autoplot(item_id_4_5_ts)

options(repr.plot.width=8, repr.plot.height=10)
sales_ts_decompose<-decompose(item_id_4_5_ts, type = c("additive"))

actual<-autoplot(sales_ts_decompose$x)+xlab("Year")+ylab("sales")+ggtitle("Actual time series of sales")
seas<-autoplot(sales_ts_decompose$seasonal)+xlab("Year")+ylab("sales")+ggtitle("Seasonality time series of sales")
tren<-autoplot(sales_ts_decompose$trend)+xlab("Year")+ylab("sales")+ggtitle("Trend time series of sales")
rand<-autoplot(sales_ts_decompose$random)+xlab("Year")+ylab("sales")+ggtitle("random time series of sales")
grid.arrange(actual,seas,tren,rand,ncol=1,top="Decomposition of sales time series")

options(repr.plot.width=5, repr.plot.height=3)
acf(item_id_4_5$sales)

adf.test(item_id_4_5$sales)

item_1_1_ts_lm = tslm(item_id_1_1_ts ~ trend + season)
item_1_1_ts_forecast = forecast(item_1_1_ts_lm,h=90)
summary(item_1_1_ts_lm)

CalcResidual = item_1_1_ts_forecast$residual
acf(CalcResidual, main = 'ACF of the residual of Regression model')

options(repr.plot.width=5, repr.plot.height=5)
plotErrors(CalcResidual)

options(repr.plot.width=5, repr.plot.height=5)
qqnorm(CalcResidual)
qqline(CalcResidual, col = 2,lwd=2,lty=2)

shapiro.test(CalcResidual)

### Residual plot
options(repr.plot.width=8, repr.plot.height=3)
autoplot(CalcResidual)

ArimaResidualFit = auto.arima(item_1_1_ts_lm$residual)
ArimaResidualFitforecast = forecast(ArimaResidualFit,h=90)
ArimaResidualFit

options(repr.plot.width=8, repr.plot.height=3)
plot(window(ArimaResidualFit$residuals,start=c(2003,01), end=c(2017,12)),ylab='Residuals', type='o',
main=expression(Residuals~~from~~the~~ARIMA(list(0,0,0))~~Model))
abline(h=0)

options(repr.plot.width=5, repr.plot.height=3)
Acf(window(ArimaResidualFit$residuals,start=c(2003,01),end=c(2017,12)),
main=expression(ACF~~of~~Residuals~~from~~the~~ARIMA(list(0,0,0))~~Model))

Box.test(ArimaResidualFit$residuals, lag = 24, type = 'Ljung', fitdf = 2)

options(repr.plot.width=4, repr.plot.height=4)
plotForecastErrors(ArimaResidualFit$residuals)

qqnorm(ArimaResidualFit$residuals)
qqline(ArimaResidualFit$residuals, col = 2,lwd=2,lty=2)

item <- list()
store <- list()

for(i in 1:50) { # 50 items
    for(j in 1:10){ # 10 stores
       
        # create searate dataframes
        assign(paste("train",j,i,sep="."),subset(train, store == j & item == i )) -> data
       
        assign(paste("timeseries_data",j,i,sep="."), ts(data["sales"], start = c(2013,01), end = c(2017,12),frequency = 12)) -> timeseries_data
         
        # Regression forecast
       
        assign(paste("linear_model",j,i,sep="."),tslm(timeseries_data ~ trend + season) ) -> linear_model
        assign(paste("linear_model_forecast",j,i,sep="."),forecast(linear_model,h=90) ) -> linear_model_forecast

       # ARIMA forecast on the linear_model_residuals
       
        assign(paste("linear_model_residuals",j,i,sep="."),auto.arima(linear_model$residual)) -> linear_model_residuals
        assign(paste("linear_model_residuals_forecast",j,i,sep="."), forecast(linear_model_residuals,h=90)) -> linear_model_residuals_forecast
       
       # Final Forecast = Regression Forecast + Residual Forecast
       
        assign(paste("y",j,i,sep="."), data.frame(linear_model_forecast)$Point.Forecast ) -> y
        assign(paste("x",j,i,sep="."), data.frame(linear_model_residuals_forecast)$Point.Forecast ) -> x
        assign(paste("final_forecast_sales",j,i,sep="."), data.frame(round(x+y,0)) ) -> final_forecast_sales  
           
       
        item[[j]] <- final_forecast_sales   # add it to your list
}
       
        assign(paste("item",i,sep="."),do.call(rbind,item) ) -> ii
   
        store[[i]] <- ii # add it to your list
   
}

assign(paste("demand.sales"),do.call(rbind,store))

# submissiond

names(demand.sales) <- "sales"

test_timeseries_stackensemble <- cbind(test,demand.sales)
test_timeseries_stackensemble <- test_timeseries_stackensemble[,c("id","sales")]

write.csv(test_timeseries_stackensemble, 'test_timeseries_stackensemble_answer.csv')
head(test_timeseries_stackensemble)

summary(linear_model.10.4)

options(repr.plot.width=5, repr.plot.height=3)
CalcResidual = linear_model.10.4$residual
acf(CalcResidual, main = 'ACF of the residual of Regression model')

options(repr.plot.width=4, repr.plot.height=4)
plotErrors(CalcResidual)
qqnorm(CalcResidual)
qqline(CalcResidual, col = 2,lwd=2,lty=2)

### Residual plot
options(repr.plot.width=8, repr.plot.height=3)
autoplot(CalcResidual)

linear_model_residuals.10.4

options(repr.plot.width=8, repr.plot.height=3)
plot(window(linear_model_residuals.10.4$residuals,start=c(2003,01), end=c(2017,12)),ylab='Residuals', type='o',
main=expression(Residuals~~from~~the~~ARIMA~~Model))
abline(h=0)

options(repr.plot.width=8, repr.plot.height=3)
Acf(window(linear_model_residuals.10.4$residuals,start=c(2003,01),end=c(2017,12)),
main=expression(ACF~~of~~Residuals~~from~~the~~ARIMA~~Model))

Box.test(linear_model_residuals.10.4$residuals, lag = 4, type = 'Ljung', fitdf = 2)

options(repr.plot.width=4, repr.plot.height=4)
plotForecastErrors(linear_model_residuals.10.4$residuals)

qqnorm(linear_model_residuals.10.4$residuals)
qqline(linear_model_residuals.10.4$residuals, col = 2,lwd=2,lty=2)




