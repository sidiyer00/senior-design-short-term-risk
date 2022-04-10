model_1 <- Macro_score_model
model_2 <- TA_score_model
model_3 <- ML_MA_Model
model_4 <- ML_Model

cumulative <- ML_MA_Model
cumulative$Label <- "cumulative"


# Make sure no date discrepancy bettwen macro model and ta model dates
mylist <- c()
for (i in 1:nrow(model_1)){
  if (sum(model_1$Date[i] == cumulative$Date) == 0){
    mylist <- c(mylist, i)
  }
}

model_1 <- model_1[-mylist,]

mylist <- c()
for (i in 1:nrow(cumulative)){
  if (sum(cumulative$Date[i] == model_1$Date) == 0){
    mylist <- c(mylist, i)
  }
}


cumulative <- cumulative[-mylist,]

mylist <- c()
for (i in 1:nrow(model_2)){
  if (sum(model_2$Date[i] == model_1$Date) == 0){
    mylist <- c(mylist, i)
  }
}
model_2 <- model_2[-mylist,]

mylist <- c()
for (i in 1:nrow(model_3)){
  if (sum(model_3$Date[i] == model_1$Date) == 0){
    mylist <- c(mylist, i)
  }
}
model_3 <- model_3[-mylist,]

mylist <- c()
for (i in 1:nrow(model_4)){
  if (sum(model_4$Date[i] == model_1$Date) == 0){
    mylist <- c(mylist, i)
  }
}
model_4 <- model_4[-mylist,]


nrow(model_1)
nrow(model_2)
nrow(model_3)
nrow(model_4)
nrow(cumulative)

# sum the various model scores
cumulative$Value <- model_1$Value + model_2$Value + model_3$Value + model_4$Value

# Re-pull QQQ and VXX Data if needed
tickers <- c("QQQ", "^VXN")
start.date2 = model_3$Date[1] # start and end dates from TA model
end.date2 = model_3$Date[nrow(model_3)]

#Alternatively you can enter your own here
# start.date2 = "2018-01-08"
# end.date2 = "2019-01-22"

# start.date2 = "2019-01-07"
# end.date2 = "2020-01-21"
# 
# start.date2 = "2020-01-07"
# end.date2 = "2021-01-25"
# 
# start.date2 = "2021-01-07"
# end.date2 = "2022-03-25"
l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = start.date2,
                         last.date = as.Date(end.date2)+1, do.cache=FALSE, be.quiet = TRUE)

price_data = data.frame(l.out$df.tickers$price.adjusted,l.out$df.tickers$ret.adjusted.prices, l.out$df.tickers$ref.date, l.out$df.tickers$ticker)
colnames(price_data) <- c("Value", "Daily Return", "Date", "Label")

cumulative <- cumulative[(which(cumulative$Date == start.date2)):(which(cumulative$Date == end.date2)),]
# Build the portfolio
mylist <- c()
for (i in 1:nrow(price_data)){
  if (sum(price_data$Date[i] == cumulative$Date) == 0){
    mylist <- c(mylist, i)
  }
}
price_data <- price_data[-mylist,]
portfolio <- cumulative
portfolio$Label <- "portfolio"
portfolio$Value <- 100



for(i in 2:nrow(portfolio)){
  if (cumulative$Value[i-1] == 0){
    portfolio$Value[i] <- portfolio$Value[i-1] * (1+price_data$`Daily Return`[i]* 1.5)
  } else if (cumulative$Value[i-1] == 1){
    portfolio$Value[i] <- portfolio$Value[i-1]  * (1+(price_data$`Daily Return`[i] * 1))
  } else if (cumulative$Value[i-1] == 2){
    portfolio$Value[i] <- portfolio$Value[i-1] * (1+(price_data$`Daily Return`[i] * 1))
  } else if (cumulative$Value[i-1] == 3){
    portfolio$Value[i] <- portfolio$Value[i-1] * (1+(price_data$`Daily Return`[i] * .75))
  } else if (cumulative$Value[i-1] == 4){
    portfolio$Value[i] <- portfolio$Value[i-1] * (1+(price_data$`Daily Return`[i] * -.5))
  }
}

portfolio[nrow(portfolio),]
# Set up QQQ portfolio
QQQ_portfolio <- portfolio

QQQ_portfolio$Value <- 100
for(i in 2:nrow(QQQ_portfolio)){
  QQQ_portfolio$Value[i] <- QQQ_portfolio$Value[i-1] * (1+price_data$`Daily Return`[i])
}
QQQ_portfolio$Label <- "QQQ Portfolio"

# Rebind everything
price_data <- price_data[,-2]

cumulative$Value <- cumulative$Value * 100 #* 10 + round(min(portfolio$Value),digits = 10)# use this to scale if using smaller time periods
cumulative$Label <- "Risk Signal"
port_comp <- rbind(QQQ_portfolio, portfolio , cumulative)
ggplot(port_comp, aes(x = port_comp$Date, y = port_comp$Value, color = port_comp$Label, group = port_comp$Label))+geom_line()+ggtitle("Cumulative Model - Market Exposure - 0 signals 150%, 4 signals -50%, else 100%") + theme(plot.title = element_text(size=10))


# Evaluate sharpe
sharpe_ratios <- data.frame(matrix(0,2,2))
sharpe_ratios$X1[1] <- mean(Delt(portfolio$Value)[-1])/sd(Delt(portfolio$Value)[-1])
sharpe_ratios$X2[1] <- mean(Delt(QQQ_portfolio$Value)[-1])/sd(Delt(QQQ_portfolio$Value)[-1])
colnames(sharpe_ratios) <- c("Portfolio Daily Sharpe", "QQQ Daily Sharpe")

sharpe_ratios_annualized <- sharpe_ratios *sqrt(252)
colnames(sharpe_ratios_annualized) <- c("Portfolio Sharpe Annualized", "QQQ Sharpe Annualized")
sharpe_ratios_annualized[1,]
