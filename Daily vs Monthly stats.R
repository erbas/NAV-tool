
# daily vs monthly pnls

pnl.daily.csv <- read.csv("Extended Markets BSc_pnl_daily.csv")
pnl.monthly.csv <- read.csv("Extended Markets BSc_pnl_monthly.csv")

pnl.daily <- xts(pnl.daily.csv[,2],ymd(pnl.daily.csv[,1]))
pnl.monthly <- xts(pnl.monthly.csv[,2],ymd(pnl.monthly.csv[,1]))

plot.zoo(cumsum(merge(pnl.monthly,pnl.daily,fill=0)),plot.type='single',col=c(3,4))

stats.daily <- calc.stats(pnl.daily, period=252)
stats.monthly <- calc.stats(pnl.monthly, period=12)
stats.compare <- cbind(stats.daily,stats.monthly)
