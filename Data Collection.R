library(quantmod)

symbols <- c("XLK", "XLV", "XLY", "XLF", "XLI", "XLP", "XLU", "XLB", "XLE")
getSymbols(symbols, from="2012-01-01", to="2019-12-31")

stocks = data.frame(XLK$XLK.Adjusted, XLV$XLV.Adjusted, XLY$XLY.Adjusted, XLF$XLF.Adjusted, 
                    XLI$XLI.Adjusted, XLP$XLP.Adjusted, XLU$XLU.Adjusted, XLB$XLB.Adjusted,
                    XLE$XLE.Adjusted)

colnames(stocks) <- symbols
stocks

