rtsplot
====


Time series plot with base R Graphics
===


The `rtsplot` package is **fast** time series plot package with base R Graphics.


Installation:
===

The current release is available on [CRAN](https://CRAN.R-project.org/package=rtsplot),
which you can install via:

```r
install.packages("rtsplot")
```

To install the development version run following code:

```R
remotes::install_bitbucket("rtsvizteam/rtsplot")
```
	

Example :
===

```R
	library(quantmod)
	
	symbol = 'AAPL'
	data = getSymbols(symbol, src = 'yahoo',  auto.assign=F)
	
	library(rtsplot)
	y = data['2007::']
	sma = TTR::SMA(Cl(y), 250)
	rsi = TTR::RSI(Cl(y), 20)	

	# plot candles and volume and table
	layout(c(1,1,1,1,2,3))
	cols = rtsplot.colors(2)
		
	rtsplot(y, type = 'l', plotX = F, col=cols[1],lwd=1.5, log='y')
		rtsplot.lines(sma, col=cols[2], lwd=1.5)
		rtsplot.legend(c(symbol, 'SMA(250)'), cols[1:2], list(y,sma))
		
		
	y = rtsplot.scale.volume(y)
	rtsplot(y, type = 'volume', plotX = F, col = 'darkgray')
		rtsplot.legend('Volume', 'darkgray', Vo(y))
		
		
	# plot rsi
	rtsplot(rsi, type = 'l', ylim=c(0,100),
		y.highlight = c(c(0,30), c(70,100)),
		y.highlight.col = grDevices::adjustcolor(c('green','red'), 50/255)
	)	
	rtsplot.legend('RSI(20)', 'black', rsi)

```


