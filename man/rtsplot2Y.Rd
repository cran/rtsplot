% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rtsplot.r
\name{rtsplot2Y}
\alias{rtsplot2Y}
\title{Plot time series with second Y axis}
\usage{
rtsplot2Y(y, las = 1, type = "l", col.axis = "red", ylim = NULL, log = "", ...)
}
\arguments{
\item{y}{\code{\link{xts}} object}

\item{las}{rotation of Y axis labels, \strong{defaults to 1}, for more info see \code{\link{par}}}

\item{type}{plot type, \strong{defaults to 'l'}, for more info see \code{\link{plot}}
also support 'ohlc', 'hl', 'candle', 'volume' types}

\item{col.axis}{axis color, \strong{defaults to 'red'}}

\item{ylim}{range on Y values, \strong{defaults to NULL}}

\item{log}{log scale x, y, xy axes, \strong{defaults to ''}}

\item{...}{additional parameters to the \code{\link{plot}}}
}
\value{
nothing
}
\description{
Detailed discussion for validity of dual Y axis at [Dual axes time series plots may be ok sometimes after all](http://freerangestats.info/blog/2016/08/18/dualaxes)
}
\examples{
# generate time series data
y = rtsplot.fake.stock.data(1000)
symbol = 'SPY'

y1 = rtsplot.fake.stock.data(1000, 100)
symbol = 'IBM'
	
 # two Y axis example
 # to plot second Y axis, free some space on left side, set LeftMargin=3
 layout(1)
 cols = c('black', 'red')

 rtsplot(y, type = 'l', LeftMargin=3, col=cols[1])
			
 rtsplot2Y(y1, type='l', las=1, col=cols[2], col.axis=cols[2])

 rtsplot.legend('SPY(rhs),IBM(lhs)', cols, list(y,y1))

}
