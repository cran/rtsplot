#' @title `rtsplot` - Time series plot with base R Graphics.
#' 
#' @description Plot time series data with base R Graphics.
#' 
#' The `rtsplot` package is **fast** time series plot package with base R Graphics.
#' 
#' @examples
#'	# generate time series data
#'	n = 1000
#'	x =  seq(Sys.Date(), by = 'day', length.out = n)
#'	y = cumsum( rnorm(n, sd = sqrt(0.1)) )
#'	y = xts::xts(y, x)
#'	symbol = 'Test'
#'
#'	sma = TTR::SMA(y, 250)
#'	rsi = TTR::RSI(y, 20)	
#'
#'	# plot candles and RSI charts
#'	layout(c(1,1,1,2))
#'	cols = rtsplot.colors(2)
#'		
#'	rtsplot(y, type = 'l', plotX = FALSE, col=cols[1],lwd=1.5)
#'		rtsplot.lines(sma, col=cols[2], lwd=1.5)
#'		rtsplot.legend(c(symbol, 'SMA(250)'), cols[1:2], list(y,sma))
#'		
#'	# plot rsi
#'	rtsplot(rsi, type = 'l', ylim=c(0,100),
#'		y.highlight = c(c(0,30), c(70,100)),
#'		y.highlight.col = grDevices::adjustcolor(c('green','red'), 50/255)
#'	)	
#'	rtsplot.legend('RSI(20)', 'black', rsi)
#' 
#' 
#' @name rtsplot
#' @docType package
#' 
NULL


###############################################################################
#' Theme
#'
#' Setup theme
#'
#' @param grid.color color for grid lines, \strong{defaults to 'gray90'}
#' @param colors RColorBrewer set to generate colors, \strong{defaults to "Set1" in RColorBrewer}
#' @param col.border border color for drawing candles, \strong{defaults to 'black'}
#' @param col.up up color for drawing candles, \strong{defaults to 'green'}
#' @param col.dn down color for drawing candles, \strong{defaults to 'red'}
#' @param col.x.highlight color for highlighting along x axis, \strong{defaults to 'orange'}
#' @param col.y.highlight color for highlighting along y axis, \strong{defaults to 'orange'}
#' @param cex font size, \strong{defaults to 1}
#' @param legend.bg.col background legend color, \strong{defaults to grDevices::adjustcolor('white', 200/255)}
#' @param n number of colors to generate
#' @param ... additional settings
#'
#' @return None
#'
#' @import grDevices
#' @export
#' @rdname Themes
###############################################################################
register.theme = function
(
	grid.color = 'gray90',
	colors = 'Set1',
	col.border = 'black',
	col.up = 'green',
	col.dn = 'red',
	col.x.highlight = 'orange',
	col.y.highlight = 'orange',
	cex = 1,
	legend.bg.col = grDevices::adjustcolor('white', 200/255)
)
	set.options('rtsplot.theme', 
		list(
			grid.color = grid.color,
			colors = colors,
			col.border = col.border,
			col.up = col.up,
			col.dn = col.dn,
			col.x.highlight = col.x.highlight,
			col.y.highlight = col.y.highlight,
			cex = cex,
			legend.bg.col = legend.bg.col
		)
	)


	
#' @export
#' @rdname Themes
rtsplot.theme = function() getOption('rtsplot.theme')


#' @export
#' @rdname Themes
rtsplot.theme.set = function(...) set.options('rtsplot.theme', ..., overwrite=TRUE)


#' @export
#' @rdname Themes
rtsplot.colors = function(n) {
	colors = rtsplot.theme()$colors
	if(is.character(colors)) {
		rtsplot.theme.set(colors = grDevices::colorRampPalette(RColorBrewer::brewer.pal(9,colors)))	
		colors = rtsplot.theme()$colors
	}
	colors(n)
}	
	

###############################################################################
#' Plot function for time series
#'
#' @param y \code{\link{xts}} object
#' @param main plot title
#' @param plotX flag to display X axis
#' @param LeftMargin to plot second Y axis, set LeftMargin=3, \strong{defaults to 0}
#' @param grid which grid lines to draw, \strong{defaults to 'xy'}
#' @param x.highlight segments to highlight along X axis, \strong{defaults to NULL}
#' @param y.highlight segments to highlight along Y axis, \strong{defaults to NULL}
#' @param y.highlight.col color to highlight segments Y axis, \strong{defaults to NULL}
#' @param las rotation of Y axis labels, \strong{defaults to 1}, for more info see \code{\link{par}}
#' @param type plot type, \strong{defaults to 'l'}, for more info see \code{\link{plot}}
#'			also support 'ohlc', 'hl', 'candle', 'volume' types
#' @param xlab X label, \strong{defaults to ''}, for more info see \code{\link{plot}}
#' @param ylab Y label, \strong{defaults to ''}, for more info see \code{\link{plot}}
#' @param ylim range on Y values, \strong{defaults to NULL}
#' @param log log scale x, y, xy axes, \strong{defaults to ''}
#' @param ... additional parameters to the \code{\link{plot}}
#'
#' @return nothing
#'
#' @examples
#'	# generate time series data
#'	n = 1000
#'	x =  seq(Sys.Date(), by = 'day', length.out = n)
#'	y = cumsum( rnorm(n, sd = sqrt(0.1)) )
#'	y = xts::xts(y, x)
#'	symbol = 'SPY'
#' 	
#' # simple example
#' highlight = which(y < 2)
#' 
#' # plot
#' layout(1)
#' rtsplot.theme.set(col.x.highlight=grDevices::adjustcolor('orange', 200/255))
#' 		
#' rtsplot(y, type = 'l', main = symbol, x.highlight = highlight)
#' 
#' @export 
###############################################################################
rtsplot <- function
(
	y,					# xts object to plot
	main = NULL,		# plot title
	plotX = TRUE,		# flag to display X axis
	LeftMargin = 0,		# to plot second Y axis, set LeftMargin=3
	
	grid = 'xy',		# add grid lines
	
	x.highlight = NULL,	# segments to highlight along X axis
	y.highlight = NULL,	# segments to highlight along Y axis
	y.highlight.col = NULL,	# color for segments to highlight along Y axis
	
	las = 1,			# rotation of Y axis labels
	type = 'l',			# plot type
	xlab = '',			# X label
	ylab = '',			# Y label
	ylim = NULL,		# range on Y values
	log = '',			# log scale x, y, xy axes
	...					# other parameters to plot
)
{
	# set plot margins : bottom,left,top,right
	hasTitle = !is.null(main);
	# if !plotX leave some space, 0.5, between sections; this way y-axis do not overlap
	graphics::par( mar = c(iif(plotX,2,0.4), LeftMargin , iif(hasTitle,2,0), 3) )
	
	# set plot y range
	if(quantmod::has.Cl(y)) y1 = quantmod::Cl(y) else y1 = y[,1]
	if( is.null(ylim) ) {
		ylim = range(y1, na.rm = TRUE)
		switch(type,
			'ohlc' = ,
			'hl' = ,
			'candle' = { ylim = range(quantmod::OHLC(y), na.rm = TRUE) },
			'volume' = { y1 = quantmod::Vo(y); ylim = range(quantmod::Vo(y), na.rm = TRUE) }
		)
	}
		
	# create plot frame, do not plot data
	temp.x = attr(y, 'index')	
	graphics::plot( temp.x, y1, xlab = xlab, ylab = ylab, main = main,
		frame.plot = FALSE,
		type = 'n', yaxt = 'n', xaxt = 'n', ylim = ylim, log = log, ... )
		
		#[R Base Plot suppress axis line but show ticks](https://stackoverflow.com/questions/42932941/r-base-plot-suppress-axis-line-but-show-ticks)
		
		grid.color = rtsplot.theme()$grid.color
		cex = rtsplot.theme()$cex
		
		# Y axis rotation in 90 degrees increments : las=0,las=1,las=2,las=3
		graphics::axis(4, las = las, col = NA, col.ticks = grid.color, cex.axis=cex)
		
		# plot X axis
		class(temp.x) = c('POSIXct', 'POSIXt')	
		xaxis.ticks = graphics::axis.POSIXct(1, temp.x,labels = plotX, tick = TRUE, 
			col = NA, col.ticks = grid.color, cex.axis=cex)
		# subticks tcl = -0.2
		
				
	# highlight logic
	if( !is.null(x.highlight) ) rtsplot.x.highlight(y, x.highlight); 	
	if( !is.null(y.highlight) ) rtsplot.y.highlight(y.highlight, y.highlight.col); 	
		
	# plot grid
	rtsplot.grid(grid, xaxis.ticks, col=grid.color)
	

	# plot data
	switch(type,
		'candle' = rtsplot.candle(y, ...),
		'hl' = rtsplot.hl(y, ...),
		'ohlc' = rtsplot.ohlc(y, ...),
		'volume' = rtsplot.volume(y, ...),
		{  graphics::lines(temp.x, y1, type=type, ...) }
	)
	
	# plot box
	#box();
}


###############################################################################
#' Plot time series with second Y axis
#'
#' @param y \code{\link{xts}} object
#' @param las rotation of Y axis labels, \strong{defaults to 1}, for more info see \code{\link{par}}
#' @param type plot type, \strong{defaults to 'l'}, for more info see \code{\link{plot}}
#'			also support 'ohlc', 'hl', 'candle', 'volume' types
#' @param ... additional parameters to the \code{\link{plot}}
#'
#' @return nothing
#'
#' @examples
#'	# generate time series data
#'	n = 1000
#'	x =  seq(Sys.Date(), by = 'day', length.out = n)
#'	y = cumsum( rnorm(n, sd = sqrt(0.1)) )
#'	y = xts::xts(y, x)
#'	symbol = 'SPY'
#'
#'	y1 = 100 + cumsum( rnorm(n, sd = sqrt(0.1)) )
#'	y1 = xts::xts(y1, x)
#'	symbol = 'IBM'
#' 	
#' # two Y axis example
#' # to plot second Y axis, free some space on left side, set LeftMargin=3
#' layout(1)
#' cols = c('black', 'red')
#' 
#' rtsplot(y, type = 'l', LeftMargin=3, col=cols[1])
#' 			
#' rtsplot2Y(y1, type='l', las=1, col=cols[2], col.axis=cols[2])
#' 
#' rtsplot.legend('SPY(rhs),IBM(lhs)', cols, list(y,y1))
#' @export 
###############################################################################
rtsplot2Y <- function(
	y,			# xts object to plot
	las = 1,	# rotation of Y axis labels
	type = 'l',	# plot type
	...			# other parameters to plot
)
{
	# exctract visible plot data
	xlim = graphics::par('usr')[1:2]

	# subset	
	class(xlim) = c('POSIXct', 'POSIXt')
	y1 = y[paste(format(xlim, '%Y:%m:%d %H:%M:%S'), sep = '', collapse = '::')]	
	

	# plot
	graphics::par(new = TRUE)
	xlim = graphics::par('usr')[1:2]
	graphics::plot( attr(y1, 'index') , y1[,1], xlim = xlim, xaxs = 'i', type = type,
		yaxt = 'n', xaxt = 'n', xlab = '', ylab = '', axes = FALSE, ... )
		
		# Y axis rotation
		graphics::axis(2, las = las, ...) 
}


###############################################################################
#' Add grid to time series plot
#'
#' @param grid which grid lines to draw, \strong{defaults to 'xy'}
#' @param xaxis.ticks location of x axis ticks
#' @param col grid color, \strong{defaults to rtsplot.theme()$grid.color}
#'
#' @return nothing
#'
#' @export 
###############################################################################
rtsplot.grid <- function
(
	grid, 
	xaxis.ticks,
	col = rtsplot.theme()$grid.color
) 
{
	#abline( h = axTicks(2), col = 'lightgray', lty = 'dotted')
	#abline( v = rtsplot.control$xaxis.ticks, col = 'lightgray', lty = 'dotted')
	
	# grid flag 'xy', 'x', 'y', '' or NA, NULL
	graphics::abline( h = graphics::axTicks(2), col = col)
	graphics::abline( v = xaxis.ticks, col = col)
	
}


###############################################################################
#' Add lines to time series plot
#'
#' @param y \code{\link{xts}} object
#' @param type line type, \strong{defaults to 'l'}, for more info see \code{\link{lines}}
#' @param col color, \strong{defaults to par('col')}
#' @param ... additional parameters to the \code{\link{lines}}
#'
#' @return nothing
#'
#' @export 
###############################################################################
rtsplot.lines <- function(
	y,					# xts object to plot
	type = 'l',			# plot type
	col = graphics::par('col'),	# color
	...					# other parameters to lines
)
{
	if(quantmod::has.Cl(y)) y1 = quantmod::Cl(y) else y1 = y[,1]	
	
	temp.x = attr(y, 'index')
	
	if( type == 'l' & len(col) > 1 ) {
		for( icol in unique(col) ) {
			graphics::lines(temp.x, iif(col == icol, y1, NA), type = type, col = icol, ...)
		}
	} else {
		graphics::lines(temp.x, y1, type = type, col = col, ...)
	}
}


###############################################################################
#' Add text to time series plot
#'
#' @param y \code{\link{xts}} object
#' @param ... additional parameters to the \code{\link{lines}}
#'
#' @return nothing
#'
#' @export 
###############################################################################
rtsplot.text <- function(
	y,					# xts object to plot
	...					# other parameters to text
)
{
	if(quantmod::has.Cl(y)) y1 = quantmod::Cl(y) else y1 = y[,1]	
	
	temp = attr(y1, 'index')
		# class(temp)='POSIXct' 
	graphics::text(temp, y1, ...)
}


###############################################################################
#' Format numbers using 1000 separator
#'
#' @param temp numbers
#' @param nround number of rounding digits, \strong{defaults to '2'}
#' @param sprefix start prefix string, \strong{defaults to ''}
#' @param eprefix end postfix string, \strong{defaults to ''}
#'
#' @return numbers formatted using 1000 separator
#'
#' @export 
###############################################################################
rtsplot.format <- function(
	temp,			# numbers
	nround = 2,		# number of rounding digits
	sprefix = '',	# start prefix string
	eprefix = ''	# end postfix string
)
{
	return( paste(sprefix, 
			format(round(as.numeric(temp), nround), big.mark = ',', scientific=FALSE),
			eprefix ,sep='') )
}


###############################################################################
#' Plot legend - shortcut to the \code{\link{legend}} function
#'
#' @param labels legend labels
#' @param fill fill colors, \strong{defaults to NULL}
#' @param lastobs list of last observations, \strong{defaults to NULL}
#' @param x location of legend, \strong{defaults to 'topleft'}
#' @param merge merge, \strong{defaults to FALSE}, see \code{\link{legend}} function for more info
#' @param bty box, \strong{defaults to 'n'}, see \code{\link{legend}} function for more info
#' @param border border color, \strong{defaults to NA - no color}
#' @param yformat format Y values function, \strong{defaults to \code{\link{rtsplot.format}}}
#' @param cex font size, \strong{defaults to 1}
#' @param ... other parameters to legend, see \code{\link{legend}} function for more info
#'
#' @return nothing
#'
#' @export 
###############################################################################
rtsplot.legend <- function
(
	labels,					# labels
	fill = NULL,			# fill colors
	lastobs = NULL, 		# last observations
	x = 'topleft',			# location of legend
	merge = FALSE, 				# merge
	bty = 'n',				# box
	border = NA,			# the border color for the boxes / NA means no color 
	yformat = rtsplot.format,	# format values
	cex = 1,
	...						# other parameters to legend
)
{
	# split fill colors & labels
	if( !is.null(fill) ) fill = spl( as.character(fill) )	
	labels = spl( as.character(labels) )
	
	# if last observations, add them to labels
	if( !is.null(lastobs) ) {
		if( is.list(lastobs) ) {
			labels1 = sapply(lastobs, function(x) unclass(mlast(x))[1])
		} else { 
			labels1 = unclass(mlast(lastobs))[1]; 
		}		
		# format last observations
		labels = paste(labels, match.fun(yformat)( labels1 ))		
	}	
	
	# plot legend
	theme = rtsplot.theme()	
	a = graphics::legend(x, legend = labels, fill = fill, merge = merge, bty = bty, border=border, cex = cex * theme$cex, ...)
		left = a$rect$left
		r = a$rect$left+a$rect$w
		
		top = a$rect$top
		b = a$rect$top-a$rect$h		
		if(graphics::par('ylog')) {
            top = 10^top
            b = 10^b
        }
        graphics::rect(left, top, r, b, col=theme$legend.bg.col, border=border)

	graphics::legend(x, legend = labels, fill = fill, merge = merge, bty = bty, border=border, cex = cex * theme$cex, ...)
}	


###############################################################################
#' Create layout
#'
#' @param ilayout matrix stored as a string 
#' @param delim delimiter, \strong{defaults to ','}
#'
#' @return nothing
#'
#' @export 
###############################################################################
rtsplot.layout <- function(
	ilayout,	# matrix stored as a string 
	delim = ','	# delimiter
)
{	
	ilayout = matrix( as.double(spl( gsub('\n', delim, ilayout), delim)), 
				nrow = len(spl(ilayout, '\n')), byrow=TRUE)
	graphics::layout(mat = ilayout)
}	




###############################################################################
# rtsplot.dx - determine data spacing along X axis
###############################################################################
rtsplot.dx <- function
(
	y	# xts object to plot
)
{ 
	# determine portion of data visible on X axis
	xlim = graphics::par('usr')[1:2]

	# subset	
	class(xlim) = c('POSIXct', 'POSIXt')
	y1 = y[paste(format(xlim, '%Y:%m:%d %H:%M:%S'), sep = '', collapse = '::')]

	
	# R by default extends xrange by 1.08
	xlim = graphics::par('usr')[1:2]
	xportion = min(1, diff(unclass(range(attr(y1, 'index'))))*1.08 / diff(xlim) )
	return( xportion * diff(xlim) / ( 2* nrow(y1)  ) )
}


###############################################################################
#' Highlight vertical segments
#'
#' @param y \code{\link{xts}} object
#' @param highlight segments to highlight along X axis
#' @param col highlight color, \strong{defaults to rtsplot.control$col.x.highlight}
#'
#' @return nothing
#'
#' @export 
###############################################################################
rtsplot.x.highlight <- function
(
	y,						# xts object to plot
	highlight,				# segments to highlight along X axis
	col = rtsplot.theme()$col.x.highlight
)
{
	if(len(col)==1) {
		rtsplot.x.highlight.helper(y, highlight, col = col)		
	} else { # do for each color
		for( icol in unique(col[highlight]) ) {
			rtsplot.x.highlight.helper(y, iif(col == icol, highlight, FALSE), col = icol)					
		}
	}
}


rtsplot.x.highlight.helper <- function
(
	y,						# xts object to plot
	highlight,				# segments to highlight along X axis
	col
)
{
	dx = rtsplot.dx(y);	
	hl_index = highlight;
	
	if( is.logical(highlight) ) hl_index = which(highlight);
	if( identical(unique(highlight) , c(0, 1)) ) hl_index = which(as.logical(highlight));

	# determine continuous segments to highlight
	hl_index1 = which(diff(hl_index) > 1 )	
	hl_index = hl_index[ sort(c(1, len(hl_index), hl_index1, (hl_index1+1))) ]
	
	# see par documentation
	temp.y = graphics::par('usr')[3:4]
	if(graphics::par('ylog')) temp.y = 10^temp.y
	
	
	temp.x = attr(y, 'index')		
	for( i in seq(1,len(hl_index),2) ) {		
		graphics::rect(temp.x[hl_index[i]] - dx/2, temp.y[1],
			temp.x[hl_index[(i + 1)]] + dx/2, temp.y[2],
            col = col, border = col ) 		
	}
	#box();		
}


###############################################################################
#' Highlight horizontal segments
#'
#' @param highlight segments to highlight along Y axis
#' @param col highlight color, \strong{defaults to rtsplot.control$col.y.highlight}
#'
#' @return nothing
#'
#' @examples
#' \donttest{ 
#' # download data
#' data.spy = getSymbols('SPY', auto.assign = FALSE)
#' rsi = RSI(Cl(data.spy), 20)
#'  	
#' #set up two regions for graphs candlestick price data on top 2/3 of the plot
#' #and rsi on the bottom 1/3 of the plot
#' layout(c(1,1,2))  
#' 	
#' rtsplot(data.spy, type = 'candle', plotX = FALSE)
#'   rtsplot.legend('SPY', 'grey70', data.spy)
#' rtsplot(rsi, type = 'l')
#' 
#' col = grDevices::adjustcolor(c('green','red'), 80/255)
#' rtsplot.y.highlight(col=col[1], highlight=c(50,100))	
#' rtsplot.y.highlight(col=col[2], highlight=c(0,50))	
#' 	
#' abline(h = 50, col = 'gray20')
#' 
#'   col = iif(mlast(rsi)>50,'black','red')
#' rtsplot.legend('RSI(20)', col, rsi, text.col=col)
#' }
#' @export 
###############################################################################
rtsplot.y.highlight <- function
(
	highlight,			# segments to highlight along Y axis
	col = rtsplot.theme()$col.y.highlight
)
{
	# see par documentation
	temp.y = graphics::par('usr')[3:4]
	if(graphics::par('ylog')) temp.y = 10^temp.y

	temp.x = graphics::par('usr')[1:2]
	if(graphics::par('xlog')) temp.x = 10^temp.x
		
	highlight[highlight == Inf] = temp.y[2]
	highlight[highlight == -Inf] = temp.y[1]
	
	for( i in seq(2,len(highlight),by=2) ) {
		icol = col[(i/2)]
		graphics::rect(temp.x[1], highlight[(i-1)],
			temp.x[2], highlight[i],
            col = icol, border = icol ) 			
	}
	#box();
}

###############################################################################
# rtsplot color helper functions for candles and volume
###############################################################################
rtsplot.candle.col <- function(	y ) {
	theme = rtsplot.theme()
	return( iif( quantmod::Cl(y) > quantmod::Op(y), theme$col.up, theme$col.dn) )
}
rtsplot.volume.col <- function( y ) { 
	theme = rtsplot.theme()
	return( iif( quantmod::Cl(y) > mlag(quantmod::Cl(y)), theme$col.up, theme$col.dn) )
}

###############################################################################
# rtsplot.candle - plot candles
#  rtsplot.candle will try to plot candles if dx is sufficient
#  otherwise ohlc or bars 
###############################################################################
rtsplot.candle <- function
(
	y,					# xts object to plot
	col = rtsplot.candle.col(y),
	border = rtsplot.theme()$col.border
)
{
	dx = rtsplot.dx(y)
	# convert dx to line width
	dxi0 = ( dx / graphics::xinch() ) * 96

	if( dxi0 < 1 ) {
		rtsplot.hl.lwd(y, col = col, lwd = 1)
	} else if ( dxi0 < 1.75 ) {
		rtsplot.ohlc.lwd(y, col = col, lwd = 1)
	} else {
		temp.x = attr(y, 'index')
		
		graphics::rect(temp.x - dx/10, quantmod::Lo(y), temp.x + dx/10, quantmod::Hi(y), 
			col = border, border = border)
		graphics::rect(temp.x - dx/2, quantmod::Op(y), temp.x + dx/2, quantmod::Cl(y), 
			col = col, border = border)	
	} 
}

###############################################################################
# rtsplot.ohlc - plot ohlc
#  rtsplot.ohlc will try to plot ohlc if dx is sufficient
#  otherwise ohlc or bars 
###############################################################################
rtsplot.ohlc <- function
(
	y,					# xts object to plot
	col = rtsplot.theme()$col.border
)
{
	dx = rtsplot.dx(y)
	# convert dx to line width
	dxi0 = ( dx / graphics::xinch() ) * 96
		
	if( dxi0 < 1 ) {
		rtsplot.hl.lwd(y, col = col, lwd = 1)
	} else if ( dxi0 < 1.75 ) {
		rtsplot.ohlc.lwd(y, col = col, lwd = 1)
	} else {
		temp.x = attr(y, 'index')
		
		graphics::rect(temp.x - dx/8, quantmod::Lo(y), temp.x + dx/8, quantmod::Hi(y), col = col, border = col)
		graphics::segments(temp.x - dx/2, quantmod::Op(y), temp.x, quantmod::Op(y), col = col)	
		graphics::segments(temp.x + dx/2, quantmod::Cl(y), temp.x, quantmod::Cl(y), col = col)	
	}
}

###############################################################################
# rtsplot.hl - plot hl
###############################################################################
rtsplot.hl <- function
(
	y,					# xts object to plot
	col = rtsplot.volume.col(y),
	border = rtsplot.theme()$col.border
)
{
	dx = rtsplot.dx(y)
	# convert dx to line width	
	dxi0 = ( dx / graphics::xinch() ) * 96
	
	if( dxi0 < 1.75 ) {
		rtsplot.hl.lwd(y, col = col, lwd = 1)
	} else {
		temp.x = attr(y, 'index')
		
		graphics::rect(temp.x - dx/2, quantmod::Lo(y), temp.x + dx/2, quantmod::Hi(y), 
			col = col, border = border)
	}
}

###############################################################################
# rtsplot.ohlc.lwd - plot ohlc using line width
###############################################################################
rtsplot.ohlc.lwd <- function
(
	y,					# xts object to plot
	lwd=1,				# line width
	...					# other parameters to segments
)
{
	dx = rtsplot.dx(y)
	temp.x = attr(y, 'index')	
	
	graphics::segments(temp.x, quantmod::Lo(y), temp.x, quantmod::Hi(y), lwd = lwd, lend = 2,  ...)
	graphics::segments(temp.x - dx/2, quantmod::Op(y), temp.x, quantmod::Op(y), lwd = lwd, lend = 2, ...)
	graphics::segments(temp.x + dx/2, quantmod::Cl(y), temp.x, quantmod::Cl(y), lwd = lwd, lend = 2, ...)
}

###############################################################################
# rtsplot.hl.lwd - plot hl using line width
###############################################################################
rtsplot.hl.lwd <- function
(
	y,					# xts object to plot
	lwd=1,				# line width
	...					# other parameters to segments
)
{
	temp.x = attr(y, 'index')	
	
	graphics::segments(temp.x, quantmod::Lo(y), temp.x, quantmod::Hi(y), lwd = lwd, lend = 2, ...)
}


###############################################################################
#' Plot volume
#'
#' @param y \code{\link{xts}} object
#' @param col color for volume bars
#' @param border color for volume bars border
#'
#' @return nothing
#'
#' @export
###############################################################################
rtsplot.volume <- function
(
	y,							# xts object to plot
	col = rtsplot.volume.col(y),	# color
	border = rtsplot.theme()$col.border
)
{
	dx = rtsplot.dx(y)
	# convert dx to line width
	dxi0 = ( dx / graphics::xinch() ) * 96
	
	temp.x = attr(y, 'index')	
	
	if( dxi0 < 1.75 ) {
		graphics::segments(temp.x, 0, temp.x, quantmod::Vo(y), col = col, lwd = 1, lend = 2)	
	} else {
		graphics::rect(temp.x - dx/2, 0, temp.x + dx/2, quantmod::Vo(y), 
			col = col, border = border)
	}
	
	idv = grep('Volume', colnames(y)) 
	temp = spl(colnames(y)[idv], ';')
	if( len(temp) > 1 ) rtsplot.legend(temp[len(temp)], x='topright')	
}


###############################################################################
#'  Scale volume
#'
#' @param y \code{\link{xts}} object
#'
#' @return adjusted y object
#'
#' @export
###############################################################################
rtsplot.scale.volume <- function(y) 
{
	Volumes = quantmod::Vo(y)
	max.vol = max(Volumes, na.rm = TRUE)
	vol.scale = list(100, '100s')
	if (max.vol > 10000) 
		vol.scale = list(1000, '1000s')
	if (max.vol > 1e+05) 
		vol.scale = list(10000, '10,000s')
	if (max.vol > 1e+06) 
		vol.scale = list(1e+05, '100,000s')
	if (max.vol > 1e+07) 
		vol.scale = list(1e+06, 'millions')
     
	idv = grep('Volume', colnames(y))
	y[, idv] = Volumes/vol.scale[[1]]
	colnames(y)[idv] = paste( colnames(y)[idv], vol.scale[[2]], sep=';' )
  	return(y)
}	



###############################################################################
#' Create Stacked plot
#'
#' @param x dates object
#' @param y matrix with weights
#' @param xlab X label, \strong{defaults to ''}, for more info see \code{\link{plot}}
#' @param cols colors, \strong{defaults to colors \code{\link{rtsplot.theme}}}
#' @param type plot type: lines, step stairs c('l','s')
#' @param flip.legend flag to reverse legend order, \strong{defaults to FALSE}
#' @param ... additional parameters to the \code{\link{plot}}
#'
#' @return nothing
#'
#' @export 
###############################################################################
rtsplot.stacked <- function
(
	x,				# x data
	y, 				# matrix with y data : len(x) = nrow(y)
	xlab='',		# x axis label	
	cols = rtsplot.colors(ncol(y)), # colors
	type=c('l','s'),# plot type  : lines, step stairs
	flip.legend = FALSE,# reverse legend order
	...				# other parameters for plot
)
{

	# transform y
	y = 100 * y
	
	y1 = list()
	y1$positive = y
		y1$positive[ y1$positive < 0 ] = 0
	
	y1$negative = y
		y1$negative[ y1$negative > 0 ] = 0
		
	# find y ranges
	ylim = c(min(rowSums(y1$negative, na.rm = TRUE)), max(1, rowSums(y1$positive, na.rm = TRUE)))
	
	# create empty plot
	# par(mar = c(4, 4, 2, 1), cex = 0.8)
	if( class(x)[1] != 'Date' & class(x)[1] != 'POSIXct') {
		graphics::plot(x, rep(0, len(x)), ylim = ylim, t = 'n', xlab = '', ylab = '', cex = graphics::par('cex'), ...)
		graphics::grid()
	} else {
		#plot(x, rep(0, len(x)), ylim = ylim, t = 'n', yaxt = 'n', xaxt = 'n', xlab = '', ylab = '', cex = graphics::par('cex'), ...)
		#	axis(2)
		#	xaxis.ticks = axis.Date(1, x, labels = TRUE, tick = TRUE)		
		#	
		#	abline( h = axTicks(2), col = 'lightgray', lty = 'dotted')
		#	abline( v = xaxis.ticks, col = 'lightgray', lty = 'dotted')		
		
		rtsplot(xts::xts(y[,1], x), ylim = ylim, cex = graphics::par('cex'), LeftMargin = 4, ...)
		graphics::axis(2, las = 1) 
		x = unclass(as.POSIXct(x))
	}
		
	graphics::mtext('Allocation %', side = 2,line = 3, cex = graphics::par('cex'))
	graphics::mtext(xlab, side = 1,line = 2, cex = graphics::par('cex'))		
	
	
	# plot stacked areas	
	if( type[1] == 'l' ) {
		prep.x = c(x[1], x, x[len(x)])     
		
		for( y in y1 ) {   	
			for (i in ncol(y) : 1) {
		    	prep.y = c(0, rowSums(y[, 1 : i, drop = FALSE]), 0)
		    	graphics::polygon(prep.x, prep.y, col = cols[i], border = NA, angle = 90)
			}
		}
    } else {
    	# http://r.789695.n4.nabble.com/how-to-fill-between-2-stair-plots-td819257.html
    	dx = mean(diff(x))
   		prep.x = c(rep(x,each=2), x[len(x)] + dx, x[len(x)] + dx)     
   		
   		for( y in y1 ) {   	
			for (i in ncol(y) : 1) {
		    	prep.y = c(0, rep(rowSums(y[, 1 : i, drop = FALSE]),each=2), 0)
		    	graphics::polygon(prep.x, prep.y, col = cols[i], border = NA, angle = 90)
		    }    
		}
	} 

    # legend
    if(flip.legend)
    	rtsplot.legend(rev(colnames(y)), rev(cols))    
    else
    	rtsplot.legend(colnames(y), cols)    
}


###############################################################################
#' \code{\link{matplot}} version for \code{\link{xts}} object
#'
#' @param y \code{\link{xts}} object
#' @param dates subset of dates\strong{defaults to NULL}
#' @param ylim range on Y values, \strong{defaults to NULL}
#' @param type plot type, \strong{defaults to 'l'}, see \code{\link{plot}} for details
#' @param cols colors
#' @param ... additional parameters to the \code{\link{matplot}}
#'
#' @return nothing
#'
#' @export 
###############################################################################
rtsplot.matplot <- function
(
	y,				# xts object or list of xts objects to plot
	dates = NULL,	# dates subset	
	ylim = NULL,
	type = 'l',
	cols = rtsplot.colors(ncol(y)), # colors
	...				# other parameters for plot
)
{
	# find ylim	
	if( is.list(y) ) {
		if(!is.null(dates)) y[[1]] = y[[1]][dates]
		
		if(is.null(ylim)) {
			ylim = c()
			n = len(y)
			for( i in 1:n ) {
	   			if(!is.null(dates)) y[[i]] = y[[i]][dates]
				ylim = range(ylim, y[[i]], na.rm = TRUE)
			}
		}
		
		rtsplot(y[[1]], ylim = ylim, col = cols[1], type = type, ...)
		if( n > 1 ) {
			for( i in 2:n ) rtsplot.lines(y[[i]], col = cols[i], type = type, ...)
		}

		rtsplot.legend(names(y), paste(1:n), y)	
				
	} else {
		n = ncol(y)
		if(!is.null(dates)) y = y[dates]
		if(is.null(ylim)) ylim = range(y, na.rm = TRUE)
		
		rtsplot(y[,1], ylim = ylim, col = cols[1], type = type, ...)
		if( n > 1 ) {
			for( i in 2:n ) rtsplot.lines(y[,i], col = cols[i], type = type, ...)
		}
		
		rtsplot.legend(names(y), paste(1:n), as.list(y))	
	}	
}


###############################################################################
#' Plot corner label
#'
#' Plot corner label, based on the [text at the upper left corner outside of the plot region](http://r.789695.n4.nabble.com/text-at-the-upper-left-corner-outside-of-the-plot-region-td885675.html)
#'
#' @param label label
#' @param col label color
#' @param x x location, \strong{defaults to -1}
#' @param y y location, \strong{defaults to 1}
#' @param xoffset x offset, \strong{defaults to NA}
#' @param yoffset y offset, \strong{defaults to NA}
#' @param space coordinate space, can be "plot" or "figure", \strong{defaults to "plot"}
#' @param cex font size, \strong{defaults to 1}
#' @param border border color, \strong{defaults to NA - no color}
#'
#' @return nothing
#'
#' @examples
#'  rtsplot.theme.set(legend.bg.col=grDevices::adjustcolor('orange', 200/255))
#'  plot(rnorm(20), rnorm(20))
#'
#'  rtsplot.corner.label('test1', y=-1, space='figure')
#'  rtsplot.corner.label('test2', y=1, space='figure')
#'  rtsplot.corner.label('test3', x=1, space='figure')
#'  rtsplot.corner.label('test4', x=1, y=-1, space='figure')
#'  rtsplot.theme.set(legend.bg.col=grDevices::adjustcolor('white', 50/255))
#'
#' @export 
###############################################################################
rtsplot.corner.label = function(
	label = NULL,
	col = 'black',
	x = -1,
	y = 1,
	xoffset = NA,
	yoffset = NA, 
	space = c("plot","figure"), 
	cex = 1,
	border = NA			# the border color for the boxes / NA means no color 
) {
	theme = rtsplot.theme()	

	if(is.na(xoffset)) xoffset = graphics::strwidth("m", cex = cex * theme$cex)/2
	if(is.na(yoffset)) yoffset = graphics::strheight("m", cex = cex * theme$cex)/2

	par.usr = graphics::par("usr")
	xpos = par.usr[(3+x)/2]
	ypos = par.usr[(3+y)/2+2]

	if(match(space[1],"figure",0)) {
		par.pin = graphics::par("pin")
		xplotrange = par.usr[2]-par.usr[1]
		yplotrange = par.usr[4]-par.usr[3]
		
		par.mai = graphics::par("mai")
		xmar = xplotrange*par.mai[3+x]/par.pin[1]
		ymar = yplotrange*par.mai[2+y]/par.pin[2]
		
		xpos = xpos+x*xmar
		ypos = ypos+y*ymar
 	}
	
	if(!is.null(label)) {
		if(match(space[1],"figure",0)) graphics::par(xpd=TRUE)

		xh = 5/3*graphics::strwidth(label, cex = cex * theme$cex)
		yh = 5/3*graphics::strheight(label, cex = cex * theme$cex)
		if(!is.null(theme$legend.bg.col)) 
			graphics::rect(xpos-xh,ypos-yh, xpos-x*xoffset + xh, ypos-y*yoffset + yh, col=theme$legend.bg.col, border=border)
		graphics::text(xpos-x*xoffset,ypos-y*yoffset,label,adj=c((1+x)/2,(1+y)/2), col = col, cex = cex * theme$cex)

		if(match(space[1],"figure",0)) graphics::par(xpd=FALSE)
	}	
}
