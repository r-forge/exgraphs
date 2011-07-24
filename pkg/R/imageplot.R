library( RColorBrewer )
# need to update this so that it automatically detects the best color scheme
imageplot <- function( mat, zrange = c( -1, 1 ), zbin = 11,
                       digits = 2, cex.col = 0.7, cex.var = 0.5,
                       xlabels = NULL, ylabels = NULL, color = rev( brewer.pal( zbin, "RdBu" ) ),
                       main = "", 
                       par.label = list( mar = c( 0.5, 0.1, 2, 0.1 ), pty = "m" ),
                       par.main = list( mar = c( 1, 4, 4, 1 ) ) )
{
    matrange <- range( mat )
    if( min( matrange ) < min( zrange ) ){ zrange[ which( zrange == min( zrange ) ) ] <- floor( min( matrange ) ) }
    if( max( matrange ) > max( zrange ) ){ zrange[ which( zrange == max( zrange ) ) ] <- ceiling( max( matrange ) ) }
    z.breaks = seq( zrange[1], zrange[2], length.out = zbin + 1 )
    z = array( as.double( cut( mat, breaks = z.breaks, labels = 1:zbin ) ), dim( mat ) )
    d = dim( mat )
    y <- 1:( d[1] + 1 )
    x <- 1:( d[2] + 1 )
    if( length( color ) < zbin ){ stop("color must be same length as zbin") }
    layout( matrix( c( 2, 1 ), 1, 2, byrow = FALSE ), c( 10.5, 1.5 ) )
    # legend
    op <- par( names( par.label ) )
    par( par.label )
    plot( c( 0, 1 ), c( min( z.breaks ), max( z.breaks ) ), type = "n",
        bty = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n" )
    for( i in 2:( length( z.breaks ) ) ) {
        rect( xleft = 0.5, ybottom = z.breaks[i - 1], 
              xright = 1, ytop = z.breaks[i], col = color[i - 1] )
        text( x = 0.45, y = z.breaks[i - 1], 
              labels = format(round(z.breaks[i - 1], digits)), 
              cex = cex.col, adj = 1, xpd = TRUE)
    }
    rect( xleft = 0.5, ybottom = z.breaks[length( z.breaks )], 
          xright = 1, ytop = z.breaks[length( z.breaks )], col = color[length( color )])
    text( x = 0.45, y = z.breaks[length( z.breaks )], 
          labels = format(round(z.breaks[length( z.breaks )], digits ) ), 
          cex = cex.col, adj = 1, xpd = TRUE )
    par( op )
    # main plot
    op <- par( names( par.main ) )
    par( par.main)
    plot( range( x ),range( y ), axes = FALSE, type = "n",
          xlim = range( x ), ylim = range( y ),
          xaxs = "i", yaxs = "i", xlab = "", ylab = "" )
    for( i in 1:d[2] ){
        for( j in d[1]:1 ){
            rect( x[i], y[j], x[i + 1], y[j + 1], col = color[z[j,i]], 
                  border = color[z[j,i]] )
        }
    }
    if( is.null( xlabels ) ){ xlabels <- 1:d[2] }
    if( is.null( ylabels ) ){ ylabels <- 1:d[1] }
    axis( 2, at = ( ( 1:d[1] ) + 0.5 ), labels = rev( ylabels ), 
             las = 2, tick = FALSE, line = -1 )
    axis( 3, at = ( ( 1:d[2] )+ 0.5 ), labels = xlabels ,
             las = 3, tick = FALSE, line = -1 )
    title( main = main )
    par( op )
    invisible( z )
}


