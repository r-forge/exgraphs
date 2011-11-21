# before after pairs plot
ba.pairs <- function( before, after, labels = NULL, xlim = NULL, ylim = NULL, col = "black" )
{
  dibf <- dim( before )
  diaf <- dim( after )
  if( any( dibf != diaf ) ) { stop() }
  if( length( col ) != 1 && length( col ) != dibf[1] ) { 
    stop("color must be 1 or as long as the data") 
  } else if( length( col ) == 1 ){ 
    col = rep( col, dibf[1] )
  }
  if( is.null( labels ) ){ labels <- if( !is.null( dimnames( before ) ) ){
                                          dimnames( before )[[2]]
                                      } else{ 1:(dibf[2]) } 
                          }
  par( mfrow = c( dibf[2], dibf[2] ) )
  for( k in 1:dibf[2] ){
    for( l in 1:dibf[2] ){
      if( k != l ){
        ydata <- c( before[,k], after[,k] )
        xdata <- c( before[,l], after[,l] )
        plot( xdata, ydata, type="n", xlab="", ylab="",
              xlim = xlim, ylim = ylim )
        for(i in 1:dibf[1]){ 
          arrows( x0 = before[i,l], x1 = after[i,l],
                  y0 = before[i,k], y1 = after[i,k],
                 col = col[i], length = 0.05 )
        }
      } else{
        plot.new()
        text( 0.5, 0.5, labels[k], cex = 2 )
      }
    }
  }
}