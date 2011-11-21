mult.coefplot <- function( bugslist, coefs, lab=NULL, main.names, 
                            par.list = list( mar = c( 2, 1, 2, 1 ), oma = c( 0, 7, 0, 0 ), 
                                             mgp = c( 2,.5, 0 ), tck = -.04 ) 
                          )
{
  nbugs <- length(bugslist)
  ncoef <- length(coefs)
  par( mfrow=c( 1, nbugs) )
  par( par.list )
  y <- 1:ncoef
  for(j in 1:nbugs){
    bugsobj <- bugslist[[j]]
    if( is.null( lab)){ lab= dimnames(bugsobj$summary)[[1]][coefs]}
    xrange <- c(min( bugsobj$summary[coefs,"2.5%"]), max( bugsobj$summary[coefs,"97.5%"]))
    plot( xrange, c(1,ncoef), type="n", ylab="", xlab="",
          axes = F, main = paste( main.names[j],"estimate +- 2sd"))
    axis( 1 ) #,at=c(0,1,2,3),lab=c(0,1,2,3))
    if( j==1 ){ axis(2, at = 1:ncoef, lab=lab, las = 2, tick= F, outer = T ) }

    cm <- bugsobj$summary[coefs,"mean"]
    cs <- bugsobj$summary[coefs,"sd"]
    points( cm, y )
    for( i in 1:ncoef ){
      lines( c( cm[i] - 2 * cs[i], cm[i] + 2 * cs[i] ), c( y[i], y[i] ) )
    }
    lines( c( 0, 0 ),c( 0, ncoef + 1 ), lty = 2 )
  }
}

mult.coefplot(list(Elle.sim4,Elle.sim4,Elle.sim4), coefs=1:4, main.names=1:3)