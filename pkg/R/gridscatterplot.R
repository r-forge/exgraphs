color.gradient<-
function( n, hue, alpha = 1 ) 
{
    hsv( h     = hue, 
         s     = seq( from = 0, to = 1, length.out = n ), 
         v     = 1, 
         alpha = alpha )
}

gridScatterPlot <- function( count, x.center = NULL, y.center = NULL, label = NULL, alpha = 1, col = NULL, 
                              mar = c( 3.5, 3.5, 2, 2 ), mgp = c( 2, 0.7, 0 ), tck = -0.01,
                              radi = 1, ... ){
    if( !is.array( count ) && !is.matrix( count ) ){ 
        stop( "count must be an array or a matrix", domain = NA )     
    }
    dm  <- dim( count )
    ldm <- length ( dm )    
    if( is.null( x.center ) ){ 
        x.breaks <- seq( 0, 1, length.out = ( dm[1] + 1 ) ) 
        x.center <- ( x.breaks[-1] + x.breaks[-length( x.breaks )] ) / 2  
    } else {
        if( dm[1] != length( x.center ) ) { stop("x length not consistent with count")}
        x.center <- x.center
    }
    if( is.null( y.center ) ){ 
        y.breaks <- seq( 0, 1, length.out = ( dm[2] + 1 ) ) 
        y.center <- ( y.breaks[-1] + y.breaks[-length( y.breaks )] ) / 2 
    } else {
        if( dm[2] != length( y.center ) ) { stop("y length not consistent with count")} 
        y.center <- y.center
    }
    if( ldm == 2 ){
        dimnames( count ) <- list( x.center, y.center )
    } else if ( ldm ==3 ){
        if( is.null( label ) ){
            label <- seq( dm[3] )
        } else {
            if( dm[3] != length( label ) ) { stop("label length not consistent with count")} 
        }
        dimnames( count ) <- list( x.center, y.center, as.character( label ) )
    } else {
        stop("count must be 2 or 3 dimensional array")
    }        
    maxcnt <- max( as.vector( count ) )
    dfz <- as.data.frame( as.table( count ) )
    if ( ldm == 2 ){
        dfz <- dfz[dfz[,3]!=0,]
        dfz <- dfz[sample(dim(dfz)[1]),] # random ordering to avoid one color always being on the top
        x   <- as.double( levels( dfz[,1] ) )[dfz[,1]]
        y   <- as.double( levels( dfz[,2] ) )[dfz[,2]]
        cls <- rep( 1, length( x ) )
        cnt <- as.double( dfz[,3] )
    } else if ( ldm == 3 ){
        dfz <- dfz[dfz[,4]!=0,]
        dfz <- dfz[sample(dim(dfz)[1]),] # random ordering to avoid one color always being on the top
        x   <- as.double( levels( dfz[,1] ) )[dfz[,1]]
        y   <- as.double( levels( dfz[,2] ) )[dfz[,2]]
        cls <- as.double( dfz[,3] )
        cnt <- as.double( dfz[,4] )
    }
    ucl <- length( unique( cls ) )
    pal <- matrix( NA, maxcnt, ucl ) # color pallet
    if( is.null( col ) ){
        hu  <- ( 1:ucl ) / ucl
    } else {
        if( length( col ) == ucl ){ 
            hu <- rgb2hsv( col2rgb( col ) )[1,]
        } else if( length( col ) == 1 ){
            hu <- rep( col, ucl )    
        } else {
            stop("col must be specified for each subset")
        }
    }
    for( i in 1:ucl ){
        pal[,i] <- color.gradient( maxcnt + 1, hue = hu[i], alpha = alpha )[-1]

    }
    jx <- jitter( x )
    jy <- jitter( y )
    plot( jx, jy, 
          type = "n",  
          col  = pal[as.matrix( cbind( cnt, cls ) )], ... )
    symbols( jx, jy, 
             circles = rep( radi / ( dm[1] ), length( x ) ), 
             add     = T, inches = F, 
             fg      = pal[as.matrix( cbind( cnt, cls ) )], 
             bg      = pal[as.matrix( cbind( cnt, cls ) )] )
    invisible( count )
}


gridScatterPlot3D <- function( count, x.center = NULL, y.center = NULL, z.center = NULL, label = NULL, alpha = 1, col = NULL, 
                              mar = c( 3.5, 3.5, 2, 2 ), mgp = c( 2, 0.7, 0 ), tck = -0.01,
                              radi = 1, ... ){
    if( !is.array( count ) ){ 
        stop( "count must be an array", domain = NA )     
    }
    dm  <- dim( count )
    ldm <- length ( dm )    
    if( is.null( x.center ) ){ 
        x.breaks <- seq( 0, 1, length.out = ( dm[1] + 1 ) ) 
        x.center <- ( x.breaks[-1] + x.breaks[-length( x.breaks )] ) / 2  
    } else {
        if( dm[1] != length( x.center ) ) { stop("x length not consistent with count")}
        x.center <- x.center
    }
    if( is.null( y.center ) ){ 
        y.breaks <- seq( 0, 1, length.out = ( dm[2] + 1 ) ) 
        y.center <- ( y.breaks[-1] + y.breaks[-length( y.breaks )] ) / 2 
    } else {
        if( dm[2] != length( y.center ) ) { stop("y length not consistent with count")} 
        y.center <- y.center
    }
    if( is.null( z.center ) ){ 
        z.breaks <- seq( 0, 1, length.out = ( dm[3] + 1 ) ) 
        z.center <- ( z.breaks[-1] + z.breaks[-length( z.breaks )] ) / 2  
    } else {
        if( dm[3] != length( z.center ) ) { stop("z length not consistent with count")}
        z.center <- z.center
    }
    if( ldm == 3 ){
        dimnames( count ) <- list( x.center, y.center, z.center )
    } else if ( ldm ==4 ){
        if( is.null( label ) ){
            label <- seq( dm[4] )
        } else {
            if( dm[4] != length( label ) ) { stop("label length not consistent with count")} 
        }
        dimnames( count ) <- list( x.center, y.center, z.center, as.character( label ) )
    } else {
        stop("count must be 3 or 4 dimensional array")
    }        
    maxcnt <- max( as.vector( count ) )
    dfz <- as.data.frame( as.table( count ) )
    if ( ldm == 3 ){
        dfz <- dfz[dfz[,3]!=0,]
        dfz <- dfz[sample(dim(dfz)[1]),] # random ordering to avoid one color always being on the top
        x   <- as.double( levels( dfz[,1] ) )[dfz[,1]]
        y   <- as.double( levels( dfz[,2] ) )[dfz[,2]]
        z   <- as.double( levels( dfz[,3] ) )[dfz[,3]]
        cls <- rep( 1, length( x ) )
        cnt <- as.double( dfz[,4] )
    } else if ( ldm == 4 ){
        dfz <- dfz[dfz[,4]!=0,]
        dfz <- dfz[sample(dim(dfz)[1]),] # random ordering to avoid one color always being on the top
        x   <- as.double( levels( dfz[,1] ) )[dfz[,1]]
        y   <- as.double( levels( dfz[,2] ) )[dfz[,2]]
        z   <- as.double( levels( dfz[,3] ) )[dfz[,3]]
        cls <- as.double( dfz[,4] )
        cnt <- as.double( dfz[,5] )
    }
    ucl <- length( unique( cls ) )
    pal <- matrix( NA, maxcnt, ucl ) # color pallet
    if( is.null( col ) ){
        hu  <- ( 1:ucl ) / ucl
    } else {
        if( length( col ) == ucl ){ 
            hu <- rgb2hsv( col2rgb( col ) )[1,]
        } else if( length( col ) == 1 ){
            hu <- rep( col, ucl )    
        } else {
            stop("col must be specified for each subset")
        }
    }
    for( i in 1:ucl ){
        pal[,i] <- color.gradient( maxcnt + 1, hue = hu[i], alpha = alpha )[-1]

    }
    jx <- jitter( x )
    jy <- jitter( y )
    jz <- jitter( z )
    plot3d( jx, jy, jz,
          type = "n",  
          col  = pal[as.matrix( cbind( cnt, cls ) )], ... )
          spheres3d(jx, jy, jz, radius= rep( radi / ( dm[1] ), length( x ) ), 
          color  = pal[as.matrix( cbind( cnt, cls ) )]
          )
    #symbols( jx, jy, 
    #         circles = rep( radi / ( dm[1] ), length( x ) ), 
    #         add     = T, inches = F, 
    #         fg      = pal[as.matrix( cbind( cnt, cls ) )], 
    #         bg      = pal[as.matrix( cbind( cnt, cls ) )] )
    invisible( count )
}

gridScatterPlot3D(aaa)
