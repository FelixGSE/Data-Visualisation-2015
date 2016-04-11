################################################################################

################################################################################

# Author:       Felix Gutmann
# Programm:     Barcelona graduate school of economics - M.S. Data Science 
# Course:       14D007 Data Visualization
# Last update:  09.04.2015

# Decription:   This file initializes both centrality meassures for Input-
#               Output Tables  

################################################################################
# Random closeness centrality
################################################################################

random.walk.closeness.centrality <- function( A ){
  
  # Get dimensions and initialize H matrix
  N <- nrow( A )
  H <- matrix( 0, nrow = N, ncol = N )
  # Intialize A and I 
  A <- diag( N ) - solve( diag( colSums( t(A) ) ) ) %*% A
  I <- solve( A[2:N,2:N] )
  
  for( i in 1:N ){
    
    # Compute current replacement indices
    ifelse( ( i - 1 ) == 0 ,
            index01 <- c( (i+1):N ),
            ifelse( i == N, 
                    index01 <- c( 1:(i-1)  ),
                    index01 <- c( 1:(i-1) , (i+1):N )
            )
    )
    
    ifelse( (i+2) > N, 
            index02 <- c( 1:i  ),
            index02 <- c( 1:i , (i+2):N )
    )
    
    
    # Update H
    H[index01,i] <- I %*% rep(1, ( N - 1 ) )
    
    # Compute next inverse by Sherman–Morrison
    if( i < N ){
      
      u   <- A[ index02, i  ] - A[ index01, (i+1) ]
      n01 <- as.numeric( ( 1 + I[ i , ] %*% u ) )
      I   <- I - ( ( I %*% u ) %*% I[ i , ]  ) / n01
      v   <- A[ i , index02 ] - A[ (i+1), index01 ]
      n02 <-  as.numeric(  ( 1+ v %*% I[,i] ) ) 
      I   <- I - ( I[ ,i] %*% ( v %*% I ) ) / n02
      I   <- solve( A[index02, index02 ]  )
      
      if( any(is.infinite( I ) ) ){
        
        I <- solve( A[ index02, index02 ] )
        
      } 
    }
  }
  
  # Compute final centrality
  final.result <- N / colSums(H) 
  
  # Return final result
  return( final.result )
  
}

################################################################################
# Random Walk counting centrality
################################################################################

random.walk.counting.centrality <- function( A ){
  
  # Compute Dimension - Nrow is enough since A is quadratic
  N               <- nrow( A )
  full.dimension  <- 1:N
  
  # Compute D - Matrix
  D    <- diag( rowSums( A , na.rm = FALSE ) ) 
  
  # Compute take matrix
  temp <- ifelse( A > 0 , 1 , 0 )
  temp <- ifelse( ( temp + t( temp ) ) > 0 , 1, 0 )
  Take <- temp * 0.5

  # Initialize result matrix - This is very inefficient!!!
  result <- matrix( NA , nrow = 0 , ncol = N ) 
  
  # Compute centrality - This is where the magic happens
  for( t in 1:N ){
    
    # Compute transition matrix for node t - exclude t from network
    temp.dimension <- setdiff( full.dimension , t )
    A.temp         <- A[temp.dimension,temp.dimension]
    D.temp         <- D[temp.dimension,temp.dimension]
    T.temp         <- solve( D.temp - A.temp )
    
    # Subset take matrix for node t
    Take.temp      <- Take[temp.dimension,temp.dimension]
    
    # Compute centrality for node t
    for( s in 1:( N - 1 ) ){
      
      Z.temp  <- diag( as.numeric( T.temp[s,] %*% A.temp ) ) 
      W.temp  <- ( Z.temp + t( Z.temp ) ) * Take.temp
    
      cs.temp <- as.numeric( colSums( W.temp , na.rm = FALSE ) )
      rs.temp <- as.numeric( rowSums( W.temp , na.rm = FALSE ) )
      
      cm.temp <- append( 0.5 * ( cs.temp + rs.temp ) , 0 , (t - 1) )
      result  <- rbind( result , cm.temp )  
   
    }
  }
  
  # Design final centrality meassure
  cm.final     <- colSums(result) +  2 * ( N - 1 ) * rep(1, N )
  numerator    <- ( N * ( N - 1 ) ) 
  final.result <- cm.final / numerator 
  
  # Return centrality
  return( final.result )
  
}

################################################################################

################################################################################