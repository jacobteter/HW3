BML <- function(r, c, p, t){
  # This function creates an S3 object of the class BML grid. It is basically a glorified matrix, with other characteristics
  # an alternative: Blue = # of blue cars, Red = # of red cars
  stopifnot(class(r) == 'numeric' & r > 0)
  stopifnot(class(c) == 'numeric' & c > 0)
  stopifnot(class(p) == 'numeric' & 0 < p & p < 1)
    
  Cars <- matrix(0, nrow = r, ncol = c)
  Car.positions <- sample(r*c, size = p*r*c)
  Cars[Car.positions] <- 1
  # Red cars designated as 2s (move on EVEN time intervals)...derived as a subset of Blue cars
  Red.positions <- sample(which(Cars == 1), runif(1)*length(Cars[which(Cars == 1)]))
  Cars[Red.positions] <- 2
  if (missing(t)) {t <- 0L}
  # else {attr(Cars, 'time') <- as.integer(t)}
  attributes(Cars) <- list(time = t, dim = c(r,c),
                           prop.red = length(Cars[Cars == 2])/length(Cars), 
                           prop.blue = length(Cars[Cars == 1])/length(Cars),
                           prop.cars = length(Cars[Cars == 1 | Cars == 2])/length(Cars))
  class(Cars) <- c('BML', 'matrix')
  return(Cars) 
}

plot.BML <- function(x, y, ...){
  # This function realigns the BML grid matrix so that it plots as a square with the alignment and color specifications of the classic BML traffic problem
  par(mar = c(1.5, 0.1, 3, 0.1), mai = c(1, 0.1, 1, 0.2), oma = c(1, 0.5, 1, 0.5))
  # ideally the margins and format would scale to the RELATIVE lengths of columns versus rows...
  # also, image plots the thing at 90 degrees counterclockwise...
  x <- t(x[nrow(x):1, ])
  image(x, col = c('white', 'blue', 'red'), axes = FALSE)
}

# This function should be masked from the end user
Flag.Blocked.Move <-function(origin, r, i, j){
  
  ### Flag.Blocked.Move is a function that's called within the main function, Move
  ### ideally it should be masked from users. It does what it says it does.
  Move.pos <- which(origin == i)[order(which(origin == i))]
  Bloc.pos <- which(origin == j)[order(which(origin == j))]
  
  ## Directly.Blocked marks the cells in the vector of MOVING POSITIONS that are actually BLOCKED 
  Directly.Blocked <- ((Move.pos-1) %in% Bloc.pos & (Move.pos %% nrow(origin) != 1)) | ((Move.pos %% nrow(origin) == 1) & Move.pos %in% (Bloc.pos - (nrow(origin))+1))  
  # ORIGINAL if ((length(Move.pos[Directly.Blocked]) > 0)) {
  if (any(Directly.Blocked)) {
    ## Rules for being Indirectly Blocked (previous to the | is for direct cases, after the | is for wrapped cases)
    Indirectly.Blocked <- (((Move.pos-1) %in% (Move.pos[Directly.Blocked]) & Move.pos %% nrow(origin) != 1) | ((Move.pos %% nrow(origin) == 1) & (Move.pos %in% (Move.pos[Directly.Blocked] - (nrow(origin)) +1))))
    Blocked <- Directly.Blocked | Indirectly.Blocked
    # Blocked <- Directly.Blocked | Indirectly.Blocked
    if (length(Move.pos[Blocked]) > length(Move.pos[Directly.Blocked])) {
      New.Blocked <- (((Move.pos-1) %in% (Move.pos[Blocked]) & Move.pos %% nrow(origin) != 1) | ((Move.pos %% nrow(origin) == 1) & (Move.pos %in% (Move.pos[Blocked] - (nrow(origin)) +1))))
      Blocked <- Blocked | New.Blocked
      while (length(Move.pos[New.Blocked]) > 0) {
        New.Blocked <- ((((Move.pos-1) %in% (Move.pos[Blocked]) & Move.pos %% nrow(origin) != 1) | ((Move.pos %% nrow(origin) == 1) & (Move.pos %in% (Move.pos[Blocked] - (nrow(origin)) +1)))) & !(New.Blocked %in% Blocked))
        Blocked <- Blocked | New.Blocked
      }
    }
    Move.pos <- Move.pos[!Blocked]
  }
  New.pos <- Move.pos[Move.pos %% nrow(origin) != 1]-1   # New positions
  Wrapped.pos <- Move.pos[Move.pos %% nrow(origin) == 1] + nrow(origin) - 1   # Wrapped positions
  Moved.pos <- c(New.pos, Wrapped.pos) # together these are all the new positions
  origin[Move.pos] <- 0   # first vacate the previous positions     
  origin[Moved.pos] <- i   # i is initialized in the master function 'Movement' as the moving car value
  class(origin) <- c('BML', 'matrix')
  return(origin)
}

Move <- function(origin, t, start = 'blue'){
  # Origin is the starting matrix
  # t is the number of time increments through which to run time, a natural number greater than 1
  stopifnot(class(origin) == c('BML', 'matrix'))
  stopifnot(class(t) == 'numeric' & t > 0)
  t <- round(t, digits  = 0)
  final.time <- sum(t,attr(origin, 'time'))
  # if (t %% 2 == 1) {s <- t-1}
  ### This still doesn't address the issue of whether or not we're on an EVEN timestep..
  # else (s <- t)
  for (r in 1:t){
    # initialize the Move-ing and (potentially) Bloc-ing cars in a way that's dependent on time:
    i <- r %% 2
    j <- (r+1) %% 2 
    i[i == 0] <- 2
    j[j == 0] <- 2
    
    if(r %% 2 == 1) {
    origin <- Flag.Blocked.Move(origin, r, i, j)
    # return(origin)
    }
    if(r %% 2 == 0) {
      # For EVEN time periods, invert the matrix: t(), switch 1s and 2s (accomplished by i and j and modular arithmetic) 
      # have to rotate first on the even steps...This requires a 90 degree counter-clockwise spin, just like for the 'image' plot:
      origin <- t(origin[,ncol(origin):1])
      origin <- Flag.Blocked.Move(origin, r, i, j)
      
      origin <- t(origin[nrow(origin):1,]) # Now be exceedingly careful with the flip-back! (must be 90 degrees CLOCKWISE)
    # return(origin)
    }
  }
  
  r <- nrow(origin)
  c <- ncol(origin)
  
  attributes(origin) <- list(time = final.time, dim = c(r,c),
                           prop.red = length(origin[origin == 2])/length(origin), 
                           prop.blue = length(origin[origin == 1])/length(origin),
                           prop.cars = length(origin[origin == 1 | origin == 2])/length(origin))  
  class(origin) <- c('BML', 'matrix')
  return(origin)
}
  
# This function should be masked from the end user
Speed.Calc <-function(origin, t){
  t <- attr(origin, 'time')
  # note that the identities of i and j next to be flipped, or t <- t+1!
  t <- t+1
  i <- t %% 2
  j <- (t+1) %% 2 
  i[i == 0] <- 2
  j[j == 0] <- 2
  ### Speed.Calc ideally should be masked from users. It does what it says it does.
  Move.pos <- which(origin == i)[order(which(origin == i))]
  Bloc.pos <- which(origin == j)[order(which(origin == j))]
  
  ## Directly.Blocked marks the cells in the vector of MOVING POSITIONS that are actually BLOCKED 
  Directly.Blocked <- ((Move.pos-1) %in% Bloc.pos & (Move.pos %% nrow(origin) != 1)) | ((Move.pos %% nrow(origin) == 1) & Move.pos %in% (Bloc.pos - (nrow(origin))+1))  
  # ORIGINAL if ((length(Move.pos[Directly.Blocked]) > 0)) {
  if (any(Directly.Blocked)) {
    ## Rules for being Indirectly Blocked (previous to the | is for direct cases, after the | is for wrapped cases)
    Indirectly.Blocked <- (((Move.pos-1) %in% (Move.pos[Directly.Blocked]) & Move.pos %% nrow(origin) != 1) | ((Move.pos %% nrow(origin) == 1) & (Move.pos %in% (Move.pos[Directly.Blocked] - (nrow(origin)) +1))))
    Blocked <- Directly.Blocked | Indirectly.Blocked
    # Blocked <- Directly.Blocked | Indirectly.Blocked
    if (length(Move.pos[Blocked]) > length(Move.pos[Directly.Blocked])) {
      New.Blocked <- (((Move.pos-1) %in% (Move.pos[Blocked]) & Move.pos %% nrow(origin) != 1) | ((Move.pos %% nrow(origin) == 1) & (Move.pos %in% (Move.pos[Blocked] - (nrow(origin)) +1))))
      Blocked <- Blocked | New.Blocked
      while (length(Move.pos[New.Blocked]) > 0) {
        New.Blocked <- ((((Move.pos-1) %in% (Move.pos[Blocked]) & Move.pos %% nrow(origin) != 1) | ((Move.pos %% nrow(origin) == 1) & (Move.pos %in% (Move.pos[Blocked] - (nrow(origin)) +1)))) & !(New.Blocked %in% Blocked))
        Blocked <- Blocked | New.Blocked
      }
    }
    Prop.speed <- length(Move.pos[!Blocked])/length(Move.pos)
  }
  else (Prop.speed <- 1)
  class(origin) <- c('BML', 'matrix')
  return(Prop.speed)
}

summary.BML <- function(Cars,...){
  # This function returns the attributes of a BML traffix grid.
  # grid dimensions, current timestep, proportion of red/blue cars, 
  # and the proportion of red or blue cars that can move on the next timestep ('instant.speed')
  stopifnot(class(Cars) == c('BML', "matrix"))
  attributes(Cars) <- list(time = attr(Cars, 'time'), 
                           # timestep = if attr(Cars, 'time')
                           dim = c(nrow(Cars),ncol(Cars)),
                           prop.red = length(Cars[Cars == 2])/length(Cars), 
                           prop.blue = length(Cars[Cars == 1])/length(Cars),
                           prop.cars = length(Cars[Cars == 1 | Cars == 2])/length(Cars),
                           instant.speed = Speed.Calc(Cars))
    return(attributes(Cars))
}
                           

