\name{Move}
\alias{Move}

\title{Move the cars t timesteps}
\description{Move the cars t timesteps, according to the rules outlined in assignment 2}
\usage{
Move(origin, t, start = "blue")
}
\arguments{
  \item{origin}{The BML matrix object}
  \item{t}{the number of timesteps for the matrix to run through}
  \item{start}{first class of car movement. Blue cars move up, red cars move right. Default value is blue (actually i think i forgot to code this option)}
}
\details{}
\value{returns a BML object at time t, where t is the number of timesteps through which the red and blue cars move from their starting postions}
\references{see the website: http://mae.ucdavis.edu/dsouza/bml.html for more than you'd ever want to know on BML traffic models}
\author{Jacob Teter}
\note{}
\seealso{BML}
\examples{
## The function is currently defined as
function (origin, t, start = "blue") 
{
    stopifnot(class(origin) == c("BML", "matrix"))
    stopifnot(class(t) == "numeric" & t > 0)
    t <- round(t, digits = 0)
    final.time <- sum(t, attr(origin, "time"))
    for (r in 1:t) {
        i <- r\%\%2
        j <- (r + 1)\%\%2
        i[i == 0] <- 2
        j[j == 0] <- 2
        if (r\%\%2 == 1) {
            origin <- Flag.Blocked.Move(origin, r, i, j)
        }
        if (r\%\%2 == 0) {
            origin <- t(origin[, ncol(origin):1])
            origin <- Flag.Blocked.Move(origin, r, i, j)
            origin <- t(origin[nrow(origin):1, ])
        }
    }
    r <- nrow(origin)
    c <- ncol(origin)
    attributes(origin) <- list(time = final.time, dim = c(r, 
        c), prop.red = length(origin[origin == 2])/length(origin), 
        prop.blue = length(origin[origin == 1])/length(origin), 
        prop.cars = length(origin[origin == 1 | origin == 2])/length(origin))
    class(origin) <- c("BML", "matrix")
    return(origin)
  }
}
\keyword{ ~array }