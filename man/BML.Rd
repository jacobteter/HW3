\name{BML}
\alias{BML}
\title{Makes a BML matrix object}
\description{The BML traffic grid is a simple model that exhibits a phase change}
\usage{BML(r, c, p, t)}
\arguments{
  \item{r}{number of rows}
  \item{c}{number of columns}
  \item{p}{proportion of cells filled with cars}
  \item{t}{timestep (default is 0, i.e. no initial timestep)}
}
\details{split of red/blue cars is randomly assigned}
\value{the BML grid is a special kind of matrix}
\references{see the website: http://mae.ucdavis.edu/dsouza/bml.html for more than you'd ever want to know on BML traffic models}
\author{Jacob Teter}
\seealso{Move}
\examples{
## The function is currently defined as
function (r, c, p, t) 
{
    stopifnot(class(r) == "numeric" & r > 0)
    stopifnot(class(c) == "numeric" & c > 0)
    stopifnot(class(p) == "numeric" & 0 < p & p < 1)
    Cars <- matrix(0, nrow = r, ncol = c)
    Car.positions <- sample(r * c, size = p * r * c)
    Cars[Car.positions] <- 1
    Red.positions <- sample(which(Cars == 1), runif(1) * length(Cars[which(Cars == 
        1)]))
    Cars[Red.positions] <- 2
    if (missing(t)) {
        t <- 0L
    }
    attributes(Cars) <- list(time = t, dim = c(r, c), prop.red = length(Cars[Cars == 
        2])/length(Cars), prop.blue = length(Cars[Cars == 1])/length(Cars), 
        prop.cars = length(Cars[Cars == 1 | Cars == 2])/length(Cars))
    class(Cars) <- c("BML", "matrix")
    return(Cars)
  }
}
\keyword{ ~array }