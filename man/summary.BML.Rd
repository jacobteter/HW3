\name{summary.BML}
\alias{summary.BML}
\title{summary method for BML objects}
\description{summary method for BML objects, gives basic info}
\usage{
summary.BML(Cars, ...)
}
\arguments{
  \item{Cars}{the BML matrix}
  \item{\dots}{user could specify other relevant attributes here}
}
\details{}
\value{returns the basic info one might want about BML grids}
\references{See the website: http://mae.ucdavis.edu/dsouza/bml.html for more than you'd ever want to know on BML traffic models}
\author{Jacob Teter}
\note{}

\seealso{BML, Move}
\examples{
## The function is currently defined as
function (Cars, ...) 
{
    stopifnot(class(Cars) == c("BML", "matrix"))
    attributes(Cars) <- list(time = attr(Cars, "time"), dim = c(nrow(Cars), 
        ncol(Cars)), prop.red = length(Cars[Cars == 2])/length(Cars), 
        prop.blue = length(Cars[Cars == 1])/length(Cars), prop.cars = length(Cars[Cars == 
            1 | Cars == 2])/length(Cars), instant.speed = Speed.Calc(Cars))
    return(attributes(Cars))
  }
}
\keyword{ ~array }