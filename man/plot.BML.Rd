\name{plot.BML}
\alias{plot.BML}
\title{Plot.BMl}
\description{Plot method for BML objects}
\usage{
plot.BML(x, y, ...)
}
\arguments{
  \item{x}{the BML object}
  \item{y}{nothing needed. Dummy to match plot signature}
  \item{\dots}{nothing here}
}
\details{}
\value{the image of the BML object, rotated and flipped so it looks right}
\references{see the website: http://mae.ucdavis.edu/dsouza/bml.html for more than you'd ever want to know on BML traffic models}
\author{Jacob Teter}
\note{}

\seealso{BML, Move}
\examples{
## The function is currently defined as
function (x, y, ...) 
{
    par(mar = c(1.5, 0.1, 3, 0.1), mai = c(1, 0.1, 1, 0.2), oma = c(1, 
        0.5, 1, 0.5))
    x <- t(x[nrow(x):1, ])
    image(x, col = c("white", "blue", "red"), axes = FALSE)
  }
}
\keyword{ ~aplot }