\name{Speed.Calc}
\alias{Speed.Calc}
\title{internal function, should be masked from user}
\description{called by summary.BML, calculates next steps relative speed}
\usage{
Speed.Calc(origin, t)}
\arguments{
  \item{origin}{the BML matrix}
  \item{t}{starting timestep (even or odd)}
}
\details{}
\value{returns the proportional speed in the subsequent timestep (i.e. number of cars that are not blocked over total number of cars)}
\references{see the website: http://mae.ucdavis.edu/dsouza/bml.html for more than you'd ever want to know on BML traffic models
}
\author{Jacob Teter}
\note{}

\seealso{summary.BML method}
\examples{
## The function is currently defined as
function (origin, t) 
{
    t <- attr(origin, "time")
    t <- t + 1
    i <- t\%\%2
    j <- (t + 1)\%\%2
    i[i == 0] <- 2
    j[j == 0] <- 2
    Move.pos <- which(origin == i)[order(which(origin == i))]
    Bloc.pos <- which(origin == j)[order(which(origin == j))]
    Directly.Blocked <- ((Move.pos - 1) \%in\% Bloc.pos & (Move.pos\%\%nrow(origin) != 
        1)) | ((Move.pos\%\%nrow(origin) == 1) & Move.pos \%in\% 
        (Bloc.pos - (nrow(origin)) + 1))
    if (any(Directly.Blocked)) {
        Indirectly.Blocked <- (((Move.pos - 1) \%in\% (Move.pos[Directly.Blocked]) & 
            Move.pos\%\%nrow(origin) != 1) | ((Move.pos\%\%nrow(origin) == 
            1) & (Move.pos \%in\% (Move.pos[Directly.Blocked] - 
            (nrow(origin)) + 1))))
        Blocked <- Directly.Blocked | Indirectly.Blocked
        if (length(Move.pos[Blocked]) > length(Move.pos[Directly.Blocked])) {
            New.Blocked <- (((Move.pos - 1) \%in\% (Move.pos[Blocked]) & 
                Move.pos\%\%nrow(origin) != 1) | ((Move.pos\%\%nrow(origin) == 
                1) & (Move.pos \%in\% (Move.pos[Blocked] - (nrow(origin)) + 
                1))))
            Blocked <- Blocked | New.Blocked
            while (length(Move.pos[New.Blocked]) > 0) {
                New.Blocked <- ((((Move.pos - 1) \%in\% (Move.pos[Blocked]) & 
                  Move.pos\%\%nrow(origin) != 1) | ((Move.pos\%\%nrow(origin) == 
                  1) & (Move.pos \%in\% (Move.pos[Blocked] - (nrow(origin)) + 
                  1)))) & !(New.Blocked \%in\% Blocked))
                Blocked <- Blocked | New.Blocked
            }
        }
        Prop.speed <- length(Move.pos[!Blocked])/length(Move.pos)
    }
    else (Prop.speed <- 1)
    class(origin) <- c("BML", "matrix")
    return(Prop.speed)
  }
}
\keyword{ ~array }