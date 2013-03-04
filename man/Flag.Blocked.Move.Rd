\name{Flag.Blocked.Move}
\alias{Flag.Blocked.Move}
\title{internal function, hopefully can mask it from the user}
\description{flags cars that are directly and indirectly blocked, called internally within Move}
\usage{Flag.Blocked.Move(origin, r, i, j)}
\arguments{
  \item{origin}{the BML matrix}
  \item{r}{current timestep}
  \item{i}{index of moving cars, logical based on mod 2 arithematic}
  \item{j}{index of stationary cars, logical based on mod 2 arithematic}
}
\value{executes the move from one timestep to the next of cars in a BML grid}
\references{see the website: http://mae.ucdavis.edu/dsouza/bml.html for more than you'd ever want to know on BML traffic models}
\author{Jacob Teter}

\seealso{Move}
\examples{
## The function is currently defined as
function (origin, r, i, j) 
{
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
        Move.pos <- Move.pos[!Blocked]
    }
    New.pos <- Move.pos[Move.pos\%\%nrow(origin) != 1] - 1
    Wrapped.pos <- Move.pos[Move.pos\%\%nrow(origin) == 1] + nrow(origin) - 
        1
    Moved.pos <- c(New.pos, Wrapped.pos)
    origin[Move.pos] <- 0
    origin[Moved.pos] <- i
    class(origin) <- c("BML", "matrix")
    return(origin)
  }
}
\keyword{ ~array }