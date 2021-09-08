\name{reprezentare_grafica}
\alias{reprezentare_grafica}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
reprezentare_grafica
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
reprezentare_grafica(functie, a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{functie}{
%%     ~~Describe \code{functie} here~~
}
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (functie, a, b)
{
    x <- seq(a, b, 0.01)
    y <- c()
    y1 <- c()
    F1 <- function(x, functie) {
        y <- integrate(Vectorize(functie), -Inf, x)
        return(y$value)
    }
    for (i in x) {
        y <- c(y, F1(i, functie))
    }
    for (i in x) {
        y1 <- c(y1, functie(i))
    }
    plot(x, y, col = "red", main = "Reprezentare grafica densitate si functie de repartitie",
        type = "l")
    lines(x, y1, col = "blue")
    legend("topleft", c("Densitate", "Functia de repartitie"),
        fill = c("blue", "red"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
