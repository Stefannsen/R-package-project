\name{verificare_densitate}
\alias{verificare_densitate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
verificare_densitate
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
verificare_densitate(functie)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{functie}{
%%     ~~Describe \code{functie} here~~
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
function (functie)
{
    ok <- 0
    y <- integrate(Vectorize(functie), -Inf, Inf)
    for (val in seq(-10000, 10000, by = 0.001)) {
        if (functie(val) < 0) {
            ok <- 1
            break
        }
    }
    z <- round(y$value, digits = 7)
    if (ok == 0 && z == 1)
        print("Functia este densitate de probabilitate")
    else print("Functia nu este densitate de probabilitate")
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
