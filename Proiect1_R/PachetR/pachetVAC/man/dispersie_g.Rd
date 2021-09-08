\name{dispersie_g}
\alias{dispersie_g}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
dispersie_g
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dispersie_g(g, densitate)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
%%     ~~Describe \code{g} here~~
}
  \item{densitate}{
%%     ~~Describe \code{densitate} here~~
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
function (g, densitate)
{
    force(g)
    force(densitate)
    h <- function(x) {
        g(x) * densitate(x)
    }
    y <- integrate(Vectorize(h), -Inf, Inf)
    a <- y$value
    force(h)
    k <- function(x) {
        g(x) * h(x)
    }
    y <- integrate(Vectorize(k), -Inf, Inf)
    b <- y$value
    return(b - a^2)
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
