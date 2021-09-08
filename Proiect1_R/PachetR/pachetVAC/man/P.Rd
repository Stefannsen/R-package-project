\name{P}
\alias{P}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
P
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
P(f, a, b, x = a, y = b, x1 = a, y1 = b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{
%%     ~~Describe \code{f} here~~
}
  \item{a}{
%%     ~~Describe \code{a} here~~
}
  \item{b}{
%%     ~~Describe \code{b} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{x1}{
%%     ~~Describe \code{x1} here~~
}
  \item{y1}{
%%     ~~Describe \code{y1} here~~
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
function (f, a, b, x = a, y = b, x1 = a, y1 = b)
{
    pmf <- function(x) {
        f(x) * normConst(f, a, b)
    }
    if (x == y)
        0
    else if (x < y & x1 == a & y1 == b) {
        if (x < a)
            x <- a
        if (y > b)
            y <- b
        integrate(Vectorize(pmf), x, y)$value
    }
    else if (x < y & x1 < y1) {
        if (x1 < a)
            x1 <- a
        if (y1 > b)
            y1 <- b
        mini <- min(x, x1)
        maxi <- max(y, y1)
        if (mini < a)
            mini <- a
        if (maxi > b)
            maxi <- b
        integrate(Vectorize(pmf), mini, maxi)$value/integrate(Vectorize(pmf),
            x1, y1)$value
    }
    else warning("Date incorecte!")
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
