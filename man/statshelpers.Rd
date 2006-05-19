\name{stats helpers}
\alias{a.findcorrelations}
\alias{a.iqr}
\alias{a.proportion.test}
\alias{a.qr}
\alias{a.rankval}
\alias{a.showextremes}
\title{Misc R statistics helper functions}
\usage{
a.findcorrelations(df, vars1=names(df), vars2=vars1, min.cor=0.5)

a.iqr(x)

a.proportion.test(x1, x2, y1, y2, totals=FALSE)

a.qr(x)

a.rankval(x)

a.showextremes(df, vars, largest=5, showalso=NULL)
}
\description{
\code{a.findcorrelations(df, vars1, vars2, min.cor}
  checks pairs of variables for correlation.
  Returns a sorted vector of pairwise correlations whose absolute value
  is greater-or-equal \code{min.cor}.
  Uses all pairs of variables from dataframe \code{df} mentioned in
  character vectors (\code{vars1} x \code{vars2}), 
  except that is returns only the lower
  triangle of that matrix if \code{vars2} is missing.
  Assesses the variable itself and also the corresponding rank vector
  (for rank correlations)
  Value pairs containing \code{NAs} are ignored in each \code{cor}
  call individually.

\code{a.iqr(x)}
  computes the inter-quartile range of numeric vector \code{x}.

\code{a.proportion.test(x1, x2, y1, y2)}
  tests the proportion x1/x2 against y1/y2 using both Fisher exact p and
  Chi-squared test, prints results of both and returns results of Chi-squared.
  If \code{totals=TRUE}, tests x1/(x2-x1) against y1/(y2-y1) instead.
  The arguments must be scalars, not vectors.

\code{a.qr(x)}
  computes the quartile ratio (q75/q25) of numeric vector \code{x}.

\code{a.rankval(x)}
  ranks the values from vector \code{x} using \code{rank()}
  but leaves \code{NAs} as \code{NAs}.

\code{a.showextremes(df, vars, largest, showalso)}
  shows the records from data frame \code{df} that contain the 
  \code{largest} largest values (or the \code{largest} smallest if
  \code{largest} is negative) with respect to each of \code{df}'s variables
  whose name is mentioned in \code{vars} (which is a character vector).
  The extreme records for each of these variables are found; from each
  record, only these variables plus those whose name is mentioned in
  character vector \code{showalso} are shown.
}
\arguments{
\item{df}{a dataframe, see description above.}
\item{vars1}{a character vector, see description above.}
\item{vars2}{a character vector, see description above.}
\item{min.cor}{a number in range 0..1, see description above.}
\item{x}{a vector, see description above.}
\item{x1}{see description above.}
\item{x2}{see description above.}
\item{y1}{see description above.}
\item{y2}{see description above.}
\item{totals}{a logical value, see description above.}
\item{vars}{a character vector, see description above.}
\item{largest}{an integer, see description above.}
\item{showalso}{a character vector, see description above.}
}
\author{Lutz Prechelt \email{prechelt@inf.fu-berlin.de}}
\examples{
data(iris)
summary(iris$Sepal.Width)
a.iqr(iris$Sepal.Width)
a.qr(iris$Sepal.Width)
a.findcorrelations(iris)
a.proportion.test(7,11, 17,19)
a.showextremes(iris, c("Sepal.Width","Sepal.Length"), largest = -5,
               showalso = c("Petal.Length"))

}
\keyword{manip} 
