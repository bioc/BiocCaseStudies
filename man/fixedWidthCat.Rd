\name{fixedWidthCat}
\alias{fixedWidthCat}
\title{Control the output of show methods}
\description{
 This function makes sure that the output of a show method fits on the
 page by inserting lines breaks into long strings 
}
\usage{
fixedWidthCat(x, width=getOption("width"))
}
\arguments{
  \item{x}{An R object which is to be shown.}
  \item{width}{The number of characters after which lines are to be broken.}
}
\value{
  A character vector of the output with long lines broken
}
\author{Florian Hahne}
\examples{
long <- paste(rep(letters[1:24], 5), sep="", collapse="")
fixedWidthCat(long)
}

\keyword{misc}