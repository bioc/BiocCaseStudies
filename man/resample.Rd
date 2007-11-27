\name{resample}
\alias{resample}

\title{Resample from ALL ExpressionSet and plot}
\description{
   A function to resample samples from each class
   of an ExpressionSet and plot the results calculated by a function
   that returns the number of differentially expressed genes between the
   classes.
 }
\usage{
resample(x, selfun, groupsize = 6 * (1:6), nrep = 25)
}

\arguments{
  \item{x}{An \code{ExpressionSet} object derived from the \code{ALL}
  data package.}
  \item{selfun}{A function that takes the resampling subset of the
    \code{ExpressionSet} and computes the number of differentially
    expressed genes between the two classes}
  \item{groupsize}{The number of samples for each class}
  \item{nrep}{number of iterations of resampling procedure}
}
\value{
  The function is called for the side effect of producing a plot.
}
\author{Florian Hahne}


\keyword{misc}
