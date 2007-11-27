\name{requiredPackages}
\alias{requiredPackages}
\title{check for missing and outdated packages}
\description{
  Both functions compare the \code{Depends} field of the
  \code{DESCRIPTION} of the \code{BiocCaseStudies} package.
  \code{requiredPackages} is run before a build of the book. It throws
  an error if there are any missing or outdated packages.
  \code{packages2install} returns a character vector of packages
  that need (re)installing.
}
\usage{
requiredPackages(load=FALSE)
packages2install()
}

\arguments{
  \item{load}{Logical. Should all packages be loaded?}
  }

\value{
  \code{requiredPackages} returns \code{invisible(NULL)}.
  The function is called for its side effects.
  \code{packages2install} returns a character vector that can be passed
  to the \code{biocLite} function from the \code{biocLite.R} script.
}

\author{Florian Hahne}
\examples{
  \dontrun{
    biocLite(packages2install())
  }
}

\keyword{misc}