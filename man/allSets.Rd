\name{allSets}
\alias{allSets}

\title{ Subsetting the ALL data set}
\description{
  A function that produces different stages of preprocesed ALL data sets
}
\usage{
allSets(type=c("neg", "af4"), step=c("subset", "unspec", "de"),
varCut=0.5)
}

\arguments{
  \item{type}{the type of sample comparison to subset to}
  \item{step}{the step until which subsetting procedure is peformed}
  \item{varCut}{The IQR percentile used for the variance filtering}
}
\details{
  There can be two different types of sample oomparisons for which to
  create subsets: Bcr/ABL vs NEG ("neg") and BCR/ABL vs ALL1/AF4 ("af4").
  The different preprocessing steps are
  \item{subset:}{samples of respective type only}
  \item{unspec:}{unspecific filtering step}
  \item{de:}{only genes that are differentially expressed}
}

\value{
 This function is called for its side effects
}

\author{Florian Hahne}

\keyword{datagen}
