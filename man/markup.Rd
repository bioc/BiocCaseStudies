\name{markup}
\alias{markup}
\alias{lcol1}
\alias{lcol2}
\alias{lcol3}
\alias{dcol1}
\alias{dcol2}
\alias{dcol3}
\alias{myref}
\alias{Ex}
\alias{solution}
\alias{myincfig}
\alias{solfig}
\alias{booklab}
\alias{indexTerm}

\title{Markup commands.}

\description{Usage of predefined markup commands for layout of Bioc Case
  Studies book.}

\details{
  The following markup commands, LaTeX makros and environments are
  available for controling the layout and structure of the book:
  
\itemize{
  \item{\code{Ex}:}{environment for exercise chunks.}
  
  \item{\code{solution}:}{environment for solutions to the exercises.}
  
  \item{\\\code{myincfig}:}{makro for figure environments with three parameters:
  (1) figure filename (2) figure width (3) figure caption}

  \item{\\\code{solfig}:}{makro for figure environments within solution
  chunks. This is necessary because LaTex doesn't allow for floats
  within minipage environments.}

  \item{\\\code{myref}:}{reference to other labs/chapters. For the book
  this is a simplewrapper around ref ignoring the second argument, for
  the labs this command is replaced in the useRlabs.sty file allowing
  for referencing bbetween the individual documents.}

  \item{\\\code{booklab}:}{macro for conditional text input with two
  parameters. The first parameter will be used for the book while the
  second will be used for the labs.}
}
The following makros will automatically create index entries as side
effect. Apart from that they do text highlighting as well.
\itemize{
   \item{\\\code{R}:}{the R language glyph (in sans serif font)}

   \item{\\\code{Rpackage}:}{an R package (in bold font)}

   \item{\\\code{Rclass}:}{an R class (in italics)}

   \item{\\\code{Rmethod}:}{an R method (in small typewriter font)}

   \item{\\\code{Rfunction}:}{an R function (in small typewriter font)}
}
Implicit index terms can be generated using

\itemize{
\item{\\\code{indexTerm}:}{with the optional first argument giving the
  actual term and the second argument giving a string that appears in
  the text. E.g. \code{indexTerm[tree]{trees}} would give you
  "trees" in the text but create an index for "tree". Omitting the
  optional first argument will create an index for the same string that
  appears in the text.}
}
Some more usful text markup that doesn't create indices:
\itemize{
   \item{\\\code{Robject}:}{an R object (in small typewriter font)}

   \item{\\\code{Rfunarg}:}{the agument to an R function (in italics)}

   \item{\\\code{code}:}{typewriter font}

   \item{\\\code{term}:}{whatever \code{\\{emph}} is set to}

   \item{\\\code{file}:}{italics}

   \item{\\\code{reg}:}{The registered trademark glyph}
}
The following environments are used to structure the document and for
parsing . They do not impose any layout.

\itemize{
   \item{\code{chapterheader}:}{this contains title, authors and abstract
   of the chapter/lab}

   \item{\code{chapterbody:}}{this contains the actual chapter body}

   \item{\code{chaptertrailer:}}{this contains session info and references
   for a chapter}

   \item{\\\code{yaa}:}{This is a wrapper for \code{input} also setting the
   graphics include path. Its first parameter is filename, second
   parameter is graphics path}
}

Color and options

\itemize{
  \item{colors:}{There are some predefined colors that should be used
  consistantly throughout the whole book for things like histograms,
  barplots, etc. They are defined by \code{BiocCaseStudies} as objects
  \code{lcol1}, \code{lcol2} and \code{lcol3} for light colors, and
  \code{dcol1}, \code{dcol2} and \code{dcol3} for dark colors.}

  \item{Sweave options:}{The boolean option \code{hideme} can be used in
  Sweave code chunks that should not be part of the Stangle output. This
  only effects Stangle, so a "regular" Sweave will evaluate these
  chunks. The intention is to have the possibility for sanity checks or
  conditional code evaluation which should not confuse the users when
  they work with the extracted code.}
}
}

\author{Florian Hahne}


\keyword{misc}
