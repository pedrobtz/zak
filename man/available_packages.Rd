% Generated manually; keep in sync with the roxygen comments in R/packages.R.
\name{available_packages}
\alias{available_packages}
\title{Available packages as a data frame}
\usage{
available_packages(repos = getOption("repos"), ...)
}
\arguments{
\item{repos}{Character vector of repository URLs, as in
\code{\link[utils]{available.packages}}.}

\item{...}{Further arguments passed to
\code{\link[utils]{available.packages}}, for example \code{type} or
\code{filters}.}
}
\value{
A data frame with one row per available package and the columns documented
in \code{\link[utils]{available.packages}} (\code{Package}, \code{Version},
\code{Depends}, \code{Imports}, \code{Repository}, ...), all of type
character. Missing fields are \code{NA}. The result has zero rows if no
repository is reachable or configured.
}
\description{
A thin wrapper around \code{\link[utils]{available.packages}} that returns a
data frame instead of a character matrix.
}
\examples{
\dontrun{
avail <- available_packages()
avail[avail$Package == "jsonlite", c("Package", "Version")]
}
}
\seealso{
\code{\link{installed_packages}}
}
