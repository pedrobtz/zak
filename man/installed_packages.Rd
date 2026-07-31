% Generated manually; keep in sync with the roxygen comments in R/packages.R.
\name{installed_packages}
\alias{installed_packages}
\title{Installed packages as a data frame}
\usage{
installed_packages(lib.loc = NULL, ...)
}
\arguments{
\item{lib.loc}{Character vector of library trees to search, or \code{NULL}
(the default) for \code{\link{.libPaths}}.}

\item{...}{Further arguments passed to
\code{\link[utils]{installed.packages}}, for example \code{priority} or
\code{fields}.}
}
\value{
A data frame with one row per installed package and the columns documented
in \code{\link[utils]{installed.packages}} (\code{Package}, \code{LibPath},
\code{Version}, \code{Priority}, \code{Depends}, \code{Built}, ...), all of
type character. Missing fields are \code{NA}.
}
\description{
A thin wrapper around \code{\link[utils]{installed.packages}} that returns a
data frame instead of a character matrix.

Note that a package installed in more than one of \code{lib.loc} yields one
row per library, distinguished by the \code{LibPath} column.
}
\examples{
inst <- installed_packages()
head(inst[, c("Package", "Version")])

## packages that ship with R
installed_packages(priority = "base")$Package
}
\seealso{
\code{\link{available_packages}}
}
