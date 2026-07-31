% Generated manually; keep in sync with the roxygen comments in
% R/package_url.R.
\name{package_url}
\alias{package_url}
\title{Full URL of a package in a CRAN-like repository}
\usage{
package_url(
  pkgs,
  type = getOption("pkgType"),
  repos = getOption("repos"),
  available = NULL
)
}
\arguments{
\item{pkgs}{Character vector of package names.}

\item{type}{Package type to build the URL for. Accepts the types known to
\code{\link[utils]{available.packages}} (\code{"source"},
\code{"win.binary"}, \code{"mac.binary"}, ...) and the OS aliases
\code{"windows"}, \code{"macos"} and \code{"linux"}. \code{"binary"} and
\code{"both"} describe a preference rather than a file and resolve to the
current platform's own type, which is \code{"source"} on platforms CRAN
publishes no binaries for.}

\item{repos}{Character vector of repository URLs, as in
\code{\link[utils]{available.packages}}. Ignored if \code{available} is
supplied.}

\item{available}{Optionally, a data frame as returned by
\code{\link{available_packages}}, to avoid contacting the repositories
repeatedly. It must describe the same \code{type}, since the file extension
follows from it.}
}
\value{
A character vector of URLs, the same length as \code{pkgs} and named by it.
Packages the repositories do not offer yield \code{NA} with a warning. If a
package is offered by more than one repository, the first one wins, in the
order \code{\link{available_packages}} returns.
}
\description{
Builds the download URL a repository serves a package at, without
downloading anything. This is the address
\code{\link[utils]{download.packages}} would fetch from, and is suitable
input for \code{install_url()}.
}
\details{
The file name is normally \code{<Package>_<Version><ext>}, where the
extension is \code{.tar.gz} for source packages, \code{.zip} for Windows
binaries and \code{.tgz} for macOS binaries. Repositories may override it
through an optional \code{File} field, which is honoured here as
\code{\link[utils]{download.packages}} does.
}
\examples{
\dontrun{
package_url("rlang")
package_url("rlang", type = "windows")

## reuse one snapshot for many lookups
avail <- available_packages(type = "source")
package_url(c("rlang", "jsonlite"), type = "source", available = avail)
}
}
\seealso{
\code{\link{available_packages}}
}
