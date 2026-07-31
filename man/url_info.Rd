% Generated manually; keep in sync with the roxygen comments in R/url_info.R.
\name{url_info}
\alias{url_info}
\title{What a URL says about itself}
\usage{
url_info(url, timeout = 10L)
}
\arguments{
\item{url}{Character vector of URLs.}

\item{timeout}{Timeout in seconds for each request.}
}
\value{
A data frame with one row per URL and the columns:
\describe{
  \item{\code{url}}{The URL, as given.}
  \item{\code{exists}}{\code{TRUE}, \code{FALSE}, or \code{NA} when the answer
    is not conclusive - the host could not be reached, libcurl is unavailable,
    or the status settles nothing (403 forbidden, 405 HEAD refused, 5xx).}
  \item{\code{status}}{Final HTTP status after redirects, \code{NA} for
    \code{file://}.}
  \item{\code{type}}{Advertised \code{Content-Type}, or \code{NA}.}
  \item{\code{bytes}}{Advertised \code{Content-Length}, or \code{NA}.}
}
}
\description{
Issues a HEAD request with \code{\link[base]{curlGetHeaders}} and reports
whether the file is there, along with the size and content type the server
advertises. Nothing is downloaded. \code{file://} URLs are answered from the
filesystem, since \code{curlGetHeaders()} handles only http, https and ftp.
}
\section{Treating the result}{

\code{exists} is deliberately three-valued. A \code{FALSE} is worth acting
on; an \code{NA} means the check could not settle the question and the
download should be attempted anyway rather than refused.
}

\examples{
## file:// URLs are answered from the filesystem
f <- tempfile(fileext = ".tar.gz")
writeBin(as.raw(c(0x1f, 0x8b)), f)
url_info(paste0("file://", f))
unlink(f)

\dontrun{
url_info("https://cloud.r-project.org/src/contrib/PACKAGES")
}
}
\seealso{
\code{\link{url_exists}}
}
