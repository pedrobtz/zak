% Generated manually; keep in sync with the roxygen comments in R/url_info.R.
\name{url_exists}
\alias{url_exists}
\title{Does a URL point at something that exists?}
\usage{
url_exists(url, timeout = 10L)
}
\arguments{
\item{url}{Character vector of URLs.}

\item{timeout}{Timeout in seconds for each request.}
}
\value{
A logical vector, named by \code{url}. \code{NA} where the check could not
settle the question; see \code{\link{url_info}}.
}
\description{
A convenience wrapper around \code{\link{url_info}} for when only the answer
matters.
}
\examples{
f <- tempfile(fileext = ".tar.gz")
file.create(f)
url_exists(paste0("file://", f))
unlink(f)
}
\seealso{
\code{\link{url_info}}
}
