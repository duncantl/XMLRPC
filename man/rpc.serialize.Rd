\name{rpc.serialize}
\alias{rpc.serialize}
\alias{rpc.serialize,ANY-method}
\alias{rpc.serialize,AsIs-method}
\alias{rpc.serialize,NULL-method}
\alias{rpc.serialize,Date-method}
\alias{rpc.serialize,POSIXt-method}
\alias{rpc.serialize,list-method}
\alias{rpc.serialize,raw-method}
\alias{rpc.serialize,vector-method}
\title{Serialize R objects to XML-RPC format}
\description{
  This function and its methods convert
  R objects to XML for use in XML-RPC requests.
}
\usage{
rpc.serialize(x, ...)
}
\arguments{
  \item{x}{the R object to be serialized in XML-RPC format}
  \item{\dots}{additional parameters understood by methods}
}
\value{
  An \code{XMLInternalNode} object representing the XML content.
}
\references{
 The XML-RPC specification at \url{http://xmlrpc.scripting.com/spec}.
}
\author{Duncan Temple Lang}
\seealso{\code{\link{xml.rpc}}}
%\examples{}
\keyword{IO}
\keyword{programming}
\concept{RPC}
\concept{XML}
