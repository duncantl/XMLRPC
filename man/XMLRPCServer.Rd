\name{XMLRPCServer}
\alias{XMLRPCServer}
\alias{XMLRPCServer-class}
\alias{XMLRPCServerConnection-class}
\alias{$,XMLRPCServer-method}
\alias{$,XMLRPCServerConnection-method}
\title{Create an instance of an XMLRPCServer object}
\description{
  The  \code{XMLRPCServer} class is a means to
  identify a string as the URL of an XML-RPC server.
  We can then use this to invoke a method provided
  by the server either via a call to \code{\link{xml.rpc}}
  or via an expression of the form
  \code{server$methodName(arg1, arg2, ...)}.

  The \code{XMLRPCServerConnection} class allows us to
  associate a \code{CURLHandle} object with an XML-RPC
  server. This connection is then  used in each of the
  calls to that server. This allows us to reuse a single
  curl connection to the server and also slightly simplifies
  passing it to each call.
}
\usage{
XMLRPCServer(url, curl = NULL, class = if (!is.null(curl)) "XMLRPCServerConnection" else "XMLRPCServer", ..., .opts = list(...))
}
\arguments{
  \item{url}{the URL for the XML-RPC server.}
  \item{curl}{either a logical value indicating whether to create
    a new \code{CURLHandle} object, or
    an instance of a \code{CURLHandle} or alternatively
    \code{NULL}. If CURL options are specified
    via the \code{\dots} or \code{.opts} parameters,
    then a CURL handle is automatically created using these.
    }
  \item{class}{the name of the class to create.}
  \item{\dots}{name=value pairs of CURL options that are used to create a
  new \code{CURLHandle} object.}
  \item{.opts}{an alternative way to specify the CURL options for the
  handle to be created.}
}
\value{
 An object of class given by the value of \code{class}.
}
\author{
  Duncan Temple Lang
}

\seealso{
\code{\link{xml.rpc}}
}
\examples{
server = XMLRPCServer('http://www.advogato.org/XMLRPC')


server = XMLRPCServer('http://www.advogato.org/XMLRPC', TRUE) 


server = XMLRPCServer('http://www.advogato.org/XMLRPC', 
                         verbose = TRUE, 
                         followlocation = TRUE,
                         cookie = "MyCookie=abcd",
                         ssl.verifypeer = FALSE) 

library(RCurl)
server = XMLRPCServer('http://www.advogato.org/XMLRPC', 
                      getCurlHandle(verbose = TRUE, 
                                    followlocation = TRUE,
                                    cookie = "MyCookie=abcd",
                                    ssl.verifypeer = FALSE))


if(url.exists(as(server, "character")))
  server$test.capitalize('abCdef')
}
\keyword{IO}
\concept{XMLRPC}
\concept{OOP}
