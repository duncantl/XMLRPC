library(XML)
library(XMLRPC)

source("R/serialize.R")

identical(saveXML(vectorArray(1:10, "int")), saveXML(XMLRPC:::vectorArray(1:10, "int")))

identical(saveXML(vectorArray(1:10, "i4")), saveXML(XMLRPC:::vectorArray(1:10, "i4")))

identical(saveXML(vectorArray(1:10, "string")), saveXML(XMLRPC:::vectorArray(1:10, "string")))

identical(saveXML(vectorArray(1:10, "boolean")), saveXML(XMLRPC:::vectorArray(1:10, "boolean")))

identical(saveXML(vectorArray(c(TRUE, FALSE), "boolean")),
          saveXML(XMLRPC:::vectorArray(c(TRUE, FALSE), "boolean")))


#########
d = c(Sys.Date(), Sys.Date()-1)
identical(saveXML(vectorArray(d, "dateTime.iso8601")),
          saveXML(XMLRPC:::vectorArray(d, "dateTime.iso8601")))


d = c(Sys.time(), Sys.time() - 1)
identical(saveXML(vectorArray(d, "dateTime.iso8601")),
          saveXML(XMLRPC:::vectorArray(d, "dateTime.iso8601")))

identical(saveXML(vectorArray(c(0, 1, 2, 3), "double")),
          saveXML(XMLRPC:::vectorArray(c(0, 1, 2, 3), "double")))
