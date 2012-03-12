setClass("XMLRPCServer", contains = "character")
setClass("XMLRPCServerConnection", 
          representation (curl = "CURLHandle"),
           contains = "XMLRPCServer")

XMLRPCServer =
function(url, curl = NULL,
         class = if(!is.null(curl))
                      "XMLRPCServerConnection"
                 else
                     "XMLRPCServer",
          ...,
          .opts = list(...))
{
   if(is.null(curl) && length(.opts) ||
        (is.logical(curl) && curl))
     curl = getCurlHandle(.opts = .opts)
   
   ans = new(class, url)
   if(!is.null(curl))
      ans@curl = curl
   
   ans
}

setMethod("$", "XMLRPCServer",
          function(x, name) {
            function(...)
               xml.rpc(as(x, "character"), name, ...)
          })

setMethod("$", "XMLRPCServerConnection",
          function(x, name) {
            function(...)
               xml.rpc(as(x, "character"), name, ..., .curl = x@curl)
          })

