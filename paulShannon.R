library (XMLRPC)
url = "http://xmlrpc-c.sourceforge.net/api/sample.php"
xml.rpc (url, 'system.listMethods')
xml.rpc (url, 'sample.add', 3L, 4L)  # [1] 7
Rprof ('test.prof', memory.profiling=FALSE, interval=0.01)
xml.rpc (url, 'sample.add', 1:5000)
