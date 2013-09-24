
setClass("netcdf",
    slots=c(file="character",dir="character",cppPtr="raw"),
    prototype=list(file="",dir="",cppPtr=raw(8))
)

netcdf = function(file,dir="")
{
    obj = new("netcdf",file=file,dir=dir)
    .Call("open_netcdf",obj,PACKAGE="eolts")
}

setClass("netcdfVariable",
    slots=c(name="character", mode="character",
        nctype="character", dimension="integer", attributes="list"),
    prototype=list(name=character(),mode=character(),nctype=character(),dimension=integer(),
  	attributes=list())
)

if (!isGeneric("format")) setGeneric("format")
setMethod("format","netcdf",
    function(x,...) {
        paste(class(x),
            ", dir=",x@dir,
            ", file=",paste(x@file,collapse=","),
            ", cppPtr=",paste(x@cppPtr,collapse=""))
    }
)

setMethod("show","netcdf",
    function(object) cat(format(object),"\n")
)

setMethod("format","netcdfVariable",
    function(x) {
        paste(class(x),
            ", name=",x@name,
            ", mode=",x@mode,
            ", nctype=",x@nctype,
            ", dimensions=(",
              paste(names(x@dimension),"(",
                    as.vector(x@dimension),")",collapse=", ",sep=""),")",
            ", attributes=(",
              paste(names(x@attributes),"(",
                sapply(x@attributes,function(x)paste(x,collapse=",")),")",collapse=",",sep=""),")",
            "\n",sep="")
    }
)

setMethod("show","netcdfVariable",
    function(object) cat(format(object),"\n")
)

if (!isGeneric("is.open")) {
    setGeneric("is.open",function(con) standardGeneric("is.open"))
}
setMethod("is.open",
    signature(con="netcdf"),
    function(con) {
        .Call("is_netcdf_open",con,PACKAGE="eolts")
    }
)

if (!isGeneric("close")) setGeneric("close")
setMethod("close",
    signature(con="netcdf"),
    function(con) {
        .Call("close_netcdf",con,PACKAGE="eolts")
        NULL
    }
)

# We could make these methods instead of functions.
# Calling them "read" would be using a common name, and might
# conflict with future releases of S, or other extensions.
# (As of Splus v6, read is not used.)
# Since all methods must have the same signature (unless we use ...)
# we probably shouldn't use the name "read" here.

if (isGeneric("readnc",where=1)) removeGeneric("readnc",where=1)
setGeneric("readnc",function(con,variables,start,count,...)
    standardGeneric("readnc"))

setMethod("readnc",
  signature(con="netcdf",variables="character",start="integer",count="integer"),
    function(con,variables,start,count,...) {
      x = .Call("read_netcdf",con,variables,start,count,PACKAGE="eolts")
      if (length(x) == 1) x = x[[1]]
      x
    }
)

setMethod("readnc",
  signature(con="netcdf",variables="character",start="missing",count="missing"),
    function(con,variables,start,count,...) {
      readnc(con,variables,integer(0),integer(0))
    }
)

setMethod("readnc",
  signature(con="netcdf",variables="list",start="integer",count="integer"),
    function(con,variables,start,count,...) {
      if (length(variables) > 0 && class(variables[[1]]) == "netcdfVariable") {
	variables = sapply(variables,function(x)x@name)
	readnc(con,variables,start,count,...)
      }
      else stop("invalid \"variables\" argument")
    }
)


setMethod("readnc",
    signature(con="netcdf",variables="list",start="missing",count="missing"),
    function(con,variables,start,count,...) {
      readnc(con,variables,start=integer(0),count=integer(0),...)
    }
)

if (isGeneric("variables",where=1)) removeGeneric("variables",where=1)
if (!isGeneric("variables",where=1))
  setGeneric("variables",function(con)
	  standardGeneric("variables"))
setMethod("variables",
    signature(con="netcdf"),
    function(con) {
        invisible(.Call("get_variables",con,PACKAGE="eolts"))
    }
)

