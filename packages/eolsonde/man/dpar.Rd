\name{dpar}
\alias{dpar}
\alias{dpar_eolsonde}
\title{dpar parameters supported by the eolsonde package}
\description{
Set or get global parameters for accessing and processing data, in similar manner to \code{\link{par}} for graphics parameters, or \code{\link{options}} and \code{\link{getOption}} for session options.  This help page describes the parameters that are supported by the eolsonde package.
}
\section{Data Parameters}{
    \describe{
        \item{\code{checkSondeStatus}}{Logical value. If TRUE, then the second column in the D files is checked to see if the record type matches one of the values in \code{dpar("sondeRecords")} and contains the PTU and GPS status digits. Default: TRUE}
        \item{\code{sondeRecords}}{Character vector, indicating the type of records to retain from a D file. "A" for aircraft data system records, "P" for pre-launch, and "S" for sounding. Default "S".}
}
}
\references{
}
\author{
Gordon Maclean
}
\seealso{
\tabular{ll}{
?command \tab link\cr
\code{? readDFile} \tab \code{\link{readDFile}}\cr
\code{? dpar_eolts} \tab \code{\link{dpar_eolts}}\cr
\code{? eolsonde} \tab \code{\link[eolsonde:eolsonde-package]{eolsonde}}\cr
}
}
\examples{
    
    myfunc  <-  function() {
        # change some parameters, saving original values
        old.dpar = dpar(checkSondeStatus=TRUE,sondeRecords=c("A","S"))
        # restore original values on function exit
        on.exit(dpar(old.dpar),add=TRUE)

        # ...
    }
}
\keyword{ ts }
