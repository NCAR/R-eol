\name{EOLsounding internal }

\alias{readin.varname}
\alias{readin.data}
\alias{readin.file}
\alias{check.files}
\alias{check.YTYPE}
\alias{check.sonderange}

\title{
  EOLsounding internal and secondary functions
}
\description{
Listed below are supporting functions for the major methods in EOLsounding.
readin.varname takes variable name as input, and return a list with name and unit as its attribute.
readin.file calls different read file functions according to ftype, for potential extension of different file types.
}
\usage{

 readin.varname(varname)
 readin.data(sounding, varname)
 readin.file(ftype="qc", file=NULL)
 check.files(fdir)
 check.YTYPE(YTYPE="height")
 check.sonderange(sonderange=NA, nsonde=0)
}

\keyword{internal}
\keyword{misc}

