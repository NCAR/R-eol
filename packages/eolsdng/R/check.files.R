check.files <-
function(fdir)
{
  if (is.null(fdir))
      stop("PLEASE SPECIFY THE LOCATION OF REQUIRED SOUNDING FILES!!!")
  files = system(paste("cd ", fdir, "; ls D*", sep="/"), TRUE)
  if (length(files) == 0)
    stop(paste("There is NO required sounding files under directory", fdir))
  return(files)
}
