readin.file <-
function(ftype="qc", file=NULL)
{
  if (is.null(file))
    stop("PLEASE SPECIFY file NAME AND LOCATION !!!")
  ftype = tolower(ftype)

  if (ftype=="qc")
    data = read.qc.eolfile(file)

  if (ftype=="raw")
    data = read.raw.Dfile(file)

  if (nrow(data) <= 1)
    print(paste(file, "has NO data !!!"))

  return(data)
}
