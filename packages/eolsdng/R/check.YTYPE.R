check.YTYPE <-
function(YTYPE="height")
{
  YTYPE = tolower(YTYPE)
  if (YTYPE != "pressure" & YTYPE != "height")
    stop("YTYPE CAN ONLY BE pressure OR height !!!!")

  return(YTYPE)
}
