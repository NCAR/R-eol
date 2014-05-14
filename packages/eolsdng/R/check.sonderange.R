check.sonderange <-
function(sonderange=NA, nsonde=0)
{
  msg = "sonderange IS A INTEGRAL VECTOR OF LENGTH 2, INDICATING THE STA
RTING/ENDING NUMBER OF SOUNDINGS TO BE PROCESSED, THE NUMBERS CAN NOT BEYOND TOTAL NUMBER OF SOUNDINGS !!!"
  if (length(sonderange) == 1) {
    if (!is.na(sonderange)) {
      print(msg)
      sonderange = NA
    }
  }
  if (length(sonderange) > 2) {
    print(msg)
    sonderange = NA
  }
  if (length(sonderange) == 2) {
    if (sonderange[1]>sonderange[2] | sonderange[1]<1 | sonderange[2]>nsonde) {
      print(msg)
      sonderange = NA
    }
  }
  return(sonderange)
}
