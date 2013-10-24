label.times <- function(t1,t2, annotate, adj, col=1, year=T, print=T)
{
# S function to print start (t1) and end (t2) times of data
# on the upper left of a plot (or as specified by 'adj')
# t1 <- start(data)
# t2 <- end(data)
t1.list <- as.list(t1)
t2.list <- as.list(t2)
if (t1.list$mon != t2.list$mon | t1.list$day != t2.list$day |
    t1.list$year != t2.list$year) {
  if (year)
    if (t1.list$year != t2.list$year)
      times <- paste(format(t1, format = "%Y %b %d %02H:%02M",
                     time.zone=unlist(options("time.zone"))), "-",
                     format(t2, format = "%Y %b %d %02H:%02M  %Z",
                     time.zone=unlist(options("time.zone"))))
    else
      times <- paste(format(t1, format = "%Y %b %d %02H:%02M",
                     time.zone=unlist(options("time.zone"))), "-",
                     format(t2, format = "%b %d %02H:%02M  %Z",
                     time.zone=unlist(options("time.zone"))))
  else
    times <- paste(format(t1, format = "%b %d %02H:%02M",
                   time.zone=unlist(options("time.zone"))), "-",
                   format(t2, format = "%b %d %02H:%02M  %Z",
                   time.zone=unlist(options("time.zone"))))
  }
else {
  if (year)
    times <- paste(format(t1, format = "%Y %b %d, %02H:%02M",
                   time.zone=unlist(options("time.zone"))), "-",
                   format(t2, format = "%02H:%02M  %Z",
                   time.zone=unlist(options("time.zone"))))
  else
    times <- paste(format(t1, format = "%b %d, %02H:%02M",
                   time.zone=unlist(options("time.zone"))), "-",
                   format(t2, format = "%02H:%02M  %Z",
                   time.zone=unlist(options("time.zone"))))
  }

if (!missing(annotate))
  times = paste(times, ", ", annotate, sep="")

if (print) {
  if (missing(adj))
    adj=0
  par.old = par(col=col)
  on.exit(par(par.old))
  mtext(times, side = 3, adj = adj, line = 0.1)
  }
invisible(times)
}
