nwords <- function(s,sep=".")
{
    sapply(strsplit(s,split=sep,fixed=TRUE),length)
}

words <- function(x,first=1,last=1000000,sep=".")
{
    if (length(first) * length(last) == 1) {
        sapply(x,
            function(x,first,last,sep)
            {
                # cat("sep=",sep,",first=",first,",last=",last,"\n")
                x = unlist(strsplit(x,split=sep,fixed=TRUE))
                nw = length(x)
                if (first <= nw) {
                    last = min(last,nw)
                    # cat("length(x)=",length(x),"\n")
                    paste(x[first:last],collapse=sep)
                }
                else ""
            },first,last,sep)
    }
    else {
        sapply(1:length(x),
            function(i,x,first,last,sep)
            {
                # cat("sep=",sep,",first=",first,",last=",last,"\n")
                x = unlist(strsplit(x[i],split=sep,fixed=TRUE))
                nw = length(x)
                fi = i
                if (fi > length(first)) fi = (i %% length(first)) + 1
                li = i
                if (li > length(last)) li = (i %% length(last)) + 1
                if (first[fi] <= nw) {
                    last = min(last[li],nw)
                    # cat("length(x)=",length(x),"\n")
                    paste(x[first[fi]:last],collapse=sep)
                }
                else ""
            },x,first,last,sep)
    }
}
