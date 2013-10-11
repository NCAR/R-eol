#
#               Copyright (C) by UCAR
# 

.onLoad = function(libname,pkgname)
{
    # cat(paste("hello from libname=",libname,", pkgname=",pkgname," .onLoad\n"))
    options(time.in.format="%Y %m %d %H:%M:%OS",
            time.out.format="%Y %02m %02d %02H:%02M:%02S %Z",
            time.zone=Sys.timezone()
    )
    splusTimeDate::timeDateOptions(
            time.in.format="%Y%m%d [%H[[:]%M[[:]%S[[.]%N]]]]",
            time.out.format="%Y %02m %02d %02H:%02M%02S.%03N"
    )

    cat(paste(pkgname,"::.onLoad, setting options():\n",
        "  time.in.format=\"",options("time.in.format")[[1]],"\"\n",
        "  time.out.format=\"",options("time.out.format")[[1]],"\"\n",
        "  time.zone=\"",options("time.zone")[[1]],"\"\n",
        "setting splusTimeDate::timeDateOptions() (used if above time.in.format fails):\n",
        "  time.in.format=",
            splusTimeDate::timeDateOptions("time.in.format")[[1]],"\n",
        "  time.out.format=",
            splusTimeDate::timeDateOptions("time.out.format")[[1]],"\n",sep=""))
    cat("For help on time formats, do ?strptime or class?splusTimeDate::timeDate\n")

}
