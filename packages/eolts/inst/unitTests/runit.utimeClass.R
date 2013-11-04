test.utime = function()
{
    x = new("utime",0)
    checkEquals(x@.Data,0)

    x = utime("1970 Jan 1 00:00:00",in.format="%Y %b %d %H:%M:%S",time.zone="UTC")
    checkEquals(x@.Data,0)

    x = new("utime",1383344676.122)
    sx = format(x,format="%Y %m %d %H:%M:%OS2",time.zone="UTC")
    checkEquals(sx,"2013 11 01 22:24:36.12")

    xx = utime(sx,in.format="%Y %m %d %H:%M:%OS",time.zone="UTC")
    checkTrue(abs(xx-x) < .01,"check equivalence of times")

    sx = format(x,format="%Y %m %d %H:%M:%OS2",time.zone="US/Arizona")
    checkEquals(sx,"2013 11 01 15:24:36.12")

    xx = utime(sx,in.format="%Y %m %d %H:%M:%OS",time.zone="US/Arizona")
    checkTrue(abs(xx-x) < .01,"check equivalence of times")

    sx = format(x,format="%Y %m %d %H:%M:%OS2",time.zone="US/Mountain")
    checkEquals(sx,"2013 11 01 16:24:36.12")

    xx = utime(sx,in.format="%Y %m %d %H:%M:%OS",time.zone="US/Mountain")
    checkTrue(abs(xx-x) < .01,"check equivalence of times")

    xx = utime(as(x,"list"))
    checkTrue(abs(xx-x) < 1.,"check equivalence of times from lists")

    xx = c(x,x+1.5)
    checkEquals(diff(xx),1.5)
    checkEquals(class(diff(xx)),"numeric")

    xx = c(x,x+1.5,xx+3)

    xx[3] = xx[2]
    checkEquals(diff(xx),c(1.5,0.0,3.0))

    return()
}
