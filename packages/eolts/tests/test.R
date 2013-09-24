creatematrix = function(x,nrow,ncol)
{
    .Call("create_matrix",x,as.integer(nrow),as.integer(ncol),PACKAGE="surfnetcdf")
}

setdims = function(x,nrow,ncol)
{
    .Call("set_dims",x,as.integer(nrow),as.integer(ncol),PACKAGE="surfnetcdf")
}

setdimnames = function(x,dns)
{
    .Call("set_dimnames",x,dns,PACKAGE="surfnetcdf")
}

createarray = function(x,dims)
{
    .Call("create_array",x,as.integer(dims),PACKAGE="surfnetcdf")
}

setadims = function(x,dims)
{
    .Call("set_adims",x,as.integer(dims),PACKAGE="surfnetcdf")
}

setadimnames = function(x,dns)
{
    .Call("set_adimnames",x,dns,PACKAGE="surfnetcdf")
}

library("surfnetcdf")

nr = 2
nc = 3

x = creatematrix(TRUE,nr,nc)
cat(storage.mode(x),'\n')
print(x)

x = creatematrix(as.integer(1),nr,nc)
cat(storage.mode(x),'\n')
print(x)

x = creatematrix(as.double(1),nr,nc)
cat(storage.mode(x),'\n')
print(x)

x = creatematrix(as.single(1),nr,nc)
cat(storage.mode(x),'\n')
cat("dim(x)=",paste(dim(x),collapse=','),'\n')
print(x)

x[] = outer(seq(from=10,to=nr*10,by=10),1:nc,FUN="+")

nr = nr + 1
nc = nc + 1
x = setdims(x,nr,nc)
cat("dim(x)=",paste(dim(x),collapse=','),'\n')
print(x)

x = setdims(1:((nr-1)*nc),nr,nc)
cat("dim(x)=",paste(dim(x),collapse=','),'\n')
print(x)

rns = paste("r",1:nr,sep="")
cns = paste("c",1:nc,sep="")
x = setdimnames(x,list(rns,cns))
print(x)

dims = c(2,4,3)
x = createarray(TRUE,dims)
cat(storage.mode(x),'\n')
print(x)

x = createarray(as.integer(1),dims)
cat(storage.mode(x),'\n')
print(x)

x = createarray(as.double(1),dims)
cat(storage.mode(x),'\n')
print(x)

x = createarray(as.single(1),dims)
cat(storage.mode(x),'\n')
cat("dim(x)=",paste(dim(x),collapse=','),'\n')
print(x)

x[] = seq(from=1,to=length(x))

dims = dims + 1
x = setadims(x,dims)
cat("dim(x)=",paste(dim(x),collapse=','),'\n')
print(x)

dims = dims - 1
x = setadims(x,dims)
cat("dim(x)=",paste(dim(x),collapse=','),'\n')
print(x)

dlist = NULL
for (d in dims) {
    dns = paste("d",match(d,dims),"-",1:d,sep="")
    if (is.null(dlist)) dlist = list(dns)
    else dlist = c(dlist,list(dns))
}
x = setadimnames(x,dlist)
print(x)

