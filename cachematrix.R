# x是需要处理的matrix
# m用来存储处理后的结果

makeCacheMatrix<-function(x=matrix()){
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve  #得到逆矩阵并将solve的值赋给m
        getsolve <- function() m                 #返回m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
cacheSolve <- function(x, ...) {
        m <- x$getsolve()             #调makeCacheMatrix中的getsolve函数
        if(!is.null(m)) {    #如果m不是NULL说明之前已经计算过solve了，直接返回结果
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()  #调用x内部的get函数，从x中调出需要处理的数据
        m <- solve(data, ...)  #计算solve
        x$setsolve(m)     #调用x内部的setsolve函数，得到结果还存到x里
        m            #返回结果
}
