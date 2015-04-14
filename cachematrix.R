#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix = function(mtx = matrix()){
	inverse_m = NULL
	set = function(y){
		mtx <<- y 
		inverse_m <<- NULL
	}
	get = function() return(mtx)
	set_inverse = function(inverse) inverse_m = inverse
	get_inverse = function() return(inverse_m)
	
	ans = list(set = set,get = get, set_inverse = set_inverse, get_inverse = get_inverse)
	return(ans)
}

#This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated (and the 
#matrix has not changed), then the cachesolve should retrieve the inverse 
#from the cache.
cacheSolve = function(mtx, ...){
	inverse_m = mtx$get_inverse()
	if(!is.null(inverse_m)){
		message("getting cached data")
		return(inverse_m)
	}
	data = mtx$get()
	inverse_m = solve(data,...)
	mtx$set_inverse(inverse_m)
	return(inverse_m)
}
