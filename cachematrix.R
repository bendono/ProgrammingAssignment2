# Generates a matrix object that can cache its own inverse.
# Argument is a matrix. If left blank, a default one is created.
# Essentially a wrapper class that contains four methods: 
# a constructor, get(), setInverse(), and getInverse().
# Designed to be used with a corresponding cacheSolve object.
makeCacheMatrix <- function(x = matrix())
{
	inv <- NULL			# The matrix inversion.
    set <- function(y)	# Constructor. Initializes state.
	{
		x <<- y
		inv <<- NULL
	}
    
	# Get the input matrix.
    get <- function() x
	# Set the matrix inversion.
	setInverse <- function(inverse) inv <<- inverse
	# Get the matrix inverison.
	getInverse <- function() inv
	# Generate list that contains above methods.
	list(set = set, 
		get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}

# Solves a matrix inversion.
# Arguments are a makeCacheMatrix object and optionally
# any parameters needed to solve the inversion.
# Returns a previously cached value if it already exists.
# Otherwise it calculates it anew.
# Designed to be used with a corresponding makeCacheMatrix object.
cacheSolve <- function(x, ...)
{
	# Get the matrix inversion from x.
	inv <- x$getInverse()
	
	# If the inversions already exists, return it.
	if (!is.null(inv))
	{
		message("Retrieve cached data...")
		return(inv)
	}
	
	# Otherwise, calculate, cache, and return the inversion anew.
	mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
