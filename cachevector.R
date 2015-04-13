

# Create Special Vector (SV)
# SV is a list of functions
## 1. set value of vector
## 2. get value of vector
## 3. set value of the mean
## 4. get the value of the mean

makeVector <- function(x=numeric()) {
      
      #initialize empty variable m
      m<- NULL
      
      #define "set" function
      ##1. define new arg variable y as x (inputted at top)
      ##2. reset m to null
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      #define "get" function
      get <- function() x
      
      #define "setmean" function (actual calc is in cachemean)
      setmean <- function(mean) m <<- mean
      
      #define "getmean" function
      getmean <- function() m
      
      #create a list of these functions
      list(set=set, get=get,setmean=setmean,getmean=getmean)
}

# Calculate mean of SV
# If mean already calc'ed, then gets mean from cache
cachemean <- function (x,...){
      
      #check the value of m from x$getmean() function
      # if m has a value, print that
      m <- x$getmean()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      #else get the value of x, calc mean, save value as m
      data <- x$get()
      m <- mean(data,...)
      x$setmean(m)
      m
      
}

#create test vector