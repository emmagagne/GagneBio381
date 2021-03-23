# Anatomy of a User-Defined Function

```R
functionName <- function(parX=defaultX,parY=defaultY){
# curly bracket marks the start of function body
# lines of R code and annotations
# May also call other functions
# May also create functions
# May define local variables
  
return(singleObject) # returns a single object (could be a list)
}

# curly bracket marks the end of the function body

functionName # print the function body
functionName() # run function with default values
functionName(parX=myMatrix,parY="Order",parZ=c(0,3,3,4))
```

## Stylistic Conventions For Writing Functions

* Use *prominent hash character fencing* at the start and the finish.
* Give a header with a function name, description, and inputs and outputs. 
* Names inside a function can be fairly *short and generic*.
* Functions should be short and simple, no more than about a screenful. 
* If it's too long and complex, break it up into several functions.
* *Provide default values* for all function arguments. 
* Ideally *use random number generators as default values* for rapid testing. 

## Scoping

* **Global variables** are visible to all parts of the code; declared in the main body.
* **Local variables** are visible only within a function; declared in the function or passed to the function through parameters.
* Functions can "see" global variables, but should not use them.
* Global environments cannot "see" variables in the function environment.
* "what happens in the function stays in the function"