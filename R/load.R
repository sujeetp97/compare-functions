# Utility functions

sampler <- function(collection, size){
  if(class(collection)[[1]] == "data.table"){
    return(collection[sample(c(1:NROW(collection)), size = size)])
  }else{
    return(sample(collection, size = size))
  }
}