# compareFunctions Package
Compare functions or code snippets that traverses through a list-like parameter

# Description
This package provides its main function compare_functions() which can compare between multiple functions that take in a list like parameter. A list like parameter is any object that can be traversed using the object[index] notation and can be subsetted using the object[start_index:stop_index] notation. The comparison will run the functions n number of times using a larger random sample size of the parameter each time, where n is given by the parameter - increments. The package also hosts a suite of S3 methods and functions to work with the result object of a comparison, and it also hosts a Shiny App that runs on this package.

# Dependent Packages
R (>= 3.4.1) , data.table (>= 1.10.4), ggplot2, shiny

# How to Install
Install devtools in R.
Run the following to install compareFunctions package

```
devtools::install_github(repo = "sujeetp97/compare-functions")
```
