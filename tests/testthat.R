# load packages
library(testthat)
library(mosap)

# load solver packages
require(gurobi)
require(Rsymphony)
require(Rcplex)
require(rcbc)

test_check("mosap")
