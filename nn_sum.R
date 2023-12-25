source("comp.R")
source("par.R")
source("aux_fun.R")
source("affn.R")



`%⊕%` <- function(nu_1, nu_2) {
     first_third <- cpy(2, inn(nu_1))
     mid_third <- nu_1 %⊟% nu_2
     last_third <- sm(2, out(nu_1))
     intermediate_network <- mid_third%•%first_third
     return_network <- last_third %•% intermediate_network
     
     return(return_network)
     
}