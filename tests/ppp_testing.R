# Test script for the PPP.  
# This script should run cleanly with the 'ppp' R package.  
# 
# This should be started with a clean R session, set the working directory as the base folder to which the zip-file from Github is downloaded.  
# This is for testing the function PrioritisationIII.R
# 
# The following tests are run on a list of 10 species with scinames from Aa to Jj, each species with 100 actions each:  

# INPUT: Set the budget to zero
# OUTPUT: The PPP should run through, removing all species

# INPUT: Set the budget to infinity
# OUTPUT: PPP shouldn't run (should still remove those with B/S/W of zero).  

# INPUT: Set the budget to zero, set one species B-with to 50%
# OUTPUT: That species (sciname = Aa) should be removed first

# INPUT: Set the budget to zero, set one species B to 0%
# OUTPUT: That species (sciname = Aa) should be removed before the PPP starts.

# INPUT: Set the budget to zero, set one species B to 500%
# OUTPUT: That species (sciname = Ff) should be kept until the end

# INPUT:  Set actions to have a cost twice the value of its associated 'species_id'
# OUTPUT: Species should then be removed in order from sciname Jj -> sciname Aa

# INPUT:  Set discount rate to 0.01
# OUPUT: The first full budget should be... 
#disc <- (1 + discount.rate)^(0:49)
#no.spp <- 10
#no.actions <- 100
#(tot <- sum(1/disc)*no.spp*no.actions)
#[1] 39588.08

# Then as each species is removed, it should reduce by 'inidivid'... 
# individ <- sum(1/disc)*no.actions
# (tot - individ)
#[1] 35629.27 # second-iteration (loop it. 3) full budget

# INPUT: Set one action cost to be $2000, funded in every year, everything else funded each year with cost of $1
# OUTPUT:

# INPUT: Set one action cost to be $0.2, funded in every year, everything else funded each year with cost of $1

# INPUT: Weights set to 0.5
# OUTPUT: All species should have a CE score of half.

# INPUT: Set the success probability of the first species to 0
# OUTPUT: This species (sciname = Aa) should be removed before the PPP starts.

# W. Probert, 2013.  

########################
## Set the parameters ##
########################

source(file = "./test_arguments.R")

########################
## Generate the data. ##
########################

source(file = "./ppp_pseudo_data.R")

########################
## Load the functions ##
########################


#####################
###   ALGORITHM   ###
#####################

# INPUT: Set the budget to zero
# OUTPUT: The PPP should run through, removing all species

set_budget <- 0
PPP.zerobudget <- PrioritisationIII(directory, weighting, 
                             multiplier, set_budget, 
                             overlap.indicator, discount.rate, 
                             T, stop.iteration, stop.no.of.species,
                             a.data, b.data, o.data, w.data, 
                             all.costs)

# INPUT: Set the budget to infinity
# OUTPUT: PPP shouldn't run (should still remove those with B/S/W of zero).  

set_budget <- Inf
PPP.infbudget <- PrioritisationIII(directory, weighting, 
                             multiplier, set_budget, 
                             overlap.indicator, discount.rate, 
                             T, stop.iteration, stop.no.of.species,
                             a.data, b.data, o.data, w.data, 
                             all.costs)

#####################
###    BENEFITS   ###
#####################

# INPUT: Set the budget to zero, set one species B-with to 50%
# OUTPUT: That species (sciname = Aa) should be removed first

set_budget <- 0
b.data[1,"bene_with_action"] <- 50
PPP.B1 <- PrioritisationIII(directory, weighting, 
                             multiplier, set_budget, 
                             overlap.indicator, discount.rate, 
                             T, stop.iteration, stop.no.of.species,
                             a.data, b.data, o.data, w.data, 
                             all.costs)


# INPUT: Set the budget to zero, set one species B to 0%
# OUTPUT: That species (sciname = Aa) should be removed before the PPP starts.  

set_budget <- 0
b.data[1,"bene_with_action"] <- 50
b.data[1,"bene_no_action"] <- 50
PPP.B2 <- PrioritisationIII(directory, weighting, 
                             multiplier, set_budget, 
                             overlap.indicator, discount.rate, 
                             T, stop.iteration, stop.no.of.species,
                             a.data, b.data, o.data, w.data, 
                             all.costs)
b.data[1,"bene_with_action"] <- 100 # reset
b.data[1,"bene_no_action"] <- 0

# INPUT: Set the budget to zero, set one species B to 500%
# OUTPUT: That species (sciname = Ff) should be kept until the end

set_budget <- 0
b.data[6,"bene_with_action"] <- 500
PPP.B2 <- PrioritisationIII(directory, weighting, 
                             multiplier, set_budget, 
                             overlap.indicator, discount.rate, 
                             T, stop.iteration, stop.no.of.species,
                             a.data, b.data, o.data, w.data, 
                             all.costs)
b.data[1,"bene_with_action"] <- 100 # reset
b.data[1,"bene_no_action"] <- 0

#####################
###     COSTS     ###
#####################

# INPUT:  Set actions to have a cost twice the value of its associated 'species_id'
# OUTPUT: Species should then be removed in order from sciname Jj -> sciname Aa

source(file.path(functions.dir,'PrioritisationIII.R'))
all.costs[,2:51] <- with(a.data, matrix(species_id*2, 1000, T))
set_budget <- 0
PPP.prop_to_sppid <- PrioritisationIII(directory, weighting, 
                             multiplier, set_budget, 
                             overlap.indicator, discount.rate, 
                             T, stop.iteration, stop.no.of.species,
                             a.data, b.data, o.data, w.data, 
                             all.costs)

# INPUT:  Set discount rate to 0.01
# OUPUT: The first full budget should be... 
#disc <- (1 + discount.rate)^(0:49)
#no.spp <- 10
#no.actions <- 100
#(tot <- sum(1/disc)*no.spp*no.actions)
#[1] 39588.08

# Then as each species is removed, it should reduce by 'inidivid'... 
# individ <- sum(1/disc)*no.actions
# (tot - individ)
#[1] 35629.27 # second-iteration (loop it. 3) full budget

all.costs[,2:51] <- matrix(1, 1000, T)
discount.rate <- 0.01
set_budget <- 0
PPP.discount <- PrioritisationIII(directory, weighting, 
                             multiplier, set_budget, 
                             overlap.indicator, discount.rate, 
                             T, stop.iteration, stop.no.of.species,
                             a.data, b.data, o.data, w.data, 
                             all.costs)

# INPUT: Set one action cost to be $2000, funded in every year, everything else funded each year with cost of $1
# OUTPUT: 

all.costs[,2:51] <- matrix(1, 1000, T)
all.costs[1,] <- 2000
discount.rate <- 0.01
set_budget <- 0
PPP.discount <- PrioritisationIII(directory, weighting, 
                             multiplier, set_budget, 
                             overlap.indicator, discount.rate, 
                             T, stop.iteration, stop.no.of.species,
                             a.data, b.data, o.data, w.data, 
                             all.costs)

# INPUT: Set one action cost to be $0.2, funded in every year, everything else funded each year with cost of $1

all.costs[,2:51] <- matrix(1, 1000, T)
all.costs[1,] <- 0.2
discount.rate <- 0.01
set_budget <- 0
PPP.discount <- PrioritisationIII(directory, weighting, 
                             multiplier, set_budget, 
                             overlap.indicator, discount.rate, 
                             T, stop.iteration, stop.no.of.species,
                             a.data, b.data, o.data, w.data, 
                             all.costs)
all.costs[,2:51] <- matrix(1, 1000, T) # reset

#####################
###    WEIGHTS    ###
#####################

# INPUT: Weights set to 0.5
# OUTPUT: All species should have a CE score of half.  

all.costs[,2:51] <- matrix(1, 1000, T)
discount.rate <- 0
set_budget <- 0
w.data$weight_value <- 0.5
PPP.discount <- PrioritisationIII(directory, weighting, 
                             multiplier, set_budget, 
                             overlap.indicator, discount.rate, 
                             T, stop.iteration, stop.no.of.species,
                             a.data, b.data, o.data, w.data, 
                             all.costs)
w.data$weight_value <- 1 # reset

#####################
###    SUCCESS    ###
#####################

# INPUT: Set the success probability of the first species to 0
# OUTPUT: This species (sciname = Aa) should be removed before the PPP starts.  

a.data$ActionObjectiveSuccessProbability[1:100] <- 0

PPP.S1 <- PrioritisationIII(directory, weighting, 
                             multiplier, set_budget, 
                             overlap.indicator, discount.rate, 
                             T, stop.iteration, stop.no.of.species,
                             a.data, b.data, o.data, w.data, 
                             all.costs)

a.data$ActionObjectiveSuccessProbability[1:100] <- 100 # reset

#####################
###   ARGUMENTS   ###
#####################

# INPUT: Set the weighting argument to 0.3
weighting <- 0.3
set_budget <- 0
PPP.weighting <- PrioritisationIII(directory, weighting, 
                             multiplier, set_budget, 
                             overlap.indicator, discount.rate, 
                             T, stop.iteration, stop.no.of.species,
                             a.data, b.data, o.data, w.data, 
                             all.costs)
weighting <- 1
# Look at the removed.spp.df list
PPP.weighting[["removed.spp.df"]]

# INPUT: Set the multiplier to 0.4

multiplier <- 0.4
set_budget <- 0
PPP.discount <- PrioritisationIII(directory, weighting, 
                             multiplier, set_budget, 
                             overlap.indicator, discount.rate, 
                             T, stop.iteration, stop.no.of.species,
                             a.data, b.data, o.data, w.data, 
                             all.costs)
multiplier <- 1 # reset



# INPUT: Set the stop.iteration to 4
stop.iteration <- 4
set_budget <- 0
PPP.stop.iteration <- PrioritisationIII(directory, weighting, 
                             multiplier, set_budget, 
                             overlap.indicator, discount.rate, 
                             T, stop.iteration, stop.no.of.species,
                             a.data, b.data, o.data, w.data, 
                             all.costs)
stop.iteration <- Inf # reset



# INPUT: Stop after 4 species have been removed.  
stop.no.of.species <- 4
set_budget <- 0
PPP.stop.no.of.species <- PrioritisationIII(directory, weighting, 
                             multiplier, set_budget, 
                             overlap.indicator, discount.rate, 
                             T, stop.iteration, stop.no.of.species,
                             a.data, b.data, o.data, w.data, 
                             all.costs)
stop.no.of.species <- 0 # reset


# INPUT: Turn on overlapping costs.  
overlap.indicator <- TRUE
PPP.overlap.indicator <- PrioritisationIII(directory, weighting, 
                             multiplier, set_budget, 
                             overlap.indicator, discount.rate, 
                             T, stop.iteration, stop.no.of.species,
                             a.data, b.data, o.data, w.data, 
                             all.costs)
overlap.indicator <- FALSE