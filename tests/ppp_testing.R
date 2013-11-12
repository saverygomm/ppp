# Test script for the PPP.  
# The following tests are run.  

# This script should run cleanly with the PPP.  
# You should start this with a clean R session, set the working directory to the base folder that you've downloaded the zip-file from Github.  


# W. Probert, 2013.  



# Parameters
directory <- getwd()
weighting <- 1
multiplier <- 1
set_budget <- 0
overlap.indicator <- FALSE
discount.rate <- 0
T <- 50
stop.iteration <- Inf
stop.no.of.species <- 0

########################
## Generate the data. ##
########################

a.data <- data.frame(
  action_id = 1:1000,
  species_id = rep(1:10, each = 100),
  TaxaCodeText = rep(LETTERS[1:10], each = 100),
  TaxaCode = rep(21:30, each = 100),
  ProjectText = rep(letters[1:10], each = 100),
  ProjectCode = rep(-c(1:10), each = 100),
  "Method" = "Method",
  Method1Text = "Method1Text",
  Method2Text = "Method2Text",
  Method3Text = "Method3Text",
  Method4Text = "Method4Text",
  Period1Cost = 1,
  Period1Sequence = paste(1:50, sep = "", collapse = ","),
  Period2Cost = 0,
  Period2Sequence = paste(1:50, sep = "", collapse = ","),
  Period3Cost = 0,
  Period3Sequence = paste(1:50, sep = "", collapse = ","),
  ManagementSiteInputSuccess = 100,
  ManagementSiteOutputSuccess = 100,
  ActionObjectiveSuccessProbability = 100
)

b.data <- data.frame(
  species_id = 1:10,
  TaxaText = LETTERS[1:10],
  TaxaCode = 21:30,
  ProjectText = letters[1:10],
  sciname = do.call(paste, list(LETTERS[1:10], letters[1:10], sep = "")),
  bene_with_action = 100,
  bene_no_action = 0,
  ProjectCode = -c(1:10)
)

o.data <- data.frame(
action_id = 1:10,
overlap_id = 1:10
)

w.data <- data.frame(
species_id = 1:10,
species_type = "Bat",
sciname = do.call(paste, list(LETTERS[1:10], letters[1:10], sep = "")),
genus = do.call(paste, list("Genus", letters[1:10], sep = "_")),
Bs = 1,
Ts = 1,
family = do.call(paste, list("Family", letters[1:10], sep = "_")),
Bg = 1,
Tf = 1,
order = do.call(paste, list("Order", letters[1:10], sep = "_")),
Bf = 1,
Tf = 1,
Endem.spp = 0,
Endem.gen = 0,
Endem.fam = 0,
weight_value = 1
)


all.costs <- data.frame(cbind("action_id" = 1:1000, as.data.frame(matrix(1, 1000, T))))  # (No. actions x T) True/false for if an action is costed
names(all.costs)[2:(T+1)] <- do.call(paste, list("Y", 1:50, sep = ""))

#####################
###   ALGORITHM   ###
#####################

# Set the budget to zero
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


# Set the budget to zero, set one species B-with to 50%
# OUTPUT: Species Aa should be removed first
set_budget <- 0
b.data[1,"bene_with_action"] <- 50
PPP.B1 <- PrioritisationIII(directory, weighting, 
                             multiplier, set_budget, 
                             overlap.indicator, discount.rate, 
                             T, stop.iteration, stop.no.of.species,
                             a.data, b.data, o.data, w.data, 
                             all.costs)


# Set the budget to zero, set one species B to 0%
# OUTPUT: Species Aa should be removed before the PPP starts
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
# OUTPUT: Species Ff should be kept until the end
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

# INPUT:  Set actions to have a cost twice its 'species_id'
# OUTPUT: Species should then be removed in order from Jj -> Aa
source(file.path(functions.dir,'PrioritisationIII.R'))
all.costs[,2:51] <- with(a.data, matrix(species_id*2, 1000, T))
set_budget <- 0
PPP.prop_to_sppid <- PrioritisationIII(directory, weighting, 
                             multiplier, set_budget, 
                             overlap.indicator, discount.rate, 
                             T, stop.iteration, stop.no.of.species,
                             a.data, b.data, o.data, w.data, 
                             all.costs)


# Set discount rate to 0.01
all.costs[,2:51] <- matrix(1, 1000, T)
discount.rate <- 0.01
set_budget <- 0
PPP.discount <- PrioritisationIII(directory, weighting, 
                             multiplier, set_budget, 
                             overlap.indicator, discount.rate, 
                             T, stop.iteration, stop.no.of.species,
                             a.data, b.data, o.data, w.data, 
                             all.costs)

# The first full budget should be... 
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
all.costs[,2:51] <- matrix(1, 1000, T)


#####################
###    WEIGHTS    ###
#####################


# INPUT: Weight
# OUTPUT: Should have a CE of half.  

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


#####################
###    SUCCESS    ###
#####################


a.data$ActionObjectiveSuccessProbability[1:100] <- 0

PPP.S1 <- PrioritisationIII(directory, weighting, 
                             multiplier, set_budget, 
                             overlap.indicator, discount.rate, 
                             T, stop.iteration, stop.no.of.species,
                             a.data, b.data, o.data, w.data, 
                             all.costs)

a.data$ActionObjectiveSuccessProbability[1:100] <- 100 # reset
