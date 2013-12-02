
# Run the test code for the 2nd generation PPP
rm(list = ls())
source("./test_arguments.R")
source("./test_ppp_II.R")

# Run the test code for the 3rd generation PPP
rm(list = ls())
source("./test_arguments.R")
source("./test_ppp_III.R")

# Clear the workspace
rm(list = ls())

# Load the results from both the PPP outputs
load(file = "")
load(file = "")

# Compare the output

identical()


