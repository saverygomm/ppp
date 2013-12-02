# To do list for the PPP test... 
- THREE: PPP Martina sent from Richard, PPP in ppp/ppp/ folder and PrioritisationIII.R 
- Compare PrioritisationIII with PrioritisationII and make sure they get the same results (else document why not). 1) Set up the same dataset, 2) Test on each and save output, 3) compare output and find/adjust differences until it's the same (add adjustments into the testing scripts for PriorisationII.R).  
- Adjust PrioritisationIII so it gets all results as expected in ppp_testing.R, and so that it's cleaner.  
- Set up an overlapping costs example.  
- Set up an example to test what happens with NA values in there.  
  - NA costs
  - NA successes
  - NA benefits
  - NA weights
- Write down what the column names should be.  
- Remove any references to column numbers (for instance, b.data[,c(1:3)])
- Change all instances of the dollar sign, to use the function with() instead (it will create a column for this otherwise).  
- At this stage, we should have an optimal PPP and a list of differences with old PPP.  
- Document all changes between PrioritisationII and PrioritisationIII.  

- Add a readme file.  
- Upload to Github with the actual PPP
- Make sure the newer PPP just has the optimization functions.  
- Finish the documentation in the R package folders.  
- Fix the script to run Martina's work - Persistence Objective, just need to fix the costing matrices.  

# CHANGES.  
- all.costs is the only costing data to be fed into the PPP.  
- all.costs also has action ID added to it as the first column.  
- log.file argument.  Save a log for the whole test script.  
- stop.no.of.species argument
- (line 60) compare with b.data, not w.data.  extra <- match(with(a.data, unique(species_id)), with(b.data, species_id))
- cost matrix calculation is changed to a loop over 50 years for ease of reading (before it was an lapply function).  
- column names are changed.  
- removed 'ordered_annual_costs'
- a.data doesn't need the Period1Costs columns... test the PPP without them added.  
- Changed the name of a.data column spp_text to ProjectText
