##################################################################################
# DataSetup and the setup functions it calls were written by Belinda Mellish
# bmellish@doc.govt.nz
# 3 July 2012
# Updated 4 December 2012, W Probert.  
# - added default argument calls
# - added year.text argument for year column name
##################################################################################

DataSetup <- function(directory, data, 
                    poly.size, poly.percent, strip.projects, projects, 
                    select.taxa, taxa, set.benefit, 
                    new.benefit, set.success,
                    new.success, overlap.indicator, output.monitoring, 
                    run.prioritisation, set_budget, summary, 
                    output.dir, prefix_text,
                    multiplier, weighting, discount.rate, 
                    T, stop.iteration, select.projects, 
                    project.list, input.w.data, year.text, ...){
  
  # This function reads in the 2 dbf files generated by the SRP GIS analysis,
  # and the 5 input files from the PPP database. It performs data setup and checking 
  # tasks prior running a modified version of the Prioritisation function PPP_v0.5.
  
  ## Argurment descriptions ################################################################################################################
  #   directory     - directory for all input and output files
  #   data          - vector ofname of: Access database and the 2 files from SRP GIS analysis
  #   poly.size     - upper limit of polygons created by super region poly considered for overlaps in conjuction with poly.percent
  #   poly.percent  - for SRP polys less than poly.size they must be at least this percentage of the orignal to be considered for overlaps
  #   strip.projects- set to TRUE if you wish to limit the number of projects prioritised
  #   projects      - the number of projects prioritised if strip.projects=TRUE (from the order projects are listed in benefits.csv)
  #   select.taxa   - set to TRUE to run prioritisation on a subset of one taxa type
  #   taxa          - taxa text string used to subset by if select.taxa is TRUE
  #   set.benefit       - set to TRUE to set benefit value for all species, FALSE uses benefit data from input file
  #   new.benefit       - benefit value used if set.benefit is TRUE
  #   set.success       - set to FALSE to use values in input file, or set to 0-100 to change all success values
  #   new.success       - success value used if set.success is TRUE
  #   overlap.indicator - set to TRUE if you want overlaps to be calculated
  #   output.monitoring - set to TRUE to include output monitoring data
  #   run.prioritisation- TRUE - prioritisation will run immediately, FALSE - csv files will be written to directory
  #   set_budget    - budget for priortisation to run to
 
  ############################################################################################################################################
  
  beginner <- proc.time()
  
  if(strtrim(version$os, 3) == "win"){
      memory.limit(4000)
  }
  
  ##############################################
  ### Read in GIS data and set up ##############
  ##############################################
  
  GIS <- ConnectToGISdata(poly.size, poly.percent, data)
  
  GIS.data <- GIS$data
  for.overlaps <- GIS$overlaps
  poly.data <- GIS$poly
  set.flag <- GIS$flag
  
  ###############################################
  ### Read in action, benefit and weights data ##
  ###############################################
  
  DataList<-ConnectToActionBenefitWeights(data)
  
  a.data <- DataList$actions
  b.data <- DataList$benefits
  w.data <- DataList$weights
  
  ##############################################
  ### Data checks ##############################
  ##############################################
 
  cat('Performing data checks','\n')
  DataChecksIII(a.data, b.data, w.data, directory, for.overlaps, poly.data)
  
  ###############################################
  ### Strip projects to run on smaller dataset ##
  ###############################################
  
  if(strip.projects==TRUE){
    
    if(select.projects==TRUE){
      cat("Subsetting projects to those in project.list",'\n')
      spp.keep <- project.list
      b.index <- unlist(lapply(spp.keep, function(x) with(b.data, which(species_id==x))))
      b.strip <- b.data[b.index,]
    }else{if(select.taxa==TRUE){ # if select.projects=F and select.taxa=T
      cat("Stripping dataset to",taxa,"only",'\n')
      b.strip<-b.data[with(b.data, which(taxa_text==taxa)),]
      spp.keep<-b.strip[,1]
      if(length(spp.keep)>length(projects)){
        cat("Stripping dataset to",projects,"projects",'\n')
        # create vector of speciesID in b.strip
        b.strip<-b.strip[1:projects,]
        spp.keep<-spp.keep[1:projects]
      }
    }else{b.strip<-b.data[1:projects,]
          spp.keep <- b.data[1:projects, "species_id"] # if select.projects=F and select.taxa=F, strip to n projects in b.data <<<<<<<<<<<< COLUMNS NAMES
    }     
    }
    # strip actions, benefit and polygon data  
    a.index<-unlist(lapply(spp.keep, function(x) with(a.data, which(species_id==x))))
    a.strip<-a.data[a.index,]
    #Strip out projects from GIS outputs
    p.index<-unlist(lapply(spp.keep, function(x) with(GIS.data, which(SpeciesID==x))))
    poly.strip<-GIS.data[p.index,]
    # Relabel for use in DataSetupMainFunction, then remove objects no longer needed
    a.data<-a.strip
    b.data<-b.strip
    GIS.data<-poly.strip
    rm(a.strip,b.strip,poly.strip, p.index)
    
  }else{spp.keep<-b.data[, "species_id"]}
  
  if(set.benefit==TRUE){
    b.data[, "bene_with_action"] <- new.benefit
    b.data[, "bene_no_action"] <- 0
  }
  
  if(set.success==TRUE){
    a.data[,c("ManagementSiteInputSuccess", "ManagementSiteOutputSuccess")] <- 1
    a.data[,"ActionObjectiveSuccessProbability"] <- new.success
    #replace all 95 values in cols 18:20 a.data with 100
  }else{
      to.replace.index <- with(a.data,which(ManagementSiteInputSuccess==95))     
      a.data[to.replace.index, "ManagementSiteInputSuccess"]<-100
      
      to.replace.index <- with(a.data,which(ManagementSiteOutputSuccess==95))
      a.data[to.replace.index, "ManagementSiteOutputSuccess"]<-100
      
      to.replace.index <- with(a.data,which(ActionObjectiveSuccessProbability==95))
      a.data[to.replace.index,"ActionObjectiveSuccessProbability"]<-100
  }
  
  
  #############################################################
  ############# Create cost period matrices ###################
  #############################################################
  
  CostList<-NewSetupCosts(a.data, T, year.text)
  
  cost.period <- CostList[["period"]]
  period_text <- CostList[["text"]]
  all.costs <- CostList[["allcosts"]]
  
  ##############################################
  ### Calculate overlaps #######################
  ##############################################
  
  if(overlap.indicator == TRUE){
        o.data <- SetupOverlaps(GIS.data, a.data)
    }else{
        o.data <- data.frame(action_id = with(a.data, action_id), overlap_id = 1:with(a.data, length(action_id)))
    }
  
  rm(poly.data, for.overlaps, GIS)
  
  if(output.monitoring == TRUE){ # if false o.data doesn't need updating so do nothing to it
    OutputMon <- SetupOutputMon(data, directory, a.data, o.data, cost.period, all.costs, spp.keep, overlap.indicator, strip.projects)
    
    a.data <- OutputMon$a
    cost.period <- OutputMon$p
    o.data <- OutputMon$o
    all.costs <- OutputMon$c
  }
  
  orig.costs <- all.costs
  write.csv(cbind(a.data$action_id, orig.costs), file.path(output.dir,"InitialCombinedCosts.csv"), row.names = FALSE)
  
  ## PrePrioritisationWorkspace ##
  
  if(run.prioritisation==TRUE){
    cat('DataSetup complete, starting prioritisation','\n')
    output_PPP<-PrioritisationII(directory, weighting, 
                                 multiplier, set_budget, overlap.indicator, 
                                 discount.rate, T, stop.iteration, 
                                 a.data, b.data, o.data, w.data, cost.period,
                                 all.costs)
    # add other input parameters
    other.para<-cbind(data, 
                      poly.size, poly.percent, strip.projects, projects, 
                      select.taxa, taxa, set.benefit, new.benefit, set.success,
                      new.success, overlap.indicator, output.monitoring)
    output_PPP$input.parameters <- cbind(output_PPP$input.parameters, other.para)
    
    if(summary){
    
      result<-summary.PPP(object=output_PPP, verbose_cost_info=TRUE, save.to.file=TRUE, directory=output.dir, prefix=prefix_text)
    
    print.summary.PPP(result)
    }
    
    if(set.flag){
      cat('\n','\n','WARNING: Number of MgmtSiteIDs DID NOT match between spatial data and database','\n')
    }
  }else{
    write.csv(a.data,"ActionData.csv", row.names=FALSE)
    write.csv(o.data,"OverlapData.csv", row.names=FALSE)
    write.csv(b.data,"BenefitData.csv", row.names=FALSE)
    write.csv(w.data,"WeightsData.csv", row.names=FALSE)
    write.csv(cost.period,"PeriodCosts.csv", row.names=FALSE)
#    write.csv(cost.period1,"Period1.csv", row.names=FALSE)
#    write.csv(cost.period2,"Period2.csv", row.names=FALSE)
#    write.csv(cost.period3,"Period3.csv", row.names=FALSE)
    
    if(set.flag==TRUE){
      cat('\nWARNING: Number of MgmtSiteIDs DID NOT match between spatial data and database','\n')
    }
    
    cat('Files written to working directory','\n')
    duration = proc.time() - beginner
    cat("Data setup took", prettyNum(duration, big.mark=',')[3], 'seconds\n')
    
    dataset <- list()
    dataset$actions <- a.data
    dataset$benefits <- b.data
    dataset$overlaps <- o.data
    dataset$weights <- w.data
    
    dataset$cost.period <- cost.period
    dataset$all.costs <- all.costs
    
    return(dataset)
  }
}
