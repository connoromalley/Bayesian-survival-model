library(RMark)

# this code is for using  RMark which is a program that gives you capture probability and survival
# probability using 'mostly' likelihood method

# we accomplish the same thing later using a bayesian method passing to jags and get similar results 


data(dipper)

dipper.model <- mark(dipper)
run.dipper <- function() {
  
  #
  # Process data
  #
  dipper.processed <- process.data( dipper, groups=("sex") )
  
  #
  # Create default design data
  #
  dipper.ddl <- make.design.data( dipper.processed )
  
  #
  # Add Flood covariates for Phi and p that have different values
  #
  dipper.ddl$Phi$Flood  <- 0
  dipper.ddl$Phi$Flood[ dipper.ddl$Phi$time==2 | dipper.ddl$Phi$time==3 ]  <- 1
  dipper.ddl$p$Flood <- 0
  dipper.ddl$p$Flood[ dipper.ddl$p$time==3 ] <- 1
  #
  #  Define range of models for Phi
  #
  Phidot <- list( formula=~1 )
  Phitime <- list( formula=~time )
  Phisex <- list( formula=~sex )
  Phisextime <- list( formula =~ sex + time )
  Phisex.time <- list( formula =~ sex*time )
  PhiFlood <- list( formula =~ Flood ) 
  
  #
  #  Define range of models for p
  #
  pdot <- list( formula =~ 1 )
  ptime <- list( formula =~ time )
  psex <- list( formula =~ sex )
  psextime <- list( formula =~ sex + time )
  psex.time <- list(formula =~ sex*time )
  pFlood <- list( formula =~ Flood )
  
  #
  # Run assortment of models
  #
  dipper.phidot.pdot <- mark(dipper.processed,dipper.ddl,
                             model.parameters=list(Phi=Phidot,p=pdot))
  dipper.phidot.pFlood <- mark(dipper.processed,dipper.ddl,
                               model.parameters=list(Phi=Phidot,p=pFlood))
  dipper.phidot.psex <- mark(dipper.processed,dipper.ddl,
                             model.parameters=list(Phi=Phidot,p=psex))
  dipper.phidot.ptime <- mark(dipper.processed,dipper.ddl,
                              model.parameters=list(Phi=Phidot,p=ptime))
  dipper.phidot.psex.time <- mark(dipper.processed,dipper.ddl,
                                  model.parameters=list(Phi=Phidot,p=psex.time))
  dipper.phitime.ptime <- mark(dipper.processed,dipper.ddl,
                               model.parameters=list(Phi=Phitime, p=ptime))
  dipper.phitime.pdot <- mark(dipper.processed,dipper.ddl,
                              model.parameters=list(Phi=Phitime,p=pdot))
  dipper.phitime.psex <- mark(dipper.processed,dipper.ddl,
                              model.parameters=list(Phi=Phitime,p=psex))
  dipper.phitime.psex.time <- mark(dipper.processed,dipper.ddl,
                                   model.parameters=list(Phi=Phitime,p=psex.time))
  dipper.phiFlood.pFlood <- mark(dipper.processed,dipper.ddl,
                                 model.parameters=list(Phi=PhiFlood, p=pFlood))
  dipper.phisex.pdot <- mark(dipper.processed,dipper.ddl,
                             model.parameters=list(Phi=Phisex,p=pdot))
  dipper.phisex.psex <- mark(dipper.processed,dipper.ddl,
                             model.parameters=list(Phi=Phisex,p=psex))
  dipper.phisex.psex.time <- mark(dipper.processed,dipper.ddl,
                                  model.parameters=list(Phi=Phisex,p=psex.time))
  dipper.phisex.ptime <- mark(dipper.processed,dipper.ddl,
                              model.parameters=list(Phi=Phisex,p=ptime))
  dipper.phisextime.psextime <- mark(dipper.processed,dipper.ddl,
                                     model.parameters=list(Phi=Phisextime,p=psextime))
  dipper.phisex.time.psex.time <- mark(dipper.processed,dipper.ddl,
                                       model.parameters=list(Phi=Phisex.time,p=psex.time))
  dipper.phisex.time.psex <- mark(dipper.processed,dipper.ddl,
                                  model.parameters=list(Phi=Phisex.time,p=psex))
  dipper.phisex.time.pdot <- mark(dipper.processed,dipper.ddl,
                                  model.parameters=list(Phi=Phisex.time,p=pdot))
  dipper.phisex.time.ptime <- mark(dipper.processed,dipper.ddl,
                                   model.parameters=list(Phi=Phisex.time,p=ptime))
  #
  # Return model table and list of models
  #
  return( collect.models() )
}

dipper.results <- run.dipper()

run.dipper.alternate=function() {
  #
  # Process data
  #
  dipper.processed <- process.data( dipper, groups=("sex") )
  #
  # Create default design data
  #
  dipper.ddl <- make.design.data( dipper.processed )
  #
  # Add Flood covariates for Phi and p that have different values
  #
  dipper.ddl$Phi$Flood  <- 0
  dipper.ddl$Phi$Flood[ dipper.ddl$Phi$time==2 | dipper.ddl$Phi$time==3 ]  <- 1
  dipper.ddl$p$Flood <- 0
  dipper.ddl$p$Flood[ dipper.ddl$p$time==3 ] <- 1
  #
  #  Define range of models for Phi
  #
  #
  
  Phidot <- list( formula=~1 )
  Phitime <- list( formula=~time )
  Phitimec <- list(formula=~time,fixed=list(time=6,value=1))
  Phisex <- list( formula=~sex )
  Phisextime <- list( formula =~ sex + time )
  Phisex.time <- list( formula =~ sex*time )
  PhiFlood <- list( formula =~ Flood ) 
  
  #
  #  Define range of models for p
  #
  pdot <- list( formula =~ 1 )
  ptime <- list( formula =~ time )
  ptimec <- list(formula=~time,fixed=list(time=7,value=1))
  psex <- list( formula =~ sex )
  psextime <- list( formula =~ sex + time )
  psex.time <- list(formula =~ sex*time )
  psex.timec <- list(formula=~sex*time,fixed=list(time=7,value=1))
  pFlood <- list( formula =~ Flood )
  
  
  #
  # Run assortment of models
  #
  
  dipper.phidot.pdot <- mark(dipper.processed,dipper.ddl,
                             model.parameters=list(Phi=Phidot,p=pdot))
  dipper.phidot.pFlood <- mark(dipper.processed,dipper.ddl,
                               model.parameters=list(Phi=Phidot,p=pFlood))
  dipper.phidot.psex <- mark(dipper.processed,dipper.ddl,
                             model.parameters=list(Phi=Phidot,p=psex))
  dipper.phidot.ptime <- mark(dipper.processed,dipper.ddl,
                              model.parameters=list(Phi=Phidot,p=ptime))
  dipper.phidot.psex.time <- mark(dipper.processed,dipper.ddl,
                                  model.parameters=list(Phi=Phidot,p=psex.time))
  dipper.phitime.ptime <- mark(dipper.processed,dipper.ddl,
                               model.parameters=list(Phi=Phitime, p=ptime))
  dipper.phitime.pdot <- mark(dipper.processed,dipper.ddl,
                              model.parameters=list(Phi=Phitime,p=pdot))
  dipper.phitime.psex <- mark(dipper.processed,dipper.ddl,
                              model.parameters=list(Phi=Phitime,p=psex))
  dipper.phitime.psex.time <- mark(dipper.processed,dipper.ddl,
                                   model.parameters=list(Phi=Phitime,p=psex.time))
  dipper.phiFlood.pFlood <- mark(dipper.processed,dipper.ddl,
                                 model.parameters=list(Phi=PhiFlood, p=pFlood))
  dipper.phisex.pdot <- mark(dipper.processed,dipper.ddl,
                             model.parameters=list(Phi=Phisex,p=pdot))
  dipper.phisex.psex <- mark(dipper.processed,dipper.ddl,
                             model.parameters=list(Phi=Phisex,p=psex))
  dipper.phisex.psex.time <- mark(dipper.processed,dipper.ddl,
                                  model.parameters=list(Phi=Phisex,p=psex.time))
  dipper.phisex.ptime <- mark(dipper.processed,dipper.ddl,
                              model.parameters=list(Phi=Phisex,p=ptime))
  dipper.phisextime.psextime <- mark(dipper.processed,dipper.ddl,
                                     model.parameters=list(Phi=Phisextime,p=psextime))
  dipper.phisex.time.psex.time <- mark(dipper.processed,dipper.ddl,
                                       model.parameters=list(Phi=Phisex.time,p=psex.time))
  dipper.phisex.time.psex <- mark(dipper.processed,dipper.ddl,
                                  model.parameters=list(Phi=Phisex.time,p=psex))
  dipper.phisex.time.pdot <- mark(dipper.processed,dipper.ddl,
                                  model.parameters=list(Phi=Phisex.time,p=pdot))
  dipper.phisex.time.ptime <- mark(dipper.processed,dipper.ddl,
                                   model.parameters=list(Phi=Phisex.time,p=ptime))
  dipper.phitime.ptimec <- mark(dipper.processed,dipper.ddl,
                                model.parameters=list(Phi=Phitime, p=ptimec))
  dipper.phisex.time.psex.timec <- mark(dipper.processed,dipper.ddl,
                                        model.parameters=list(Phi=Phisex.time,p=psex.timec))
  dipper.phisex.time.ptimec <- mark(dipper.processed,dipper.ddl,
                                    model.parameters=list(Phi=Phisex.time,p=ptimec))
  #
  # Return model table and list of models
  #
  return( collect.models() )
  
}
dipper.results.alternate <- run.dipper.alternate()
#
# Merge two sets of models into a single model list and include the
# initial model as a demo for merge.mark
#
dipper.cjs <- merge.mark(dipper.results,dipper.results.alternate,dipper.model)
dipper.cjs
#
# next delete some of the models to show how this is done with remove.mark
#
dipper.cjs <- remove.mark(dipper.cjs,c(2,4,9))
dipper.cjs


