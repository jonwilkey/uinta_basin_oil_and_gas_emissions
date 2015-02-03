# Function Info -----------------------------------------------------------
# Name:      DCAupdateCDF.R (DCA Cumulative Distribution Function Update)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# field - List of fields to be analyzed individually

# ver - Version number for results version tracking

# DCA.CDF.type - character string, either "Density" or "Quantile", which acts a
# switch for determining whether to generate CDF using density (CDFd.R) or
# quantile (CDFq.R) functions.

# cdf.oil.from - Lower limit for CDF function for (qo, b, Di, tdelay)

# cdf.oil.to - Upper limit for CDF function for (qo, b, Di, tdelay)

# cdf.oil.np - Number of points at which to estimate CDF for (qo, b, Di, tdelay)

# cdf.gas.from - Same as above, but for gas

# cdf.gas.to - Same as above, but for gas

# cdf.gas.np - Same as above, but for gas

# DCA.CDF.xq - Vector of probability points at which to estimate quantiles

# path - path names for file directoires (data, plotting, etc.)

# tstart - Lower-limit cutoff date for which wells to include in CDF analysis

# tstop - Upper-limit cutoff date for which wells to include in CDF analysis


# Outputs -----------------------------------------------------------------

# DCA.cdf.coef.oil - List object containing CDF results for oil for all
# coefficients for each field.

# DCA.cdf.coef.gas - same as above, but for gas


# Description -------------------------------------------------------------

# This function pulls subsets from the fit results data.frames mo and mg to 
# generate cumulative distribution functions (CDF) for each parameter in the 
# hyperbolic decline curve equation for each field. These CDF results are then
# saved as a list and exported.


# Function ----------------------------------------------------------------
DCAupdateCDF <- function(field, ver, DCA.CDF.type, cdf.oil.from, cdf.oil.to,
                      cdf.oil.np, cdf.gas.from, cdf.gas.to, cdf.gas.np,
                      DCA.CDF.xq, path, tstart, tstop) {
  
  # Load DCA fit data -------------------------------------------------------
  
  # From DCAupdate function export, load data.frames "mo" and "mg"
  load(file.path(path$data, paste("DCA_fits_", ver, ".rda", sep = "")))
  
  # Drop values higher than cdf.oil/gas.to cutoffs and within tstart/tstop time
  # limits
  mo <- subset(mo, subset = (qo.1 <=         cdf.oil.to[1] &
                             b.1 <=          cdf.oil.to[2] &
                             Di.1 <=         cdf.oil.to[3] &
                             tdelay <=       cdf.oil.to[4] &
                             h_first_prod >= tstart &
                             h_first_prod <= tstop))
  
  mg <- subset(mg, subset = (qo.1 <=         cdf.gas.to[1] &
                             b.1 <=          cdf.gas.to[2] &
                             Di.1 <=         cdf.gas.to[3] &
                             tdelay <=       cdf.gas.to[4] &
                             h_first_prod >= tstart &
                             h_first_prod <= tstop))
  
  
  # Analysis ----------------------------------------------------------------
  
  # Predefine coefficient CDF list
  DCA.cdf.coef.oil <- NULL
  DCA.cdf.coef.gas <- NULL
  
  # Set initial value of list element counter
  list.ind <- 1
  
  # Define coefficient names vector
  coef.name <- c("qo", "b", "Di", "tdelay")
  
  # Define ind999 vector
  ind999 <- NULL
  
  # For each field, get cumulative distribution function for each decline curve 
  # coefficient for first curve fits and time-delay between first production and
  # start of first curve.
  for (i in 1:(length(field)-1)) {
    
    # Get subset of fits for this field
    otemp <- subset(mo,
                    subset = (fit.1 == 1 &
                                w_field_num == field[i]),
                    select = c("qo.1",
                               "b.1",
                               "Di.1",
                               "tdelay"))
    gtemp <- subset(mg,
                    subset = (fit.1 == 1 &
                                w_field_num == field[i]),
                    select = c("qo.1",
                               "b.1",
                               "Di.1",
                               "tdelay"))
    
    # Get CDF for each coefficient for all fields except catch-all Field 999
    for (j in 1:ncol(otemp)) {
      
      # Switch handles two analysis types, first using density() function to get
      # CDF, other uses quantile() function.
      switch(DCA.CDF.type,
             
             # If getting CDF via density() function
             Density = {
               
               # CDFd call, assigned to DCA as list element "list.ind"
               DCA.cdf.coef.oil[[list.ind]] <- CDFd(vector = otemp[,j],
                                                    from =   cdf.oil.from[j],
                                                    to =     cdf.oil.to[j],
                                                    np =     cdf.oil.np[j])
               
               DCA.cdf.coef.gas[[list.ind]] <- CDFd(vector = gtemp[,j],
                                                    from =   cdf.gas.from[j],
                                                    to =     cdf.gas.to[j],
                                                    np =     cdf.gas.np[j])
             },
             
             # If getting CDF via quantile() function
             Quantile = {
               
               # CDFq call, assigned to DCA as list element "list.ind"
               DCA.cdf.coef.oil[[list.ind]] <- CDFq(vector = otemp[,j],
                                                    xq =     DCA.CDF.xq)
               
               DCA.cdf.coef.gas[[list.ind]] <- CDFq(vector = gtemp[,j],
                                                    xq =     DCA.CDF.xq)
             })
      
      # Set name for list
      names(DCA.cdf.coef.oil)[[list.ind]] <- paste("OF.", field[i], ".", coef.name[j], sep = "")
      names(DCA.cdf.coef.gas)[[list.ind]] <- paste("GF.", field[i], ".", coef.name[j], sep = "")
      
      # Increment list element counter
      list.ind <- list.ind+1
    }
    
    # Find row indices in this field (mo and mg have same p_api index) and use
    # them to build ind999 exclusion list
    ind <- which(mo$w_field_num == field[i])
    ind999 <- c(ind999, ind)
  }
  
  # For Field 999, start by dropping all rows in exclusion index
  otemp <- mo[-ind999,]
  gtemp <- mg[-ind999,]
  
  # Next, drop anything which doesn't have fit and only select desired columns
  otemp <- otemp[which(otemp$fit.1 == 1),][,8:11]
  gtemp <- gtemp[which(gtemp$fit.1 == 1),][,8:11]
  
  # Finally, increment i and get CDFs for coefficients
  i <- 11
  
  # Get CDF for catch-all Field 999
  for (j in 1:ncol(otemp)) {
    
    # Switch handles two analysis types, first using density() function to get
    # CDF, other uses quantile() function.
    switch(DCA.CDF.type,
           
           # If getting CDF via density() function
           Density = {
             
             # CDFd call, assigned to DCA as list element "list.ind"
             DCA.cdf.coef.oil[[list.ind]] <- CDFd(vector = otemp[,j],
                                                  from =   cdf.oil.from[j],
                                                  to =     cdf.oil.to[j],
                                                  np =     cdf.oil.np[j])
             
             DCA.cdf.coef.gas[[list.ind]] <- CDFd(vector = gtemp[,j],
                                                  from =   cdf.gas.from[j],
                                                  to =     cdf.gas.to[j],
                                                  np =     cdf.gas.np[j])
           },
           
           # If getting CDF via quantile() function
           Quantile = {
             
             # CDFq call, assigned to DCA as list element "list.ind"
             DCA.cdf.coef.oil[[list.ind]] <- CDFq(vector = otemp[,j],
                                                  xq =     DCA.CDF.xq)
             
             DCA.cdf.coef.gas[[list.ind]] <- CDFq(vector = gtemp[,j],
                                                  xq =     DCA.CDF.xq)
           })
    
    # Set name for list
    names(DCA.cdf.coef.oil)[[list.ind]] <- paste("OF.", field[i], ".", coef.name[j], sep = "")
    names(DCA.cdf.coef.gas)[[list.ind]] <- paste("GF.", field[i], ".", coef.name[j], sep = "")
    
    # Increment list element counter
    list.ind <- list.ind+1
  }
  
  
  # Export Results ----------------------------------------------------------
  
  # Save CDF results
  save(file=file.path(path$data,
                      paste("DCA_CDF_coef_", ver, ".rda", sep = "")),
       list=c("DCA.cdf.coef.oil",
              "DCA.cdf.coef.gas"))
}