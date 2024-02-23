

# COMPUTE AVERAGE RATE OF CHANGE OF A GIVEN METRIC AMONG TIME PERIODS


# We have a dataframe with this structure
  # value is the state of a given metric in a given year
  # year

# Example
# landscape1 <- data.frame(value=c(6.61, 19.85, 40.66, 13.94, 38.06),
#                          year=c(1990, 2000, 2006, 2012, 2018))
  
annualchange <- function(landscape1) {
  
  # convert variables into numeric
  landscape1$value <- as.numeric(landscape1$value)
  landscape1$year <- as.numeric(landscape1$year)
  
  # vector of years from recent to older
  years1 <- landscape1$year[order(landscape1$year, decreasing=T)]
  
  # vector of results
  interv1 <- vector(length=length(years1)-1); interv1[] <- NA
  
  # for every time period, compute difference (recent - older)
  for (i in 1:length(interv1)) {
    
    # difference for that period
    t1 <- landscape1$value[landscape1$year==years1[i]]-landscape1$value[landscape1$year==years1[i+1]]
    
    # number of years between observations
    t2 <- years1[i]-years1[i+1]
    
    # annual rate of change
    interv1[i] <- t1/t2
      
  }
  
  return(mean(interv1))
  
}

# annualchange(landscape1)


