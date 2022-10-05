## Determine if variable is a date, time, or date/time variable in R.
## The following 2 functions are used by describe.vector
## timeUsed assumes is date/time combination variable and has no NAs
testDateTime <- function(x, what=c('either','both','timeVaries'))
{
  what <- match.arg(what)
  cl <- class(x)
  if(!length(cl))
    return(FALSE)

  dc <- c('Date', 'POSIXt','POSIXct','dates','times','chron')
  
  dtc <- c('POSIXt','POSIXct','chron')
  
  switch(what,
         either = any(cl %in% dc),
         both   = any(cl %in% dtc),
         timeVaries = {
           if('chron' %in% cl || 'Date' %in% cl) { 
             ## chron or S+ timeDate
             y <- as.numeric(x)
             length(unique(round(y - floor(y),13))) > 1
           }
           else length(unique(format(x,'%H%M%S'))) > 1
         })
}
