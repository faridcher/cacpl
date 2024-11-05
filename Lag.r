# Hmsic:Lag(1:5) # same effect but with NA. we want to avoid the NAs
# Lag(1:5,-1)
# Lag(1:5,1)
Lag <- function(x, shift=1)
  if(shift==0) x else c(tail(x,-shift), head(x,shift))
