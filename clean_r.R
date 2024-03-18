try(
  dev.off(),  # Clean the plot section
  silent = TRUE) # try() Avoid errors in case there were no plots

cat('\f') # Clean console

# Remove all variables, except the ones listed in a variable called "fixed_data"
if(exists('fixed_data')){
  rm(list = ls()[!ls() %in% c(fixed_data, "fixed_data")])
}else{rm(list = ls())}

try(invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, 
                     character.only=TRUE, unload=TRUE)), silent = TRUE) # Detach all packages
gc() # Free unused memory

rstudioapi::restartSession()
