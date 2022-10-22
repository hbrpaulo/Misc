try(
  dev.off(),  # Clean the plot section
  silent = TRUE) # try() Avoid errors in case there were no plots

cat('\f') # Clean console

# Remove all variable, except the ones listed in a variable called "fixed_data"
rm(list = ls()[!ls() %in% c(fixed_data, "fixed_data")])

gc() # Free unused memory

rstudioapi::restartSession() # Detach all packages
