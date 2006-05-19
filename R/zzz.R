.First.lib = function(libname, pkgname) {
  library(lattice)  # needed for calling the panel functions
  library(grid)     # current.viewport is needed by panel.bwstrip
  library(MASS)     # just frequently useful
  a.resetplotparams() # set local style graphics parameters
  options(digits=5) # the default of 7 is exaggerated for our purposes
  options(STERM='iESS', editor='gnuclient.exe')
}
