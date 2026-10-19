# units (through udunits2 and expat), sf (through GDAL) and xml2 (through
# libxml2) leak memory in system libraries, R-hub's valgrind check counts these
# leaks as errors so we skip the tests that load these packages under valgrind
skip_on_valgrind <- function() {
  # valgrind preloads its own libraries in the processes it runs
  skip_if(grepl("vgpreload", Sys.getenv("LD_PRELOAD")), "running under valgrind")
}
