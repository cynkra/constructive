pkgname <- "DBItest"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('DBItest')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("context")
### * context

flush(stderr()); flush(stdout())

### Name: make_context
### Title: Test contexts
### Aliases: make_context set_default_context get_default_context

### ** Examples

## Don't show: 
if (requireNamespace("RSQLite", quietly = TRUE)) withAutoprint({ # examplesIf
## End(Don't show)
make_context(
  new(
    "DBIConnector",
    .drv = RSQLite::SQLite(),
    .conn_args = list(dbname = tempfile("DBItest", fileext = ".sqlite"))
  ),
  tweaks = tweaks(
    constructor_relax_args = TRUE,
    placeholder_pattern = c("?", "$1", "$name", ":name"),
    date_cast = function(x) paste0("'", x, "'"),
    time_cast = function(x) paste0("'", x, "'"),
    timestamp_cast = function(x) paste0("'", x, "'"),
    logical_return = function(x) as.integer(x),
    date_typed = FALSE,
    time_typed = FALSE,
    timestamp_typed = FALSE
  ),
  default_skip = c("roundtrip_date", "roundtrip_timestamp")
)
## Don't show: 
}) # examplesIf
## End(Don't show)



cleanEx()
nameEx("make_placeholder_fun")
### * make_placeholder_fun

flush(stderr()); flush(stdout())

### Name: make_placeholder_fun
### Title: Create a function that creates n placeholders
### Aliases: make_placeholder_fun
### Keywords: internal

### ** Examples

body(DBItest:::make_placeholder_fun("?"))
DBItest:::make_placeholder_fun("?")(2)
DBItest:::make_placeholder_fun("$1")(3)
DBItest:::make_placeholder_fun(":name")(5)



cleanEx()
nameEx("tweaks")
### * tweaks

flush(stderr()); flush(stdout())

### Name: tweaks
### Title: Tweaks for DBI tests
### Aliases: tweaks

### ** Examples

## Not run: 
##D make_context(..., tweaks = tweaks(strict_identifier = TRUE))
## End(Not run)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
