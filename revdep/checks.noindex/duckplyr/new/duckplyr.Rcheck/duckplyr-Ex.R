pkgname <- "duckplyr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('duckplyr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("as_duckplyr_df")
### * as_duckplyr_df

flush(stderr()); flush(stdout())

### Name: as_duckplyr_df
### Title: Convert to a duckplyr data frame
### Aliases: as_duckplyr_df as_duckplyr_tibble

### ** Examples

tibble(a = 1:3) %>%
  mutate(b = a + 1)

tibble(a = 1:3) %>%
  as_duckplyr_df() %>%
  mutate(b = a + 1)



cleanEx()
nameEx("config")
### * config

flush(stderr()); flush(stdout())

### Name: config
### Title: Configuration options
### Aliases: config

### ** Examples

# options(duckdb.materialize_message = FALSE)
data.frame(a = 3:1) %>%
  as_duckplyr_df() %>%
  inner_join(data.frame(a = 1:4), by = "a")

rlang::with_options(duckdb.materialize_message = FALSE, {
  data.frame(a = 3:1) %>%
    as_duckplyr_df() %>%
    inner_join(data.frame(a = 1:4), by = "a") %>%
    print()
})

# Sys.setenv(DUCKPLYR_OUTPUT_ORDER = TRUE)
data.frame(a = 3:1) %>%
  as_duckplyr_df() %>%
  inner_join(data.frame(a = 1:4), by = "a")

withr::with_envvar(c(DUCKPLYR_OUTPUT_ORDER = "TRUE"), {
  data.frame(a = 3:1) %>%
    as_duckplyr_df() %>%
    inner_join(data.frame(a = 1:4), by = "a")
})

# Sys.setenv(DUCKPLYR_FORCE = TRUE)
add_one <- function(x) {
  x + 1
}

data.frame(a = 3:1) %>%
  as_duckplyr_df() %>%
  mutate(b = add_one(a))

try(withr::with_envvar(c(DUCKPLYR_FORCE = "TRUE"), {
  data.frame(a = 3:1) %>%
    as_duckplyr_df() %>%
    mutate(b = add_one(a))
}))

# Sys.setenv(DUCKPLYR_FALLBACK_INFO = TRUE)
withr::with_envvar(c(DUCKPLYR_FALLBACK_INFO = "TRUE"), {
  data.frame(a = 3:1) %>%
    as_duckplyr_df() %>%
    mutate(b = add_one(a))
})



cleanEx()
nameEx("df_from_file")
### * df_from_file

flush(stderr()); flush(stdout())

### Name: df_from_file
### Title: Read Parquet, CSV, and other files using DuckDB
### Aliases: df_from_file duckplyr_df_from_file df_from_csv
###   duckplyr_df_from_csv df_from_parquet duckplyr_df_from_parquet
###   df_to_parquet

### ** Examples

# Create simple CSV file
path <- tempfile("duckplyr_test_", fileext = ".csv")
write.csv(data.frame(a = 1:3, b = letters[4:6]), path, row.names = FALSE)

# Reading is immediate
df <- df_from_csv(path)

# Materialization only upon access
names(df)
df$a

# Return as tibble, specify column types:
df_from_file(
  path,
  "read_csv",
  options = list(delim = ",", types = list(c("DOUBLE", "VARCHAR"))),
  class = class(tibble())
)

# Read multiple file at once
path2 <- tempfile("duckplyr_test_", fileext = ".csv")
write.csv(data.frame(a = 4:6, b = letters[7:9]), path2, row.names = FALSE)

duckplyr_df_from_csv(file.path(tempdir(), "duckplyr_test_*.csv"))

unlink(c(path, path2))

# Write a Parquet file:
path_parquet <- tempfile(fileext = ".parquet")
df_to_parquet(df, path_parquet)

# With a duckplyr_df, the materialization occurs outside of R:
df %>%
  as_duckplyr_df() %>%
  mutate(b = a + 1) %>%
  df_to_parquet(path_parquet)

duckplyr_df_from_parquet(path_parquet)

unlink(path_parquet)



cleanEx()
nameEx("fallback")
### * fallback

flush(stderr()); flush(stdout())

### Name: fallback
### Title: Fallback to dplyr
### Aliases: fallback fallback_sitrep fallback_review fallback_upload
###   fallback_purge

### ** Examples

fallback_sitrep()



cleanEx()
nameEx("is_duckplyr_df")
### * is_duckplyr_df

flush(stderr()); flush(stdout())

### Name: is_duckplyr_df
### Title: Class predicate for duckplyr data frames
### Aliases: is_duckplyr_df

### ** Examples

tibble(a = 1:3) %>%
  is_duckplyr_df()

tibble(a = 1:3) %>%
  as_duckplyr_df() %>%
  is_duckplyr_df()



cleanEx()
nameEx("methods_overwrite")
### * methods_overwrite

flush(stderr()); flush(stdout())

### Name: methods_overwrite
### Title: Forward all dplyr methods to duckplyr
### Aliases: methods_overwrite methods_restore

### ** Examples

tibble(a = 1:3) %>%
  mutate(b = a + 1)

methods_overwrite()

tibble(a = 1:3) %>%
  mutate(b = a + 1)

methods_restore()

tibble(a = 1:3) %>%
  mutate(b = a + 1)



cleanEx()
nameEx("new_relational")
### * new_relational

flush(stderr()); flush(stdout())

### Name: new_relational
### Title: Relational implementer's interface
### Aliases: new_relational rel_to_df rel_filter rel_project rel_aggregate
###   rel_order rel_join rel_limit rel_distinct rel_set_intersect
###   rel_set_diff rel_set_symdiff rel_union_all rel_explain rel_alias
###   rel_set_alias rel_names

### ** Examples

new_dfrel <- function(x) {
  stopifnot(is.data.frame(x))
  new_relational(list(x), class = "dfrel")
}
mtcars_rel <- new_dfrel(mtcars[1:5, 1:4])

rel_to_df.dfrel <- function(rel, ...) {
  unclass(rel)[[1]]
}
rel_to_df(mtcars_rel)

rel_filter.dfrel <- function(rel, exprs, ...) {
  df <- unclass(rel)[[1]]

  # A real implementation would evaluate the predicates defined
  # by the exprs argument
  new_dfrel(df[seq_len(min(3, nrow(df))), ])
}

rel_filter(
  mtcars_rel,
  list(
    relexpr_function(
      "gt",
      list(relexpr_reference("cyl"), relexpr_constant("6"))
    )
  )
)

rel_project.dfrel <- function(rel, exprs, ...) {
  df <- unclass(rel)[[1]]

  # A real implementation would evaluate the expressions defined
  # by the exprs argument
  new_dfrel(df[seq_len(min(3, ncol(df)))])
}

rel_project(
  mtcars_rel,
  list(relexpr_reference("cyl"), relexpr_reference("disp"))
)

rel_order.dfrel <- function(rel, exprs, ...) {
  df <- unclass(rel)[[1]]

  # A real implementation would evaluate the expressions defined
  # by the exprs argument
  new_dfrel(df[order(df[[1]]), ])
}

rel_order(
  mtcars_rel,
  list(relexpr_reference("mpg"))
)
## Don't show: 
if (requireNamespace("dplyr", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf
## End(Don't show)
rel_join.dfrel <- function(left, right, conds, join, ...) {
  left_df <- unclass(left)[[1]]
  right_df <- unclass(right)[[1]]

  # A real implementation would evaluate the expressions
  # defined by the conds argument,
  # use different join types based on the join argument,
  # and implement the join itself instead of relaying to left_join().
  new_dfrel(dplyr::left_join(left_df, right_df))
}

rel_join(new_dfrel(data.frame(mpg = 21)), mtcars_rel)
## Don't show: 
}) # examplesIf
## End(Don't show)

rel_limit.dfrel <- function(rel, n, ...) {
  df <- unclass(rel)[[1]]

  new_dfrel(df[seq_len(n), ])
}

rel_limit(mtcars_rel, 3)

rel_distinct.dfrel <- function(rel, ...) {
  df <- unclass(rel)[[1]]

  new_dfrel(df[!duplicated(df), ])
}

rel_distinct(new_dfrel(mtcars[1:3, 1:4]))

rel_names.dfrel <- function(rel, ...) {
  df <- unclass(rel)[[1]]

  names(df)
}

rel_names(mtcars_rel)



cleanEx()
nameEx("new_relexpr")
### * new_relexpr

flush(stderr()); flush(stdout())

### Name: new_relexpr
### Title: Relational expressions
### Aliases: new_relexpr relexpr_reference relexpr_constant
###   relexpr_function relexpr_window relexpr_set_alias

### ** Examples

relexpr_set_alias(
  alias = "my_predicate",
  relexpr_function(
    "<",
    list(
      relexpr_reference("my_number"),
      relexpr_constant(42)
    )
  )
)



cleanEx()
nameEx("stats_show")
### * stats_show

flush(stderr()); flush(stdout())

### Name: stats_show
### Title: Show stats
### Aliases: stats_show

### ** Examples

stats_show()

tibble(a = 1:3) %>%
  as_duckplyr_df() %>%
  mutate(b = a + 1)

stats_show()



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
