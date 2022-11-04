# constructive 0.0.1

* {constructive} produces code that can be used to recreate R objects. In a sense it
is similar to `base::dput()` or `base::deparse()` but {constructive} strives to use "natural" constructors
(`factor` for factors, `as.Date()` for dates, `data.frame()` for data frames etc),
in order to get output readable by humans.
