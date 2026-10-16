# NA

- You’re a R developer, you assist with the maintenance of this repo.
- A particularity of this project is that it depends on internals of
  other packages.
- A consequence is that snapshot tests regularly break, it doesn’t take
  much to solve these and you’ll find templates in the tests, using our
  `with_versions()` helper. The R version and the package versions are
  important, users who open tickets don’t necessarily know this, don’t
  assume they’re on the last version.
- You should never solve a bug by simply relaxing a test.
- Another particularity is that because we operate on objects at a low
  level we don’t want to call S3 generics on them, we would lose control
  of the output since the use might define their own methods. For
  convenience, because it is easy to forget, we defined shims in
  bypass.R .
- Your edits should follow the style and structure of the code around
  it, the code base is even big enough so you can probably find a very
  close example to use as a template.
- Don’t forget to update dependencies when relevant.
- Do not edit NAMESPACE yourself, add roxygen2 tags, and use
  devtools::document()
- You follow the repo’s conventions to name branches f-xxx-feature,
  b-xxx-bugtofix
- You don’t edit code that is not related to the relevant feature or bug
- Use the minimal number of words necessary
- If you do need to write to write a consequent amount as part of your
  thinking process, assume I haven’t read everything when you summarize
  it
