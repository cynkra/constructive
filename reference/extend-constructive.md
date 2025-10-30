# Extend constructive

We export a collection of functions that can be used to design custom
methods for
[.cstr_construct](https://cynkra.github.io/constructive/reference/dot-cstr_construct.md)()
or custom constructors for a given method.

- [.cstr_new_class](https://cynkra.github.io/constructive/reference/templates.md)()
  : Open template to support a new class

- [.cstr_new_constructor](https://cynkra.github.io/constructive/reference/templates.md)()
  : Open template to implement a new constructor

- [.cstr_construct](https://cynkra.github.io/constructive/reference/dot-cstr_construct.md)()
  : Low level generic for object construction code generation

- [.cstr_repair_attributes](https://cynkra.github.io/constructive/reference/dot-cstr_repair_attributes.md)()\`
  : Helper to repair attributes of objects

- [.cstr_options](https://cynkra.github.io/constructive/reference/dot-cstr_options.md)()
  : Define and check options to pass to custom constructors

- [.cstr_apply](https://cynkra.github.io/constructive/reference/dot-cstr_apply.md)()
  : Build recursively the arguments passed to your constructor

- [.cstr_wrap](https://cynkra.github.io/constructive/reference/dot-cstr_wrap.md)()
  : Wrap argument code in function code (rarely needed)

- [.cstr_pipe](https://cynkra.github.io/constructive/reference/dot-cstr_pipe.md)()
  : Pipe a call to another (rarely needed)

- [.cstr_combine_errors](https://cynkra.github.io/constructive/reference/dot-cstr_combine_errors.md)()
  : helper function report several errors at once when relevant
