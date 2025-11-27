# constructive.core

Core infrastructure for the constructive package ecosystem.

## Overview

This package contains the core functionality for generating R code that recreates R objects:

- The default `.cstr_construct()` method
- Infrastructure functions for code generation (`.cstr_apply()`, `.cstr_wrap()`, etc.)
- Base type constructors (character, integer, double, logical, complex, raw, list, environment, function, NULL, etc.)
- Helper utilities for deparse, formatting, and attribute repair
- Main user-facing functions (`construct()`, `construct_multi()`, `construct_diff()`, etc.)

## Purpose

The `constructive.core` package is designed to be a low-dependency foundation that other packages in the constructive ecosystem can build upon. It provides the essential machinery for code generation without including class-specific methods for packages like ggplot2, data.table, tibble, etc.

## Installation

This package is part of the constructive monorepo and is typically installed automatically as a dependency of the main `constructive` package.

## License

MIT
