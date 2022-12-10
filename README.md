# A Meal Planner in Idris (Ampii)

A simple application for manging recipes, planning shopping trips, and
tracking nutrient goals. For now it must be used entirely within the
Idris repl.

## Why

I started this app while studying
(Idris)[https://www.idris-lang.org/], as part of my larger goal of
understanding (Dependent Type
Theory)[https://en.wikipedia.org/wiki/Dependent_type]

I am new to Idris and dependent types: contributions are welcome from
anyone who understands this language, or anyone who has an interest in
using a dependently-typed language to solve a practical problem.

I chose Idris because it has an (accessible
book)[https://www.manning.com/books/type-driven-development-with-idris]
that explains how to work with it, that is available in a variety of
formats.

I chose this problem domain for two reasons:
- It's a complex topic, that feels like a good showcase for *type-driven development*.
- Performance shouldn't be a limiting factor, even if Idris ends up being slow.

## Features and Usage

For now, there's no main loop, and everything runs via the Idris REPL.

You'll need to install Idris 1.3, following directions given in the
book above (or via https://www.idris-lang.org/). You will also want to
install Idris support for your preferred text editor.

You will need to install the following dependencies:

| Library          | URL                                      |
|------------------|------------------------------------------|
| Idris containers | https://github.com/jfdm/idris-containers |


## Roadmap

The immediate goal is to get *Ampii* working as a command-line
application which is feature-complete.

