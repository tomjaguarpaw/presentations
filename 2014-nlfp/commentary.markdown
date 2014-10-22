# Opaleye


* Embedded Domain Specific Language for relational queries compiling
  to SQL
* Used in production by my client for the last six months
* Currently not open source

Before saying more about that I'm going to jump in and show you some code

# Person table

* "Wire" essentially means "Column"
* Query returning rows of tuples
* You can actually use any Haskell type here
* The SQL generated is similar to this

# A bigger query

Let me tell you what it does first ...

* If you're familiar with HaskellDB, Opaleye is somewhat similar
* Two major differences

1. Opaleye doesn't force you to use a particular record implementation
2. HaskellDB uses a monad interface which allows the application
programmer to construct invalid queries.  For type safety Opaleye uses
an arrow interface

With this code as an example I'll describe the overall aims of Opaleye.

## Type safety

1. Can't express invalid queries
2. Can't compare column values that are the same type merely by coincidence

## Composability

The application programmer should be able to break down large queries
into smaller ones.

Related to composability is extensibility: Opaleye provides basic
functionality but I want other to be able to build functionality on
top.

# Type safety

* Obvious form of type safety: can't compare integer to a string
* Another form: shouldn't be able to compare two unrelated primary
keys, even though they may both be implemented as integers underneath

There are more complicated forms of type safety that I didn't have
time to go into related to the choice of Arrow API, as opposed to
Monad API.
