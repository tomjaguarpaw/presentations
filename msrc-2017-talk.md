* Thanks for the opportunity to speak at short notice

* Name's Tom, engineer

* Previous life, Haskell programmer at a few different companies

* Today talk to you about Opaleye

* Database library I wrote and maintain

* Used by 5-10 companies -- in Haskell world quite a lot!

* Opaleye is Embedded Domain Specific Language for relational queries -- generates SQL

* Based on vererable old library HaskellDB

* And on theoretical work David Spivak MIT

* Unlike any other database library I'm aware of a goal of Opaleye is have denotational semantics

* Not a *formal* one but in principle to have denotational semantics

* Goal of having denotational semantics is part of Opaleye's two main goals: typesafe & composable

* Well-typed queries should not crash -- at SQL run time

* Semantics of queries  should be determined from the smaller queries they're built up from

* In a sense goal of Opaleye is to have some of the same properties that Haskell itself has

* And partially piggy-back on Haskell's type system and semantics to achieve this

* How do we program databases?

* Seems different from other programming languages

* Have SQL that *only* runs on the database

* Although it reads nicely -- for small snippets -- it doesn't interact well with whatever other programming language

* So what is this strange SQL thing doing?

* Basically operations on sets or lists

* LINQ: Many people in this room know much more about that than I do

* Opaleye is "library level LINQ"
