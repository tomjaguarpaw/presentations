* What

    * Opaleye is ESDL for generating SQL to send to a database

    * Idea is to provide similar functionality to object relational
      mappers

    * Take principled approach based on underlying theoretical
      structure of relational queries, not just an adhoc approach
      which seems to work

    * Doesn't just generate SQL string

    * Provides utility functions for connecting to database

    * Anyone can provide their own methods for connecting if they
      prefer

* Why

    * Want to be able to write for our database just like Haskell

    * Main benefits: type safety; composability

    * Type safety

        * well typed programs can't crash at runtime

        * types give refined information about program behaviour

        * latter is a speciality of Haskell

     * Composability

        * can build programs up from smaller parts

        * meaning of the combination is the combination of the
          meanings (semantic composability)

        * statement might seem trivial because we take it for granted

        * not easy to come up with building blocks that are fine
          grained and correspond to meaningful and useful things in
          your model

        * Take principled approach based on underlying theoretical
          structure of relational queries, not just an adhoc approach
          which seems to work

        * don't just want to combine -- want to combine at a fine
          level of granularity

        * really important to be able to name and reuse subexpressions

        * pretty much the essence of abstraction

    * Going to take a risk and claim that Opaleye is the only Haskell
      SQL generator with these properties

    * HaskellDB, Esqueleto, Groundhog all get it wrong for one reason
      or another

    * Downside of this is that Opaleye may be harder to use in the
      same way that Haskell is harder to use than Python

* Where

    * In production at my client for over a year

    * Have queries that are composed of many tens of component parts;
      hundreds of lines in total; compile to hundreds of lines of SQL;
      do exactly what we want

* How

    * Based Opaleye on some theory by David Spivak at MIT

    * implementation managed to reuse some of the HaskellDB internals

    * say thank you to the developers of HaskellDB because they helped
      the development of Opaleye

    * HaskellDB is a good library in many ways albeit with many flaws

    * closed beta program -- get in touch if you want to participate

* Type safety (simple)

    * obvious example of trying to compare columns of different types

    * provide newtype wrapping like Haskell for hiding implementation

    * can't access non-existent columns

* Type safety (further info)

    * Haskell distinguishes pure computations and those in IO

    * Similar concept in Opaleye

    * QueryArr and ExprArr

    * ExprArr does not look up data


