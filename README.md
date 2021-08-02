# natural-language

This implements a grammar probabilistically as a generalized unfold (a [futumorphism](https://blog.sumtypeofway.com/posts/recursion-schemes-part-4.html)). That's in Syntax.hs.

Then an interpreter is the dual operation, a generalized fold (a [histomorphism](https://blog.sumtypeofway.com/posts/recursion-schemes-part-4.html)). This is implemented in Semantics.hs, with an example at the top. I add exceptions and scope using monadic effects.


Then the basic Rational Speech Acts model can be defined by composing the two. That's in Pragmatics.hs. That is, a naive speaker model generates language using the unfold. A listener model, using the fold, does Bayesian inference to ask: on the assumption that the speaker's utterance, once evaluated, was truthful, what must the world be like. Note that this means parsing isn't involved (or rather: parsing should be part of the inference algorithm, but isn't yet). The informative speaker model then, given a world, chooses the utterance from the grammar which maximizes the listener model's probability mass on that world.