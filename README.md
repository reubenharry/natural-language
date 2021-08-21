# Example

Generate informative language using a probabilistic context free grammar and a compositional semantics. Below is a series of sentences produced to convey a particular state, from a small grammar. You can see successive updates the the listener's belief state.

```sh

This is the world that the speaker is trying to convey

[(Jane,[("runner","True")]),(Jill,[("runner","True")])]

Press Return to compute the speaker's next utterance

>

These are all the possible worlds, according to the listener:

[(Jane,[("runner","False")])]
[(Jane,[("runner","True")])]
[(John,[("runner","False")])]
[(John,[("runner","True")])]
[(Jill,[("runner","False")])]
[(Jill,[("runner","True")])]
[(Jane,[("runner","False")]),(Jill,[("runner","False")])]
[(Jane,[("runner","False")]),(Jill,[("runner","True")])]
[(Jane,[("runner","True")]),(Jill,[("runner","False")])]
[(Jane,[("runner","True")]),(Jill,[("runner","True")])]      <------------------------ TRUE WORLD
[(John,[("runner","False")]),(Jane,[("runner","False")])]
[(John,[("runner","False")]),(Jane,[("runner","True")])]
[(John,[("runner","False")]),(Jill,[("runner","False")])]
[(John,[("runner","False")]),(Jill,[("runner","True")])]
[(John,[("runner","True")]),(Jane,[("runner","False")])]
[(John,[("runner","True")]),(Jane,[("runner","True")])]
[(John,[("runner","True")]),(Jill,[("runner","False")])]
[(John,[("runner","True")]),(Jill,[("runner","True")])]
[(John,[("runner","False")]),(Jane,[("runner","False")]),(Jill,[("runner","False")])]
[(John,[("runner","False")]),(Jane,[("runner","False")]),(Jill,[("runner","True")])]
[(John,[("runner","False")]),(Jane,[("runner","True")]),(Jill,[("runner","False")])]
[(John,[("runner","False")]),(Jane,[("runner","True")]),(Jill,[("runner","True")])]
[(John,[("runner","True")]),(Jane,[("runner","False")]),(Jill,[("runner","False")])]
[(John,[("runner","True")]),(Jane,[("runner","False")]),(Jill,[("runner","True")])]
[(John,[("runner","True")]),(Jane,[("runner","True")]),(Jill,[("runner","False")])]
[(John,[("runner","True")]),(Jane,[("runner","True")]),(Jill,[("runner","True")])]

And this is the optimal utterance by the speaker:

Jane runs

Press Return to compute the speaker's next utterance

>

These are all the possible worlds, according to the listener:

[(Jane,[("runner","True")])]
[(Jane,[("runner","True")]),(Jill,[("runner","False")])]
[(John,[("runner","False")]),(Jane,[("runner","True")])]
[(Jane,[("runner","True")]),(Jill,[("runner","True")])]      <------------------------ TRUE WORLD
[(John,[("runner","True")]),(Jane,[("runner","True")])]
[(John,[("runner","False")]),(Jane,[("runner","True")]),(Jill,[("runner","False")])]
[(John,[("runner","False")]),(Jane,[("runner","True")]),(Jill,[("runner","True")])]
[(John,[("runner","True")]),(Jane,[("runner","True")]),(Jill,[("runner","False")])]
[(John,[("runner","True")]),(Jane,[("runner","True")]),(Jill,[("runner","True")])]

And this is the optimal utterance by the speaker:

Jill runs

Press Return to compute the speaker's next utterance

>

These are all the possible worlds, according to the listener:

[(Jane,[("runner","True")]),(Jill,[("runner","True")])]      <------------------------ TRUE WORLD
[(John,[("runner","False")]),(Jane,[("runner","True")]),(Jill,[("runner","True")])]
[(John,[("runner","True")]),(Jane,[("runner","True")]),(Jill,[("runner","True")])]

And this is the optimal utterance by the speaker:

everyone runs

Press Return to compute the speaker's next utterance

>

These are all the possible worlds, according to the listener:

[(Jane,[("runner","True")]),(Jill,[("runner","True")])]      <------------------------ TRUE WORLD
[(John,[("runner","True")]),(Jane,[("runner","True")]),(Jill,[("runner","True")])]

The speaker's best course of action is to say nothing

```

And here's a second example:

```sh

This is the world that the speaker is trying to convey

[(John,[("runner","False")]),(Jane,[("runner","True")])]

Press Return to compute the speaker's next utterance

>

These are all the possible worlds, according to the listener:

[(Jane,[("runner","False")])]
[(Jane,[("runner","True")])]
[(John,[("runner","False")])]
[(John,[("runner","True")])]
[(Jill,[("runner","False")])]
[(Jill,[("runner","True")])]
[(Jane,[("runner","False")]),(Jill,[("runner","False")])]
[(Jane,[("runner","False")]),(Jill,[("runner","True")])]
[(Jane,[("runner","True")]),(Jill,[("runner","False")])]
[(Jane,[("runner","True")]),(Jill,[("runner","True")])]
[(John,[("runner","False")]),(Jane,[("runner","False")])]
[(John,[("runner","False")]),(Jane,[("runner","True")])]      <------------------------ TRUE WORLD
[(John,[("runner","False")]),(Jill,[("runner","False")])]
[(John,[("runner","False")]),(Jill,[("runner","True")])]
[(John,[("runner","True")]),(Jane,[("runner","False")])]
[(John,[("runner","True")]),(Jane,[("runner","True")])]
[(John,[("runner","True")]),(Jill,[("runner","False")])]
[(John,[("runner","True")]),(Jill,[("runner","True")])]
[(John,[("runner","False")]),(Jane,[("runner","False")]),(Jill,[("runner","False")])]
[(John,[("runner","False")]),(Jane,[("runner","False")]),(Jill,[("runner","True")])]
[(John,[("runner","False")]),(Jane,[("runner","True")]),(Jill,[("runner","False")])]
[(John,[("runner","False")]),(Jane,[("runner","True")]),(Jill,[("runner","True")])]
[(John,[("runner","True")]),(Jane,[("runner","False")]),(Jill,[("runner","False")])]
[(John,[("runner","True")]),(Jane,[("runner","False")]),(Jill,[("runner","True")])]
[(John,[("runner","True")]),(Jane,[("runner","True")]),(Jill,[("runner","False")])]
[(John,[("runner","True")]),(Jane,[("runner","True")]),(Jill,[("runner","True")])]

And this is the optimal utterance by the speaker:

Jane runs

Press Return to compute the speaker's next utterance

>

These are all the possible worlds, according to the listener:

[(Jane,[("runner","True")])]
[(Jane,[("runner","True")]),(Jill,[("runner","False")])]
[(John,[("runner","False")]),(Jane,[("runner","True")])]      <------------------------ TRUE WORLD
[(Jane,[("runner","True")]),(Jill,[("runner","True")])]
[(John,[("runner","True")]),(Jane,[("runner","True")])]
[(John,[("runner","False")]),(Jane,[("runner","True")]),(Jill,[("runner","False")])]
[(John,[("runner","False")]),(Jane,[("runner","True")]),(Jill,[("runner","True")])]
[(John,[("runner","True")]),(Jane,[("runner","True")]),(Jill,[("runner","False")])]
[(John,[("runner","True")]),(Jane,[("runner","True")]),(Jill,[("runner","True")])]

And this is the optimal utterance by the speaker:

the woman runs

Press Return to compute the speaker's next utterance

>

These are all the possible worlds, according to the listener:

[(Jane,[("runner","True")])]
[(John,[("runner","False")]),(Jane,[("runner","True")])]      <------------------------ TRUE WORLD
[(John,[("runner","True")]),(Jane,[("runner","True")])]

And this is the optimal utterance by the speaker:

the runner runs

Press Return to compute the speaker's next utterance

>

These are all the possible worlds, according to the listener:

[(Jane,[("runner","True")])]
[(John,[("runner","False")]),(Jane,[("runner","True")])]      <------------------------ TRUE WORLD

The speaker's best course of action is to say nothing

```

# How to run

You'll need `stack` installed.

Clone the repo, and run `stack build` then `stack exec natural-language-exe`. This should open a repl.

# What this is

This implements a grammar probabilistically as a generalized unfold (a [futumorphism](https://blog.sumtypeofway.com/posts/recursion-schemes-part-4.html)). That's in Syntax.hs.

Then an interpreter is the dual operation, a generalized fold (a [histomorphism](https://blog.sumtypeofway.com/posts/recursion-schemes-part-4.html)). This is implemented in Semantics.hs, with an example at the top. I add exceptions and scope using monadic effects.


Then the basic Bayesian pragmatic model (aka RSA) can be defined by composing the two. That's in Pragmatics.hs. That is, a naive speaker model generates language using the unfold. A listener model, using the fold, does Bayesian inference to ask: on the assumption that the speaker's utterance, once evaluated, was truthful, what must the world be like. Note that this means parsing isn't involved (or rather: parsing should be part of the inference algorithm, but isn't yet). The informative speaker model then, given a world, chooses the utterance from the grammar which maximizes the listener model's probability mass on that world.

By doing Bayesian pragmatics with an actual compositional semantics, you get some nice things. For example, to communicate w2, defined in Semantics.hs, the informative speaker model will prefer "the woman runs" over "Jane runs", and this is because it forces the listener to accomodate a semantic presupposition that there is only one woman in the domain. 
