# Hummilang

> Hummingbirds have a variety of calls, chips, chatters and squeals to communicate with each other.

I'm pretty sure they communicate in LISP, so Hummilang is a LISP.

## Goals

1. Implement a Lisp according to "Lisp in small pieces".
2. Run programs of a more mathematically compatible notation.

## Status

Just started...

## Devlog

### Chapter 1

Up and running fast!

![GIF of Ron Burgundy saying "That escalated quickly"](https://media.giphy.com/media/VdnlLVCpJdvARvByh0/giphy.gif)

I like how this book starts off with a very straight forward but workable implementation.
The progression is clear, and it's possible to naively guess at how to implement the parts.
The author points out important considerations as to why structural choices are made.
Clearly I'll need a more axiomatic implementation that relies not on Clojure primitives to achieve self-hosting. It's really cool to see the expressive self-referential power kicking in already, culminating with a function call.

Side rant:
Popular languages shamefully diverge from math notation.
`{}` should be sets.
`()` should be ordered pairs, tuples.
`{<k, v>...}` should be mappings (where the `<>` are drawn less angular).
Ranges `[0,1) [0,1] (0,1] (0,1)` overlap with pairs/tuples and is asymmetrical.
`=>` is a binary operator that should yield TRUE given FALSE (from falsehood, anything follows).
“Higher-order” refers to arity (unary, binary, higher-order), not taking a function as an argument.
I'd like a chance to right some of these egregious wrongs, but will save tilting at that castle until much later.
I bet that's exactly what McCarthy was thinking when he proposed m-expressions.
Supposedly macros are not possible with m-expressions,
I don't really see why.
Don't get me wrong, I like an s-expression as much as the next acolyte, it's more the notational choices that bug me.
I'd prefer to code in STEM notation even if it's harder to write, I claim it would be easier to read (this is quite a stretch, I'm insisting that the benefits of an axiomatic basis for notation is more powerful than linguistic colloquialism).
ZFC lays out the scientific community's common language, but programming language designers mostly say "nah I'll just make up some new syntax, thanks".
