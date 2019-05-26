# crafting-interpreters

I translated part of [crafting interpreter][ci]'s tree walking interpreter
into OCaml to become more familiar with the language, to help with reading
[TaPL].

I stopped at the resolver since the AST matching is getting a little notation.
burdensome, and it's effectively the same operation as converting to [De
Bruijn indices][db]. I'm also not especially interested in OO, so I didn't
take the time for those chapter.

[db]: https://en.wikipedia.org/wiki/De_Bruijn_index
[ci]: http://craftinginterpreters.com
[TaPL]: https://www.cis.upenn.edu/~bcpierce/tapl/