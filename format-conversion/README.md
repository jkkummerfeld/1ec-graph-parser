# Format conversion
This program can convert between our representation and the standard penn treebank structure.
Below our representation is refered to as 'shp' for 'split head parse'.
The simplest way to run it is:

PTB -> SHP, `python reprint_trees.py -i p -o hr -e hj0 -h j1 < input_data.ptb`

SHP -> PTB, `python reprint_trees.py -i h -o o -e he < input_data.shp`

# Other conversion
The program has a range of options for conversions (in each case the character in brackets is what should be used).
There are six options, each with various arguments:

- Input, `-i [(p)enn treebank | (c)onll | split (h)ead ]`
- Output, `-o (s)ingle_line [ with (t)races] | (m)ulti_line [with (t)races] | (o)riginal ptb | (t)ex | (w)ords | (o)ntonotes | (p)os tagged | split (h)ead [with (r)everesed null-null] | (d)ependencies ]`
- Edits, `-e [ remove (t)races, remove (f)unction tags, apply (c)ollins rules, (h)omogenise top, remove trivial (u)naries, right (b)inarise coordination, cc(k)-style binarise coordination, remove (e)xtra coordination nodes, (j)kk-style coordination[012345], redirect (g)apping ]`
- Head rules, `-h [ (c)ollins | (p)ennconverter | (t)iger | (j)kk[0123456] ]`
- Gold input, `-g <gold filenmae>` (can be used by the tex output)
- Length limit, `-l [ number ]` (will leave out sentences longer than this)

