# Format conversion
This program can convert between our representation and the standard penn treebank structure.
Below our representation is refered to as 'shp' for 'split head parse'.
The simplest way to run it is:

PTB -> SHP
```Shell
python reprint_trees.py -i p -o hr -e hj0 -h j1 < input_data.ptb
```

SHP -> PTB
```Shell
python reprint_trees.py -i h -o o -e he < input_data.shp
```

# Other conversions
The program has a range of options for conversions (in each case the character in brackets is what should be used).
There are six options, each with various arguments:

- Input, `-i [ (p)enn treebank | (c)onll | split (h)ead ]`
- Output, `-o [ (s)ingle_line [ with (t)races] | (m)ulti_line [with (t)races] | (o)riginal ptb | (t)ex | (w)ords | (o)ntonotes | (p)os tagged | split (h)ead [with (r)everesed null-null] | (d)ependencies ]`
- Edits, `-e [ remove (t)races, remove (f)unction tags, apply (c)ollins rules, (h)omogenise top, remove trivial (u)naries, right (b)inarise coordination, cc(k)-style binarise coordination, remove (e)xtra coordination nodes, (j)kk-style coordination[0-6], redirect (g)apping ]`
- Head rules, `-h [ (c)ollins | (p)ennconverter | (t)iger | (j)kk[0-5] ]`
- Gold input, `-g <gold filenmae>` (can be used by the tex output)
- Length limit, `-l [ number ]` (will leave out sentences longer than this)

Note - not all of these have been thoroughly tested.
The options I have used least are conll input, and output to tex, ontonotes, and dependencies.

For edits, the seven types of coordination we define (j[0-6]) are:

0. `(A  ,  B  ,  C  and  D)`
1. `(A (, B (, C (and D))))`
2. `(A , (B , (C and D)))`
3. `(A (, B) (, C) (and D))`
4. `((((A ,) B ,) C and) D)`
5. `(((A , B) , C) and D)`
6. `((A ,) (B ,) (C and) D)`

For head rules, the six variants we define (j[0-5]) change the head in coordination:

0. First non-punctuation
1. First conjunction
2. First non-punctuation non-conjunction
3. Last non-punctuation
4. Last conjunction
5. Last non-punctuation non-conjunction
