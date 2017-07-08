# Rule Generation

This code converts the templates we wrote by hand into the inference rules for the algorithm.
In the process, several properties are automatically enforced:

- All internal points receive a parent
- No cycles are formed

See the paper for proofs that these can be enforced during rule generation.

### Usage instructions

The code has a few options:

- --check, this sanity checks the remplates (e.g. that the position of the external point in the goal is consistent with the input items)
- --codegen, this generates the rules, if the following options are present
- --arcs, generates rules for adding arcs (if 'codegen' is true)
- --combinations, generates rules for combining items (if 'codegen' is true)

For example, this will generate the rules used for our experiments (should match the file in the resources folder of the parser):

```
python3 convert_templates_to_rules.py codegen arcs combinations < templates_used.tex > example
```

There is also a separate file for code that generates feasible parents (which is imported and used by the converter).
When run on its own it gives a list of valid parent combinations:

```
python3 feasible_parents.py
```

### Differences in templates

The templates given here differ slightly from the figure in the paper:

- These are simplified to not have the 'hat' notation (this is simply not tracked), and to restrict the rules for B further, requiring a direct edge between the ends of the span in the left item. These changes do not have a practical impact since we prune away all items of type B anyway.
- The notation is slightly different for parents. Instead of 'd', 'n', and 'p', we use the letter of the position (I, J, K, L, X) with lowercase indicating indirect. This is the same as in Kummerfeld (2016).
