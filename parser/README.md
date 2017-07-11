This folder contains the first-order parser and a neural network tagger.

# Simple usage

The simplest way to run this code is to use the jar at (TODO) amd run 

TODO:
- Usage instructions
- Description of what is here
- Example run
- Example output

# Design

### Beams

In each cell I have a beam, but that beam is actually composed of entirely
independent sub-beams, identifiable by a key (a Long). How should the key be
defined? Three options are:

1. Based on parents, item type, etc
2. Based on the spines
3. A hybrid

There is tension here between:

- Efficiency A, have as much on a single subbeam as possible, so states are pruned as soon as possible based on score.
- Efficiency B, have subbeams defined so that we can easily determine compatible pairs for binary composition (eg. put the spine for one side in, so we can filter those items based on the item they must combine with)
- Accuracy, maintain diversity in the overall beam by pushing things into more subbeams where they can live on until we know which is better.

Before having 1ec parsing I had a mixture of these. In the main beam I had a
version of (1), just whether it was created by the binary, init, or arc step. I
also had a second set of beams to be used only during the binary step, to get
efficiency B.

Assigning spines at the very bottom seems difficult, there just isn't much
information. We gain information as time goes on, because the spines constrain
the arcs, and the arcs have scores associated with them. I'm now switching to
having one beam per cell (rather than three, two of which were for binaries),
and defining subbeams based on the three spines.

### Scoring Spines

The spine scores are calculated at two points, during init and during Xterval
creation. They can be handled in several ways:

1 - Only include one side at the bottom. Not good, as different items get the
same scores and are pruned randomly by the beam.
2 - Include both sides at the bottom, then during combining, remove one of the
scores. Now we get more separation earlier on, but the combinations are not
monotonic, making cube pruning inexact.
3 - Include both sides, but with scores divided by two. Now we get some
separation, and cube pruning is exact.

Before 1ec I did (3), but that doesn't work for 1ec parsing as extra spines are introduced by Xtervals.
Instead I went with (2).
