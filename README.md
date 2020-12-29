# algebraic-graphs-io

This package collects I/O utilities for `algebraic-graphs` : parsers and serializers for common graph data interchange formats, as well as functionality for downloading and caching larger datasets.

## Formats

Currently the following formats are supported :

* GML : used by a few common graph software packages (NetworkX, Gephi, graphviz, and others)

* .tsv : tab-separated list of edge data, used e.g. for the Graph Challenge dataset [1]


## Datasets

The package contains some small example datasets (e.g. "lesmiserables" and "karateclub"); these are provided ready for consumption in `Algebra.Graph.IO.Datasets`.

There are also bindings to larger datasets, such as the ones provided by the LINQS group [2] (e.g. "citeseer" and "cora").


## Contributing

PRs and contributions welcome!


## References

[1] GraphChallenge https://graphchallenge.mit.edu/data-sets

[2] LINQS https://linqs.soe.ucsc.edu/data
