## micro.cluster.fk

This project is designed to assist categorization of fluorescence microscopy images based on configuration of
fluorescent signals. This was designed specifically to track chromosomal loci marked by fluorescent-tagged arrays
after induction of a DNA break to drive recombination of the chromosomes and rearrangement of these markers in
diploid yeast cells. Fluorescence data is captured in live cells and separate fluorescence channels are saved as csv files containing three dimensional location and intensity data for each field of images captured.

Image files are processed using Volocity version 5.5.1 on MacOS 10.10.5 "Yosemite"

The raw CSV files are produced with legacy Macintosh computer file separators. These need to be converted for
use in the R programming language. This is currently done with a script in the convert-files.Rmd file.

CSV files are then clustered by position to group signals into cell-specific bins for comparisons of colocalization
and presence of individual signals.