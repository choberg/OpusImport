# OpusImport
Import Bruker OPUS files (".0") in mathematica.

Installation:

In Mathematica click File -> Install...
Select "Package" as Type
Select "From File..." and pick the "OpusImport.m" file.
To install click "OK"

How to use:

You want to load OpusImport by
Get["OpusImport`"]

To load a file use 
ImportOpus[examplefile, type]
with type being e. g. "single", "ref", "absorbance", "interferogram_single", "interferogram_ref"

To retrieve meta information from a file use
"single_meta", "ref_meta", "inteferogram_single_meta", "inteferogram_ref_meta", "absorbance_meta"
