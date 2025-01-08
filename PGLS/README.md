This folder contains the R script to produce the PGLS analysis that looks at the correlation between genomic diversity values (*H* and *N<sub>e</sub>*) and conservation status or
available present and past areas.

The script also includes the production of the Figure 3 and Figure 4. 

In the folder are also present example files that are necessary for the analysis.

The files are:

Species.txt -> A list with the species name under exam in a single column with no header 

Enm_areas.csv -> This file will be produced in the first step of the PGLS_setup.R script. It contains the suitable area (>0.36) of each period under exams. 

Species_Ne.xlsx -> an Excel file containing the mean *N<sub>e</sub>* estimation and Variance for each species 

Species_traits.xlsx -> an Excel file containing the *H* values, habitat type and conservation status for each species 

Turtle_tree.nwk -> a reduced phylogenetic tree from Thomson et al. (2021) (https://doi.org/10.1073/pnas.2012215118)
