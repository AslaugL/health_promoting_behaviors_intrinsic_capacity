[![DOI](https://zenodo.org/badge/736318316.svg)](https://zenodo.org/doi/10.5281/zenodo.10495184)

# Content
Scripts to perform the analysis and generate tables of the study "Health-promoting behaviors in older adulthood and intrinsic capacity 10 years later: The HUNT study", doi: 10.1186/s12889-024-17840-3

To recreate the analyses, the data used in the study must first be obtained from HUNT Databank, see the HUNT Databank page "[Access to HUNT data and samples for research purposes](https://www.ntnu.edu/hunt/data)" for how to apply for this data.

# How to recreate the analysis
First run the "data_cleaning" script. Change the path of the first "tmp" variable to the name of the file received by HUNT DB. After this the script can be run as is. This script contain data exploration and imputation of missing values, in addition to formatting the data for analysis.

The script "regression_models" contain the regression analyses done.

Finally, in the "table_formatting" script, the data is formatted for the tables found in the article.


