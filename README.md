# SleepAnalysis

Only Sleep_analysis.R is currently working.

Open the file in Rstudio.
'Source' the file.

  `source('Sleep_analysis.R')`

The required packages will be automatically installed if needed.

Then call the function as follows:

  `Sleep_select(criteria)`

Argument (criteria) can be a number (ex: 2670), a string of text (ex: "Green"), or a combination of both (ex: c(2670, "green") ).
The arguments will be used to look for a file matching and the script will work on ALL files within the same folder.
The data should be sorted by experiment and by animal within a single folder.
*Only one recording session per folder*. Example: "Documents\R\Sleep\input\2023-09-09_TRF_baseline\2670"

For example, to analyze only the recording from mouse 2670 during the TRF paradigm:

`Sleep_select(c("TRF", "2670"))`

The root folder for input data needs to be "input". Example of data organization: "Sleep\input\2023-06-19_Green_chronic\2505"
The final folder should contain the mouse ID for the correct output file naming.

The default argument is "csv" which will process ALL .csv files in the input folder. It can take a while.

The output files will be saved in a folder called output.

* Multi-notebook.rmd is a notebook to compare the output from different animal/experiment.

* Script for temperature analysis is still under development.

* Multi_comparison.R is an obsolete version of multi-notebook.rmd
