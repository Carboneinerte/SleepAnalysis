# SleepAnalysis

Only Sleep_analysis.R is currently working.

Open the file in Rstudio. Select all (Ctrl + A) and Run it.
The required packages will be automatically installed if needed.

Then call the function as follow:
Sleep_select(criteria)

Argument (criteria) can be number ex : 2670) or a string of text (ex: "Green") or a combination of both (ex: c(2670, "green") ).
The arguments will be use to look for a file matching and the script will work on ALL files within the same folder. The data should be sorted by animal and experiment within a single folder.

Root folder for input data needs to be "input". Example of data organization: "Sleep\input\2023-06-19_Green_chronic\2505"
It is better that the final folder contains the mouse ID for correct naming of the output file.

The default argument is "csv" which will process ALL csv files in the input folder

Output file will be saved in a folder called output.



An alternative way to analyze multiple recording sessions is detailed at the end of the Sleep_analysis file. 
