# CAVE (Coronavirus Adverse Vaccine Events)
Shiny/R app that generates personalised wordclouds of potential side effects.
This code is provided 'as is' with no warranty or support

##STEPS to get running:
1. Unzip cave-data into the same directory as app.R
2. Edit and run app.R (see below)

##STEPS to get update:
1. Download VAERS data for 2021 (https://vaers.hhs.gov/data/datasets.html)
2. Download and familliarise yourself with the data dictionary (https://vaers.hhs.gov/docs/VAERSDataUseGuide_en_September2021.pdf)
3. Edit and run preprocess.R (see below)
4. Edit and run app.R (see below)


##preprocess.R
------------
This file preprocesses the VAERS 2021 data into four CSV files (three are used by CAVE).

1. Note the directory where you downloaded the 2021 VAERS data (three CSVs)
2. Enter this as a path into lines 69-71 for the three VAERS CSVs 
3. Uncomment and change the path on line 130 where you want to put the 'full' data output (this is not used by CAVE)
4. Uncomment and change the path on line 160 where you want to put the 'died' data output (this is used by CAVE)
5. Uncomment and change the path on line 181 where you want to put the 'subset' data output (this is used by CAVE)
6. Uncomment and change the path on line 200 where you want to put the 'grouped' data output (this is used by CAVE)
7. Run preprocess.R
8. Copy the files in [4], [5] and [6] to the same directory where you pulled CAVE so they are local to app.R

##app.R
-----
This is the main Shiny app

9. (Only if updating) Change the filename on lines 20, 22 and 23 to be the names of the files copied from step [8] above
10. Run the app 
