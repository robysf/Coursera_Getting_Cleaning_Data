# Getting and Cleaning Data course assignment, markdown file

## Preamble

Thanks for reading! I've commented on my own code directly in the run_analysis.R file as well, but I will also provide this Read Me markdown file. I'm fairly new at coding, so some of my code is more inelegant and brute-force than I would like, but I will walk you through my steps. I'm also fairly busy so I kept things as simple as possible. Hope it makes sense. 


## Reading in data files

The first section of code *(lines 13-20 in run_analysis.R)* simply uses read.table() to read the relevant data files into R.

**NOTE: You must run this code from a working directory that includes the unzipped data file provided by the course, otherwise unmodified. It should contain files called README.txt, features.txt, features-info.txt, and activity_labels.txt, as well as subdirectories called "test" and "train". 


## Renaming variables

I then chose to rename variables next (this is part of **task 4**) to make column selection easier.

#### Subjects and activities

I named the single column in the subject and activity files either "subject" or "activity", respectively, to keep things clear later after combining datasets and data labels *(lines 23-26 in run_analysis.R)*.

#### Measurements

I simply did this by reassigning the column names of the test and train datasets to the values specified in the features.txt file provided *(lines 27-29 in run_analysis.R)*. I did not attempt to rename these activities to a more readable form than the authors provided, as in my opinion this was a bit beyond the scope of what I can afford to put into this class. I did, however, reformat the names to make them non-redundant and "legal" as R variable names by removing "()" *(line 27 in run_analysis.R)*.


## Combine datasets

For **task 1**, I first added a column name for subject and activity prior to combining data, so that the order of assignment would not be lost. I then combined datasets simply using rbind() in the base R package, adding one dataset immediately after the other.

#### Add subject and activity variables

First, I used cbind() to add subject and activity variables (activities still coded as numbers at this point) to the test and train datasets *(lines 32-33 in run_analysis.R)*

#### Combine test and train datasets

Once the data points were linked to their respective subject and activity, I then used rbind() to simply stick one dataset on the bottom of the other *(line 36 in run_analysis.R)*. All column orders should have been preserved, as the only data transformations were done identically to both datasets. As noted above, rather than chaining, I'm saving intermediate files at each step for clarity, with names that I hope are somewhat intuitive for each intermediate step. 


## Trim dataset to include only measurements of mean and standard deviation

For **task 2**, I am using the select() function from the dplyr package to keep only rows that contain the desired measurements (means and std devs), as well as the subject and activity number variables *(lines 39-46 in run_analysis.R)*


## Reassign activity numbers with descriptive activity names

For **task 3**, I used the first of two brute-force, non-scalable methods in this script - here, to reassign variables from numbers to descriptive names. I did this by subsetting the new pared dataset on activity number, then adding a new column to that subset with the relevant activity name, obtained from the "activity labels" text file *(lines 50-55 in run_analysis.R)*. Since there are only 6 described activities, this seemed reasonable. For simplicity's sake, I then removed the activity number variable and kept only the activity name, which again for clarity I moved to the "front" (left) of the variable columns *(lines 58-62 in run_analysis.R)*


## Group data by subject and activity, and calculate means

This is the most brute-force, non-scalable step I took. I first used the dplyr package's group-by() function to reorder the data by subject (first) and activity (second) *(line 69 in run_analysis.R)*. I then used the summarize() function of dplyr to take the mean of each variable by group *(lines 71-151 in run_analysis.R)*. Unfortunately, it seems that one cannot apply summarize() to a range of variables/columns - it has to be applied one at a time. It seems from my reading that the ddply package might have a way to apply a similar function to a range of columns, but I did not have the time to explore this so I ended up just rewriting the summarize() function for each of the ~78 variables that contained "mean" or "std" in their name, all in one executable line of code (though stretched across 80 lines of text!). My apologies. I look forward to learning from others' more elegant code on a better way to do this. 


## Write out the resulting data to a text file

I used write.table() to save this final dataset to a file called "tidydata.txt" *(line 154 in run_analysis.R)*. 
