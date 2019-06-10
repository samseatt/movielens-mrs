Readme file for the MovieLens project.

About this Project

Project GitHub Repository: https://github.com/samseatt/movielens-mrs
To remain consistent with the project-inception artifacts, I have used R version 3.5.2 (2018-12-20) for devel- oping and running my scripts.

This Readme file describes how to set up this project (if you are interested beyond the artifact attached with this submission). Basically you can download my RStudio project file in the GitHub repository, or you may simply Run model.R file or Knit movielens-project-report.Rmd file from any RStudio project, provided you have some basic R libraries installed.

An errata file may also be posted in that repository in case I discover any errors post-submission.
In the interest of execution time of these scripts, I have also saved the curated training and validation sets online in an AWS database as RData objects (edx.rda and validation.rda):
• https://adaprise-01.s3.amazonaws.com/edx/edx.rda 1
• https://adaprise-01.s3.amazonaws.com/edx/validation.rda

The scripts then load the objects listed above into the local environment.
You will need to make appropriate edits in the code depending on the option you want to run and where you want to get the dataset from.

Before committing to run these artifacts, be aware that the data download (over the internet, in this case) and several operations on that full dataset (including some transformations, plots and model training) take significant time. The entire end-to-end script (either when you run model.R in its entirety, or Knit the full movielens-mrs.Rmd) takes about 10 minutes to run on my Macbook Pro laptop.
