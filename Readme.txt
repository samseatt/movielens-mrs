Readme file for the MovieLens project.

title: "MovieLens RMSE - Final Report"
author: "Sam Seatt"
date: "6/09/2019"

Project GitHub Repository: https://github.com/samseatt/movielens-mrs

R code for machine learning modeling:
  code/model.R
  
R code to do data partitioning:
  code/data_partitionl.R
  
Project RMD report markup file location:
  moivelens-mrs.Rmd
  
Generated PDF report:
  moivelens-mrs.pdf
  
Instructions:
  Open the moivelens-mrs-heart.Rproj project in RStudio.
  Knit moivelens-mrs.Rmd to generate HTML or PDF project report. Ignore Ghostscript errors when knitting a PDF report.
  Run code/model.R to execute the models outside of the RMD file.About this Project

To remain consistent with the project-inception artifacts, I have used R version 3.5.2 (2018-12-20) for developing and running my scripts.

An errata file may also be posted in that repository in case I discover any errors post-submission.

In the interest of execution time of these scripts, I have also saved the curated training and validation sets online in an AWS database as RData objects (edx.rda and validation.rda):

• https://adaprise-01.s3.amazonaws.com/edx/edx.rda 1
• https://adaprise-01.s3.amazonaws.com/edx/validation.rda

The scripts then load the objects listed above into the local environment.
You will need to make appropriate edits in the code depending on the option you want to run and where you want to get the dataset from.

Before committing to run these artifacts, be aware that the data download (over the internet, in this case) and several operations on that full dataset (including some transformations, plots and model training) take significant time. The entire end-to-end script (either when you run model.R in its entirety, or Knit the full movielens-mrs.Rmd) takes about 10 minutes to run on my Macbook Pro laptop.
