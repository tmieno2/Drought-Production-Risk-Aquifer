# Aquifer depletion exacerbates agricultural drought losses in the US High Plains

## Instruction for Reproducing the Results and Figures

Here is the breakdown of the steps of completing the project.

1. Download and process datasets
2. Combine the processed datasets
3. Conduct analysis and create figures (no tables in this article)
4. Produce the manuscript

We refer to these numbers in the rest of the instruction.

## Step 1: Download codes and minimum datasets

Download the computer programs from this repository. You can either clone or download the files as a zipped folder. You will be using the parent folder as the root folder. Then, create a folder named "Results" in the root folder. If done correctly, the folder system looks like this:

![folder-structure](https://github.com/tmieno2/Drought-Production-Risk-Aquifer/blob/master/Misc/folder-structure.png)

In the "Data" folder, it has metadata file and a folder named "data-processed," which contains data files to complete steps 2 through 4.

As part of the requirement for publication in Nature Water, the same set of data files are also available at the Figshare (DOI: 10.6084/m9.figshare.24492196).

## Step 2: Download additional datasets (optional)

The data files that comes with the GitHub repository only allow you to go through steps 2 through 4 (Note, you can still reproduce all the results and figures reported in the article). The raw data files and some of the processed data files were not shared on the GitHub repository due to its size limitation. GitHub repositories are not intended for data storage, rather they are mainly for computer programs. If you are interested in reproducing the entire steps including step 1 (downloading and processing datasets to generate the final analysis data), then you need to download additional data files.

A complete set of data files used and generated in the project including raw data files is available at this [Dropbox folder](https://www.dropbox.com/scl/fo/bghhwlidmi7wx1ok0az5n/h?rlkey=tgbix1hp7g9np9etlo1z3biyr&dl=0). Importantly, this folder maintains the folder organization and file locations as employed during our project's execution. 

Please note that the **gridMET** folder inside the **data-raw** folder, containing daily weather data files saved by variable-year, is particularly large, weighing in at roughly 37GB. While our codes include a segment to download this data, if you'd prefer bypassing this potentially time-consuming step, downloading the folder directly is an option.

All the downloaded data files should be stored in Data folder (you can replace the data files that came with cloning the GitHub folder as they are identical). 

## Step 3: Install all the R packages

The root folder is set up as an RStudio project. 

+ Open the project on RStudio
+ Run `renv::restore()` (install the `renv` packages first, if you have not done so). This will install all the R packages used for this project. Note that the packages installed here are specific to this project. See [here](https://rstudio.github.io/renv/articles/renv.html) for an introduction of how to use the `renv` package.

Codes to load the packages are in the **.Rprofile** file. This file is run automatically when you open the project. Since you did not have all the R packages for this project, they were not loaded when you first open the project to set things up as outlined above. So, go to Session -> Restart R to restart an R session, which will run **.Rprofile** again and now you have all the packages loaded. Next time you start the project, you do not have to do anything. The codes will be ready to be run.

## Step 4: Run codes

Open **master.rmd** inside the **Codes** folder.

+ **Reproduce every step of the project**: Run the codes from the top (This takes a while and uses up lots of storage memory). 
+ **Reproduce from step 2 on**: Run the codes starting from line 47.
+ **Reproduce from step 3 on**: Run the codes starting from line 53.

Please note that our team utilizes Rmarkdown (.rmd) as the main file format for coding. The **master.rmd** initially converts the Rmarkdown files into R files using `knitr::purl()`, and subsequently `source()` them for reproduction. If you wish to work on a particular process, refer to the corresponding Rmarkdown file rather than the associated R file. This is because the latter is not as well-organized since `knitr::purl()` strips all the section headers and non-R texts.

