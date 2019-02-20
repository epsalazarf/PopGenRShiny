# ShinyPopGen: Shiny Apps for Population Genomics

*By Pavel Salazar-Fernandez (epsalazarf@gmail.com)*

*Human Population and Evolutionary Genomics Lab - LANGEBIO*

## About
*Documentation: [Shiny by RStudio](http://shiny.rstudio.com/)*

This document explains how to use R Shiny Apps that perform common plotting tasks in population genetics analyses. All apps are available in their respective directories (titled as *Application name [APP]*).

If you find a bug or are otherwise unable to use these apps, feel free to contact me for support.

## Shiny
Shiny is an R package for building interactive web applications using R, without requiring knowledge in HTML, CSS nor JavaScript. It is very useful for visualizing and manipulating data without tampering with code.

### Installation
To install, simply run the following command on an opened R session:

`install.packages("shiny")`

### Learning Shiny
If you are insterested in developing Shiny apps, its webpage have some useful tutorials:

[Welcome to Shiny](http://shiny.rstudio.com/tutorial/lesson1/)

Some example apps with basic and advanced code are also available here:

[Shiny Gallery](http://shiny.rstudio.com/gallery/)

Using Shiny is much better in RStudio, an IDE for R:

[RStudio for Desktop](https://www.rstudio.com/products/rstudio-desktop/)

## Requirements

**All apps require R and its Shiny library.**

Each app requires an specific set of files to be able to run, indicated in its documentation here. Still, a single directory may contain all the different files required, as long as they are derived from the same sample.

Possible directory contents:
- **popinfo file** (*see below for details*), and no other TXT file.
- PCA files: .eval and .evec (only one of each file).
- MDS files: .mds file.
- Admixture files: all .Q files of the same sample, each with different K numbers.

### About the popinfo file
A `popinfo` is a simple text file that contains additional information about the samples, usually provided with the reference panel.

The format is as follows:
* The first row must be the headers of the columns.
* Each following row contains the corresponding information for a sample.
* Each field is separated either by spaces or tabulations.
* The file is saved as a TXT type, and no other .txt files must be present in the directory.

Required columns:
* ID - Unique sample names, used for matching the sample names between the different files.
* POP - Tag for the population of origin, used for grouping together several members from the same population.

Recommended columns:
* POP_SIMPLE - Contains the name of the population. If present, menus will show both tag and name for easier browsing.
* REGION - Geographical region designation that can be used to group many populations together. Useful for coloring and subsetting.

Other columns may be added to provide more information (linguistic family, continents, dataset, etc.), but are optional for this procedure. It is recommended that only those samples that were used in the analyses are contained in this file.

*The more comprehensive a popinfo file is, the more options will become available for the plotting apps.* These options include sorting, labeling, coloring and subsetting. Errors in the popinfo file may hinder or break the apps, so consider checking your popinfo file if an app fails.

### Running an App
To open an app, you have these alternatives:
* Open RStudio, load the `app.R` file and then click the button `Run`. A web browser window must open.
* Copy the entire code of `app.R` as text and paste it in an R console. A web browser window containing the app must open.

If an app is unable to open or breaks during operation, close the app window, use the `STOP` button to end all current processes, and restart it.

## PCA Plotter [PCAPlot]
Creates interactive PCA (principal component analysis) plots, where samples can be subset and colored by category.

Requirements:
- EVAL and EVEC files from the PCA program (e.g. *smartpca*). Only one per directory.
- popinfo file

Pipeline:
1. Reads first .evec and .eval files from a chosen directory.
2. Identifies names, regions and populations from a given popinfo.txt
3. Generates a PCA plot with color-coded regions.

Features:
- Plot types: Can select between points or tags for the plot.
- Select Population: Shows only a particular population.
- Emphasize Population: Highlights points or tags for a chosen population.
- Color by Category: User can choose the criteria for coloring.
- Show/Hide Legend: Displays all values from the selected category.
- (NEW) Interactive Zoom: select an area and double click to zoom in, double click again to zoom out.

Output:
- Plots can be saved by right-clicking the image. Default height: 900px.

## MDS Plotter [MDSPlot]
Creates interactive MDS (multidimensional scaling) plots, where samples can be subset and colored by category.

Requirements:
- MDS file from the plink. Only one per directory.
- popinfo file

Pipeline:
1. Reads first .mds file from a chosen directory.
2. Identifies names, regions and populations from a given popinfo.txt
3. Generates a MDS plot with color-coded regions.

Features:
- Plot types: Can select between points or tags for the plot.
- Select Population: Shows only a particular population.
- Emphasize Population: Highlights points or tags for a chosen population.
- Color by Category: User can choose the criteria for coloring.
- Show/Hide Legend: Displays all values from the selected category.
- Zoom: Expands view to better visualize a population.

Output:
- Plots can be saved by right-clicking the image. Default height: 900px.

## ADMIXTURE Plotter [AdmixPlot]
Creates interactive admixture plots, where samples can be sorted and/or subset, and components can be custom-recolored.

Requirements:
- R library: `shinyjs`
- Q files from the admixture program (all Q files in the directory are read)
- popinfo file

Pipeline:
1. Reads popinfo file from a chosen directory.
2. Reads .Q file of selected K.
3. Generates plot.

Features:
- Changing the 'Ks' parameter changes the plot showed.
- Select two or more populations for display.
- Add borders to individual bars.
- Collapse sample tags in population tags.
- Arrange samples by increasing biggest component.
- Subset by a value from a category.
- Custom re-coloring of components with visual color picker.

Output:
- Plots can be saved by right-clicking the image. Default height: 720px.

## 1000 Genomes Sampler [1KGSample]
Generates a list and a popinfo table for a subset of 1000 Genomes Project samples, useful for plink merging.

Requirements:
- None, all data required is contained in the app directory.

Pipeline:
1. Reads the popinfo file for the 1000 Genomes Project samples.
2. Identifies categories for easier subsetting.
3. Generates a table with random samples.

Features:
- Choose number of samples
- Subset regions/populations
- Preview the table
- Save table as a popinfo file and a list of IDs for the plink command `--keep`

Output:
- Sample ID list and its corresponding popinfo file are both saved within  the `1KGSample/output` 

## Population Sampler [POPSample]
Generates a list and a popinfo table from a given popfile, useful for plink merging.

Requirements:
- The input popinfo file should contain a column header named "POP"

Pipeline:
1. Reads the popinfo file given.
2. Identifies populations for easier subsetting.
3. Generates a table with random samples.

Features:
- Choose number of samples
- Subset regions/populations
- Preview the table
- Save table as a popinfo file and a list of IDs for the plink command `--keep`

Output:
- Sample ID list and its corresponding popinfo file are both saved within  the `POPSample/output` directory. Files are overwritten each time the "Save Tables" button is clicked.