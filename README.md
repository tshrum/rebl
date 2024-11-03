---
title: "REBL Scale"
author: "Dr. Trisha Shrum, Chris Donovan\n"
date: "November 02, 2024"
output: 
    github_document:
        toc: true
        toc_depth: 1
        html_preview: true
    html_document:
        toc: true
        toc_depth: 1
        number_sections: true
---



# Introduction

This is the repository for the Repeated Environmental Behavior Latent (REBL) Scale project. It contains all the code to build the REBL scale from raw data through visualizations. 

Clean .rds files including REBL scores can be found in the `2_clean/clean_dataframes/` folder. Clean .csv files are in the `2_clean/clean_dataframes/csv/` folder. 

# Navigating the Project

After opening `reb_scale.Rproj`, start with `table_of_contents.R`. This will list all the relevant scripts in the order they should be run. Run the `housekeeping.R` script, which will load helper functions that are used throughout the project. Additional required packages are then included in the top of each script. Using `ctrl/cmd + left click` on the text of a file path in the table of contents will open that script in a new tab. 

All the objects and packages required for each script are loaded at the top of the page, and all important outputs are saved at the end of each script. So, you can run the entire project from the table of contents, but you can also just skip to any script and run it from there.

# File Structure

-   `1_raw/` is for raw, untouched, datasets. Metadata is embedded into these files using # as a comment symbol.
-   `2_clean/` is for cleaned (wrangled, recoded, imputed, etc.) data.
-   `3_functions/` is where custom functions are stored. The housekeeping script will load these. 
-   `4_scripts/` is for all R scripts.
-   `5_objects/` is for intermediate .rds objects.
-   `6_outputs/` is where tabular outputs are kept.
-   `7_plots/` is for figures, diagrams, pngs, pdfs, etc.
