# Macro Scorecard

This Shiny app visualizes macroeconomic data across countries and categories like PMIs, inflation, and policy rates.

There are 4 pages on the macro website.

Colors.R – Centralizes all brand and theme colors.
Data_load.R – Reads in raw data sources like the csv file.
Data_validation.R – runs sanity checks on raw data to test for any missing values or errors. Throws warning if something is off.
Dataprep.R – Takes validated raw data and transforms it into the exact shape needed for plotting.
Functions.R – Houses reusable helper functions that the server or UI call directly
Setup.R – Loads libraries, sets global options and attaches themes or plotting defaults.
Variables.R – Defines static objects, constants, and anything else that doesn’t change at runtime. 
