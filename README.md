# VFT_autoanalyzer_mice
Code that digitizes and analyzes strings of six Xs and Os from the VFT updown test (0.16, 0.4, 0.6, 1.0, 2.0)

Currently the code comes in two forms. The first, updown_code, is fully functional for data with 2-5 groups BUT is not able to graph or perform statistics on data in which the same mice are testesd at multiple timepoints.

The second code, updown_timepoints_code, is able to operate on multiple groups but is not finished -- it is unable to perform statistics and may drop some warnings when the data is graphed, although the graphs should be correct. 

RStudio is reccomended

The functions work on skinny data: one column should be called vft_t and contain the xs and os (doesn't matter between upper and lower case). A second column should be called group and contain a string that denotes the experimental group the mouse belongs to. Nothing else should be required.

To use the code: 
1. Load the packages required
2. Load your data as a csv file (read.csv() or xlsx file).
3. Define your groups using the provided groups vector. Place each group name in quotes
4. Run the rest of the code in the file you are using to load the plots 
5. Call the function makegraphs() on your data 
6. Type "graph" in the console to see your data graphed

To output your data, call write.csv(data_done, file = "(whatever you want to name it)") and a new csv spreadsheet will be written to your working directory. The variable vft_tq contains the quantified threshold data.
