# VFT_autoanalyzer_mice
Code that digitizes and analyzes strings of six Xs and Os from the VFT updown test (0.16, 0.4, 0.6, 1.0, 2.0)

Currently the code comes in two forms. The first, updown_code, is fully functional for data with 2-5 groups BUT is not able to graph or perform statistics on data in which the same mice are testesd at multiple timepoints.

The second code, updown_timepoints_code, is able to operate on multiple groups but is not finished -- it is unable to perform statistics and may drop some warnings when the data is graphed, although the graphs should be correct. 

To use the code: 
1. Load the packages required
2. Load your data as a csv file (read.csv() or xlsx file). 
3. Define your groups using the provided groups vector. Place each group name in quotes
4. Run the rest of the code in the file you are using to load the plots 
5. Call the function makegraphs() on your data 
6. Type "graph" in the console to see your data graphed
