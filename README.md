These scripts were used to run the models. 
The models were run on the High Performance Computer at NYU.

The order is the following:
1.  `27_create_parameter_grid.R`:
 
  - This creates a dataframe that contains all the combinations of parameters that you want to vary for the analysis.

2.  `02_TMLE_HPC.R`:
   
  - This is the script that actually runs the tmle function taking the arguments determined in the previous script.
     
3.  `03_Contrast.R`

  - This code loads the multiple models we ran, runs the contrasts we want and save them in a rds dataset.
