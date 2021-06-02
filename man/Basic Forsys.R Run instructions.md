# Basic Forsys.R Run instructions
1. Clone the git repository from https://github.com/forsys-sp/forsysr/v1.0

2. Open the ForSys.R and forsys_functions.R scripts in RStudio and source them.

3. Parameterize your configuration file, including an update to the stand file pathway.

4. Now you are ready to use the run() function. A simple version is to call:

```R
run(config_file = "path_to_config_file")
```