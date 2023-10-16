# Associative Learning with Artificial Neural Network (ALANN)
16/10/2023

# Summary
This is an ongoing project led by Prof. Andrew Delamater in collaboration with Dr. Santiago Castiello de Obeso. The aim of this project is to implement computational models to understand the principles of learning and behaviour.

# Instructions
This project is programmed in R. Thus, the user will need to install the latest versions of R programming language and R Studio. 
1. Open ALANN.Rproj R project
2. Open main.R within the project
3. Prepare the training.csv file (multiple phases, parameters, hidden units, and different inputs and outputs)
4. Run script adjusting lines 29 to 35
5. See Figures in lines 65 to  74

# Content
This project is programmed in R. Thus, the user will need to install the latest versions of R programming language and R Studio. In this folder you will:

-ALANN.Rproj (R project): run the open this project to automatically open the R work directory 
-main.R (R script): this script uses functions to run neural network models' simulations
-functions.R (R script): here you will find all the functions needed in main.R
	See the function's description commented within functions.R
-figures (folder): the weights plots (heatmaps) will be stored here
-outputs (folder): activations files after a simulation will be stored here
-weights (folder): weights files after a simulation will be stored here
-*.csv (csv file): this are input files for a simulation
  -phase: phases (integer numbers)
  -matType: three values (INPUT, OUTPUT, and TEST; the last is optional)
  -trialType: character, to identify the trials for training (INPUT and OUTPUT) and TEST
  -in.*: input units, where each column should be called different starting with "in."
  -out.*: output units, where each column should be called different starting with "out."
  -par.*: parameters 
    -par.nBlock: each row of this column correspond to the number of blocks for each phase
    -par.nH.*: number of hidden units, visual, multimodal, and auditory (nHV, nHMM, nHA)
    -par.nI.*: number of input units, context, visual, and auditory (ctx, vis, aud)

# Contact
Andrew Delamater (AndrewD@brooklyn.cuny.edu)
Santiago Castiello de Obeso (santiagocdo@gmail.com)

# Relevant References
Delamater, A. R. (2012). 
On the nature of CS and US representations in Pavlovian learning. 
Learning & Behavior, 40, 1-23.

Castiello, S., Zhang, W., & Delamater, A. R. (2021). 
The retrosplenial cortex as a possible “sensory integration” area: A neural network modeling approach of the differential outcomes effect in negative patterning. 
Neurobiology of learning and memory, 185, 107527.