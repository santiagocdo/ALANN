# Associative Learning with Artificial Neural Network (ALANN)
19/11/2023



# Summary
This is an ongoing project led by Prof. Andrew Delamater in collaboration with Dr. Santiago Castiello de Obeso. The aim of this project is to understand the "microstructure of cognition" 
implement computational models to understand the principles of learning and behaviour.



# Instructions
This project is programmed in R. Thus, the user will need to install the latest versions of R programming language and R Studio. 

## 1. Open ALANN.Rproj R project ##

## 2. Open main.R within the project ##

## 3. Prepare the training.csv file. The columns are: ##
  
  phase = discrete numbers
  
  matType = 3 types: INPUT, OUTPUT, and TEST (optional)
  
  trialType = trial type names as strings 	
  
  in.$ = many columns as you want inputs, starting with in. then input name.  
  
  out.$ = many columns as you want outputs, starting with out then output name.  
  
  par.$ = parameters (par)
  
    par.nH.$ = number of hidden units (nH)
      par.nH.nHV = number of hidden visual (nHV)
      par.nH.nHMM = number of multiSmodal units (nHMM)
      par.nH.nHA = number of hidden auditory (nHA)	
    par.nI.$ = number of input units (nI)
      par.nI.ctx = number of context units (fully connected)
      par.nI.vis = number of visual units (connected with multimodal and visual hidden units)
      par.nI.aud = number of auditory units (connected with multimodal and auditory hidden units)
      
## 4. Adjust the main.R script: ##
  
  4.1. Decide which model you want to run in line 53. 
    
    mod1: Back-Propagation (BP); Delamater (2012) - Learning & Behaviour
    mod2: BP dynamic alpha; Delamater & Castiello (2022) - gregynog Associative Learning Symposoum
    mod3: BP; Xie & Seung (2003) - Neural Computation
    mod4: Contrast Hebbian Learning (CHL); Xie & Seung (2003) - Neural Computation
    mod5: CHL with random feedback; Detorakis, et al. (2019) - Neural Networks
  
  4.2. Adjust parameters depending on the model (modType) in lines 64 to 90.
  
## 5. Visualize Figures from lines 138 to 148 ##



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



# Contact:

Andrew Delamater (AndrewD@brooklyn.cuny.edu)

Santiago Castiello de Obeso (santiagocdo@gmail.com)



# References:

Delamater, A. R. (2012). 
On the nature of CS and US representations in Pavlovian learning. 
Learning & Behavior, 40, 1-23.

Castiello, S., Zhang, W., & Delamater, A. R. (2021). 
The retrosplenial cortex as a possible 'sensory integration' area: A neural network modeling approach of the differential outcomes effect in negative patterning. 
Neurobiology of learning and memory, 185, 107527.