####
# Steps to install packages that are not on CRAN.
####


### Windows ###

All the code was run from Windows. See the next section for Mac. 

1. Install R 

2. Install RStudio

3. Install Rtools

	Please install this as per instructions here: https://cran.r-project.org/bin/windows/Rtools/
	One can install it at this path C:\rtools40, which is the default. 
	
	Once the installation is complete, create a .Renviron file in your Documents folder
	and include the line:
	PATH="${RTOOLS40_HOME}\usr\bin;${PATH}"

	Then, restart R and RStudio if either one is open. 

4. Add current repository as an RProject in RStudio

	Let's assume that you are currently storing the replication folder here:
	- C:/Users/[user]/Desktop

	Open RStudio, go to File, and New Project...
	After a window pops up, click on the Existing directory, and put in the path to the
	replication folder above. 

	This will recognize the entire folder as a project and will adjust the relative 
	paths according to this folder. 
	
5. Set up / change .Rprofile file 

	The .Rprofile file should already exist. At the moment, it only includes
	the link used for downloading new packages. It can be changed to any other country 
	server too.  

	> # Set the default CRAN mirror 
	> options(repos=structure(c(CRAN="options(repos=structure(c(CRAN="http://cran.us.r-project.org")))")))

	The .Rprofile can be customized according to the user's needs and preferences, 
	but for the purposes of this replication exercise, this should be sufficient.

6. Setting up the master file

	The master.r file contains the main settings, and calls the files which
	pull data and then plot the desired plots for each lecture. 
	There already exists a master.r file in this folder, users may want to add to it. 

### Mac ### 


1. Install R 

2. Install RStudio 

3. In the terminal, run the command: 
	xcode-select --install

4 - 6: Same as above




