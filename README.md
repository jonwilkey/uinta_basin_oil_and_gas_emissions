# README #

The purpose of this repository is to:

* Track changes in scripts (primarily for myself).
* Provide a method for sharing the latest version of model code with the group (for everyone).

This repository is not:

* A replacement for the wiki, which will still be the primary source for posting project updates.

### How do I get set up? ###

There are a couple of things to do to get setup to run the scripts that I've written:

1. Download and install R (http://www.r-project.org/)
2. (Recommended) Download and install RStudio (a really good IDE for R) (http://www.rstudio.com/products/rstudio/download/)
3. Download and install Git for version control (http://git-scm.com/downloads) - for sharing scripts
3. You'll also need setup an account on bitbucket.org (I think there is a link and instructions in the version control invite message I sent out on June 3, 2014). Of course you probably aren't reading this guide here unless you've already worked that requirement out...
4. Download this shared Dropbox folder (https://www.dropbox.com/sh/4noib6mxd3eta2a/AABTc687OHQewbXAgEmqqBVDa) - for sharing data files
5. Launch RStudio
6. From the menu, select **File > New Project**
7. In the dialogue menu that pops up, select **Version Control**
8. In the next menu select **Git**
9. Paste the repository URL from bitbucket.org into the **Repository URL** box. You can connect using either SSH or HTTPS.
10. For HTTPS, use this link (https://wilkey@bitbucket.org/wilkey/ub_oilandgas.git). You'll be prompted for your username and password when you connect to the server.
11. For SSH, use this link (git@bitbucket.org:wilkey/ub_oilandgas.git). It's more complicated to setup initially, but it's a one time deal and you won't need to sign in again as long as your signed into your account on the same computer. bitbucket.org has a detailed set of instructions on SSH setup here (https://confluence.atlassian.com/display/BITBUCKET/Set+up+SSH+for+Git).
12. A folder will be created containing all of the scripts saved on the server.
13. The name of that folder will be whatever you enter in the **Project directory name** dialogue box. By default I believe it's the same name as the bitbucket.org project name (ub_oilandgas).
14. The folder will be located as a subdirectory of whatever folder you point to in the **Create project as subdirectory of** dialogue box. By default it's a subdirectory of ~/R, which is the R folder generated in your home directory when you first run R (or install any packages in R).
15. After entering all the information, click the **Create Project** button.
16. Open the script **conventional_v10.R** , located in the directory you created in Step 14. This is the main script for the conventional oil and gas Monte Carlo model.
17. Under the section of the scripts labeled **# Paths** starting on approx. line 50, change the paths for each for each of the directories listed to the location on your computer where you downloaded the Dropbox shared folder contents (I repeat this code segment in every script).
18. Make sure that you have the libraries necessary to run the script in question. There should be a list of the libraries used starting on approx. line 88 under **# Libraries** (also repeated in every section, not always the same libraries are used, depends on the script). Right now in conventional_v10.R they are sqldf, zoo, and data.table. you can install each using the following command in the R console (window in lower left of RStudio):

install.packages("package_name")

Alternatively, you can use the menu option Tools > Install Packages... and then enter the names of the packages you wish to install.

You can now finally run the script. Do this using the menu option Code > Source or any of its keyboard shortcuts and variations. The script editor (top left window) also contains a drop-down source menu button in the upper-right corner. By default, source runs without printing to the console, however you can also run source with echo and it will print every line in the script as it executes them.

### Questions? ###

* Documentation for Git [here](http://git-scm.com/doc)
* Documentation for Bitbucket [here](https://confluence.atlassian.com/x/bgozDQ)