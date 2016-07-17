# README #

### Model Setup ###

Please complete the following steps to setup the model:

0. Setup an account on bitbucket.org
1. [Download](http://www.r-project.org/) and install R.
2. (Recommended) [Download](http://www.rstudio.com/products/rstudio/download/) and install RStudio.
3. [Download](http://git-scm.com/downloads) and install Git for version control.
4. Download [this](https://www.dropbox.com/sh/sfc47tx1c2d9nzw/AADDnBzPnw4S5ywAVfVZ6L1Ya?dl=0) shared Dropbox folder.
5. Launch RStudio.
6. From the menu, select **File > New Project**.
7. In the dialogue menu that pops up, select **Version Control**.
8. In the next menu select **Git**.
9. Paste the repository URL from bitbucket.org into the **Repository URL** box. You can connect using either SSH or HTTPS.
10. For HTTPS, use the link `https://wilkey@bitbucket.org/wilkey/ub_o-g_emissions.git` You'll be prompted for your username and password when you connect to the server.
11. For SSH, use the link `git@bitbucket.org:wilkey/ub_o-g_emissions.git`. Detailed instructions on SSH setup are available [here](https://confluence.atlassian.com/display/BITBUCKET/Set+up+SSH+for+Git).
12. A local copy of the server repository will be created by Git on your computer in a folder of your choice. The name of that folder will be whatever you enter in the **Project directory name** dialogue box (by default it's the same name as the bitbucket.org project name: ub_o-g_emissions). The folder will be located as a subdirectory of whatever folder you point to in the **Create project as subdirectory of** dialogue box. By default it's a subdirectory of ~/R, which is the R folder generated in your home directory when you first run R (or install any packages in R).
13. After entering all the information, click the **Create Project** button.
14. Open the script `main.R`, located in the local repository directory you created previously. This is the main script for the model.
15. Under the section of the scripts labeled `1.1 Paths`, change the paths for each of the directories listed to the location on your computer where you downloaded (a) the repository and (b) the shared Dropbox folder's contents. After altering the paths, save the script.
16. Download and install the packages necessary to run the model. A list of the packages used is given under the section header `1.3 Packages`. You can install packages using the command `install.packages("package_name")` in the R console. Alternatively, you can use the menu option **Tools > Install Packages...** and then enter the names of the packages you wish to install.
17. All modeling options are contained in the script `IO_options.R`, located in the local repository directory. Alter the input options as desired, and save the script.
18. You can now run the script using the menu option **Code > Source** or any of its keyboard shortcuts and variations.

### Documentation ###

* The model's User Manual is available [here](https://www.dropbox.com/s/mrl31iyp58wlyzm/User%20Manual%20for%20Uinta%20Basin%20Oil%20and%20Gas%20Emissions%20Model.pdf?dl=0).
* Documentation for Git [here](http://git-scm.com/doc).
* Documentation for Bitbucket [here](https://confluence.atlassian.com/x/bgozDQ).

### Version History ###

The following table lists the version history of the results saved in the prepared data folder (and their corresponding entries in `IO_options.R`):

| Version | Notes                                               |
|---------|-----------------------------------------------------|
| v1      | Cross validate - train 1984-2009, predict 2010-2014 |
| v2      | Prediction - train 1984-2014, predict 2015-2019     |