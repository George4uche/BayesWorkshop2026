---
output:
  word_document: default
  html_document: default
  pdf_document: default
---
# Set up instructions Part I: Software and configurations
## Overview

All of the software and online tools for this workshop are free to use. Several are managed by companies that offer paid plans and upgrades. The free versions are more than adequate for this workshop and most researcher needs. Here is a list of what we will be using in this workshop. 

### Necessary tools:

**R** is a free, open-source language and software environment that is widely used for data analysis (https://www.r-project.org/). 
**RStudio** is an integrated development environment (IDE) developed by Posit that provides a lot of useful tools for working with R. 

**RStudio Desktop** (https://posit.co/download/rstudio-desktop/) is a software application for Rstudio that you download and install on your computer. It is also free and open-source, though there is a paid version available from Posit (**only the free version is required for this workshop**). R and R studio are separate pieces of software that interact with each other. RStudio can be used as an IDE for other software environments, such as Python. Conversely, there are other software applications that can be used as an IDE for R, such as Microsoft Visual Studio. Generally, RStudio is the preferred IDE for R, and the one we will use for this workshop. 

**C++ toolchain**: Some of the tools you will use for Bayesian data analysis require extra software to compile code written in C++ or Fortran. If you are using Windows, you will need to install **Rtools** (https://cran.r-project.org/bin/windows/Rtools/). Mac users will need to install Xcode and a GNU Fortran compiler (see: https://mac.r-project.org/tools/).  

**Git** (https://git-scm.com/) is a version control system that tracks files and manages changes with multiple collaborators. It is a free, open-source software that you install on your computer.

**GitHub** (https://github.com/) is a cloud-based collaborative platform that stores code and helps manage projects with multiple collaborators. Code is stored in a folder called a repository. GitHub works with the Git software on your computer so you can work on files locally and sync your changes to the remote repository on GitHub. To use GitHub, you must create a user account using an email address. GitHub offers free and paid plans for individuals and organizations. The free plan includes unlimited private and public repositories and is sufficient for for this workshop. Github also offers extensive documentation via https://docs.github.com and AI coding assistance through Github Copilot (limited for the free plan).

### Optional tools:

**Posit Cloud** (https://posit.cloud/) is a platform that allows you to work with RStudio online, without the need to download or install anything on your computer. The free version has a limit number of projects and computing hours, but should be sufficient for entry level users. Posit Cloud allows for multiple coding environments, and the RStudio projects have a similar interface as the RStudio Desktop. It is useful if you have inconsistent access to a particular computer, or if you want to work collaboratively on an R project (see below). For the course, we will be working with RStudio Desktop, with Posit Cloud as an optional resource.
 

## Installation steps

These instructions will allow you to work with R and RStudio on your local machine, with version control through Git and Github.

----------------------

1. Install the most recent release of **R (v 4.5.1)** from https://cran.rstudio.com/.

At the top of the page, select the Download link for your system (Windows, macOS, or Linux). The installation steps differ across operating systems. This page gives instructions for each operating system: https://rstudio-education.github.io/hopr/starting.html#how-to-download-and-install-r. For Windows and Mac, the installation involves downloading and running an installer program.   

--------------------

2.	Install the most recent release of **RStudio Desktop** from https://posit.co/download/rstudio-desktop/. This also involves downloading and running an installer program.

---------------

3. Install a **C++ toolchain** (https://mc-stan.org/install/#prerequisite-c17-toolchain) 

### Windows: Install **Rtools**

Visit https://cran.r-project.org/bin/windows/Rtools/. Select RTools 4.5, select the link for the Rtools45 installer, and run the downloaded installer executable program. Accept the default options.

### MacOS:

Mac Users should install Xtools and a GNU Fortran compiler, available here: https://mac.r-project.org/tools/.  


### Linux:

Run the command `sudo apt-get install build-essential`.

--------------------

4. If you don't have one already, **create a GitHub account** at https://github.com/. See here for detailed instructions: https://docs.github.com/en/get-started/start-your-journey/creating-an-account-on-github.  

--------------------

5.	Install the **Git** software from http://git-scm.com/downloads. 

For Windows, this involves downloading an running and installer program. The installer program will ask a series of questions. Generally, the defaults should be fine, except that you may want to change the default repository name to "main" instead of master. You can also check whether Git is already installed in Windows by opening the Command Prompt and entering git --version. If Git is not installed, this will produce an error message about an unidentified command.   

----------------------

6. Configure Git with your GitHub account. 

In order to integrate the Git software with your GitHub repository, you need to provide Git with the user name and email from your GitHub account. This page gives instructions for how to do this on different operating systems: https://r-bio.github.io/git-installation/.

For Windows, launch "git bash" from your list of programs. This will open a command line terminal. In the terminal, type:

```
git config --global user.name yourusername
git config --global user.email your@email.com 
```

Replace *yourusername* and *your@email.com* with the username and email you used for your GitHub account. 

----------------------

7. Add Git as a PATH variable (Windows operating systems only).

For Windows, you need to take an extra step of adding Git to your System variables so other programs like Rstudio know where to look for it. First, double check where git is installed on your computer. In the file manager, look for `C:\Program Files (x86)\Git` or `C:\Program Files\Git`  and locate the `bin\` within.

Then, open the Control Panel and look for Advanced System Settings. I found this in Windows 11 by typing Control Panel into the search bar and entering "Advanced System Settings" into the control panel search bar. 

![Advanced System Settings](setupimages/advancedSystemSettingsScreenshot.png)


Then, select Environment Variables.
 
 ![Environment Variables](setupimages/environmentVariablesScreenshot1.png)
 
Under System variables, scroll down to the Variable called "Path". Select it and push the "Edit" button.

 ![System Variables](setupimages/pathEditScreenshot.png)

Add the file path of the bin folder within Git to the Path variable (e.g., `C:\Program Files\Git\bin`). In Windows 11, this can be done by clicking 'New' within the editing window and typing in the path to the bin folder (see below). In older versions of Windows, you may need to edit the variable by adding `;` followed by the path to the bin folder.  

 ![System Variables](setupimages/gitbinScreenshot.png)

The `Git\cmd` folder should already be included here. If it is not, you should add it as well following the above steps.
 




-------------------------

## Configuring Rstudio Desktop

Once you have these three software programs installed, you will need to take a few extra steps to configure Rstudio Desktop.

------------------------

1. Double check that Rstudio is running the most recent version of R.

If you have an older version of R on your computer, it is possible that Rstudio may be using that instead of the newer one. To check, open the RStudio software and click on the Tools menu at the top of the application window. Within Tools, select Global Options. Select the *R General* tab on the left and the *Basic* tab on the top. You should see something like this:

![Global Settings](setupimages/RstudioRversionScreenshot.png)

The R version at the top should be the one you most recently installed (i.e., 4.5.1). If it is not, click change and follow the prompts to find the most current version of R.  

--------------

2. Set up version control.

This page shows instructions for different operating systems: https://r-bio.github.io/git-installation/. Once you have gone through the steps of linking your Github account and adding Git as a Windows Path variable (above), you can follow these instructions to set up Git and GitHub within R Studio:
 https://docs.posit.co/ide/user/ide/guide/tools/version-control.html


Open RStudio Desktop. In the toolbar, click Tools, Global Options, and then Git/SVN in the side bar. Click the 'Enable Version control interface for RStudio projects' checkbox. The Git executable box should give the location of the git.exe file on your computer. If it does not, you can browse to find it. 

![Global Settings](setupimages/GitOptionsRStudioScreenshot.png)


You may need to enter a path for where the git.exe file is located on your computer (Git executable). The SSH key is an optional feature you can use to connect to GitHub. We will ignore it for now, but you can learn more about it here: https://docs.github.com/en/authentication/connecting-to-github-with-ssh/about-ssh  

-----------------

3. Set a custom package directory (optional).

R is inherently modular. Its functionality comes from packages developed contributed by the community of users. When you install a package, R downloads a folder containing the package code and documentation into a specific location on your computer. You don't have to do anything to set this up, but it is helpful to know where your packages live if you need to upgrade your R software or switch computers. 

To see where R saves packages, open Rstudio, type `.libPaths()` in the console window, and press enter.

*These steps may seem overwhelming at first, but it is much easier to take care of these details when you set up your system for the first time.*

![.libPaths](setupimages/libPathsScreenshot.png)

The output shows two paths. The first path the default location where R will install new packages. On my clean install on Windows 11 using R v. 4.5.1, R automatically created a user package library in my User profile  `C:/Users/kathe/AppData/Local/R/win-library/4.5`. That means, if I install a new package, it will only show up on my user account, and not for other users on the same computer. 

The second path is the system package library `C:/Program Files/R/R-4.5.1/library`. This folder contains the built-in packages that come with the R software. Any packages installed here will be available for all users on your computer. However, it is generally best practice to use a separate, user package library, which is the default in R 4.5.1. 

While it is fine to keep the defaults, it can be useful to change where R reads and installs packages. For example, I like to set up my user package library in my Microsoft One Drive folder so it automatically backs up to the cloud.

To set up a custom R package library:

1. Create a folder where you want the library to live. I made a folder called `R/win-library/4.5` within my One Drive documents folder.

2. In Windows, you can go to Advanced System Settings -> Environment Variables and look for a variable called R_LIBS_USER under your user variables. If it is not there, click "New" to create R_LIBS_USER and enter the library path as the variable value (see below). Otherwise, modify R_LIBS_USER to point to your library path. 

![R_LIBS_USER](setupimages/R_LIBS_USER_Screenshot.png)

Alternatively, you can also modify the library path with a package called "usethis", as shown in this video: https://rfortherestofus.com/2021/09/how-to-set-your-default-package-install-location-in-r

3. Restart R studio and enter `.libPaths()` into the console. The first result should be the folder you made.

![new libPath](setupimages/libPathsScreenshot2.png)
 
 

 
