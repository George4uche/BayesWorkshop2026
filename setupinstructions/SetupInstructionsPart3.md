# Set up instructions Part III: Project workflow

## Overview
These instructions assumed that you have completed all the steps in Part I, including making a Github account and enabling version control in RStudio. 

These instructions are for setting up the project workflow we will use in the workshop. For a more basic introduction about the mechanics of R coding, check out Chapters 2 and 6 of the online book *R for Data Science* (https://r4ds.hadley.nz/). This tutorial also gives a useful overview about setting up projects with GitHub: https://rverse-tutorials.github.io/RWorkflow-NWFSC-2020/week1-rstudio.html

We are using a GitHub repository for the workshop materials because Git/GitHub are great tools for managing team projects and are widely used in scientific data analysis. However, there are some **limitations** that are useful to know about:

1. **GitHub is not good for storing data.** GitHub is fine for small data files, but you should keep your data in a separate location if you are working with large files or files with sensitive information. There are multiple cloud storage systems that are good for group work, such as Google Drive, Dropbox, Box, OneDrive, etc. For long-term data storage and archiving, it's a good idea to use a repository, such as Figshare (https://figshare.com/) or the Open Science Framework (https://osf.io/). Both of these have integrations with GitHub. 

2. **GitHub is not good for synchronous editing.** GitHub works best one one person modifies a file at a time. If two people modify, save, and push a file to the remote repository, it will result in conflicts that have to be resolved manually. If you want to work on a document simultaneously, you're better off using Google Docs or something similar.

3. **Git is not good for tracking binary files.** The detailed line-by-line tracking from Git only works for plain-text files like R scripts, not for binary files like Microsoft Word documents, PDFs, or Excel spreadsheets. For binary files, Git tracks when they are created, deleted, and modified, but not how they are modified. GitHub can be used to store and share binary files, so long as they are not too large.    

## Components:

- **RStudio Project**: A project is a tool within RStudio that is used to organize scripts and data. In RStudio Desktop, a project corresponds to a folder on your local hard drive that houses all files pertaining to a particular project. The project folder contains a file ending in .Rproj (e.g., myproject.Rproj). When you open the .Rproj file in RStudio, the R session automatically sets the project folder as the working directory, which is where R will load and save files for the project. The .Rproj file also remembers which script windows were open last time you exited, and any settings you modified. RStudio projects can be used on their own or with GitHub. We will be doing the latter.
    
- **Local repository**: This is a folder on your local machine that the Git software tracks. Similar to the RStudio project, the Git functionality comes from a set of files saved in this folder. As you modify and save files in the local repository, you make "commits" to Git with some comment about what you've changed (e.g., "created a readme file", "fixed an error", etc.). Git keeps track of every file in the repository and logs everything everything that has changed between commits. You can tell Git to ignore certain files and file types by listing them in a file called .gitignore (more on that later). 
    
  
- **Remote repository**: This is a folder that lives on the GitHub cloud server. After you make commits on your local repository, you "push" them to the remote repository on Github. You can also "pull" from your remote to your local repository to update it with changes pushed by others.    

Here is a diagram summarizing the workflow with RStudio Projects, Git, and Github. The same folder houses the RStudio project and local repository.

![RProject/Git/GitHub Workflow](setupimages/githubworkflowdiagram.png)

## Some Git/GitHub terminology
Definitions copied and from here:
 https://docs.github.com/en/get-started/learning-about-github/github-glossary

- **Clone**: A clone is a copy of a repository that lives on your computer instead of on a website's server. When you make a clone, you can edit the files in your preferred editor and use Git to keep track of your changes without having to be online. The repository you cloned is still connected to the remote version so that you can push your local changes to the remote to keep them synced when you're online. 

- **Branch**: A branch is a parallel version of a repository. It is contained within the repository, but does not affect the primary or main branch, allowing you to work freely without disrupting the "live" version. When you've made the changes you want to make, you can merge your branch back into the main branch to publish your changes. *This is more useful when you are working on a software application like an R package. A lot of entry level research coders do everything on the 'main' or 'master' branch. The RStudio Git menu has tools to create and switch branches, but it is important to pay careful attention before making and pushing commits!* 

- **Fork**: A fork is a personal copy of another user's repository that lives on your account. Forks allow you to freely make changes to a project without affecting the original upstream repository. You can also open a pull request in the upstream repository and keep your fork synced with the latest changes since both repositories are still connected. *A fork is similar to a branch, except that it's a new repository. Branches must eventually be merged or deleted. Forks can merge with the original upstream repository, but can also go in a totally different direction.*

- **Commit**: A commit, or "revision", is an individual change to a file (or set of files). When you make a commit to save your work, Git creates a unique ID (a.k.a. the "SHA" or "hash") that allows you to keep record of the specific changes committed along with who made them and when. Commits usually contain a commit message which is a brief description of what changes were made. *In the Git/Github workflow in RStudio, commits are made to your local repository, then pushed to the remote repository.*

- **Stage**: Git "stages" all files in a repository that have been created or modified since the last commit (excluding those specified in the .gitignore). When you make a commit, you select staged files and write a commit message. 

- **Pull**: Pull refers to when you are fetching in changes and merging them. For instance, if someone has edited the remote file you're both working on, you'll want to pull in those changes to your local copy so that it's up to date. 

- **Push**: To push means to send your committed changes to a remote repository on GitHub.com. For instance, if you change something locally, you can push those changes so that others may access them.



## Workshop setup:

We created a repository on github to organize workshop materials (https://github.com/lakechadproject/BayesWorkshop2026). We recommend that students fork (copy) this repository into their personal github accounts. This allows students to edit and commit code without making changes to the original repository.  

----------------------

### Fork the workshop repository

1. Log into github.com and navigate to  https://github.com/lakechadproject/BayesWorkshop2026   See below for some notes on the GitHub workflow. 

----------------------
2.	Fork BayesWorkshop2026. Click on the ![Fork Icon](setupimages/ForkIcon.png)in the upper right corner. 


	  
----------------------
3. Click “Create new fork”. The "Owner" should be your user account. By default, the repository name is the same as the repository you are copying. There two buttons on the home page of the repository. “Sync fork” pulls any changes from the original repository. “Contribute” allows you to contribute changes to the original repository. **Note: for simplicity, we are going to keep all forking operations within GitHub webpage.**

### Set up an RStudio project

4. Open RStudio. On the top menu bar, select File then New Project.
----------------------
5. Select Version Control from the Create Project Wizard window. Then select Git.
----------------------
6. Enter the URL for your forked repository. 
Because you forked the repository, the url on your system should be https://github.com/YOURUSERNAME/BayesWorkshop2026. You can also log into GitHub, navigate to the repository, and copy the link from your browser. By default, the project directory name matches the repository name. 

----------------------
7. Click "Browse" to select where you want the project directory to be saved on your computer. The default is the Home directory on your system (usually your user account Documents folder).


![R Project Git Setup](setupimages/RStudioGithubSetupScreenshot.png)

----------------------
8. Click "Create Project." Follow the prompts to log into GitHub to authorize. 
----------------------
9. Close the browser and open your project in RStudio. There should be now be a "Git" toolbar in the upper right side of your RStudio window. If you click on this toolbar, you should see a list of all the changes that have been made since your last "commit". 
![RStudio Git Toolbar](setupimages/RStudioGitWindow1.png)

-----------------------------


### Try out the Git Workflow. 

With group repositories, your Git workflow in RStudio should always be **pull, commit, push**. It's always a good idea to pull changes from the remote repository every time you open your RStudio project.

10. **Make your first commit!** 
Likely, your Git window will show two files that RStudio created when you initiated the project directory. One is the `{PROJECTNAME}.Rproj` file, which tells RStudio that this directory is an R Project. The other is the `.gitignore` file, which tells Git which files to ignore when you make commits. Click on the .gitignore file in your Files pane to open it in the RStudio editor. You will notice that the .gitignore file already contains a few items. These are accessory files that work behind the scienes of an RStudio project and have no use outside of your file system.

![.gitignore](setupimages/GitIgnoreScreenshot1.png)

For your first, check the boxes next to the two staged files and click on the checkmark icon to open the Git commit window. **For the sake of developing good habits, press the "pull" arrow to import any changes from the remote repository.** At this stage, you should get a message saying that your local repository is already up to date. 

![First Commit Step 1](setupimages/FirstCommitScreenshot2.png)

Here, you can select each of the staged files to see what has changed since your previous commit. Since these are new files, everything has changed. 

Check all the files you want to include in the commit (in this case, all of them). Write an informative message describing your commit, then press the "Commit" button. This tells Git to commit the changes to your local repository. After you do this (and only after!) click on the "push" button to send the commit to your remote repository on Github. You should see your commit next time you log into GitHub on your internet browser. 

-----------------

11. Modify your .gitignore file.

It can be useful to tell Git to ignore specific files or file types. For example, you might have files with private information you do not want to share publicly, or you may want to ignore large binary files that take up a lot of space and are not useful to track on Github.

If you want Git to ignore a specific file, you use the same window you used to make a commit, but press "Ignore" instead.     

This opens your .gitIgnore file in a new window and gives you the option to save it with the ignored file added. Once you save this, the ignored files should no longer be included in the list of staged files in the Git window.   

![.gitignore](setupimages/GitIgnoreScreenshot3.png)

You can also edit the .gitignore file through the RStudio editor. In addition to listing specific files, Git can ignore files based on a pattern. For example, adding `*.docx` to my .gitignore tells git to ignore all Microsoft Word documents (the `*` means anything before the file extension .docx). 

After you modify your .gitignore file, you will need to commit your changes to your local repository and push them to the remote repository, as you did in step 10. 

  
