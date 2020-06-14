# analysis

Holds PERTS data analysis projects, one per subdirectory.

Data should not be stored in this repository, or ever comitted to version control in general!

## Setup

After cloning, you have to install the `gymnast` submodule.

1. Open the Terminal application
2. Change directories to your new copy of `analysis`. The command will be something like `cd ~/Sites/analysis`
3. Run this command: `git submodule update --init`

## Development

Do all your coding from the root working directory of the repo. The following tricks make this very easy:

1. Create a project in RStudio. This helps make sure your working directory always starts in the right place.
2. Use the package `here` to build file paths when writing RMarkdown files. This will let you work with .Rmd files and associated resources in a subdirectory without ever using `setwd()`, which is broken for .Rmds anyway.
3. Use `import_module` instead of `source()` to bring in files. See `gymnast/README.md`. The function `import_module` should be automatically installed if your working directory starts in root of the repo (because of `.Rprofile` file), but you can always install it manually; again see the gymnast readme.
