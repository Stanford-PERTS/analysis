#!/bin/bash  

# see http://adamyoung.net/OS-X-Run-Shell-Scripts-From-Finder
# this file must be executable to work, run:
# chmod +x commit.command

cd perts_analyses
git add .  
read -p "Commit description: " desc  
git commit -m "$desc"
git push
