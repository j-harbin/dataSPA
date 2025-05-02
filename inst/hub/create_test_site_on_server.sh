#! /bin/bash

# update the package and download the data locally
git pull
'C:/Program Files/R/R-4.3.3/bin/Rscript.exe' inst/hub/download_PPT_data.R

# you can connect to the server using this ssh bash command, but you shouldn't need to with the scp commands below
# ssh -i C:/Users/DaigleR/.ssh/id_rsa -p 22018 rdaigle@glf-proxy

# copy the data to the server
scp -i C:/Users/DaigleR/.ssh/id_rsa -P 22 dataSPA_om.rds rdaigle@mar-spa.ent.dfo-mpo.ca:/home/mar-spa/
scp -i C:/Users/DaigleR/.ssh/id_rsa -P 22 dataSPA_SAL.rds rdaigle@mar-spa.ent.dfo-mpo.ca:/home/mar-spa/

# start the test site script in a tmux session so the script doesn't hang
ssh -i C:/Users/DaigleR/.ssh/id_rsa -p 22 rdaigle@mar-spa.ent.dfo-mpo.ca << EOF
   tmux new-session -A -s marspa -c /home/mar-spa -d
   tmux send-keys "sh /home/mar-spa/make_test_site.sh" C-m
EOF
