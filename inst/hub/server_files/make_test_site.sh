#! /bin/bash

git -C dataSPA/ pull
Rscript render_all.R
rm -rf /var/www/html/mar-spa-test
mv /home/mar-spa/mar-spa /var/www/html/mar-spa-test