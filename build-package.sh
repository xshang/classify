cd ..
R CMD BUILD classify
R CMD INSTALL classify_*.tar.gz
mv classify_*.tar.gz classify/
