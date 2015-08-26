#!/bin/csh
mkdir work
cd work
foreach f (../data/*.tar.gz)    
  tar zxvf $f 
end
cd ..
