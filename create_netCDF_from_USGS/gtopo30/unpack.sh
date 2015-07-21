#!/bin/csh
foreach f (data/*.tar.gz)    
  tar zxvf $f 
end

