#!/bin/bash

cp -v black.scm filter.dat

rm -f filter2.dat

perl -pe 's/[^[:ascii:]]//g;' filter.dat > filter2.dat

diff filter.dat filter2.dat








    

    

    
    
