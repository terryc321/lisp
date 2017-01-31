#!/bin/bash

cp -v black.scm black2.scm

rm -f black2.out

perl -pe 's/[^[:ascii:]]//g;' black2.scm > black2.out

diff black2.scm black2.out




    

    

    
    
