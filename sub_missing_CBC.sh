#!/bin/bash
for FILE in /nfs/turbo/umms-alextsoi/lichengyun/lab/CBC/CBC_mis/*.txt
do
    awk 'BEGIN { FS = OFS = "\t" } { for(i=1; i<=NF; i++) if($i ~ /^ *$/) $i = "X" }; 1' $FILE > "${FILE/.txt/sub.txt}"
done

  
  


