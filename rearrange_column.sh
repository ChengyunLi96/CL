#!/bin/bash
for FILE in /nfs/turbo/umms-alextsoi/lichengyun/lab/CBC/CBC_submis/*sub.txt
do
    awk 'BEGIN { FS = OFS = "\t" } {print $1, $3, $2, $4, $5, $6, $7, $8, $9, $10, $11 }' $FILE > "${FILE/sub.txt/col.txt}"
done

  
  


