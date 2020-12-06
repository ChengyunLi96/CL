#!/bin/bash
python -m pip install tqdm
for FILE in /nfs/turbo/umms-alextsoi/lichengyun/lab/CBC/CBC_submis/*col.txt
do
    sort $FILE > "${FILE/col.txt/_sorted.txt}"
    tail -n +2 uniq_rx_pso_skin.csv | sort | uniq > uniq_rx_pso_skin_sorted.txt
done
python inner_join_with_range_CBC.py

