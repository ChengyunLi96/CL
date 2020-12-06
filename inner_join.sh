#!/bin/bash
sort 788-0.txt > 788-0_sorted.txt
tail -n +2 rx_pso_all.csv | sort > rx_pso_all_sorted.txt
python -m pip install tqdm
python inner_join_with_range.py