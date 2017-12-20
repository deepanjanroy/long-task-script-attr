#!/bin/bash -x
for filelist in /usr/local/google/home/dproy/code/long_tasks_analysis/splitpaths/*; do
  echo "############################################################################"
  echo "Processing: $filelist"
  base=$(basename $filelist .txt)
  outfile="/usr/local/google/home/dproy/code/long_tasks_analysis/splitresults/${base}.json"
  echo "Outfile: $outfile"
  blaze run //googleclient/chrome/speed/big_data/slow_reports:process_traces -c opt -- --metric_name longTasksAnalysis --metric_type mapper --input_file=${filelist} --flume_exec_mode=LOCAL_PROCESSES --output_text --output_path=${outfile}
done

