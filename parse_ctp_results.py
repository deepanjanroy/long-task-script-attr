import csv
import json
import sys

# Quick and dirty debug logging.
debug_mode = False
def log_debug(*args, **kwargs):
  if debug_mode:
    for arg in args:
      print args
    for k, v in kwargs.items():
      print v

def load_json_results_from_file(filename):
  results = [];
  with open(filename) as f:
    for line in f:
      try:
        results.append(json.loads(line))
      except:
        log_debug("----------------------")
        log_debug("Could not parse json: ")
        log_debug(line)
        log_debug("----------------------")
  print "Loaded " + filename
  return results

def get_unique_histogram_value(histogram):
  """If histogram has a unique value, returns that value. Otherwise returns a
  string of the format "Not Unique. {count: <number of values>,
  sampleValues: <a representative sample of values>}". If no value is found,
  returns an empty string.

  The decision to return a string instead of raising an exception in these
  failure cases is intentional. The json results produced by cluster telemetry
  / chrome trace processor pipeline often has all kinds of errors, and we don't
  want to choke on them, but we also want to be aware of their presence so we
  can fix the errors if possible.
  """
  if 'running' in histogram:
    running_stats = histogram['running']
    running_max = running_stats[1]
    running_min = running_stats[4]
    if running_min == running_max:
      return running_min
    else:
      return "Not Unique. {count: {count}, sampleValues: {samples}}".format(
        count=running_stats[0], sampleValues=histogram.get('sampleValues', []))
  return ''

def parse_results_json_list(result_json_list):
  """
  Produces a list of trace_data dicts. A trace_data dict contains information
  about a single trace. It's format is
  {
    telemetry_info: <metadata about the run>
    metrics: <dict of all histograms gathered from the run>
  }
  """
  results = []
  for result_json in result_json_list:
    trace_data = {}
    metrics_dict = {}
    trace_data['metrics'] = metrics_dict
    histograms = result_json['pairs']['histograms']
    for histogram in histograms:
      if histogram.get('type', '') == 'TelemetryInfo':
        trace_data['telemetry_info'] = histogram
      if 'name' in histogram:
        metrics_dict[histogram['name']] = get_unique_histogram_value(histogram)
    results.append(trace_data)
  return results

def get_csv_dicts(trace_data_list):
  """
  Converts list of trace_data to list of csv_dict, a flat dictionary that will
  be written out to the csv file.
  """
  csv_dicts = []
  for trace_data in trace_data_list:
    csv_dict = {}
    csv_dict['site'] = trace_data['telemetry_info']['storyDisplayName']
    csv_dict['cache_temperature'] = (trace_data['telemetry_info']
                                     ['storyGroupingKeys']
                                     ['cache_temperature'])
    csv_dict.update(trace_data['metrics'])
    csv_dicts.append(csv_dict)
  return csv_dicts


def write_csv(trace_data_list, output_filename):
  csv_dicts = get_csv_dicts(trace_data_list)

  # Not all histograms contain all metrics so we need to gather all the
  # possible fieldnames first.
  fieldnames = set()
  for d in csv_dicts:
    fieldnames = fieldnames.union(d.keys())

  with open(output_filename, 'w') as f:
    writer = csv.DictWriter(f, list(fieldnames), extrasaction='ignore')
    writer.writeheader()
    writer.writerows(csv_dicts)

def main():
  # TODO(dproy): It may eventually make sense to use a real argument parser.
  if len(sys.argv) < 2:
    print "Usage: {0} <ctp-results> [output-filename]".format(sys.argv[0])
    print "<ctp-results> is the results file produced by chrome trace processor."
    print "[output-filename] is the produced csv file. Defaults to out.csv."

  input_filename = sys.argv[1]
  if len(sys.argv) > 2:
    output_filename = sys.argv[2]
  else:
    output_filename = "out.csv"

  result_json_list = load_json_results_from_file(input_filename)
  trace_data_list = parse_results_json_list(result_json_list)
  write_csv(trace_data_list, output_filename)

  print "Wrote csv output to " + output_filename
  print "Total results processed:", len(result_json_list)

if __name__ == '__main__':
  main()
