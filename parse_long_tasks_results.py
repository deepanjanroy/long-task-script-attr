"""Parses results json files produced by breakdown_mapper.

TODO(dproy): This file is awkwardly coupled with parse_ctp_results.py right
now, and also contains lots of copy pasta. Consider refactoring.
"""

from collections import defaultdict
from pprint import pprint
from parse_ctp_results import load_json_results_from_file, log_debug
from enum import Enum
import csv
import json
import math
import sys
import traceback

def filter_subtasks(long_task_breakdowns):
  for long_task in long_task_breakdowns:
    long_task['subtasks'] = [st for st in
        long_task['subtasks'] if st['type'] == 'FunctionCall']

def process_json_results(result_json_list):
  """
  Converts list of trace_data to list of csv_dict, a flat dictionary that will
  be written out to the csv file

  This function is truly a hallmark of defensive programming.
  """
  return_dicts = []
  samples_without_breakdown = 0
  unhandled_errors = 0
  telemetry_info_error = 0

  for results_json in result_json_list:
    pairs = results_json['pairs']
    if 'TelemetryInfo' not in pairs:
      # This result was a probably total failure and it cannot be processed at
      # all. Print the raw json and give up.
      log_debug('Ignoring result: No TelemetryInfo found')
      log_debug(results_json)
      telemetry_info_error += 1
      continue
    telemetry_info = pairs['TelemetryInfo']
    if 'stories' not in telemetry_info:
      log_debug('Ignoring result: stories not in TelemetryInfo')
      telemetry_info_error += 1
      continue

    storyTags = telemetry_info['storyTags']
    cache_temperature_tags = [t for t in storyTags if 'cache_temperature' in t]
    if len(cache_temperature_tags) == 0:
      print 'Ignoring result: cache temperature not in TelemetryInfo'
      telemetry_info_error += 1
      continue

    if len(cache_temperature_tags) > 1:
      print "Internal Error: More than one cache-temperature in story tags"
      unhandled_errors += 1
      continue

    cache_temperature_tag = cache_temperature_tags[0]

    failures = results_json['failures']
    for f in failures:
      log_debug("Result contains failures:")
      # Some results have partial failures. There are lot of edge cases that
      # may not be worth fixing, so print the failures and carry on processing
      # whatever possible.
      log_debug(f)

    try:
      return_dict = {}
      stories = telemetry_info['stories']
      assert len(stories) == 1
      cache_temperature = cache_temperature_tag.split(':')[1]
      return_dict['site'] = stories[0] + ':' + cache_temperature

      if 'longTaskBreakdowns' in pairs:
        filter_subtasks(pairs['longTaskBreakdowns'])  # In place changes.
        return_dict['longTasksBreakdowns'] = pairs['longTaskBreakdowns']
        return_dicts.append(return_dict)
      else:
        samples_without_breakdown += 1

    except Exception as e:
      # Catching all other errors. Results json files contain all kinds of
      # errors and it's not desirable to halt the entire script because 1 in
      # 15000 entries was malformed.
      print "Error while processing result this results record:"
      print results_json
      print "Error:"
      traceback.print_exc()
      print "Skipping result and moving on"
      unhandled_errors += 1

  success_count = len(return_dicts)
  print '------------------------------------------------'
  print "Total input data rows: ", len(result_json_list)
  print "Successful data extraction: ", success_count
  print "Telemetry info error: ", telemetry_info_error
  print "Samples without breakdown: ", samples_without_breakdown
  print "Unhandled errors: ", unhandled_errors
  assert len(result_json_list) == (success_count +
      telemetry_info_error + unhandled_errors + samples_without_breakdown)
  return return_dicts

def write_json_list(json_list, output_filename):
  with open(output_filename, 'w') as f:
    json.dump(json_list, f)
  print "Wrote json output to " + output_filename

def key_mirror(l): return {x: x for x in l}
Patterns = key_mirror([
  "Empty",
  "VeryLittleScript",
  "SingleDominating",
  "SingleNotDominating",
  "MultipleEvenlyDistributed",
  "MultipleOneDominating",
])

def get_pattern(long_task):
  LT_DOMINANCE_THRESHOLD = 0.9
  VERY_LITTLE_SCRIPT_THRESHOLD = 12
  SCRIPT_DOMINANCE_THRESHOLD = 0.75
  subtasks = long_task['subtasks']

  if len(subtasks) == 0:
    return Patterns['Empty']

  total_subtask_time = sum([t['totalTime'] for t in subtasks])
  if total_subtask_time < VERY_LITTLE_SCRIPT_THRESHOLD:
    return Patterns['VeryLittleScript']

  if len(subtasks) == 1:
    t = subtasks[0]
    if t['totalTime'] > (LT_DOMINANCE_THRESHOLD * long_task['duration']):
      return Patterns['SingleDominating']
    else:
      return Patterns['SingleNotDominating']

  for t in subtasks:
    if t['totalTime'] > SCRIPT_DOMINANCE_THRESHOLD * total_subtask_time:
      return Patterns["MultipleOneDominating"]
    else:
      return Patterns["MultipleEvenlyDistributed"]

def get_num_urls(subtasks):
  urls = set()
  for task in subtasks:
    urls.add(task['url'])
  return len(urls)

NOT_SCRIPT = 'not-script'

def get_perfect_proportions(breakdown):
  subtasks = breakdown['subtasks']
  duration = breakdown['duration']
  total = sum([t["totalTime"] for t in subtasks], 0)
  proportions = defaultdict(int)
  for s in subtasks:
    proportions[s['url']] += s['totalTime']
  proportions[NOT_SCRIPT] = duration - total
  for k, v in proportions.iteritems():
    proportions[k] = v / duration
  return proportions

count = 0
max_count = 3

def get_sampling_proportions(breakdown, start, interval):
  samples = defaultdict(int)
  total_samples = 0
  mt_start = breakdown['start']  # Main Thread task start time.
  mt_duration = breakdown['duration']
  mt_end = mt_start + mt_duration
  subtasks = breakdown['subtasks']
  sampled_time = mt_start + start
  while sampled_time < mt_end:
    total_samples += 1
    # I'm sure we can optimize this but not worth risking correctness.
    in_script_task = False
    for st in subtasks:
      if st['start'] < sampled_time < st['start'] + st['totalTime']:
        samples[st['url']] += 1
        in_script_task = True
    if not in_script_task:
      samples[NOT_SCRIPT] += 1
    sampled_time += interval

  # Turning samples into proportions.
  for k, v in samples.iteritems():
    samples[k] = float(v) / total_samples
  return samples

def get_first_n_proportions(breakdown, n):
  subtasks = breakdown['subtasks']
  duration = breakdown['duration']
  proportions = defaultdict(int)
  total_accounted = 0
  for s in subtasks[:n]:
    proportions[s['url']] += s['totalTime']
    total_accounted += s['totalTime']
  proportions[NOT_SCRIPT] = duration - total_accounted
  for k, v in proportions.iteritems():
    proportions[k] = v / duration
  return proportions

def diff_proportions_sum_squared(p1, p2):
  keys = set.union(set(p1.keys()), set(p2.keys()))
  sum_squared = 0
  for k in keys:
    diff = p1[k] - p2[k]
    sum_squared += diff**2
  return sum_squared

def print_debug_info(breakdown):
  global count
  if count < max_count:
    pprint(breakdown)
    p1 = get_perfect_proportions(breakdown)
    p2 = get_sampling_proportions(breakdown, 0, 20)
    p3 = get_sampling_proportions(breakdown, 45, 20)
    p4 = get_first_n_proportions(breakdown, 1);
    p5 = get_first_n_proportions(breakdown, 2);
    pprint(dict(p1))
    # pprint(dict(p2))
    # pprint(dict(p3))
    pprint(dict(p4))
    pprint(dict(p5))
    # print "Diff p1~p2: "
    # print diff_proportions_sum_squared(p1, p2)
    # print "Diff p1~p3: "
    # print diff_proportions_sum_squared(p1, p3)
    count += 1

def write_csv(json_list, output_filename):
  diff = diff_proportions_sum_squared
  csv_dicts = []
  no_start_time = 0
  count = 0
  for json_dict in json_list:
    count += 1
    sys.stdout.write('Input row processed: '  + str(count) + '\r')
    sys.stdout.flush()
    for breakdown in json_dict["longTasksBreakdowns"]:
      duration = breakdown["duration"]
      subtasks = breakdown["subtasks"]
      start_time = breakdown["start"]
      if start_time is None:
        no_start_time += 1
      return_dict = {'duration': duration, 'start_time': start_time}
      if (len(subtasks) > 0 and start_time is not None):
        # print_debug_info(breakdown)
        pp = get_perfect_proportions(breakdown)
        first_subtask_length = subtasks[0]["totalTime"]
        first_subtask_url = subtasks[0]["url"]
        all_subtasks_lengths_from_same_url = [task["totalTime"]
            for task in subtasks if task["url"] == first_subtask_url]
        first_subtask_url_duration = sum(all_subtasks_lengths_from_same_url, 0)
        total_function_call_duration = sum([t["totalTime"] for t in subtasks], 0)
        length_longest_subtask = max(t["totalTime"] for t in subtasks)
        sum_first_three = sum([t["totalTime"] for t in subtasks[:3]], 0)
        total_fcalls_at_least_12ms = sum([t["totalTime"] for t in subtasks
                                            if t["totalTime"] >= 12], 0)
        sum_first_three_12 = sum([t["totalTime"] for t in subtasks[:3]
                                  if t["totalTime"] >= 12], 0)
        num_urls = get_num_urls(subtasks)
        return_dict.update({
          'fsl': first_subtask_length,
          'fslTotal': first_subtask_url_duration,
          'numSubtasks': len(subtasks),
          'fcTotal': total_function_call_duration,
          'pattern': get_pattern(breakdown),
          'lengthLongestSubtask': length_longest_subtask,
          'sumFirstThree': sum_first_three,
          'fcTotal12': total_fcalls_at_least_12ms,
          'sumFirstThree12': sum_first_three_12,
          'numUrls': num_urls,
          'error_first_n_3': diff(pp,
                                    get_first_n_proportions(breakdown, 3)),
          'error_first_n_2': diff(pp,
                                    get_first_n_proportions(breakdown, 2)),
          'error_first_n_1': diff(pp,
                                    get_first_n_proportions(breakdown, 1))
        })
        for interval in [1, 8, 16, 24, 32, 40, 50]:
          for start_time in [8, 16, 24, 32, 40, 50]:
            key = 'error_start{0}_interval{1}'.format(start_time, interval)
            return_dict[key] = diff(
              pp, get_sampling_proportions(breakdown, start_time, interval))
        csv_dicts.append(defaultdict(lambda: None, return_dict))

  print "Longs tasks with no start time: ", no_start_time
  write_csv_dicts(csv_dicts, output_filename)

def get_sampling_erorrs(breakdown):
  return_dict = {}
  perfect_proportions = get_perfect_proportions(breakdown)
  for interval in xrange(1, 2):
    for start_time in xrange(0, 10, 5):
      key = "samplingErrorSumSquared_" + str(start_time) + '_' + str(interval)
      sampling_proportions = get_sampling_proportions(breakdown,
                                                      start_time, interval)
      diff = diff_proportions_sum_squared(perfect_proportions,
                                          sampling_proportions)
      return_dict[key] = diff
  return return_dict

def get_mean_sampling_errors(breakdowns):
  error_data = []
  breakdown_to_pp = {}
  for i, breakdown in enumerate(breakdowns):
    breakdown_to_pp[i] = get_perfect_proportions(breakdown)

  total = 7 * 6
  done = 0

  for interval in [1, 8, 16, 24, 32, 40, 50]:
    for start_time in [8, 16, 24, 32, 40, 50]:
      sampling_errors = []
      for i, breakdown in enumerate(breakdowns):
        sampling_proportions = get_sampling_proportions(breakdown,
                                                        start_time, interval)
        diff = diff_proportions_sum_squared(breakdown_to_pp[i],
                                            sampling_proportions)
        sampling_errors.append(diff)
      error_data.append({
        'error' : sum(sampling_errors) / len(sampling_errors),
        'interval' : interval,
        'start_time' : start_time
      })
      done += 1
      print "Done: ", done
      print "Progress: ", float(done) / total * 100
  return error_data

def get_mean_prop_error(breakdowns, test_prop_func):
  sampling_errors = []
  for breakdown in breakdowns:
    perfect_prop = get_perfect_proportions(breakdown)
    test_prop = test_prop_func(breakdown)
    diff = diff_proportions_sum_squared(perfect_prop, test_prop)
    sampling_errors.append(diff)
  return sum(sampling_errors) / len(sampling_errors)

def write_csv_dicts(csv_dicts_list, output_filename):
  if len(csv_dicts_list) == 0:
    print "Empty csv dicts. No csv written"
    return

  with open(output_filename, 'w') as f:
    writer = csv.DictWriter(f, csv_dicts_list[0].keys())
    writer.writeheader()
    for row in csv_dicts_list:
      writer.writerow(row)

  print "Wrote " + str(len(csv_dicts_list)) + " entires to " + \
    output_filename + "."

def get_long_tasks_with_scripts(json_list):
  long_task_breakdowns = []
  for json_dict in json_list:
    for breakdown in json_dict['longTasksBreakdowns']:
      subtasks = breakdown["subtasks"]
      start_time = breakdown["start"]
      if len(subtasks) > 0 and start_time is not None:
        long_task_breakdowns.append(breakdown)

  return long_task_breakdowns

def write_mean_sampling_error_csv(json_list, output_filename):
  long_task_breakdowns = get_long_tasks_with_scripts(json_list)
  sampling_errors = get_mean_sampling_errors(long_task_breakdowns)
  write_csv_dicts(sampling_errors, output_filename)

def write_tall_sampling_errors_csv(long_tasks, output_filename):
  csv_dicts = []
  diff = diff_proportions_sum_squared
  for lt in long_tasks:
    pp = get_perfect_proportions(lt)
    csv_dicts.append({
      'error': diff(pp, get_sampling_proportions(lt, 16, 16)),
      'sampling_strategy': 'sampling_start16_interval16',
    })
    csv_dicts.append({
      'error': diff(pp, get_first_n_proportions(lt, 3)),
      'sampling_strategy': 'first_three',
    })
    csv_dicts.append({
      'error': diff(pp, get_first_n_proportions(lt, 2)),
      'sampling_strategy': 'first_two',
    })
    csv_dicts.append({
      'error': diff(pp, get_first_n_proportions(lt, 1)),
      'sampling_strategy': 'first_one',
    })
  write_csv_dicts(csv_dicts, output_filename)

def factorize_urls(long_task):
  url_to_factors = {NOT_SCRIPT: NOT_SCRIPT}
  for st in long_task['subtasks']:
    if st['url'] not in url_to_factors:
      url_to_factors[st['url']] = 'URL '  + str(len(url_to_factors))
  return url_to_factors

def normalize_props(url_to_factors, prop):
  """Takes a prop. All urls are turned into numbered factors. All urls in
  url_to_factors are present in the return prop, with value 0 if not present in
  original prop.
  """
  transformed = {}
  for k, v in url_to_factors.iteritems():
    transformed[v] = prop[k]
  return transformed

def get_examples_at_percentile(long_tasks, prop_func, comp_prop_func,
                                 percentiles):
  return_dicts = []
  errors = []
  diff = diff_proportions_sum_squared
  for lt in long_tasks:
    pp = get_perfect_proportions(lt)
    test_prop = prop_func(lt)
    error = diff(pp, test_prop)
    errors.append({'error': error, 'long_task': lt})

  errors = sorted(errors, key=lambda x: x['error'])
  for p in percentiles:
    i = int(math.floor(p * (len(errors) - 1)))
    long_task = errors[i]['long_task']
    url_to_factors = factorize_urls(long_task)
    t = lambda p: normalize_props(url_to_factors, p)
    # TODO: We should also plumb the entire long task.
    return_dicts.append({
      'percentile': p,
      'perfect_proportions': t(get_perfect_proportions(long_task)),
      'approximate_proportions': t(prop_func(long_task)),
      'comparative_proportions': t(comp_prop_func(long_task)),
      'sum_squared_error': errors[i]['error']
    })
  return return_dicts

def write_error_percentile_json(long_tasks, output_filename):
  percentile_examples = [
    {
      'test_approximation': 'First Three',
      'comparative_approximation': 'Sampling_Start16_Interval_16',
      'examples': get_examples_at_percentile(long_tasks,
          lambda b: get_first_n_proportions(b, 3),
          lambda b: get_sampling_proportions(b, 16, 16),
          [0, .25, .50, .75, .90, 1])
    }
  ]

  with open(output_filename, 'w') as f:
    json.dump(percentile_examples, f)

  print "Wrote output to ", output_filename

def main():
  # TODO(dproy): It may eventually make sense to use a real argument parser.
  if len(sys.argv) < 2:
    print "Usage: {0} <ctp-results> [output-filename]".format(sys.argv[0])
    print "<ctp-results> is the results file produced by chrome trace processor."
    print "[output-filename].json is the produced json file. Defaults to out.json."

  input_filename = sys.argv[1]
  if len(sys.argv) > 2:
    output_filename_prefix = sys.argv[2]
  else:
    output_filename_prefix = "out"

  result_json_list = load_json_results_from_file(input_filename)
  cleaned_json_list = process_json_results(result_json_list)
  long_tasks = get_long_tasks_with_scripts(cleaned_json_list)

  # write_json_list(cleaned_json_list, output_filename_prefix + '.csv')
  write_csv(cleaned_json_list, output_filename_prefix + '.csv')
  # write_mean_sampling_error_csv(cleaned_json_list, "sampling_errors.csv")
  # write_error_percentile_json(long_tasks, "error_percentile_examples.json");
  # write_tall_sampling_errors_csv(long_tasks, "tall_sampling_errors.csv")

  print "long tasks with scripts: ", len(long_tasks)
  print "Total results processed:", len(result_json_list)

if __name__ == "__main__":
  main()
