rm(list=ls())
library(tidyverse)
setwd('/usr/local/google/home/dproy/code/long_tasks_analysis/')

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



df <- read_csv("out.csv")
  # filter(!is.na(fsl)) %>%
  # mutate(fslPerc = fsl / duration, fslTotalperc = fslTotal / duration)

sampling_error_df <- read_csv("sampling_errors.csv")
tall_err_df <- read_csv("tall_sampling_errors.csv")

## Computing stats
num_total_long_tasks = length(df$duration)
df_with_scripts <- df %>% filter(numSubtasks > 0) 
# Same thing as above. Just making sure.
df_with_no_na_fsl <- df %>% filter(!is.na(fsl))
stopifnot(length(df_with_no_na_fsl$duration) == length(df_with_scripts$duration))
num_long_tasks_with_function_call = length(df_with_scripts$duration)

## Plotting stuff
max_num_subtask = 15
mns_string = paste(">", max_num_subtask, sep = "")
max_num_urls = 10
mnu_string = paste(">", max_num_urls, sep = "")

plot_df <- df_with_scripts %>%
  mutate(fslPerc = fsl / duration,
         fslTotalperc = fslTotal / duration,
         fcTotalperc = fcTotal / duration,
         enoughScripts = fcTotal > 10,
         firstThreeOverTotal = sumFirstThree / fcTotal,
         firstThreeOverTotal12 = sumFirstThree12 / fcTotal12,
         boundedNumSubtask = factor(ifelse(numSubtasks > max_num_subtask, mns_string, numSubtasks),
                                    levels = c(seq(1, max_num_subtask), mns_string)),
         boundedNumUrls = factor(ifelse(numUrls > max_num_urls, mnu_string, numUrls),
                                levels = c(seq(1, max_num_urls), mnu_string)))

## Computing more stats
# Greater than 90% attribution with 3 subtasks when there are more than 3 scripts
moreThanThreeSubtaskDf = plot_df %>% filter(numSubtasks > 3)
moreThanThreeSubtaskDfWithVeryLittleScript = moreThanThreeSubtaskDf %>%
  filter(pattern == "VeryLittleScript")
percVeryLittleScriptWithMoreThanThreeSubtasks = percent(
  nrow(moreThanThreeSubtaskDfWithVeryLittleScript) / nrow(plot_df))

# threeSubtaskMoreThan90_12 = nrow(filter(moreThanThreeSubtaskDf, firstThreeOverTotal12 > .75)) / nrow(moreThanThreeSubtaskDf)
# 
# threeSubtaskMoreThan90 = percent(
#   nrow(filter(moreThanThreeSubtaskDf, firstThreeOverTotal > .75)) / nrow(moreThanThreeSubtaskDf))



p1 <- ggplot(data=plot_df, mapping = aes(x = fslTotalperc)) +
  stat_bin(mapping = aes(y=..ncount.., fill=..ncount..), binwidth=0.01, fill = "#FF6666") +
  stat_ecdf()

p2 <- ggplot(data=plot_df, mapping = aes(x = fslPerc)) +
  stat_bin(mapping = aes(y=..ncount.., fill=..ncount..), binwidth=0.01, fill = "#FF6666") +
  stat_ecdf()


df_bounded_numsubtask <- plot_df %>%
  mutate(boundedNumSubtask = factor(ifelse(numSubtasks > max_num_subtask, mns_string, numSubtasks),
                                    levels = c(seq(1, max_num_subtask), mns_string)))
numsubtask_bins = factor(df_bounded_numsubtask$boundedNumSubtask,
                         levels = c(seq(1, max_num_subtask), mns_string))

p3 <- ggplot(data=df_bounded_numsubtask, mapping = aes(x = numsubtask_bins)) +
  geom_bar(mapping = aes()) +
  geom_text(stat = "count",
            aes(y = ..count..,
                label = scales::percent((..count..)/sum(..count..))),
            vjust=-0.25) + 
  labs(x = "number of top level v8 function calls")

p4 <- ggplot(data=plot_df, mapping = aes(x = fcTotalperc)) +
  stat_bin(mapping = aes(y=..ncount.., fill=..ncount..), binwidth=0.01, fill = "#FF6666") +
  facet_wrap(~enoughScripts, nrow=2)

p5 <- ggplot(data=plot_df, mapping = aes(x = pattern)) +
  geom_bar(mapping = aes()) +
  geom_label(stat = "count",
             aes(y = ..count..,
                 label = scales::percent((..count..)/sum(..count..))),
             hjust=1.1) + 
  coord_flip()

p6 <- ggplot(data = filter(plot_df, pattern != 'VeryLittleScript'),
             mapping = aes(x = pattern)) +
  geom_bar(mapping = aes()) +
  geom_label(stat = "count",
             aes(y = ..count..,
                 label = scales::percent((..count..)/sum(..count..))),
             hjust=1.1) + 
  coord_flip()

# Maybe we can just put numsubtask in the data frame to fix this?
p7 <- ggplot(data=filter(df_bounded_numsubtask), mapping = aes(x = numsubtask_bins)) +
  geom_bar(mapping = aes()) +
  geom_label(stat = "count",
             aes(y = ..count..,
                 label = scales::percent((..count..)/sum(..count..))),
             vjust=-0.25)

# Length of longest subtask
p8 <- ggplot(data = filter(plot_df, numSubtasks > 3)) + 
  geom_histogram(mapping = aes(x = lengthLongestSubtask, y = ..count..),
                 binwidth = 2) + 
  scale_x_continuous(breaks = seq(0, 350, by=10),
                     limits = c(0, 350))

# Histogram of durations
p9 <- ggplot(data = filter(plot_df)) + 
  geom_histogram(mapping = aes(x = duration, y = ..density..),
                 binwidth = 1) + 
  # geom_density(aes(duration), color = "#FF0000", alpha = 0.1) +
  scale_x_continuous(breaks = seq(50, 1000, by=100),
                     limits = c(0, 1000))

# Proportion of data
p10 <- ggplot(data = filter(moreThanThreeSubtaskDf,
                            pattern != 'VeryLittleScript'),
              mapping = aes(x = firstThreeOverTotal, y = ..count..)) + 
  geom_histogram(mapping = aes(x = firstThreeOverTotal, y = ..count..),
                 binwidth = 0.1, center = 0.05) + 
  stat_bin(geom = "text",
           aes(label = scales::percent((..count..)/sum(..count..))),
           binwidth = 0.1,
           boundary = 1,
           vjust=-0.5) +
  scale_x_continuous(breaks = seq(0, 1, by=0.1))

p11 <- ggplot(data=moreThanThreeSubtaskDf,
              mapping = aes(x = pattern)) +
  geom_bar(mapping = aes()) +
  geom_label(stat = "count",
             aes(y = ..count..,
                 totalRows = nrow(plot_df),
                 label = scales::percent((..count..)/totalRows),
             hjust=0.6)) + 
  coord_flip()

p12 <- ggplot(data = filter(moreThanThreeSubtaskDf,
                            pattern != 'VeryLittleScript',
                            firstThreeOverTotal < 0.1),
              mapping = aes(x = lengthLongestSubtask, y = ..count..)) + 
  geom_histogram(mapping = aes(x = lengthLongestSubtask, y = ..count..),
                 binwidth = 2) + 
  scale_x_continuous(breaks = seq(0, 350, by=50),
                     limits = c(0, 350))

p13 <- ggplot(data=plot_df, mapping = aes(x = boundedNumUrls)) +
  geom_bar(mapping = aes()) +
  geom_text(stat = "count",
            aes(y = ..count..,
                label = scales::percent((..count..)/sum(..count..))),
            vjust=-0.25) + 
  labs(x = "number of unique urls in toplevel v8 function calls")

p14 <- ggplot(data=plot_df, mapping = aes(x = fcTotalperc)) +
  geom_histogram(mapping = aes(y=..ncount..), binwidth=0.05) + 
  geom_text(stat = "bin",
            aes(y = ..ncount..,
                label = scales::percent((..count..)/sum(..count..))),
            binwidth = 0.05,
            vjust=-0.25)

p15 <- ggplot(data=plot_df, mapping = aes(x = samplingErrorSumSquared_0_8)) +
  geom_histogram(mapping = aes(y=..ncount..), binwidth=0.01) + 
  geom_density()
  # geom_text(stat = "bin",
  #           aes(y = ..ncount..,
  #               label = scales::percent((..count..)/sum(..count..))),
  #           binwidth = 0.05,
  #           vjust=-0.25)

p16 <- ggplot(data=plot_df, mapping = aes(x = samplingErrorSumSquared_50_8)) +
  geom_histogram(mapping = aes(y=..ncount..), binwidth=0.01) + 
  geom_density(bw = 0.01)
# geom_text(stat = "bin",
#           aes(y = ..ncount..,
#               label = scales::percent((..count..)/sum(..count..))),
#           binwidth = 0.05,
#           vjust=-0.25)

#  facet_wrap(~boundedNumUrls)
p17 <- ggplot(data=plot_df, mapping = aes(y = samplingErrorSumSquared_0_8, x = 1)) +
  geom_jitter()

p18 <- ggplot(data=sampling_error_df, mapping = aes(x = interval, y = error, color = as.factor(start_time)))  +
  geom_line() + 
  geom_point() +
  labs(color = "sampling start time") + 
  ylab("mean sum squared error") + 
  scale_x_continuous(breaks = c(1, 8, 16, 24, 32, 40, 50))



p19 <- ggplot(tall_err_df, aes(sampling_strategy, error)) + 
  geom_violin()

get_sampling_error_keys <- function() {
  start_times <- c(8, 16, 24, 32, 40, 50)
  intervals <- c(1, 8, 16, 24, 32, 40, 50)
  keys <- c()
  for (s in start_times) {
    keys <- c(keys, paste("error_start", s, "_interval", intervals, sep = ""))
  }
  return(keys)
}

sampling_error_keys <- get_sampling_error_keys()
first_n_error_keys <- str_c("error_first_n_", c(1, 2, 3))

error_keys = c(sampling_error_keys, first_n_error_keys)

error_gathered_df <- plot_df %>%
  gather(key = "approx_strategy", value = "error",
         !!error_keys, factor_key = TRUE)

sampling_param_pattern = "^error_start([\\d]+)_interval([\\d]+)$"

sampling_err_df <- error_gathered_df %>%
  filter(approx_strategy %in% sampling_error_keys) %>%
  tidyr::extract(approx_strategy, c("start", "interval"),
                 sampling_param_pattern, convert = TRUE)

first_n_display <- list(
  error_first_n_1 = "First One",
  error_first_n_2 = "First Two",
  error_first_n_3 = "First Three"
)

first_n_err_df <- error_gathered_df %>%
  filter(approx_strategy %in% first_n_error_keys) %>%
  mutate(
    n_value = factor(sapply(approx_strategy,
                            function(x) { first_n_display[[as.character(x)]] }),
                     levels = unlist(first_n_display, use.names = FALSE))
  )

# Use ..._by_pattern further below if you want to facet wrap by pattern.
summary_sampling_err_df <- sampling_err_df %>%
  group_by(start, interval) %>%  
  summarize(
    mean_error = mean(error),
    median_error = median(error),
    error_90th = quantile(error, 0.9),
    error_nth = quantile(error, 0.8)
  )

summary_sampling_err_df_by_pattern <- sampling_err_df %>%
  group_by(start, interval, pattern) %>%
  summarize(
    mean_error = mean(error),
    median_error = median(error),
    error_90th = quantile(error, 0.9),
    error_nth = quantile(error, 0.8)
  )

summary_first_n_err_df <- first_n_err_df %>%
  group_by(n_value) %>%
  summarize(
    mean_error = mean(error),
    meadian_error = median(error),
    error_90th = quantile(error, 0.9),
    error_nth = quantile(error, 0.8)
  )

summary_first_n_err_df_by_pattern <- first_n_err_df %>%
  group_by(n_value, pattern) %>%
  summarize(
    mean_error = mean(error),
    meadian_error = median(error),
    error_90th = quantile(error, 0.9),
    error_nth = quantile(error, 0.8)
  )

p21 <- ggplot(data=summary_sampling_err_df, mapping = aes(x = interval, y = mean_error, color = as.factor(start)))  +
  geom_line() +
  geom_point() +
  labs(color = "Sampling Start Time",
       linetype = "First N Strategy") +
  ylab("median sum squared error") +
  scale_x_continuous(breaks = c(1, 8, 16, 24, 32, 40, 50)) + 
  scale_color_brewer(palette = "Dark2")
  # geom_hline(data = summary_first_n_err_df,
  #            mapping = aes(yintercept = mean_error, linetype = n_value),
  #            color = "#00AAFF")
  # facet_wrap(~pattern)


p22 <- ggplot(data=sampling_err_df, mapping = aes(x = interval, y = error, color = as.factor(start)))  +
  geom_line(stat="summary", fun.y=mean) +
  geom_point(stat="summary", fun.y=mean) +
  labs(color = "Sampling Start Time",
       linetype = "First N Strategy") +
  ylab("mean sum squared error") +
  scale_x_continuous(breaks = c(1, 8, 16, 24, 32, 40, 50)) + 
  scale_color_brewer(palette = "Dark2") +
  geom_hline(data = summary_first_n_err_df_by_pattern,
             mapping = aes(yintercept = mean_error, linetype = n_value),
             color = "#00AAFF")

p23 <- ggplot(data=summary_sampling_err_df, mapping = aes(x = interval, y = mean_error, color = as.factor(start)))  +
  geom_line() +
  geom_point() +
  labs(color = "Sampling start time (ms)",
       linetype = "First N Strategy") +
  ylab("Mean sum squared error") +
  xlab("Sampling interval (ms)") + 
  scale_x_continuous(breaks = c(1, 8, 16, 24, 32, 40, 50)) + 
  scale_color_brewer(palette = "Dark2") + 
  labs(title = "Mean approximation errors for sampling approaches")

p24 <- ggplot(data=summary_sampling_err_df, mapping = aes(x = interval, y = median_error, color = as.factor(start)))  +
  geom_line() +
  geom_point() +
  labs(color = "Sampling start time (ms)",
       linetype = "First N Strategy") +
  ylab("Median sum squared error") +
  xlab("Sampling interval (ms)") + 
  scale_x_continuous(breaks = c(1, 8, 16, 24, 32, 40, 50)) + 
  scale_color_brewer(palette = "Dark2") + 
  labs(title = "Median approximation errors for sampling approaches")

p25 <- ggplot(data=summary_sampling_err_df, mapping = aes(x = interval, y = error_90th, color = as.factor(start)))  +
  geom_line() +
  geom_point() +
  labs(color = "Sampling start time (ms)") + 
  ylab("90th percentile of sum squared error") +
  xlab("Sampling interval (ms)") + 
  scale_x_continuous(breaks = c(1, 8, 16, 24, 32, 40, 50)) + 
  scale_color_brewer(palette = "Dark2") + 
  labs(title = "90th percentile approximation errors for sampling approaches")

p26 <- ggplot(data=summary_sampling_err_df_by_pattern, mapping = aes(x = interval, y = mean_error, color = as.factor(start)))  +
  geom_line() +
  geom_point() +
  labs(color = "Sampling start time (ms)") + 
  ylab("Mean sum squared error") +
  xlab("Sampling interval (ms)") + 
  scale_x_continuous(breaks = c(1, 8, 16, 24, 32, 40, 50)) + 
  scale_color_brewer(palette = "Dark2") + 
  labs(title = "Mean approximation errors for sampling approaches by pattern") + 
  facet_wrap(~pattern)

# p20 <- ggplot(gathered_df, aes(sampling_strategy, error)) +
#   geom_violin() +
#   scale_y_log10(breaks=c(1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1, 2))
# # facet_wrap(~pattern)

p27 <- ggplot(data=first_n_err_df, mapping = aes(x = n_value, y = error))+
  geom_violin() + 
  scale_y_log10(breaks=c(1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1, 2, 4, 8)) + 
  ylab("Error (log scaled axis)") + 
  labs(title = "Distribution of errors for first-n approximations by pattern") + 
  facet_wrap(~pattern)


p28 <- ggplot(data=summary_sampling_err_df_by_pattern, mapping = aes(x = interval, y = mean_error, color = as.factor(start)))  +
  geom_line() +
  geom_point() +
  labs(color = "Sampling Start Time",
       linetype = "First N Strategy") +
  ylab("median sum squared error") +
  scale_x_continuous(breaks = c(1, 8, 16, 24, 32, 40, 50)) + 
  scale_color_brewer(palette = "Dark2") + 
  geom_hline(data = summary_first_n_err_df_by_pattern,
           mapping = aes(yintercept = mean_error, linetype = n_value),
           color = "#00AAFF") + 
  facet_wrap(~pattern)

direct_comparison_df = plot_df %>% 
  select(error_start16_interval16, error_first_n_3, pattern) %>%
  gather(approximation, error,
         error_start16_interval16, error_first_n_3)

p29 <- ggplot(data=direct_comparison_df, mapping = aes(x = approximation, y = error))+
  geom_violin() + 
  scale_y_log10(breaks=c(1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1, 2, 4, 8)) + 
  ylab("Sum squared error (log scaled axis)") + 
  xlab("Approximation") +
  labs(title = "-----")
  # facet_wrap(~pattern)


# printing the error table. 
# mean(plot_df$error_first_n_1)
# mean(plot_df$error_first_n_2)
# mean(plot_df$error_first_n_3)
# median(plot_df$error_first_n_3)
# median(plot_df$error_first_n_2)
# median(plot_df$error_first_n_1)
# median(plot_df$error_first_n_1)
# quantile(plot_df$error_first_n_1, 0.9)
# quantile(plot_df$error_first_n_2, 0.9)
# quantile(plot_df$error_first_n_3, 0.9)

# ggsave("first_n_violin_by_pattern.png", p27)
# ggsave("sampling_error_graph_mean_by_pattern.png", p26)
# ggsave("sampling_error_graph_90th.png", p25)
# ggsave("sampling_error_graph_median.png", p24)
# ggsave("sampling_error_graph_mean.png", p23)
# ggsave("approx_error_facet_pattern.png", p22)
# ggsave("approx_error_all.png", p21)
# ggsave("sampling_error.png", p18)
# ggsave("num_toplevel_urls.png", p13)
# ggsave("zoom_multi_more_than_three_first_bin.png", p12)
# ggsave("some_of_first_three_over_total.png", p10)
# ggsave("patterns_with_more_than_three_subtasks.png", p11)
# ggsave("distribution_of_longest_subtask_length.png", p8)
# ggsave("num_toplevel_subtasks.png", p3)
# ggsave("distribution_of_long_task_patterns-all.png", p5)
# ggsave("distribution_of_long_task_patterns-significant_script.png", p6)
