setwd('E:/Box Sync/Projects/Contextual Cueing')
setwd('C:/Users/Gavin/Box Sync/Projects/Contextual Cueing/ContextualCueingNew_Expt2/Data')

library(tidyverse)
library(ez)
library(broom)
library(ggplot)
library(dylpr)


## Functions

nzmean <- function(x) {
  zvals <- x==0
  if (all(zvals)) 0 else mean(x[!zvals])
}


# get all files 
all_files = list.files(pattern=".csv")
all_data <- do.call(rbind, lapply(all_files, read.csv, header=TRUE))

# get accuracy 
# get list of bad subjects <.9 accuracy

descrip <- all_data %>%
  mutate(hit = ifelse(Error == 0, 1, 0)) %>%
  group_by(sub_id) %>%
  summarise(accuracy = mean(hit))

bad_subs <- (descrip %>%
               filter(accuracy < .9))$sub_id


# make df with subject data
# 5 epochs
# exclude errors
# remove bad subjects

subject_df <- all_data %>%
  mutate(epoch = ceiling(block/5)) %>%
  filter(!is.element(sub_id, bad_subs)) %>%
  filter(Error == 0) %>%
  group_by(repeat., d_setsize, epoch, sub_id) %>%
  summarise(rt = mean(RT))

# clean data
# remove outliers (2.5 SD)
# remove errors
# remove bad subjects
# change display repeat from 1 0 to new old

clean_data <- all_data %>%
  filter(Error == 0) %>%
  mutate(display = if_else(repeat. ==0, 'new', 'old')) %>%
  group_by(display, d_setsize, sub_id) %>%
  mutate(upperlimit = mean(RT) + 2.5*sd(RT), lowerlimit = mean(RT) - 2.5*sd(RT)) %>%
  mutate(outlier = RT > upperlimit | RT < lowerlimit) %>%
  filter(outlier == FALSE) %>%
  mutate(epoch = ceiling(block/5))

clean_df <- clean_data %>% 
  group_by(display, d_setsize, epoch, sub_id) %>%
  summarise(rt = mean(RT))


## Write data to excel
write.xlsx(data.frame(clean_df), 'clean_df.xlsx')

# make stuff factors for ANOVA/plotting
clean_df$epoch <- as.factor(clean_df$epoch)
clean_df$display <- as.factor(clean_df$display)
clean_df$d_setsize <- as.factor(clean_df$d_setsize)

## do ANOVA
ezANOVA(clean_df,
        dv = rt,
        wid = sub_id,
        within = .(d_setsize, display, epoch))

# make plot
plot_df <- clean_df %>%
  group_by(epoch, d_setsize, display) %>%
  summarise(rt = mean(rt))
plot_df$epoch <- as.factor(plot_df$epoch)

ggplot(plot_df %>% filter(d_setsize == 23), aes(x=epoch, y=rt, color = display, group=display)) + 
  geom_point(size=2) +
  geom_line(size=2) +
  xlab("Epoch") +
  ylab("RT") +
  scale_y_continuous(limits=c(400, 1400)) +
  ggtitle("23 Lures")


## Plot for overall RT data across set sizes

overall_plot_df <- clean_df %>%
  group_by(display, d_setsize) %>%
  summarise(rt = mean(rt)) 
# mutate(display = if_else(repeat. == 0, "new", "old"))

overall_plot_df$display <- as.factor(overall_plot_df$display)
overall_plot_df$d_setsize <- as.numeric(as.character(((overall_plot_df$d_setsize))))

ggplot(overall_plot_df %>%filter(d_setsize>1), aes(x=d_setsize, y=rt, color=display, group=display)) +
  geom_point(size=2) +
  geom_line(size=2) +
  xlab("Setsize") +
  ylab("RT")+
  # scale_y_continuous(limits=c(400, 1400)) +
  ggtitle("Overall Plot")

##################### OTHER ANALYSES

## display effects?

old_df <- clean_data %>%
  filter(display == 'old') %>%
  group_by(sub_id, tloc, d_setsize) %>%
  summarise(rt = mean(RT))

new_df <- clean_data %>%
  filter(display == 'new') %>%
  group_by(sub_id, d_setsize) %>%
  summarise(rt = mean(RT))

for (i in 1:nrow(old_df)){
  subid <- old_df$sub_id[i]
  dsetsize <- old_df$d_setsize[i]
  new_rt <- (new_df %>%
               filter(sub_id == subid, dsetsize == d_setsize))$rt
  old_df$new_rt[i] <- new_rt
  
}

old_df <- old_df %>%
  mutate(effect = as.integer(rt) - as.integer(new_rt)) %>%
  mutate(direction = effect<0)

showed_effect <- old_df %>%
  filter(direction>0)

for (i in nrow(showed_effect)){
  subid <- showed_effect$sub_id[i]
  tloc <- showed_effect$tloc[i]
  
  effect_displays <- clean_data %>%
    filter(sub_id == subid & tloc == tloc & repeat. == 1)
  
  
}
