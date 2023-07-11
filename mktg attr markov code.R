# install packages if necessary
#install.packages(dplyr)
#install.packages(stringr)
#install.packages(ggplot2)
#install.packages(reshape2)
#install.packages(ggrepel)
#install.packages(ChannelAttribution)

# load packages
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)
library(ggrepel)
library(ChannelAttribution)

# set sed for reproducibility
set.seed(923)

# define channels
channels <- c('A', 'B', 'C', 'D', 'E', 'F')

# create sample data
data <- data.frame(id = sample(c(1000:7500), 50000, replace = TRUE),
                   date = sample(c(1:185), 50000, replace = TRUE),
                   channel = sample(channels, 50000, replace = TRUE,
                                    prob = c(0.34, 0.24, 0.25, 0.27, 0.32, 0.28))) %>%
        mutate(date = as.Date(date, origin = "2023-01-01"),
               channel = paste0('channel_', channel))

# data summary
n_distinct(data$id)

data %>%
  group_by(id) %>% 
  summarize(n_touches=n()) %>% 
  ungroup() %>%
  group_by(n_touches) %>%
  summarise(n_paths = n()) %>% 
  ggplot(aes(x=n_touches, y=n_paths)) + 
    geom_col()

# convert to necessary format
data_mod <- data %>%
  arrange(id, date) %>%
  group_by(id) %>%
  summarise(path = paste(channel, collapse = ' > ')) %>%
  ungroup() %>%
  mutate(conv = sample(c(0:1), n(), replace = TRUE, 
                       prob = c(0.4, 0.3)),
         conv_null = ifelse(conv==1, 0, 1)) %>%
  ungroup()

# single touch, multi touch attribution
h_mod <- heuristic_models(data_mod, var_path = 'path', var_conv = 'conv')


# markov attribution
m_mod <- markov_model(data_mod, var_path = 'path', var_conv = 'conv', var_null = 'conv_null', out_more = TRUE)



# merge models
mod_combo <- merge(h_mod, m_mod$result, by = 'channel_name') %>%
  rename(linear_attr = linear_touch)

# visualizations 
# transition matrix heatmap 
trans_mtrx <- m_mod$transition_matrix %>%
  mutate(channel_from = case_when(channel_from == 1 ~ "channel A",
                                  channel_from == 2 ~ "channel B",
                                  channel_from == 3 ~ "channel C",
                                  channel_from == 4 ~ "channel D",
                                  channel_from == 5 ~ "channel E",
                                  channel_from == 6 ~ "channel F",
                                  channel_from == '(start)' ~ '(start)'),
         channel_to = case_when(channel_to == 1 ~ "channel A",
                                channel_to == 2 ~ "channel B",
                                channel_to == 3 ~ "channel C",
                                channel_to == 4 ~ "channel D",
                                channel_to == 5 ~ "channel E",
                                channel_to == 6 ~ "channel F",
                                channel_to == '(null)' ~ '(null)',
                                channel_to == '(conversion)' ~ '(conversion)'))

max_prob <- max(trans_mtrx$transition_probability)

ggplot(trans_mtrx, aes(y = channel_from, x = channel_to, fill = transition_probability)) +
  theme_minimal() +
  geom_tile(colour = "white", width = .9, height = .9)  +
  scale_fill_gradient2(low = "royalblue3", mid = "cornsilk", high = "darkorange", midpoint = max_prob/2) +
  geom_text(aes(label = round(transition_probability, 2)), fontface = "bold", size = 3.5) +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 0.5, vjust = 0.5)) +
  ggtitle("Transition Matrix")

# model comparison df
model_comp <- melt(mod_combo, id.vars = 'channel_name', variable.name = 'conv_type') %>%
  mutate(value = round(value))

# chart to compare attribution results
ggplot(model_comp, aes(x = conv_type, y = value, group = channel_name)) +
  geom_line(aes(color = channel_name), linewidth = 2.5, alpha = 0.8) +
  geom_point(aes(color = channel_name), size = 5) +
  geom_label_repel(aes(label = paste0(channel_name, ': ', value), fill = factor(channel_name)),
                   fontface = 'bold', color = 'white', size = 4,
                   box.padding = unit(0.25, 'lines'), point.padding = unit(0.5, 'lines'),
                   max.iter = 100) +
  theme(legend.position = 'none',
        plot.title = element_text(size = 20, face = "bold", vjust = 2, hjust = 0.5),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 16, hjust = 0.5, vjust = 0.5, face = "bold", color = 'black')) +
  labs(x = 'Attribution Type', y = 'Conversions') +
  ggtitle('Model Comparison') 

