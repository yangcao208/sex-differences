library(tidyverse)
library(magrittr)
library(scales)

cis = readxl::read_excel('C:/Users/yangc/OneDrive/Desktop/F11-traits.xlsx')

cis %<>% mutate(x='',
                stars = case_when(pvalue < 1e-3 ~ '***',
                                  pvalue < 1e-2 ~ '**',
                                  pvalue < 0.05 ~ '*'))

ggplot(cis, aes(x=x, y=fct_reorder(trait_name, desc(pvalue)), fill=bicor)) +
  geom_tile(color='black') +
  geom_text(aes(label=stars)) +
  scale_fill_gradient2(low = 'blue', high = 'red') +
  scale_x_discrete(expand = c(0, 0)) +
  xlab('') +
  ylab('') +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        legend.key.width=unit(1, "mm"))

#ggsave('C:/Users/yangc/OneDrive/Desktop/ascc2-trait.5x4.20210113.pdf', height=4, width=5)
#ggsave('C:/Users/yangc/OneDrive/Desktop/ascc2-trait.4x4.20210113.pdf', height=4, width=4)
#ggsave('C:/Users/yangc/OneDrive/Desktop/ascc2-trait.3.5x4.20210113.pdf', height=4, width=3.5)
ggsave('C:/Users/yangc/OneDrive/Desktop/F11-trait.3.75x4.20210113.pdf', height=4, width=3.75)
ggsave('C:/Users/yangc/OneDrive/Desktop/F11-trait.3.6x4.20210113.pdf', height=4, width=3.6)
