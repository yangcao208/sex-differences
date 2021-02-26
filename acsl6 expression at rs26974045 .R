library(tidyverse)
library(odbc)

cs = 'SERVER=10.14.48.13;DATABASE=HMDP;Trusted_Connection=Yes;DRIVER={ODBC Driver 13 for SQL Server}'
con <- dbConnect(odbc::odbc(), .connection_string = cs)

acsl6 = dbGetQuery(con, "select *
from TranscriptAbundance.HeartFailureLeftVentricle_Isoproterenol_avg
where gene_symbol='Acsl6'") %>%
  gather(strain, value, -(probesetID:gene_symbol))

ggplot(acsl6, aes(x=fct_reorder2(strain, probesetID, value, .desc=F), y=value)) +
  geom_point() +
  xlab('') +
  ylab('Acsl6 expression') +
  facet_grid(probesetID~.) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, size=5, hjust=1, vjust=0.5))


ggplot(acsl6 %>% filter(probesetID=='ILMN_3125814'), aes(x=fct_reorder(strain, value), y=value)) +
  geom_point() +
  xlab('') +
  ylab('Acsl6 expression') +
  ggtitle('Acsl6 (isoproterenol treatment)') +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, size=5, hjust=1, vjust=0.5),
        plot.title = element_text(hjust=0.5))

