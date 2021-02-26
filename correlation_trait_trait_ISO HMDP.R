# correlation between mtDNA and Mitral inflow E to A velocity ratio (day 7) in ISO-HMDP

library(tidyverse)
library(odbc)

cs = 'SERVER=10.14.48.13;DATABASE=HMDP;Trusted_Connection=Yes;DRIVER={ODBC Driver 13 for SQL Server}'
con <- dbConnect(odbc(), .connection_string = cs)

traits = dbGetQuery(con, "select Mouse_number, strain, [mitochondrial copy number (heart)] as mt, [Mitral inflow E velocity (day 14)] as ea7
                    from ClinicalTraits.HeartFailure_matrix
                    where [mitochondrial copy number (heart)] is not null or [Mitral inflow E velocity (day 14)] is not null
                    ")

ggplot(traits %>% filter(!is.na(mt) & !is.na(ea7)), aes(x=mt, y=ea7)) +
  geom_point() +
  geom_smooth(method='lm', se=F) +
  xlab('mtDNA copy number') +
  ylab('') +
  ggtitle('Mitral inflow E velocity (day 14)') +
  theme_set(theme_gray(base_size = 20))

ggsave('mtDNA_vs_Mitral inflow E velocity (day 14).pdf', width=6, height=6)