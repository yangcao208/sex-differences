library(tidyverse)
library(gghighlight)
library(odbc)

cs = 'SERVER=10.14.48.13;DATABASE=HMDP;Trusted_Connection=Yes;DRIVER={ODBC Driver 13 for SQL Server}'
con = dbConnect(odbc(), .connection_string = cs)


# HFpEF male

res = readxl::read_excel('HFpEF_vs_Chow_deseq2_results_20200306.xlsx')

p1 = ggplot(res, aes(x=log2FoldChange, y=-log10(padj))) +
  geom_point() +
  gghighlight(abs(log2FoldChange) > 3 | -log10(padj) >= 10, use_direct_label = T, label_key = gene_symbol) +
  ggtitle('Chow vs. HFpEF (C57BL/6J)') +
  xlab(expression(log[2]("fold-change"))) +
  ylab(expression(log[10]("adjusted p-value"))) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))

ggsave('chow_vs_hfpef_male_volcano_20200306.pdf', plot=p1, width=6, height=6)


# HFpEF female
res = readxl::read_excel('HFpEF_vs_Chow_deseq2_results_20200202.xlsx')

p1 = ggplot(res, aes(x=log2FoldChange, y=-log10(padj))) +
  geom_point() +
  gghighlight(abs(log2FoldChange) > 3 | -log10(padj) >= 10, use_direct_label = T, label_key = gene_symbol) +
  ggtitle('Chow vs. HFpEF (C57BL/6J)') +
  xlab(expression(log[2]("fold-change"))) +
  ylab(expression(log[10]("adjusted p-value"))) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5))

ggsave('chow_vs_hfpef_volcano_20200202.pdf', plot=p1, width=6, height=6)



# hfHMDP

results_limma = data.table::fread('hfHMDP.heart.limma.diffexp.txt')
mitocarta = dbGetQuery(con, "select *
from TranscriptAnnotation.MitoCarta2")


library(ggrepel)
volcano2 = function(limma_results, subtitle, pval = 1e-20, logfc = 2, top=20){
  hl_colors = c('red', 'blue', 'black', '#bebebeaa') %>% purrr::set_names()
  limma_results %<>% arrange(adj.P.Val) %>%
    mutate(rank = 1:n(),
           color = ifelse(adj.P.Val <= pval, 'black', '#bebebeaa')) %>%
    arrange(desc(adj.P.Val))
  ggplot(limma_results, aes(x=logFC, y=-log10(adj.P.Val), color=color)) +
    geom_point() +
    geom_label_repel(data = dplyr::filter(limma_results, rank <= top),
                     aes(x=logFC, y=-log10(adj.P.Val), label=gene_symbol)) +
    scale_color_manual(values = hl_colors) +
    ggtitle(str_glue('high-fat HMDP heart differential expression ({subtitle})')) +
    xlab(expression(log[2]("fold-change"))) +
    ylab(expression(-log[10]("adjusted p-value"))) +
    guides(color=FALSE) +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))
}

p1 = volcano2(results_limma, 'F vs M', pval=.05)
ggsave('m:/projects/lusis/hfHMDP_heart_volcano_20200429.pdf', p1, width=6, height=6)

p1_mito = volcano2(results_limma %>% semi_join(mitocarta), 'F vs M', pval=.05)
ggsave('m:/projects/lusis/hfHMDP_heart_volcano_20200429.mito_genes.pdf', p1_mito, width=6, height=6)
