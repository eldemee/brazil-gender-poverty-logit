library(tidyverse)
library(ggplot2)

#DF MODIFY
df <- readRDS('C:/Users/demet/Desktop/project/enade_2008_2018.rds')

df$TP_SEXO <- ifelse(df$TP_SEXO == 'M', 'Men', 'Women')
uni <- c("Universidade", "Universidade Especializada", "Centro Universitario")
facul <- c("Faculdade", "Faculdade ", "Faculdades Integradas", "Faculdade de Tecnologia")
ifed <- c("Centro Federal de Educacao Tecnologica",
          "Instituto Federal de Educacao, Ciencia e Tecnologia",
          "Instituto Superior ou Escola Superior")

df$ORGANIZACAO <- ifelse(df$ORGANIZACAO %in% uni, 'University',
                         ifelse(df$ORGANIZACAO %in% facul, 'College',
                                ifelse(df$ORGANIZACAO %in% ifed, 'Institute', 'Other')))
careers <- unique(df$GRUPO)

well_paid <- careers[
  grepl(
    "ENGENHARIA|COMPUTACAO|INFORMACAO|MEDICINA|ODONTOLOGIA|FARMACIA|
     BIOMEDICINA|ESTATISTICA|ECONOMICAS|CONTABEIS|ADMINISTRACAO|
     AGRONOMIA|VETERINARIA|PETROLEO|AERO|AUTOMACAO|REDES|LOGISTICA|
     FINANCEIRA|RADIOLOGIA",
    careers,
    ignore.case = TRUE
  )
]

df$well_paid <- ifelse(df$GRUPO %in% well_paid, 'well', 'poorly')
t <- table(df$TP_SEXO, df$well_paid)
p <- prop.table(t, margin = 1)

df_plot <- as.data.frame(p)
colnames(df_plot) <- c("Sex", "WellPaid", "Proportion")

ggplot(df_plot, aes(Sex, Proportion, fill=WellPaid)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Proportion", x = "Sex", fill = "Well paid") +
  ggtitle('Proportion of students enrolled in a well-paid career by gender') +
  theme_classic()
