library(MASS)
library(readxl)
library(tidyverse)
# df <- read_excel('../data/report1567817814178.xlsx')
df <-
  read_excel('../data/Preliminary Berklee Audition and Ensemble Data vSep6.xlsx')
audition_vars <-
  df %>% select(starts_with('audition')) %>% colnames()
x_vars <- c("1", audition_vars, "scholarship_rating")
model <- paste0('out ~ ', paste0(x_vars, collapse=' + '))


df_c <- df[complete.cases(df[audition_vars]), ] %>% mutate_if(is.character, factor)
df_c$out <- factor(df_c$ensemble_overall)

model_glm <- glm(model, data=df_c, family=binomial(link='logit'))
df_c$pred <- predict(model_glm) %>% round()

olm <-
  polr(model, df_c, Hess = TRUE, method='probit')
df_c$pred <- predict(olm)


getPctCorrect <-
  function(df,
           col_predicted = 'pred',
           col_actual = 'out') {
    xtab <- table(df[[col_actual]], df[[col_predicted]])
    return(list(
      pct_correct = sum(diag(xtab)) / sum(xtab),
      deviations = (as.numeric(df[[col_predicted]]) - as.numeric(df[[col_actual]])) %>% factor %>% summary,
      table = xtab
    ))
  }

getPctCorrect(df_c)