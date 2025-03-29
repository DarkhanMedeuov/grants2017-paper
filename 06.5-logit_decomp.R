library(Decomp)
library(stringr)
library(dplyr)
library(magrittr)
library(rlang)
library(purrr)
library(CovTools)
library(colorDF)
library(tidyr)


grants_def %>% 
  logit_decomposition(y = win, x = hirsh, score, independent = TRUE)

library(stringr)

.data = grants_def
y = grants_def$win
x = grants_def$sex
score = grants_def$score
.data %<>% rename(win = rlang::enquos(y) %>% paste() %>% str_remove("~"), 
                  sex = rlang::enquos(x) %>% paste() %>% str_remove("~"))

formula <- paste("y ~ x + ", enquos(score) %>% paste() %>% 
                     str_remove("~") %>% paste(collapse = "+")) %>% 
  as.formula()
formula <- formula(win ~ sex + score)

reg_full <- glm(formula, data = .data, family = binomial(link = "logit"))
formula <- "y ~ x" %>% as.formula()
reg_base <- glm(win ~ sex, data = .data, family = binomial(link = "logit"))
aux <- 
  rlang::enquos(...) %>% paste() %>% stringr::str_remove("~") %>% 
  purrr::map(~{
    paste(.x, " ~ x") %>% as.formula() %>% lm(data = .data)
    })
lm(cbind(score, hirsh, scopus) ~ sex, data = .data) %>% summary()



lf <- aux %>% length()
res_df <- aux %>% purrr::map(~{
  .x$residuals
  }) %>% 
  as.data.frame()
  names(res_df) <- paste("res_", 1:lf, sep = "")
  .data %<>% bind_cols(res_df)
  formula <- paste("y ~ x +", paste("res_", 1:lf, collapse = "+", 
                                    sep = "")) %>% as.formula()
  reg_RE <- glm(formula, data = .data, family = binomial(link = "logit"))
  aux_2 <- paste("res_", 1:lf, sep = "") %>% purrr::map(~{
    paste(.x, " ~ x + y", sep = "") %>% as.formula() %>% 
      lm(data = .data)
  })
  aux_coeffs_1 <- aux_2 %>% purrr::map(~{
    .x$coefficients[2]
  }) %>% unlist()
  aux_coeffs_2 <- aux_2 %>% purrr::map(~{
    .x$coefficients[3]
  }) %>% unlist()
  inverse <- as.matrix(.data %>% dplyr::select(starts_with("res_"))) %>% 
    CovTools::PreEst.glasso(parallel = TRUE, method = list(type = "BIC", 
                                                           param = c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000)))
  a <- c()
  for (i in (1:lf)) {
    i_list <- Map("*", inverse$C[, i][-i], aux_coeffs_2[1:lf][-i])
    i_list %<>% Reduce("+", .) %>% unlist()
    a <- c(a, i_list)
  }
  diag <- (reg_RE$coefficients[3:(2 + lf)]/aux_coeffs_2[1:lf]) - 
    (a/aux_coeffs_2)
  if (independent == TRUE) {
    diag <- (reg_RE$coefficients[3:(2 + lf)]/aux_coeffs_2[1:lf])
  }
  uncorr <- (aux_coeffs_1 * aux_coeffs_2 * diag) + aux_coeffs_2 * 
    (reg_RE$coefficients[3:(2 + lf)] - (diag * aux_coeffs_2))
  aux_coeffs <- aux %>% purrr::map(~{
    .x$coefficients[2]
  }) %>% unlist()
  decomp <- tibble(variable = rlang::enquos(...) %>% paste() %>% 
                     stringr::str_remove("~"), corr_part = aux_coeffs * reg_full$coefficients[3:(2 + 
                                                                                                   lf)], uncorr_part = uncorr)
  decomp %<>% bind_rows(decomp %>% summarise(corr_part = sum(corr_part), 
                                             uncorr_part = sum(uncorr_part)) %>% mutate(variable = "sum")) %>% 
    dplyr::select(variable, corr_part, uncorr_part)
  decomp %<>% mutate(sum = corr_part + uncorr_part)
  total_bias <- reg_base$coefficients[2] - reg_full$coefficients[2]
  correlated_bias <- reg_RE$coefficients[2] - reg_full$coefficients[2]
  uncorrealted_bias <- reg_base$coefficients[2] - reg_RE$coefficients[2]
  decomp %<>% bind_rows(c(correlated_bias, uncorrealted_bias, 
                          total_bias) %>% as_tibble() %>% dplyr::rename(Biases = "value") %>% 
                          tibble::rownames_to_column() %>% pivot_longer(-rowname) %>% 
                          pivot_wider(names_from = rowname, values_from = value) %>% 
                          setNames(names(decomp)))
  return(print(colorDF::colorDF(decomp, theme = "wb"), cat(crayon::bgCyan("Decomposition of logit model"))))

