library(lmtest)
library(cquad)
library(tidyverse)

fe_ologit <- function (data, y_var, x_vars, i_var, t_var) {
  data <- data %>% as_tibble()
  all_vars <- c("y", "k", y_var, x_vars, i_var, t_var)

  data_copies <-
    matrix(nrow = 0, ncol = length(all_vars), dimnames = list(NULL, all_vars)) %>%
    as_tibble()

  for (k in sort(unique(data[[y_var]]))[-1]) {
    y <-
      (data[[y_var]] >= k) %>%
      as.numeric() %>%
      as_tibble() %>%
      rename(y = value) %>%
      bind_cols(k = k)
    data_copy <- bind_cols(y, data[c(y_var, x_vars, i_var, t_var)])
    data_copies <- bind_rows(data_copies, data_copy)
  }

  data_copies <-
    data_copies %>%
    mutate(i_k = str_c(!!sym(i_var), "_", k)) %>%
    na.omit()

  m <-
    cquad(
      as.formula(str_c(y_var, " ~ ", str_c(x_vars, collapse = " + "))),
      data = data_copies,
      index = c("i_k", t_var),
      model = "basic",
      dyn = FALSE
    )

  m %>% coeftest() %>% print()
  p_values <- pnorm(abs(m$coefficients / m$ser), lower.tail = FALSE) * 2

  return(list(m, p_values))
}
