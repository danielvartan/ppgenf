hyp_test <- function(p_value, 
                     alpha = 0.05, 
                     latex_hyp = NULL, 
                     class = "warning") {
  class_choices <- c("note", "warning", "important", "tip", "caution")
  
  checkmate::assert_number(p_value, lower = 0, upper = 1)
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  checkmate::assert_character(latex_hyp, null.ok = TRUE)
  checkmate::assert_choice(class, class_choices)
  
  if (is.null(latex_hyp)) {
    latex_hyp <- paste0(
      "$$", "\n",
      "\\begin{cases}", "\n",
      "\\text{H}_{0}: \\text{As avaliações são independentes} \\\\", "\n",
      "\\text{H}_{a}: \\text{As avaliações não são independentes}", "\n",
      "\\end{cases}", "\n",
      "$$"
    )
  }
  
  if (p_value < alpha) {
    reject <- "0"
    favour <- "a"
  } else {
    reject <- "a"
    favour <- "0"
  }
  
  cat(paste0(
    "\n",
    "::: {.callout-", class, "}", "\n",
    "## Teste de hipótese", "\n\n",
    latex_hyp, "\n\n",
    "__Rejeito $H_{", reject, "}$ em favor de $H_{", favour, 
    "}$__ ($\\text{valor-p} = ",
    format(p_value, decimal.mark = ",", digits = 6), "$).", "\n",
    ":::",
    "\n"
  ))
}

p_value_exp <- function(n, int_begin, int_end, p_value) {
  checkmate::assert_number(n)
  checkmate::assert_number(int_begin)
  checkmate::assert_number(int_end)
  
  cat(paste0(
    "\n",
    "A partir de ", inline_tex(n), " amostras aleatórias, ",
    "seguindo o método exposto, ",
    "a probabilidade de os três avaliadores darem uma nota entre ",
    inline_tex(int_begin, round_d = 5, nsmall = 0), " e ", 
    inline_tex(int_end, round_d = 5, nsmall = 0), " foi de ",
    inline_tex(p_value, round_d = 5), ".",
    "\n"
  ))
}

format_num <- function(x,
                       round_d = 3, 
                       nsmall = 0,
                       big_mark = ".",
                       decimal_mark = ",") {
  checkmate::assert_number(x)
  checkmate::assert_number(round_d)
  checkmate::assert_number(nsmall)
  checkmate::assert_string(big_mark)
  checkmate::assert_string(decimal_mark)
  
  x |> 
    round(round_d) |>
    format(big.mark = big_mark, decimal.mark = decimal_mark, nsmall = nsmall)
}

inline_tex <- function(x, format_num = TRUE, ...) {
  checkmate::assert_atomic(x)
  checkmate::assert_flag(format_num)
  
  if (is.character(x)) x <- x |> trimws()
  if (is.numeric(x) & isTRUE(format_num)) x <- x |> format_num(...)
  
  paste0("$", x, "$")
}
