#'Função para cálculo do coeficiente de variação
coef_var <- function(x){
  x <- na.omit(x)
  100*sd(x)/mean(x)
}

#'Função para cálculo do erro padrão da média
erro_pad <- function(x){
  x <- na.omit(x)
  n <- length(x)
  sd(x)/sqrt(n)
}

#'Função para criação da estatística descritiva
estatisticas <- function(x){
  round(c(
    media = mean(x),
    mediana= median(x),
    desvio_padrao = sd(x),
    cv = coef_var(x),
    assimetria = agricolae::skewness(x),
    curtose = agricolae::kurtosis(x)
  ),4)
}
