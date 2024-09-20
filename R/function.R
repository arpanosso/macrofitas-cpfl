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

#' Função para cálculo do teste t de Student em dados
#' agrupados pela função dplyr::nest
my_t_test <- function(df){
  df <- df |> filter(amostra != "salmo")
  epoca <- df$data
  especie <- df$amostra
  y <- df$value
  ep <- unique(epoca)
  esp <- unique(especie)
  tab <- data.frame(Especie = "a",
                    Fobs = 0,
                    p_valor_f = 0,
                    tobs = 0,
                    p_valor_t = 0,
                    media_02_2024 = "a",
                    media_06_2024 = "a")
  for(i in seq_along(esp)){
    f <- especie == esp[i]
    vt <- var.test(y[f] ~ epoca[f])
    tab[i,1] <- esp[i]
    tab[i,2] <- vt$statistic[[1]]
    tab[i,3] <- vt$p.value[[1]]
    if(vt$p.value[[1]] > 0.05){
      tt <- t.test(y[f] ~ epoca[f],var.equal=TRUE)
    }else{
      tt <- t.test(y[f] ~ epoca[f],var.equal=FALSE)
    }
    tab[i,4] <- tt$statistic[[1]]
    tab[i,5] <- tt$p.value[[1]]
    if(tt$p.value[[1]] <= 0.05){
      l1 <- " b"
      l2 <- " a"
      if(tt$estimate[[1]] > tt$estimate[[2]]){
        l1 <- " a"
        l2 <- " b"
      }
      tab[i,6] <- paste0(as.character(round(tt$estimate[[1]],3)),l1)
      tab[i,7] <- paste0(as.character(round(tt$estimate[[2]],3)),l2)
    }else{
      l1 <- " a"
      tab[i,6] <- paste0(as.character(round(tt$estimate[[1]],3)),l1)
      tab[i,7] <- paste0(as.character(round(tt$estimate[[2]],3)),l1)
    }
  }
  return(tab[c(1,6,7)])
}

