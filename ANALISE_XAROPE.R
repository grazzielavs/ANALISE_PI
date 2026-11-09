# ==========================================================
# 1. IMPORTAÇÃO DOS DADOS
# ==========================================================

# Selecionar o arquivo CSV
arquivo <- "DADOS/Banana1.csv"

# Importar os dados
dados <- read.csv(
  arquivo,
  fileEncoding = "UTF-8-BOM",
  check.names = FALSE
)

# Conferir a importação
dim(dados)

# Mostrar os nomes das colunas
names(dados)

# Verificar os tipos das variáveis
str(dados)

# Visualizar as respostas
View(dados)
# ==========================================================
# 2. PREPARAÇÃO
# ==========================================================

# Selecionar as variáveis utilizadas no trabalho
idade <- as.numeric(dados[[1]])
consumo <- dados[[2]]
nota <- as.numeric(dados[[4]])
preco <- as.numeric(dados[[6]])

# Conferir os dados
summary(idade)
table(consumo)
summary(nota)
summary(preco)

# Verificar se existem valores ausentes
colSums(is.na(data.frame(idade, consumo, nota, preco)))
# ==========================================================
# 3. GERAL
# ==========================================================

# Função para calcular as medidas estatísticas
estatisticas <- function(x) {
  c(
    Minimo = min(x),
    Maximo = max(x),
    Media = mean(x),
    Mediana = median(x),
    Desvio_Padrao = sd(x)
  )
}

# Calcular as estatísticas das variáveis
tabela_1 <- rbind(
  Idade = estatisticas(idade),
  Avaliacao = estatisticas(nota),
  Preco = estatisticas(preco)
)

# Arredondar os resultados para duas casas decimais
tabela_1 <- round(tabela_1, 2)

# Exibir a tabela
print(tabela_1)

# Abrir a tabela no visualizador
View(tabela_1)
# Criar a pasta de resultados
dir.create("resultados", showWarnings = FALSE)

# Exportar a Tabela 1 em CSV
write.csv2(
  tabela_1,
  file = "resultados/tabela_1_estatistica_descritiva.csv",
  row.names = TRUE
)

# ==========================================================
# 4. AVALIAÇÃO X IDADE
# ==========================================================

# Criar grupos etários
grupo_idade <- cut(
  idade,
  breaks = c(16, 24, 32, Inf),
  right = FALSE,
  labels = c("16-23", "24-31", "32 ou mais")
)

# Calcular medidas estatísticas por grupo
resumo_idade <- data.frame(
  Faixa = levels(grupo_idade),
  N = as.integer(table(grupo_idade)),
  Media = as.numeric(tapply(nota, grupo_idade, mean)),
  Mediana = as.numeric(tapply(nota, grupo_idade, median)),
  DP = as.numeric(tapply(nota, grupo_idade, sd))
)

# Exibir resultados
print(resumo_idade)

# Definir cores
verde <- "#217A49"
amarelo <- "#D3A100"

# Posições no gráfico
pos <- seq_len(nrow(resumo_idade))

# Ajustar margens
par(mar = c(5, 5, 4, 2))

# Criar gráfico
plot(
  pos, resumo_idade$Media,
  type = "n",
  ylim = c(6.5, 10.6),
  xaxt = "n",
  xlab = "Faixa etária (anos)",
  ylab = "Nota atribuída",
  main = "Avaliação do produto por faixa etária"
)

# Adicionar nomes das faixas
axis(
  1,
  at = pos,
  labels = resumo_idade$Faixa
)

# Desvio-padrão
arrows(
  x0 = pos,
  y0 = resumo_idade$Media - resumo_idade$DP,
  x1 = pos,
  y1 = resumo_idade$Media + resumo_idade$DP,
  angle = 90,
  code = 3,
  length = 0.07,
  col = verde,
  lwd = 2
)

# Média
points(
  pos, resumo_idade$Media,
  pch = 19,
  col = verde,
  cex = 1.6
)

# Mediana
points(
  pos, resumo_idade$Mediana,
  pch = 18,
  col = amarelo,
  cex = 1.7
)

# Legenda
legend(
  "bottomleft",
  legend = c("Média", "Mediana", "Média ± 1 DP"),
  col = c(verde, amarelo, verde),
  pch = c(19, 18, NA),
  lty = c(NA, NA, 1),
  lwd = c(NA, NA, 2),
  bty = "n"
)

# ==========================================================
# 5. VALOR X AVALIAÇÃO
# ==========================================================

# Criar grupos de acordo com a nota
grupo_nota <- factor(nota, levels = sort(unique(nota)))

# Calcular as estatísticas por nota
resumo_preco <- data.frame(
  Nota = levels(grupo_nota),
  N = as.integer(table(grupo_nota)),
  Media = as.numeric(tapply(preco, grupo_nota, mean)),
  Mediana = as.numeric(tapply(preco, grupo_nota, median)),
  DP = as.numeric(tapply(preco, grupo_nota, sd))
)

# Exibir os resultados
print(round(resumo_preco[, c("N", "Media", "Mediana", "DP")], 2))

# Posições dos grupos
pos <- seq_len(nrow(resumo_preco))

# Configurar margens
par(mar = c(5, 5, 4, 2))

# Criar gráfico
plot(
  pos, resumo_preco$Media,
  type = "n",
  ylim = c(7, 18),
  xaxt = "n",
  xlab = "Nota atribuída ao produto",
  ylab = "Disposição a pagar (R$)",
  main = "Disposição a pagar por avaliação"
)

# Identificar as notas
axis(
  1,
  at = pos,
  labels = resumo_preco$Nota
)

# Média +/- desvio-padrão
arrows(
  x0 = pos,
  y0 = resumo_preco$Media - resumo_preco$DP,
  x1 = pos,
  y1 = resumo_preco$Media + resumo_preco$DP,
  angle = 90,
  code = 3,
  length = 0.07,
  col = verde,
  lwd = 2
)

# Média em verde
points(
  pos, resumo_preco$Media,
  pch = 19,
  col = verde,
  cex = 1.6
)

# Mediana em amarelo
points(
  pos, resumo_preco$Mediana,
  pch = 18,
  col = amarelo,
  cex = 1.7
)

# Legenda
legend(
  "topleft",
  legend = c("Média", "Mediana", "Média ± 1 DP"),
  col = c(verde, amarelo, verde),
  pch = c(19, 18, NA),
  lty = c(NA, NA, 1),
  lwd = c(NA, NA, 2),
  bty = "n"
)

# ==========================================================
# 6. CONSUMO X AVALIACAO
# ==========================================================

# Organizar as categorias
grupo_consumo <- factor(
  consumo,
  levels = c("Não", "Raramente", "Sim")
)

# Calcular as estatísticas por grupo
resumo_consumo <- data.frame(
  Consumo = levels(grupo_consumo),
  N = as.integer(table(grupo_consumo)),
  Media = as.numeric(tapply(nota, grupo_consumo, mean)),
  Mediana = as.numeric(tapply(nota, grupo_consumo, median)),
  DP = as.numeric(tapply(nota, grupo_consumo, sd))
)

# Exibir os resultados
print(resumo_consumo)

# Definir as posições
pos <- seq_len(nrow(resumo_consumo))

# Configurar as margens
par(mar = c(5, 5, 4, 2))

# Criar o gráfico
plot(
  pos, resumo_consumo$Media,
  type = "n",
  ylim = c(7, 10.5),
  xaxt = "n",
  xlab = "Consumo de mel tradicional",
  ylab = "Nota atribuída",
  main = "Avaliação segundo o consumo de mel"
)

# Identificar os grupos
axis(
  1,
  at = pos,
  labels = resumo_consumo$Consumo
)

# Representar média +/- desvio-padrão
arrows(
  x0 = pos,
  y0 = resumo_consumo$Media - resumo_consumo$DP,
  x1 = pos,
  y1 = resumo_consumo$Media + resumo_consumo$DP,
  angle = 90,
  code = 3,
  length = 0.07,
  col = verde,
  lwd = 2
)

# Média em verde
points(
  pos, resumo_consumo$Media,
  pch = 19,
  col = verde,
  cex = 1.6
)

# Mediana em amarelo
points(
  pos, resumo_consumo$Mediana,
  pch = 18,
  col = amarelo,
  cex = 1.7
)

# Adicionar legenda
legend(
  "bottomleft",
  legend = c("Média", "Mediana", "Média ± 1 DP"),
  col = c(verde, amarelo, verde),
  pch = c(19, 18, NA),
  lty = c(NA, NA, 1),
  lwd = c(NA, NA, 2),
  bty = "n"
)

# ==========================================================
# 7. EXPORTAÇÃO
# ==========================================================

# Criar pasta para os gráficos
dir.create("graficos", showWarnings = FALSE)

# Função para gerar e salvar gráficos
salvar_grafico <- function(tabela, grupos, titulo,
                           eixo_x, eixo_y, limites,
                           arquivo, pos_legenda) {
  
  png(
    filename = file.path("graficos", arquivo),
    width = 1800,
    height = 1100,
    res = 200
  )
  
  pos <- seq_len(nrow(tabela))
  
  par(mar = c(5, 5, 4, 2))
  
  plot(
    pos, tabela$Media,
    type = "n",
    ylim = limites,
    xaxt = "n",
    xlab = eixo_x,
    ylab = eixo_y,
    main = titulo,
    cex.main = 1.1
  )
  
  axis(1, at = pos, labels = grupos)
  
  # Média +/- desvio-padrão
  arrows(
    pos, tabela$Media - tabela$DP,
    pos, tabela$Media + tabela$DP,
    angle = 90,
    code = 3,
    length = 0.07,
    col = verde,
    lwd = 2
  )
  
  # Média
  points(
    pos, tabela$Media,
    pch = 19,
    col = verde,
    cex = 1.5
  )
  
  # Mediana
  points(
    pos, tabela$Mediana,
    pch = 18,
    col = amarelo,
    cex = 1.6
  )
  
  legend(
    pos_legenda,
    legend = c("Média", "Mediana", "Média ± 1 DP"),
    col = c(verde, amarelo, verde),
    pch = c(19, 18, NA),
    lty = c(NA, NA, 1),
    lwd = c(NA, NA, 2),
    bty = "n"
  )
  
  dev.off()
}

# FIGURA 1 - IDADE X AVALIAÇÃO
salvar_grafico(
  resumo_idade,
  resumo_idade$Faixa,
  "Avaliação do produto por faixa etária",
  "Faixa etária (anos)",
  "Nota atribuída",
  c(6.5, 10.6),
  "figura_1_idade_avaliacao.png",
  "bottomleft"
)

# FIGURA 2 - NOTA X PREÇO
salvar_grafico(
  resumo_preco,
  resumo_preco$Nota,
  "Disposição a pagar por avaliação",
  "Nota atribuída ao produto",
  "Disposição a pagar (R$)",
  c(7, 18),
  "figura_2_nota_preco.png",
  "topleft"
)

# FIGURA 3 - CONSUMO X AVALIAÇÃO
salvar_grafico(
  resumo_consumo,
  resumo_consumo$Consumo,
  "Avaliação segundo o consumo de mel",
  "Consumo de mel tradicional",
  "Nota atribuída",
  c(7, 10.6),
  "figura_3_consumo_avaliacao.png",
  "bottomleft"
)

# Conferir os arquivos gerados
list.files("graficos")
