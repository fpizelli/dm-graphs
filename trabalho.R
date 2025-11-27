# Lista de pacotes necessários para o código funcionar
pacotes <- c("readr", "igraph", "visNetwork", "htmltools", "scales")

# Verifica cada pacote; se não estiver instalado, instala e carrega
for(p in pacotes) {
  if(!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

# ============================================================
# Carregar dataset
# ============================================================

# Lê o arquivo CSV contendo relações "aluno -> característica" com peso
dados <- read_csv("dataset.csv", show_col_types = FALSE)

# Exibe o dataset para conferência
print(dados)

# ============================================================
# Matriz de incidência
# ============================================================

# Constrói a matriz de incidência: linhas = alunos, colunas = características
matriz_inc <- xtabs(weight ~ from + to, data = dados)

# Converte a tabela em matriz numérica simples
matriz_inc <- unclass(matriz_inc)

# Mostra a matriz de incidência no console
cat("\n--- Matriz de incidência ---\n")
print(matriz_inc)

# ============================================================
# Matriz de similaridade
# ============================================================

# Multiplica a matriz de incidência pela sua transposta
# Resultado: participantes conectados por similaridade de preferências
s <- matriz_inc %*% t(matriz_inc)

# Remove autoconexões no grafo (diagonal zerada)
diag(s) <- 0

# Exibe a matriz de similaridade
cat("\n--- Matriz de similaridade ---\n")
print(s)

# ============================================================
# Matriz de coocorrência
# ============================================================

# Multiplica a transposta pela matriz original
# Resultado: características conectadas se aparecem juntas em alunos
c <- t(matriz_inc) %*% matriz_inc

# Remove autoconexões
diag(c) <- 0

# Exibe a matriz de coocorrência
cat("\n--- Matriz de coocorrência ---\n")
print(c)

# ============================================================
# Grafos iGraph
# ============================================================

# Grafo bipartido baseado na matriz de incidência
grafo_inc <- graph_from_incidence_matrix(matriz_inc, weighted = TRUE)

# Grafo de alunos baseado na similaridade
grafo_sim <- graph_from_adjacency_matrix(s, weighted = TRUE, mode = "undirected")

# Grafo de características baseado na coocorrência
grafo_co  <- graph_from_adjacency_matrix(c, weighted = TRUE, mode = "undirected")

# ============================================================
# Plots R Base
# ============================================================

# Configura 3 gráficos lado a lado
par(mfrow = c(1,3))

# Gráfico do grafo de incidência
plot(grafo_inc, edge.width = E(grafo_inc)$weight,
     main = "Grafo de incidência")

# Gráfico do grafo de similaridade
plot(grafo_sim, edge.width = E(grafo_sim)$weight,
     main = "Grafo de similaridade")

# Gráfico do grafo de coocorrência
plot(grafo_co, edge.width = E(grafo_co)$weight,
     main = "Grafo de coocorrência")

# Volta ao layout normal
par(mfrow = c(1,1))

# ============================================================
# Métricas
# ============================================================

# Métricas do grafo de incidência
cat("\n=========== Métricas: Incidência ===========\n")
cat("Vértices:", length(V(grafo_inc)), "\n")     # Quantidade de vértices
cat("Arestas:", ecount(grafo_inc), "\n")         # Quantidade de arestas
print(degree(grafo_inc))                         # Grau de cada vértice
cat("Densidade:", edge_density(grafo_inc), "\n") # Densidade do grafo

# Métricas do grafo de similaridade
cat("\n=========== Métricas: Similaridade ===========\n")
cat("Vértices:", length(V(grafo_sim)), "\n")
cat("Arestas:", ecount(grafo_sim), "\n")
print(degree(grafo_sim))
cat("Densidade:", edge_density(grafo_sim), "\n")

# Métricas do grafo de coocorrência
cat("\n=========== Métricas: Coocorrência ===========\n")
cat("Vértices:", length(V(grafo_co)), "\n")
cat("Arestas:", ecount(grafo_co), "\n")
print(degree(grafo_co))
cat("Densidade:", edge_density(grafo_co), "\n")

# ============================================================
# Visualização interativa
# ============================================================

# Converte os três grafos para formato aceito pelo visNetwork
vis_inc <- toVisNetworkData(grafo_inc)
vis_sim <- toVisNetworkData(grafo_sim)
vis_co  <- toVisNetworkData(grafo_co)

# ------------------------------------------------------------
# Dimensões de nós normalizado por grafo
# ------------------------------------------------------------

# Cálculo do grau dos vértices em cada grafo
deg_inc <- degree(grafo_inc)
deg_sim <- degree(grafo_sim)
deg_co  <- degree(grafo_co)

# Tabela de nós com tamanho proporcional ao grau — grafo de incidência
node_inc <- data.frame(
  id = vis_inc$nodes$id,
  label = vis_inc$nodes$label,
  size = scales::rescale(deg_inc, to = c(10, 40))
)

# Tabela dos nós — grafo de similaridade
node_sim <- data.frame(
  id = vis_sim$nodes$id,
  label = vis_sim$nodes$label,
  size = scales::rescale(deg_sim, to = c(10, 40))
)

# Tabela dos nós — grafo de coocorrência
node_co <- data.frame(
  id = vis_co$nodes$id,
  label = vis_co$nodes$label,
  size = scales::rescale(deg_co, to = c(10, 40))
)

# Arestas extraídas dos dados do visNetwork
links_inc <- as.data.frame(vis_inc$edges)
links_sim <- as.data.frame(vis_sim$edges)
links_co  <- as.data.frame(vis_co$edges)

# Altera a coluna de peso da aresta para o nome "width"
colnames(links_inc)[3] <- "width"
colnames(links_sim)[3] <- "width"
colnames(links_co)[3]  <- "width"

# ============================================================
# Funções de plot
# ============================================================

# Função para visualizar o grafo de incidência no Viewer
plot_inc <- function() {
  visNetwork(node_inc, links_inc) %>%
    visEdges(smooth = FALSE) %>%                    # Arestas sem curva
    visOptions(highlightNearest = TRUE,             # Destaque ao clicar
               nodesIdSelection = TRUE) %>%         # Caixa de seleção de nós
    visIgraphLayout(layout = "layout_with_fr")      # Layout por força
}

# Função para visualizar o grafo de similaridade
plot_sim <- function() {
  visNetwork(node_sim, links_sim) %>%
    visEdges(smooth = FALSE) %>%
    visOptions(highlightNearest = TRUE,
               nodesIdSelection = TRUE) %>%
    visIgraphLayout(layout = "layout_with_fr")
}

# Função para visualizar o grafo de coocorrência
plot_co <- function() {
  visNetwork(node_co, links_co) %>%
    visEdges(smooth = FALSE) %>%
    visOptions(highlightNearest = TRUE,
               nodesIdSelection = TRUE) %>%
    visIgraphLayout(layout = "layout_with_fr")
}

# ============================================================
# Renderização de grafos no Viewer
# ============================================================

# Define qual grafo será exibido
grafo_ativo <- "inc"   # entre "inc", "sim", ou "co"

# Executa a função correspondente ao grafo escolhido
if (grafo_ativo == "inc") {
  browsable(plot_inc())
} else if (grafo_ativo == "sim") {
  browsable(plot_sim())
} else if (grafo_ativo == "co") {
  browsable(plot_co())
} else {
  cat("Opção inválida para grafo_ativo! Utilize apenas inc, sim ou co.\n")
}
