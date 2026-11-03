# =============================================================================
# Tarea 4 — Parte 1: Tres gráficos, tres bases (Semana 10)
# Fundamentos de Programación para Análisis Económico · UdeC-EAN
# Nombre: Javier Caceres Venegas
# =============================================================================

# Cargar librerías vistas en clase
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Crear directorio de salida para las figuras si no existe
dir.create("output/figuras", recursive = TRUE, showWarnings = FALSE)

# Formateador chileno para pesos
pesos_chile <- label_currency(big.mark = ".", decimal.mark = ",")

# -----------------------------------------------------------------------------
# GRÁFICO 1 — CASEN: Educación e ingreso por género
# -----------------------------------------------------------------------------
# Cargar base reducida CASEN (remover NAs en ingreso)
casen <- read.csv("data/raw/casen_reducido.csv") |> 
  filter(!is.na(ingreso))

# Gráfico: aes(color = genero) global para obtener una recta por grupo
g1 <- ggplot(casen, aes(x = educ, y = ingreso, color = genero)) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_color_manual(values = c(F = "#e69b0a", M = "#223c6a"),
                     labels = c(F = "Mujeres", M = "Hombres")) +
  scale_y_continuous(labels = pesos_chile) +
  labs(
    title    = "A más educación, mayor ingreso — y la brecha de género persiste",
    subtitle = "Muestra reducida de CASEN (55 observaciones)",
    x        = "Años de escolaridad",
    y        = "Ingreso total ($)",
    color    = "Género",
    caption  = "Fuente: CASEN (muestra del curso)."
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(face = "bold", color = "#223c6a"),
    legend.position = "bottom"
  )

g1

# Respuesta a la pregunta del esqueleto:
# ¿Qué pregunta responde este gráfico que no respondería la versión con una sola recta?
# Respuesta: Responde si el retorno marginal de la educación (la pendiente) difiere según género.
# Una sola recta mostraría la relación promedio nacional, pero ocultaría si la brecha salarial se
# amplía o estrecha a medida que aumentan los años de escolaridad.

ggsave("output/figuras/t4_p1_grafico1.png", g1, width = 8, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# GRÁFICO 2 — Ingreso mínimo: Nominal vs Real (2014-2025)
# -----------------------------------------------------------------------------
salario <- read.csv("data/raw/salario_minimo_chile.csv")

# Formato largo y cálculo del salario real (pesos de 2010)
salario_largo <- salario |>
  mutate(Real = salario_minimo / ipc * 100) |>
  rename(Nominal = salario_minimo) |>
  pivot_longer(cols = c(Nominal, Real), names_to = "serie", values_to = "monto")

g2 <- ggplot(salario_largo, aes(x = anio, y = monto, color = serie, linetype = serie)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c(Nominal = "#223c6a", Real = "#e69b0a")) +
  scale_linetype_manual(values = c(Nominal = "solid", Real = "dashed")) +
  scale_y_continuous(labels = pesos_chile) +
  scale_x_continuous(breaks = seq(2014, 2025, by = 2)) +
  annotate("text", x = 2024.5, y = 529000, label = "Nominal", color = "#223c6a", fontface = "bold", hjust = 0) +
  annotate("text", x = 2024.5, y = 285000, label = "Real", color = "#e69b0a", fontface = "bold", hjust = 0) +
  coord_cartesian(clip = "off") +
  labs(
    title    = "El salario mínimo nominal creció más rápido que el poder adquisitivo real",
    subtitle = "Evolución en pesos nominales vs. pesos reales de 2010 (2014–2025)",
    x        = "Año",
    y        = "Monto ($)",
    caption  = "Fuente: Banco Central e INE."
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(face = "bold", color = "#223c6a"),
    legend.position = "none",
    plot.margin     = margin(r = 40)
  )

g2

# Respuesta a las preguntas del esqueleto:
# 1. ¿Por qué aquí el eje Y no necesita partir en 0 y en un gráfico de barras sí?
#    En las líneas evaluamos la tendencia y la variación a lo largo del tiempo. En barras, la altura
#    representa una magnitud absoluta proporcional al área de la barra; truncar el eje altera dicha proporción.
# 2. ¿Qué ganas usando color Y tipo de línea?
#    Aplica codificación redundante, haciendo el gráfico accesible para personas daltónicas e impresiones en blanco y negro.

ggsave("output/figuras/t4_p1_grafico2.png", g2, width = 8, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# GRÁFICO 3 — ENE: Composición del empleo por sexo según categoría
# -----------------------------------------------------------------------------
regiones <- c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso",
              "O'Higgins", "Maule", "Biobío", "La Araucanía", "Los Lagos",
              "Aysén", "Magallanes", "Metropolitana", "Los Ríos",
              "Arica y Parinacota", "Ñuble")

ene <- read.csv2("data/raw/ene-2026-06-mjj.csv") |>
  filter(activ == 1) |>
  mutate(
    sexo_txt = ifelse(sexo == 1, "Hombres", "Mujeres"),
    categoria = case_when(
      categoria_ocupacion == 3 ~ "Asalariado privado",
      categoria_ocupacion == 4 ~ "Asalariado público",
      categoria_ocupacion == 2 ~ "Cuenta propia",
      TRUE                     ~ "Otras"
    )
  )

g3 <- ggplot(ene, aes(x = sexo_txt, fill = categoria, weight = fact_cal)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = label_percent()) +
  labs(
    title    = "El sector público absorbe mayor proporción de empleo femenino",
    subtitle = "Estructura del empleo según categoría ocupacional ponderada (ENE mjj 2026)",
    x        = NULL,
    y        = "Proporción del empleo (%)",
    fill     = "Categoría",
    caption  = "Fuente: Encuesta Nacional de Empleo (INE). Ponderado con fact_cal."
  ) +
  theme_minimal() +
  theme(
    plot.title      = element_text(face = "bold", color = "#223c6a"),
    legend.position = "bottom"
  )

g3

# Cifras ponderadas para respuesta:
# ene |> group_by(sexo_txt, categoria) |> summarise(pob = sum(fact_cal)) |> mutate(pct = pob/sum(pob)*100)
# Respuesta a las preguntas del esqueleto:
# 1. ¿En qué categoría está la mayor diferencia entre hombres y mujeres, y en cuál casi no hay?
#    La mayor diferencia está en 'Asalariado público' (17.0% en mujeres vs 9.2% en hombres).
#    La menor diferencia está en 'Cuenta propia' (21.2% en mujeres vs 21.1% en hombres).
# 2. ¿Qué cambiaría si borras weight = fact_cal?
#    Se graficarían los conteos muestrales no ponderados, perdiendo la representatividad poblacional.

ggsave("output/figuras/t4_p1_grafico3.png", g3, width = 8, height = 5, dpi = 300)

