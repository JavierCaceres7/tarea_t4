# =============================================================================
# Tarea 4 — Parte 2: Portafolio, figura compuesta y reflexión (Semana 11)
# Fundamentos de Programación para Análisis Económico · UdeC-EAN
# Nombre: Javier Caceres Venegas
# =============================================================================

library(dplyr)
library(ggplot2)
library(patchwork)
library(ggrepel)
library(scales)

dir.create("output/figuras", recursive = TRUE, showWarnings = FALSE)

pesos_chile <- label_currency(big.mark = ".", decimal.mark = ",")
miles_chile <- label_number(scale = 1e-3, suffix = " mil", big.mark = ".", decimal.mark = ",")

regiones <- c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso",
              "O'Higgins", "Maule", "Biobío", "La Araucanía", "Los Lagos",
              "Aysén", "Magallanes", "Metropolitana", "Los Ríos",
              "Arica y Parinacota", "Ñuble")

casen <- read.csv("data/raw/casen_reducido.csv") |> filter(!is.na(ingreso))

ene <- read.csv2("data/raw/ene-2026-06-mjj.csv") |>
  filter(activ == 1) |>
  mutate(
    informal   = ocup_form == 2,
    sexo_txt   = ifelse(sexo == 1, "Hombres", "Mujeres"),
    region_txt = regiones[region]
  )

# -----------------------------------------------------------------------------
# GRÁFICO 1 — Scatter CASEN (Modelo base de referencia)
# -----------------------------------------------------------------------------
p1 <- ggplot(casen, aes(x = educ, y = ingreso, color = genero)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_manual(values = c(F = "#e69b0a", M = "#223c6a")) +
  scale_y_continuous(labels = miles_chile) +
  labs(
    title    = "Educación e ingreso en CASEN",
    x        = "Años de escolaridad",
    y        = "Ingreso mensual",
    color    = "Género"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", color = "#223c6a"))

# -----------------------------------------------------------------------------
# GRÁFICO 2 — Tasa de informalidad por región (Resaltando Ñuble)
# -----------------------------------------------------------------------------
tasa_nacional <- 100 * weighted.mean(ene$informal, ene$fact_cal)

inf_region <- ene |>
  group_by(region_txt) |>
  summarise(tasa = 100 * weighted.mean(informal, fact_cal), .groups = "drop")

p2 <- ggplot(inf_region, aes(x = tasa, y = reorder(region_txt, tasa), fill = region_txt == "Ñuble")) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", tasa)), hjust = -0.15, size = 3) +
  geom_vline(xintercept = tasa_nacional, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c(`TRUE` = "#e69b0a", `FALSE` = "grey75")) +
  scale_x_continuous(limits = c(0, 42)) +
  labs(
    title    = "Ñuble presenta la segunda tasa de informalidad más alta de Chile",
    x        = "Tasa de informalidad (%)",
    y        = NULL,
    caption  = "Fuente: ENE mjj 2026. Línea punteada indica el promedio nacional (26.5%)."
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", color = "#223c6a"))

ggsave("output/figuras/t4_p2_grafico2.png", p2, width = 8, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# GRÁFICO 3 — Distribución de horas habituales por sexo (Boxplot + Puntos)
# -----------------------------------------------------------------------------
ene_horas <- ene |>
  mutate(horas_limpias = na_if(na_if(habituales, 888), 999)) |>
  filter(!is.na(horas_limpias))

p3 <- ggplot(ene_horas, aes(x = sexo_txt, y = horas_limpias, fill = sexo_txt)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, show.legend = FALSE) +
  scale_fill_manual(values = c(Hombres = "#223c6a", Mujeres = "#e69b0a")) +
  labs(
    title    = "Distribución de horas semanales trabajadas según sexo",
    x        = NULL,
    y        = "Horas semanales habituales"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", color = "#223c6a"))

# Justificación del geom:
# Se eligió Boxplot sobre la Densidad Suavizada porque las horas de trabajo presentan
# alta concentración discreta en tramos legales (ej. 40, 42 y 45 horas). La densidad
# simula continuidad artificiosa, mientras que el boxplot resume con honestidad
# la mediana y el rango intercuartílico sin distorsión.

ggsave("output/figuras/t4_p2_grafico3.png", p3, width = 8, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# GRÁFICO 4 — Informalidad Hombres vs Mujeres por Región (ggrepel)
# -----------------------------------------------------------------------------
inf_sexo_region <- ene |>
  group_by(region_txt) |>
  summarise(
    tasa_h = 100 * weighted.mean(informal[sexo_txt == "Hombres"], fact_cal[sexo_txt == "Hombres"]),
    tasa_m = 100 * weighted.mean(informal[sexo_txt == "Mujeres"], fact_cal[sexo_txt == "Mujeres"]),
    .groups = "drop"
  )

p4 <- ggplot(inf_sexo_region, aes(x = tasa_h, y = tasa_m)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(color = "#223c6a", size = 2.5) +
  geom_text_repel(aes(label = region_txt), size = 3, max.overlaps = Inf, seed = 123) +
  labs(
    title    = "Informalidad por región: Brecha por género",
    x        = "Informalidad Hombres (%)",
    y        = "Informalidad Mujeres (%)",
    caption  = "Fuente: ENE mjj 2026. La línea diagonal indica equidad de tasas (y = x)."
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", color = "#223c6a"))

ggsave("output/figuras/t4_p2_grafico4.png", p4, width = 8, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# GRÁFICO 5 — Pregunta Propia: Ingreso medio por Sector en CASEN (IC al 95%)
# -----------------------------------------------------------------------------
# ¿Existen diferencias estadísticamente significativas en el ingreso medio según sector económico?
ic_sector <- casen |>
  group_by(sector) |>
  summarise(
    n     = n(),
    media = mean(ingreso),
    se    = sd(ingreso) / sqrt(n),
    t     = qt(0.975, df = n - 1),
    bajo  = media - t * se,
    alto  = media + t * se,
    .groups = "drop"
  )

p5 <- ggplot(ic_sector, aes(x = media, y = reorder(sector, media))) +
  geom_pointrange(aes(xmin = bajo, xmax = alto), color = "#223c6a", size = 0.6) +
  scale_x_continuous(labels = pesos_chile) +
  labs(
    title    = "Educación presenta el ingreso promedio más elevado",
    subtitle = "Ingreso medio estimado e intervalo de confianza al 95%",
    x        = "Ingreso promedio ($)",
    y        = NULL,
    caption  = "Fuente: Muestra reducida CASEN."
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", color = "#223c6a"))

ggsave("output/figuras/t4_p2_grafico5.png", p5, width = 8, height = 5, dpi = 300)

# -----------------------------------------------------------------------------
# COMPOSICIÓN DE LA FIGURA FINAL (patchwork)
# -----------------------------------------------------------------------------
# Regla de sintaxis: Uso de paréntesis externos obligatorios para asociar metadatos generales
figura_compuesta <- (p2 | (p3 / p4)) +
  plot_layout(widths = c(1, 1.2)) +
  plot_annotation(
    title    = "Estructura del empleo e informalidad laboral en Chile",
    subtitle = "Análisis comparativo por regiones, género y jornada laboral (ENE 2026)",
    caption  = "Fuente: Encuesta Nacional de Empleo (INE) mjj 2026. Datos ponderados.",
    tag_levels = "A"
  )

figura_compuesta

# Exportación en PNG (300 dpi) y PDF vectorial
ggsave("output/figuras/t4_p2_figura_compuesta.png", figura_compuesta, width = 12, height = 7, dpi = 300)
ggsave("output/figuras/t4_p2_figura_compuesta.pdf", figura_compuesta, width = 12, height = 7)

# -----------------------------------------------------------------------------
# REFLEXIÓN
# -----------------------------------------------------------------------------
# 1. De los cinco gráficos, ¿cuál comunica mejor su hallazgo y por qué?
#    El Gráfico 2 (de barras por región) es el mejor porque va directo al grano. 
#    Al ordenar las barras de mayor a menor, pintar de color dorado solo a Ñuble 
#    y poner la línea del promedio de Chile, cualquier persona entiende la idea 
#    principal en menos de tres segundos, sin necesidad de leer explicaciones largas.
#
# 2. En el gráfico 3, ¿por qué elegiste ese geom y no otro?
#    Preferí el diagrama de caja (boxplot) en lugar de una gráfica de densidad 
#    porque las horas de trabajo se concentran en números muy específicos que manda 
#    la ley (como 40, 42 o 45 horas). Una gráfica de densidad habría hecho creer 
#    que los datos se mueven de forma suave y continua, lo cual no es real.
#
# 3. ¿Qué agrega la figura compuesta que no dan los gráficos por separado?
#    Junta tres piezas clave del mercado de trabajo en una sola mirada: 
#    cómo cambia la situación según la región (Panel A), cómo se reparten las 
#    horas de trabajo (Panel B) y las diferencias entre hombres y mujeres 
#    por zona (Panel C). Ver todo junto ayuda a entender el problema completo 
#    mucho más rápido.