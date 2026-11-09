paquetes <- c("jsonlite", "tidyverse", "lubridate", "stringr", "stringi", "tidytext",
              "stopwords", "syuzhet", "topicmodels", "tm", "textclean", "writexl",
              "scales", "ggalluvial", "plotly", "networkD3", "gganimate", "gifski",
              "digest", "visdat", "rlang")
nuevos <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(nuevos) > 0) install.packages(nuevos)
cat("Listo. Se instalaron", length(nuevos), "paquetes nuevos.\n")