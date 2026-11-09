library(tidyverse)
source("R01_config.R")

if (file.exists("lexicon_sentimientos.csv")) {
  cat("Ya existe lexicon_sentimientos.csv (se respeta tu version editada).\n")
} else {
  # "palabra*" = empieza con..., "palabra" = exacta, "~..." = regex tal cual. Sin tildes.
  conv <- function(w) {
    if (startsWith(w, "~")) return(substring(w, 2))
    if (grepl("\\*$", w))   return(paste0("\\b", sub("\\*$", "", w), "\\w*"))
    paste0("\\b", w, "\\b")
  }
  PAL <- list(
    "alegría" = c("jaj*", "jeje*", "jiji*", "xd", "feliz", "felices", "alegr*", "genial",
                  "excelente", "muy bien", "que bien", "buenisimo*", "chevere", "bacan",
                  "divertid*", "risa*", "yupi", "viva", "celebr*", "festej*", "contento*",
                  "contenta*", "me encanta*", "encantad*", "aprobe", "aprobado*", "lo logre",
                  "lo logramos", "excelente dia"),
    "gratitud" = c("gracias*", "graciass*", "agradec*", "agradezco", "mil gracias", "bendic*",
                   "dios le pague", "dios se lo pague", "se agradece", "muy amable", "amable",
                   "gracias a", "se los agradezco"),
    "esperanza" = c("espero", "esperemos", "esperanza*", "ojala*", "confio", "confiemos",
                    "animo*", "suerte", "exito*", "dios quiera", "se puede", "lo lograremos",
                    "vamos a poder", "adelante", "no se rindan", "fuerza", "con fe",
                    "buena suerte", "mucha suerte", "pronto"),
    "seguridad" = c("seguro", "segura", "seguros", "claro que si", "por supuesto", "sin duda",
                    "confirmado*", "garantizad*", "estoy seguro", "estoy segura",
                    "definitivamente", "obvio", "exacto", "correcto", "asegur*", "certeza",
                    "confirmo", "es verdad", "confiable*", "verificad*", "cien por ciento"),
    "admiración" = c("admir*", "felicitaciones", "felicit*", "crack", "capo", "genio*",
                     "maestro", "buen trabajo", "excelente trabajo", "buenazo*", "espectacular*",
                     "impresionante*", "respeto", "orgullo*", "orgullos*", "bravo", "aplaus*",
                     "brillante*", "talentos*", "un monstruo", "increible trabajo", "ejemplo"),
    "tranquilidad" = c("tranquil*", "tranqui", "calma*", "relaj*", "sin problema*",
                       "no hay problema", "no pasa nada", "con calma", "~(?<!en )\\bpaz\\b",
                       "descansa*", "despreocup*", "sin apuro", "sin prisa", "todo bien",
                       "~\\bno\\s+se\\s+preocup\\w*", "~\\bno\\s+te\\s+preocup\\w*",
                       "~\\bno\\s+hay\\s+apuro"),
    "sorpresa" = c("~\\bwo+w+\\b", "~\\buy+\\b", "~\\bomg\\b", "increible", "no puedo creer",
                   "no lo puedo creer", "sorpres*", "inesperad*", "de repente", "en serio",
                   "enserio", "de verdad", "asombr*", "impact*", "no sabia", "no lo sabia",
                   "sera posible", "que fuerte"),
    "confusión" = c("no entiendo", "no entendi", "no comprendo", "confund*", "confusion*",
                    "no se como", "no se que", "no se donde", "no se cual", "perdido", "perdida",
                    "como asi", "que paso", "no me queda claro", "no esta claro", "duda*",
                    "dudo", "alguien sabe", "me explican", "alguien me explica", "no logro",
                    "no encuentro", "como se hace", "como hago", "que significa", "ayuda*",
                    "ayudenme", "auxilio"),
    "preocupación" = c("~(?<!no se )(?<!no te )(?<!no )\\bpreocup\\w*", "miedo*", "nervios*",
                       "ansied*", "ansios*", "angusti*", "urgente*", "urge", "temo", "temor*",
                       "riesgo*", "peligro*", "cuidado", "plazo*", "ultimo dia", "llego tarde",
                       "no llego", "no alcanzo", "no me da el tiempo", "estres*", "reprobar",
                       "reprobare", "se acaba el tiempo", "falta poco", "inquiet*",
                       "~(?<!sin )(?<!no hay )\\bproblema\\w*"),
    "tristeza" = c("trist*", "llor*", "pena", "lamento", "lamentabl*", "lastima*", "deprim*",
                   "depre", "duele", "dolor*", "sufr*", "descanse en paz", "qepd", "fallec*",
                   "murio", "condolencias", "luto", "desanim*", "que mal", "me siento mal",
                   "me fue mal"),
    "enojo" = c("enoj*", "enfad*", "molest*", "bronca*", "rabia*", "furios*", "odio", "harto",
                "harta", "hartos", "estupid*", "idiota*", "imbecil*", "mierda", "carajo*",
                "maldit*", "chucha", "puta", "joder", "pesim*", "inaceptable*", "abusiv*",
                "reclam*", "ya basta", "fastidi*", "irrit*", "indignad*"),
    "decepción" = c("decepcion*", "desilusion*", "defraud*", "esperaba mas", "que decepcion",
                    "falso", "falsa", "engan*", "estaf*", "mentira*", "mentiros*", "incumpl*",
                    "no cumpl*", "burla*", "no vale la pena", "me fallaron", "fallaron",
                    "reprobe", "reprobado*", "no aprobe", "no pase", "perdi la materia",
                    "mala calidad"),
    "cansancio" = c("cansad*", "cansanci*", "agotad*", "agotamiento", "tengo sueno",
                    "me muero de sueno", "sin dormir", "desvel*", "trasnoch*", "extenuad*",
                    "agobi*", "fatiga*", "no doy mas", "flojera*", "pereza", "exhaust*",
                    "ya no puedo", "madrug*"),
    "curiosidad" = c("curios*", "me pregunto", "quisiera saber", "quiero saber",
                     "me gustaria saber", "me interesa*", "interesante*", "interesad*",
                     "como funciona", "por que", "sabias que", "cuentame", "en que consiste",
                     "de que trata", "me intriga", "intrigante*", "investig*",
                     "quiero aprender", "quisiera aprender", "descubr*"),
    "frustración" = c("frustr*", "no funciona", "no sirve", "no carga", "no abre", "otra vez",
                      "de nuevo", "siempre lo mismo", "no hay caso", "inutil*", "me rindo",
                      "no puedo mas", "imposible*", "no me deja", "error*", "falla*",
                      "se cayo", "se cae", "no responde", "no entra", "no me sale")
  )
  EMO <- list(
    "alegría"      = "😀 😃 😄 😁 😆 😂 🤣 😊 🙂 🥳 🎉 😹 🤗 😅",
    "gratitud"     = "🙏 🤝 💐 ❤ 💙 💚 💛 🧡 💜 🫶 🥰 😘",
    "esperanza"    = "🤞 🍀 🌈 ✨ 🕊",
    "seguridad"    = "👍 ✅ ☑ ✔ 👌 🔒",
    "admiración"   = "😍 🤩 🔥 💪 👏 🏆 ⭐ 🌟 💯 🫡",
    "tranquilidad" = "😌 😇 🧘 ☺ 🌿 ☕",
    "sorpresa"     = "😮 😲 😯 😱 🤯 😳 🙀 ❗",
    "confusión"    = "🤔 😕 🤷 ❓ 😵",
    "preocupación" = "😟 😰 😨 😧 😬 ⚠ 🚨 😥",
    "tristeza"     = "😢 😭 😞 😔 ☹ 🙁 💔 🥺 😿",
    "enojo"        = "😡 🤬 😠 👎 😤 💢 🖕",
    "decepción"    = "😒 🙄 😑 😐 🤦 🤡",
    "cansancio"    = "😴 😪 🥱 😩 😫 💤",
    "curiosidad"   = "🧐 👀 🤓 🔍",
    "frustración"  = "😖 😣 😓 🙃"
  )
  lp <- imap_dfr(PAL, ~ tibble(sentimiento = .y, tipo = "palabra",
                               patron = vapply(.x, conv, character(1)), peso = 1))
  le <- imap_dfr(EMO, function(x, s) {
    e <- gsub("\uFE0F", "", strsplit(x, " ")[[1]]); e <- e[e != ""]
    tibble(sentimiento = s, tipo = "emoji", patron = e, peso = 1)
  })
  write_excel_csv(bind_rows(lp, le), "lexicon_sentimientos.csv")
  cat("Creado lexicon_sentimientos.csv\n")
}

L <- read_csv("lexicon_sentimientos.csv", show_col_types = FALSE)
print(L %>% count(sentimiento, tipo) %>% pivot_wider(names_from = tipo, values_from = n))
cat("Sentimientos:", n_distinct(L$sentimiento), "| entradas:", nrow(L), "\n")
registrar("4. Lexico", "Diccionario propio de sentimientos (palabras y emojis)", nrow(L), nrow(L), "lexicon_sentimientos.csv")