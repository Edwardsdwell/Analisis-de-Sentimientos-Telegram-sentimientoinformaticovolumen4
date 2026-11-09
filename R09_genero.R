library(tidyverse); library(stringi); library(scales)
source("R01_config.R")
SOLO_DICCIONARIO <- FALSE     # TRUE = usa solo nombres del diccionario (mas preciso, menos cobertura)

B <- readRDS(file.path(DIR_PROC, "listo_sent.rds")); d <- B$d; sent <- B$sent
cols <- paste0("s_", sent)
nom <- readRDS(file.path(DIR_PROC, "_nombres_LOCAL.rds"))

H <- c("alejandro","alvaro","andres","angel","antonio","armando","arturo","benjamin","bryan","brayan",
       "carlos","cesar","christian","cristian","cristhian","daniel","david","diego","edgar","eduardo","edwin",
       "elmer","emanuel","enrique","erick","erik","ernesto","fabian","felipe","fernando","flavio","franco",
       "francisco","freddy","gabriel","gerardo","gonzalo","gustavo","hector","helmer","henry","hernan","hugo",
       "humberto","ian","ivan","jaime","javier","jean","jefferson","jeferson","jesus","jhon","john","jhonny",
       "johnny","jorge","jose","josue","juan","julio","kevin","leonardo","leandro","luis","marco","marcos",
       "mario","mauricio","miguel","nelson","nicolas","oscar","pablo","patricio","paul","pedro","rafael",
       "ramiro","raul","reynaldo","ricardo","roberto","rodrigo","roger","ronald","ruben","samuel","santiago",
       "sebastian","sergio","steven","stephen","victor","vladimir","walter","wilson","willy","wilmer","william",
       "yamil","yerko","zenon","adrian","abraham","adolfo","agustin","aldo","alfredo","alan","alberto",
       "amilcar","anibal","boris","bruno","camilo","cristobal","dennis","denis","dylan","efrain","elias",
       "emilio","ezequiel","fidel","franz","froilan","gael","gilmar","guido","gary","giovanni","ismael",
       "israel","joel","jonathan","jhonatan","jhoel","limbert","lucas","mathias","matias","maximo","moises",
       "noel","omar","orlando","osvaldo","ramon","renato","rene","rolando","rosendo","saul","teodoro",
       "tomas","ulises","vicente")
M <- c("adriana","alejandra","alexandra","alicia","alison","alondra","ana","andrea","angela","angelica",
       "antonia","ariana","ashley","barbara","beatriz","belen","brenda","camila","carla","carmen","carolina",
       "cecilia","claudia","cristina","daniela","daysi","deysi","diana","dayana","elena","elizabeth","erika",
       "esther","eva","fabiola","fernanda","flor","gabriela","gladys","gloria","graciela","helen","ines",
       "irene","isabel","ivana","jackeline","jazmin","jennifer","jessica","jimena","joana","johana","judith",
       "julia","juliana","karen","karina","katherine","katia","kimberly","laura","leticia","lidia","liliana",
       "lizeth","lourdes","lucia","luz","magaly","maria","mariana","marcela","margarita","marisol","marta",
       "mayra","melany","melissa","mercedes","micaela","mirian","miriam","monica","nadia","natalia","natalie",
       "nancy","nayra","noemi","norma","olga","paola","patricia","paula","pamela","rachel","raquel","rebeca",
       "rocio","rosa","rosario","rosmery","ruth","sabrina","sandra","sara","silvia","sofia","sonia",
       "stephanie","tamara","tatiana","teresa","valentina","valeria","vanessa","vera","veronica","victoria",
       "viviana","wendy","ximena","yesica","yessica","yolanda","zulma","abigail","aracely","araceli","ayelen",
       "bianca","brigitte","celeste","dalia","dulce","evelyn","fatima","jhoselin","joselin","juana","lesly",
       "leslie","lilian","lisbeth","lorena","luciana","mabel","madeleine","maite","maribel","marlene",
       "milenka","mishel","michelle","nicole","nelly","nohemi","pilar","priscila","romina","scarlett",
       "selena","shirley","susana","thalia","vivian","yamila","yenny","zoe")
exc_a <- c("luca","joshua","noah","nikita","garcia")   # terminan en -a pero son hombres
exc_o <- c("rocio","consuelo","socorro")               # terminan en -o pero son mujeres

primer <- function(x) {
  if (is.na(x)) return(NA_character_)
  x <- stri_trans_general(tolower(x), "Latin-ASCII")
  tk <- strsplit(str_squish(stri_replace_all_regex(x, "[^a-z ]", " ")), " ")[[1]]
  tk <- tk[nchar(tk) >= 2 & !tk %in% c("lic","ing","dr","dra","ph","mgr","el","la","don","dona","mr","ms")]
  if (length(tk) == 0) NA_character_ else tk[1]
}
clas <- function(p) {
  if (is.na(p))                                                    return(c("Indeterminado", "sin nombre"))
  if (p %in% H)                                                    return(c("Hombre", "diccionario"))
  if (p %in% M)                                                    return(c("Mujer", "diccionario"))
  if (!SOLO_DICCIONARIO && nchar(p) >= 4 && grepl("a$", p) && !p %in% exc_a) return(c("Mujer", "terminacion"))
  if (!SOLO_DICCIONARIO && nchar(p) >= 4 && grepl("o$", p) && !p %in% exc_o) return(c("Hombre", "terminacion"))
  c("Indeterminado", "sin regla")
}
pn <- map_chr(nom$nombre, primer)
cl <- t(vapply(pn, clas, character(2)))
us <- tibble(usuario = nom$usuario, sexo = cl[, 1], metodo = cl[, 2])
write_csv(us, file.path(DIR_PROC, "usuarios_sexo.csv"))       # solo hash + sexo, sin nombres

cat("--- Usuarios por sexo inferido y metodo ---\n"); print(count(us, sexo, metodo))

bs <- d %>% filter(usable) %>% left_join(us, by = "usuario") %>%
  mutate(sexo = coalesce(sexo, "Indeterminado"))
cat("\n--- Mensajes analizados por sexo ---\n"); print(bs %>% count(sexo) %>% mutate(pct = percent(n / sum(n), 0.1)))

bs$con_sent <- bs$n_senal > 0
cat("\n% de mensajes con algun sentimiento, por sexo:\n")
print(bs %>% group_by(sexo) %>% summarise(mensajes = n(), con_sent = percent(mean(con_sent), 0.1),
                                          sent_por_msg = round(mean(n_senal), 3)))

bh <- bs %>% filter(sexo %in% c("Hombre", "Mujer"))
res <- map_dfr(sent, function(s) {
  v <- bh[[paste0("s_", s)]] > 0
  kH <- sum(v[bh$sexo == "Hombre"]); nH <- sum(bh$sexo == "Hombre")
  kM <- sum(v[bh$sexo == "Mujer"]);  nM <- sum(bh$sexo == "Mujer")
  p <- suppressWarnings(prop.test(c(kH, kM), c(nH, nM))$p.value)
  tibble(sentimiento = s, kH, kM, pct_H = kH / nH, pct_M = kM / nM, p = p)
}) %>% mutate(p_holm = p.adjust(p, "holm"),
              mas_expresa = if_else(pct_H > pct_M, "Hombre", "Mujer"),
              signif = p_holm < 0.05)
print(res %>% arrange(p_holm) %>% mutate(across(c(pct_H, pct_M), ~ round(. * 100, 2))), n = 20)
write_csv(res, file.path(DIR_TAB, "sentimientos_por_sexo.csv"))

pl <- res %>% mutate(lab = paste0(sentimiento, if_else(signif, " *", ""))) %>%
  pivot_longer(c(pct_H, pct_M), names_to = "sexo", values_to = "pct") %>%
  mutate(sexo = recode(sexo, pct_H = "Hombre", pct_M = "Mujer"))
g <- ggplot(pl, aes(reorder(lab, pct), pct, fill = sexo)) +
  geom_col(position = "dodge") + coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c(Hombre = "#2171b5", Mujer = "#d6604d"), name = NULL) +
  labs(title = "Sentimientos segun sexo inferido por nombre",
       subtitle = "% de los mensajes de cada grupo que expresan el sentimiento  (* = diferencia significativa, Holm p<0.05)",
       x = NULL, y = NULL, caption = "Sexo inferido por diccionario de nombres; no es un dato declarado") +
  theme_minimal(base_size = 11)
ggsave(file.path(DIR_GRAF, "g_sentimientos_sexo.png"), g, width = 10, height = 6.5, dpi = 150); print(g)

registrar("8. Genero", "Sexo inferido por diccionario de nombres + terminacion; sentimientos por sexo con prueba de proporciones",
          nrow(bs), nrow(bs), "usuarios_sexo.csv, g_sentimientos_sexo.png")
cat("OK.\n")