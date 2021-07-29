library(tidyverse)
library(readxl)

# 7,593 responses - 16 NAs
# 7,577 responses
raw_data <- read_excel("data/Padrões de Comportamento com relação ao COVID-19 (Responses).xlsx") %>% 
  drop_na()


# Constructs --------------------------------------------------------------

confidence <- c(
  # Você acredita que comportamento saudáveis podem ajudá-lo a se manter saudável após ler informações dos veículos de comunicação?
  "hb_a_pbe",  # 5-point likert
  # Você se sente seguro em manter comportamentos saudáveis?
  "hb_a_se",  # 5-point likert
  # O quão confiante você está com relação à: Habilidade do ...
  # 4-point likert
  "confi_gov",
  "confi_hos",
  "confi_wor",
  "confi_media")


vulnerability <- c(
  # Você acredita que é suscetível a ser infectado pelo COVID-19 depois de receber informações dos veículos de comunicação?
  "hb_b_psu",  # 5-point likert
  # Você acredita que contrair COVID-19 é um risco severo para sua sáude após ler informações dos veículos de comunicação?
  "hb_b_pse",  # 5-point likert
  # O quanto você está com medo de contrair o Coronavírus?
  "afra1",  # 4-point likert
  # O quanto você está com medo de que algum familiar próximo contraia o Coronavírus?
  "afra2"  # 4-point likert
)

behavior <- c(
  # O quão frequente você realizou as seguintes ações durante a pandemia?
  # 5-point likert
  "be_01",  # locais com tranmissão
  "be_02",  # lavar mãos
  "be_03",  # evitou tocar no rosto
  "be_04",  # cotoveo tosse/espirro
  "be_05",  # guardanapo no lixo
  "be_06",  # compartilha toalha
  "be_07",  # ventilar comodos
  "be_08",  # manter 1m distância
  "be_09",  # aperto de mão
  "be_10",  # utensílios comuns
  "be_11",  # produtos animais
  "be_12",  # visitar parentes
  "be_13",  # tocar objetos em público
  "be_14",  # locais aglomerados
  "be_15",  # cuspir no chão
  "be_16",  # comida saudáveis
  "be_17",  # exercício físico
  "be_18",  # higienizou residência
  "be_19",  # máscara facial
  "be_20"  # transporte público
)

control_vars <- c(
  "age",
  "sex",
  "marriage",
  "education",
  "income"
)

processed_data <- raw_data %>% 
  mutate_at(vars(starts_with("hb_")), ~ case_when(
    . == "Discordo fortemente" ~ 1,
    . == "Discordo" ~ 2,
    . == "Não concordo nem discordo" ~ 3,
    . == "Concordo" ~ 4,
    . == "Concordo fortemente" ~ 5)) %>% 
  mutate_at(vars(starts_with("confi_")), ~ case_when(
    . == "Totalmente discrente" ~ 1,
    . == "Com um pouco de dúvida" ~ 2,
    . == "Confiante" ~ 3,
    . == "Muito confiante" ~ 4)) %>% 
  mutate_at(vars(starts_with("afra")), ~ case_when(
    . == "Não estou com medo" ~ 1,
    . == "Um pouco amedrontado" ~ 2,
    . == "Amedrontado" ~ 3,
    . == "Bastante amedrontado" ~ 4)) %>% 
  mutate_at(vars(starts_with("be_")), ~ case_when(
    . == "Nunca" ~ 1,
    . == "Pouco" ~ 2,
    . == "Algumas vezes" ~ 3,
    . == "Frequentemente" ~ 4,
    . == "Sempre" ~ 5)) %>% 
  mutate(
    age = case_when(
      age == "Abaixo de 17 anos" ~ 1,
      age == "18-30 anos" ~ 2,
      age == "31-50 anos" ~ 3,
      age == "51-70 anos" ~ 4,
      age == "Acima de 70 anos" ~ 5
    ),
    sex = case_when(
      sex == "Feminino" ~ "Feminino",
      sex == "Masculino" ~ "Masculino",
      sex == "Prefiro não dizer" ~ NA_character_
    ),
    sex = as_factor(sex),
    marriage = as_factor(marriage),
    income = case_when(
      income == "Até R$ 178" ~ 1,
      income == "De R$ 179 a R$ 368" ~ 2,
      income == "De R$ 369 a R$ 1.008" ~ 3,
      income == "De R$ 1.009 a R$ 3.566" ~ 4,
      income == "Acima de R$ 3.566" ~ 5
    )
  ) %>% 
  drop_na() %>% 
  mutate(
    behavior = be_01 + be_02 + be_03 + be_04 + be_05 + be_07 + be_08 + be_09 + be_10 + be_12 + be_13 + be_14 + be_15 + be_16 + be_17 + be_18 + be_19 + be_20,
    vulnerability = hb_b_psu + hb_b_pse + afra1 + afra2
  )

summ_table <- raw_data %>% 
  mutate_at(vars(starts_with("hb_")), ~ case_when(
    . == "Discordo fortemente" ~ 1,
    . == "Discordo" ~ 2,
    . == "Não concordo nem discordo" ~ 3,
    . == "Concordo" ~ 4,
    . == "Concordo fortemente" ~ 5)) %>% 
  mutate_at(vars(starts_with("confi_")), ~ case_when(
    . == "Totalmente discrente" ~ 1,
    . == "Com um pouco de dúvida" ~ 2,
    . == "Confiante" ~ 3,
    . == "Muito confiante" ~ 4)) %>% 
  mutate_at(vars(starts_with("afra")), ~ case_when(
    . == "Não estou com medo" ~ 1,
    . == "Um pouco amedrontado" ~ 2,
    . == "Amedrontado" ~ 3,
    . == "Bastante amedrontado" ~ 4)) %>% 
  mutate_at(vars(starts_with("be_")), ~ case_when(
    . == "Nunca" ~ 1,
    . == "Pouco" ~ 2,
    . == "Algumas vezes" ~ 3,
    . == "Frequentemente" ~ 4,
    . == "Sempre" ~ 5)) %>% 
  mutate(
    age = as_factor(age),
    sex = case_when(
      sex == "Feminino" ~ "Feminino",
      sex == "Masculino" ~ "Masculino",
      sex == "Prefiro não dizer" ~ NA_character_
    ),
    sex = as_factor(sex),
    marriage = as_factor(marriage),
    income = as_factor(income)
  ) %>% 
  drop_na() %>% 
  mutate(
    behavior = be_01 + be_02 + be_03 + be_04 + be_05 + be_07 + be_08 + be_09 + be_10 + be_12 + be_13 + be_14 + be_15 + be_16 + be_17 + be_18 + be_19 + be_20,
    vulnerability = hb_b_psu + hb_b_pse + afra1 + afra2
  ) %>% 
  select(behavior,  # Dependent
         hb_a_se, confi_gov, confi_hos, confi_wor, confi_media,  # Independent
         vulnerability,  # Moderator
         age, sex, marriage, income  # control
  )
