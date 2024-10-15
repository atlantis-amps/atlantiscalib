
#path with base run
base.path <- "/nfsdata/ampscalibration"
#directory where to store runs
run.dir <- "/nfsdata/ampscalibration"
#name of biology prm
prm.name <- "AMPSbioparam_mv1_2024_V7.prm"



eachrun <- 1
this.run <- prm.modify[prm.modify$run_no==eachrun,]
base.run <- this.run$based_on_run
bio.prm <- readLines(paste0(base.path,"/", base.run,"/", prm.name))
ppreymod.col <- 1
ppreymod.row <- 0
pred.group <- "PS"
pred.stage <- "1"


pprey.cols <- c("BB","PB","PL","PS","MA","SG","ZS","ZM","ZL","ZG","AUR","SQX","BMD","BD","BG","BMS","DUN","BML","PWN","BFF","BIV","GEC","BC","HEP","HEC","FPS","POP","CHY","CHS","CSY","CSS","CSN","CDS","CNY","CNS","CHC","CYE","CKS","CRH","CRW","CRC","COH","COS","COD","COY","COR","CDR","CMS","CMF","CMH","PIS","SAL","SAF","FMM","FVS","ROC","MRO","DVR","MVR","SMD","FDF","HAP","DOG","SBL","SSK","RAT","SB","SP","BE","HSL","CSL","PIN","PHR","ROR","TOR","HUW","DL","DR","DC","DL_s","DR_s","DC_s")

pprey.matrix <- bio.prm[17161:17846]
pprey.names <- grep("^pPREY",pprey.matrix, value = TRUE) %>%
  gsub("\t","", .) %>%
  tibble::as_tibble() %>%
  tidyr::separate(value,into=c("pprey_name","entries"), sep=" ") %>%
  dplyr::mutate(preystage= pprey_name) %>%
  tidyr::separate(preystage, into=c("ppreystage","ppredstage"), sep=6) %>%
  dplyr::mutate(preystage = stringr::str_extract(ppreystage, "[0-9]")) %>%
  dplyr::mutate(predstage = stringr::str_extract(ppredstage, "[0-9]")) %>%

  tidyr::separate(ppredstage, into = c("pred", "predstage"), sep = "(?<=\\d) (?=\\w)")

pprey.values <- pprey.matrix[!grepl("^pPREY",pprey.matrix)] %>%
  gsub("\t"," ", .) %>%
  tibble::as_tibble() %>%
  tidyr::separate(value,into=pprey.cols, sep=" ") %>%
  dplyr::mutate_at(dplyr::vars(BB:DR_s), as.numeric) %>%
  dplyr::filter(!is.na(BB)) %>%
  dplyr::bind_cols(pprey.names, .)



if(ppreymod.col == 1){


}

