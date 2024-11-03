#' Modify pprey
#'
#' @param eachrow row from calibration spreadsheet
#' @param biology.prm biology prm
#'
#' @return
#' @export
#'
#' @examples
modify_pprey <- function(eachrow, biology.prm){

    # Load packages into session

    pred.mod <- eachrow$pred_mod
    prey.mod <- eachrow$prey_mod
    pred.group <- eachrow$pred_group
    prey.group <- eachrow$prey_group
    pred.stage <- eachrow$pred_stage
    prey.stage <- eachrow$prey_stage
    pred.mod.val <- eachrow$pred_mod_val
    prey.mod.val <- eachrow$prey_mod_val

    pprey.cols <- c("BB","PB","PL","PS","MA","SG","ZS","ZM","ZL","ZG","AUR","SQX","BMD","BD","BG","BMS","DUN","BML","PWN","BFF","BIV","GEC","BC","HEP","HEC","FPS","POP","CHY","CHS","CSY","CSS","CSN","CDS","CNY","CNS","CHC","CYE","CKS","CRH","CRW","CRC","COH","COS","COD","COY","COR","CDR","CMS","CMF","CMH","PIS","SAL","SAF","FMM","FVS","ROC","MRO","DVR","MVR","SMD","FDF","HAP","DOG","SBL","SSK","RAT","SB","SP","BE","HSL","CSL","PIN","PHR","ROR","TOR","HUW","DL","DR","DC","DL_s","DR_s","DC_s")

    pprey.index <- 1:length(pprey.cols)

    names(pprey.index) <- pprey.cols

    pprey.matrix <- biology.prm[17162:17846]

    pprey.names <- grep("^pPREY",pprey.matrix, value = TRUE) %>%
      gsub("\t","", .) %>%
      tibble::as_tibble() %>%
      tidyr::separate(value,into=c("pprey_name","entries"), sep=" ") %>%
      dplyr::mutate(preystage= pprey_name) %>%
      tidyr::separate(preystage, into=c("ppreystage","ppredstage"), sep=6) %>%
      dplyr::mutate(preystage = stringr::str_extract(ppreystage, "[0-9]")) %>%
      dplyr::mutate(predstage = stringr::str_extract(ppredstage, "[0-9]")) %>%
      dplyr::mutate(pred = stringr::str_extract(ppredstage, "[:alpha:]+[ ?[:alpha:]]*"))

    pprey.values <- pprey.matrix[!grepl("^pPREY", pprey.matrix)] %>%
      gsub("\t"," ", .) %>%
      tibble::as_tibble() %>%
      tidyr::separate(value,into=pprey.cols, sep=" ") %>%
      dplyr::mutate_at(dplyr::vars(BB:DR_s), as.numeric) %>%
      dplyr::filter(!is.na(BB)) %>%
      dplyr::bind_cols(pprey.names, .) %>%
      dplyr::mutate(index = 1:nrow(.)) %>%
      dplyr::select(index, everything()) %>%
      dplyr::mutate(across(c(BB:DC_s), as.numeric))


    if(prey.mod == 1){

      #obtain index for prey
      prey.index <- as.numeric(pprey.index[names(pprey.index)==prey.group]) + 8

      new.prey <- pprey.values[,prey.index]*prey.mod.val

      pprey.values[,prey.index] <- new.prey

    }

    if(pred.mod == 1){

      mod.row <-  pprey.values[pprey.values$pred==pred.group & pprey.values$preystage==prey.stage & pprey.values$predstage==pred.stage,]

      row.index <- mod.row$index

      mod.row.mod <- mod.row[,9:ncol(pprey.values)]

      mod.row.num <- mod.row.mod*pred.mod.val

      #modify prey by index

      pprey.values[pprey.values$index==row.index, 9:ncol(pprey.values)] <- mod.row.num

    }

    pprey.corr.matrix <- pprey.values

    pprey.vec <- vector()

    for(eachrow in 1:nrow(pprey.corr.matrix)){

      print(eachrow)

      pprey.vec[eachrow] <- paste(paste(as.character(c(pprey.corr.matrix[eachrow,2],pprey.corr.matrix[eachrow,3])), collapse = " "),
                                  paste("\n"),
                                  paste(pprey.corr.matrix[eachrow,9:ncol(pprey.corr.matrix)] , collapse = " "),
                                  paste("\n"))

      print(pprey.vec[eachrow])
    }

    biology.prm <- biology.prm[-c(17162:17846)]

    biology.prm[17162:(17162+length(pprey.vec))] <- pprey.vec

    return(biology.prm)
  }



