
#' Explore diet interactions
#'
#' @param prm.modify
#' @param run.dir
#' @param runs.modify
#' @param fungrouplist
#' @param atlantis.bgm
#' @param prm.name
#' @param init_file
#'
#' @return
#' @export
#'
#' @examples
check_diets_init <- function(prm.modify, run.dir, runs.modify, fungrouplist, atlantis.bgm, prm.name, init_file){

  # Code to explore Atlantis diet interactions from biol.prm and init.nc
  # Conceptually similar to Javier's ReactiveAtlantis code, but more detail
  # For each predator, it takes PPREY matrix and identifies the "favorite" prey items as prescribed in the preferences
  # For each prey items (by ontogenetic stage of the predator), it takes:
  # horizontal dists, vertical dists, gape size from biol.prm, weight at age from init.nc
  # The function returns: by stage: horizontal overlap (map and coefficient), vertical overlap; by age class: gape size limitations

  # Read data ---------------------------------------------------------------

  # geometry
  bgm <- rbgm::read_bgm(atlantis.bgm)

  amps_sf <- rbgm::box_sf(bgm)
  sf::st_crs(amps_sf) <- sf::st_crs(attr(amps_sf$geometry, "crs")$proj)

  # functional groups
  grps <- fungrouplist
  fg <- grps %>% dplyr::filter(Code!="DIN") %>% dplyr::pull(Code) # groups from group file, remove DIN from diets
  verts <- grps %>% dplyr::filter(GroupType %in% c("MAMMAL","BIRD","SHARK","FISH")) %>% dplyr::pull(Code)
  inverts <- dplyr::setdiff(fg, verts)

  # utility group tables
  vertebrate_groups <- grps %>% dplyr::filter(GroupType%in%c("FISH","SHARK","BIRD","MAMMAL")) %>% dplyr::mutate(BiomassType="vertebrate")
  plankton_groups <- grps %>% dplyr::filter(GroupType %in% c("PWN",'CEP','LG_ZOO','MED_ZOO','SM_ZOO','LG_PHY','SM_PHY')) %>%
    dplyr::mutate(BiomassType="plankton")
  bottom_groups <- grps %>% dplyr::filter(GroupType %in% c("MOB_EP_OTHER",'SED_EP_FF','SED_EP_OTHER','PHYTOBEN')) %>%
    dplyr::mutate(BiomassType="2D")
  other_groups <- grps %>% dplyr::filter(GroupType %in% c("LG_INF","MICROPHTYBENTHOS","SED_BACT","PL_BACT","SM_INF","CARRION","LAB_DET","REF_DET")) %>%
    dplyr::mutate(BiomassType="other")
  biomass_groups <- dplyr::bind_rows(vertebrate_groups,plankton_groups,bottom_groups,other_groups)

  # add to grps df
  grps <- grps %>% dplyr::left_join(biomass_groups)

  scenario.names <- prm.modify[prm.modify$run_no%in%runs.modify,]$run_name

  folder.paths <- paste0(run.dir,"/",scenario.names,"/outputFolder")

  folder.num <- 1:length(folder.paths)

  for(eachnum in folder.num){

    this.run <- scenario.names[eachnum]
    this.path <- folder.paths[eachnum]

    #needed in case the file is going to be overwritten
    system(paste0("sudo chmod -R a+rwx ", this.path), wait = TRUE)

    # age at maturity info
    agemat <- read.table(here::here("data-raw/age_mat.txt"))
    agemat <- agemat %>% dplyr::mutate(species = substr(V1, 1, 3)) %>% dplyr::rename(agemat = V2) %>% dplyr::select(species, agemat)


# prm of run to look at
bio_prm <- readLines(here::here(paste0("data-raw/",prm.name)))
# init.nc of run to look at

init <- tidync::tidync(paste0("data-raw/", init_file))
init_nc <- ncdf4::nc_open(paste0("data-raw/",init_file))

# Turn PRM and INIT to data frames  ---------------------------------------
# turn pprey matrix to a data frame to work with
no_age <- c(grps$GroupType[grps$NumStages==1], "DCsed", "DLsed", "DRsed")

# identify and index the PPREY matrix from the PRM file
diets_start <- grep("pPREY1BE1", bio_prm) # flag of the first line in the PRM - change to first species
pprey_ind <- which(startsWith(x=bio_prm, "pPREY")==T)
diets_end <- max(pprey_ind)+2
DM_prm <- bio_prm[seq(diets_start,diets_end)]
names_pprey <- bio_prm[pprey_ind]
val_pprey <- bio_prm[pprey_ind+1]

# get rid of tabs for AMPS
names_pprey <- gsub("\t", "", names_pprey)
val_pprey <- gsub("\t", " ", val_pprey)

DM_to_format <- t(
  sapply(seq(1, length(val_pprey)),
         function(x){
           vec <- unlist(strsplit(val_pprey[x], " "))
           vec <- vec[vec != ""] # drop trailing empty spaces if there are any
           return(vec)
         })
)

colnames(DM_to_format) <- c(fg,c("DCsed","DLsed","DRsed"))

pprey_mat <- DM_to_format %>%
  tidyr::as_tibble() %>%
  cbind(label=gsub("\\ .*","",names_pprey)) %>%
  cbind(Predator=gsub("pPREY","",gsub('[[:digit:]]+', '', gsub("\\ .*","",names_pprey)))) %>%
  cbind(PreyAgeClass=ifelse(substr(gsub("pPREY", "", gsub("\\ .*","", names_pprey)),1,1)%in%c(1,2),
                            substr(gsub("pPREY", "", gsub("\\ .*","", names_pprey)),1,1),
                            "1")) %>%
  cbind(PredatorAgeClass=ifelse(substr(gsub("pPREY", "", gsub("\\ .*","",names_pprey)),
                                       nchar(gsub("pPREY", "", gsub("\\ .*","",names_pprey))),
                                       nchar(gsub("pPREY", "", gsub("\\ .*","",names_pprey))))%in%c(1,2),
                                substr(gsub("pPREY", "", gsub("\\ .*","",names_pprey)),
                                       nchar(gsub("pPREY", "", gsub("\\ .*","",names_pprey))),
                                       nchar(gsub("pPREY", "", gsub("\\ .*","",names_pprey)))),
                                "2")
  ) %>%
  dplyr::mutate(PredatorAgeClass=ifelse(PredatorAgeClass==1,"juvenile", "adult"),
         PreyAgeClass=ifelse(PreyAgeClass==2,"adult", "juvenile")) %>%
  dplyr::select(c("label","Predator", "PreyAgeClass", "PredatorAgeClass", colnames(DM_to_format)))

# create dataframe of horizontal distributions
s <- data.frame()
for(i in 1:length(fg)){

  print(paste("doing s for", fg[i]))

  this_fg <- fg[i]

  # find the handles for the S parameter lines
  # how many s params?
  tot_lines <- grep(paste0("F", this_fg, "_S.*89"), bio_prm)
  f_lines <- length(tot_lines) / 2

  if(this_fg %in% verts) {
    s_pars <- c(
      paste0(paste0("F", this_fg, "_S"),1:f_lines," 89"),
      paste0(paste0("F", this_fg, "_S"),1:f_lines,"juv 89")
    )

    s_lines <- sapply(s_pars, function(s_par) grep(s_par, bio_prm))

  } else {
    s_pars <- paste0(paste0("F", this_fg, "_S"),1:f_lines,".*89")
    s_lines <- sapply(s_pars, function(s_par) grep(s_par, bio_prm))
    if(!is.numeric(s_lines)) {next} else {s_lines <- s_lines[1,]}

  }

  if(!is.numeric(s_lines)) {next}
  s_vec_lines <- s_lines + 2 # +2 because the s vector is 2 lines below in GOA prm, in other models may be different

  # read names and values
  s_df <- data.frame()
  for(j in 1:length(s_lines)){
    s_tmp <- trimws(bio_prm[s_vec_lines[j]], which  = "right")

    if(this_fg %in% verts){
      s_mat <- matrix(as.numeric(unlist(stringr::str_split(s_tmp, " "))), nrow = 1)
    } else {
      s_mat <- matrix(as.numeric(unlist(stringr::str_split(s_tmp, "   "))), nrow = 1) # inverts now are separated by 3 spaces
    }

    s_df_tmp <- data.frame(s_mat) %>% `colnames<-`(0:88)
    s_df_tmp <- cbind("name" = names(s_lines[j]), s_df_tmp)
    s_df <- rbind(s_df, s_df_tmp)
  }

  # add columns for stage and season
  s_df <- s_df %>%
    dplyr::mutate(species = this_fg,
           stage = ifelse(grepl("juv", name),
                          "juvenile",
                          "adult"),
           season = as.numeric(gsub("[^0-9]", "", gsub("89", "", name)))) %>%
    dplyr::select(-name)
  # pivot
  s_df_long <- s_df %>%
    tidyr::pivot_longer(cols = -c(species, stage, season), names_to = "b", values_to = "s") %>%
    dplyr::mutate(b = as.numeric(b))

  # add to df
  s <- rbind(s, s_df_long)
}

# create dataframe for vertical distributions
# there are day and night distributions for juveniles and adults, so 4 vectors each for verts, 2 each for inverts
v <- data.frame()
for(i in 1:length(fg)){

  print(paste("doing v for", fg[i]))

  this_fg <- fg[i]

  # find the handles for the VERTday and VERTnight parameter lines
  if(this_fg %in% verts) {
    v_pars <- c(
      paste0(paste0("VERTday_", this_fg),1:2,".*6"),
      paste0(paste0("VERTnight_", this_fg),1:2,".*6")
    )
  } else {
    v_pars <- c(
      paste0("VERTday_", this_fg,".*6"),
      paste0("VERTnight_", this_fg,".*6")
    )
  }

  v_lines <- sapply(v_pars, function(v_par) grep(v_par, bio_prm))
  if(!is.numeric(v_lines)) {next}
  v_vec_lines <- v_lines + 2 # +2 because the v vector is 2 lines below in GOA prm, in other models may be different

  # read names and values
  v_df <- data.frame()
  for(j in 1:length(v_lines)){
    v_mat <- matrix(as.numeric(unlist(stringr::str_split(bio_prm[v_vec_lines[j]], "   "))), nrow = 1)
    v_mat <- matrix(v_mat[,1:6],nrow=1)
    v_df_tmp <- data.frame(v_mat) %>% `colnames<-`(6:1) # 1 is the surface here
    v_df_tmp <- cbind("name" = names(v_lines[j]), v_df_tmp)
    v_df <- rbind(v_df, v_df_tmp)
  }

  # add columns for stage and day/night
  v_df <- v_df %>%
    dplyr::mutate(species = this_fg,
           stage = ifelse(grepl("1", name),
                          "juvenile",
                          "adult"),
           time = ifelse(grepl("day", name),
                         "day",
                         "night")) %>%
    dplyr::select(-name)
  # pivot
  v_df_long <- v_df %>%
    tidyr::pivot_longer(cols = -c(species, stage, time), names_to = "z", values_to = "v") %>%
    dplyr::mutate(z = as.numeric(z))

  # add to df
  v <- rbind(v, v_df_long)

}

# create a dataframe with gape size information
g <- data.frame()
for(i in 1:length(fg)){

  print(paste("doing g for", fg[i]))

  this_fg <- fg[i]

  # find the handles for the KLP_ and KUP parameter lines

  g_pars <- c(
    paste0("KLP_", this_fg),
    paste0("KUP_", this_fg)
  )

  g_lines <- sapply(g_pars, function(g_par) grep(g_par, bio_prm))
  if(!is.numeric(g_lines)) {next}

  # read names and values
  g_df <- data.frame()
  for(j in 1:length(g_lines)){
    this_g_line <- unlist(stringr::str_split(bio_prm[g_lines[j]], "   "))
    this_g_name <- this_g_line[1]
    this_g_val <- as.numeric(this_g_line[2])
    g_df_tmp <- data.frame("name" = this_g_name, "g" = this_g_val)
    g_df <- rbind(g_df, g_df_tmp)
  }

  # add columns for species and whether it is upper or lower limit
  g_df <- g_df %>%
    dplyr::mutate(species = this_fg,
           limit = ifelse(grepl("KLP", name),
                          "low",
                          "high")) %>%
    dplyr::select(-name)

  # add to df
  g <- rbind(g, g_df)

}


# diet_from_init("CMS")

# apply function
#purrr::map(verts, purrr::possibly(diet_from_init,NA, run.dir, this.run, pprey_mat))
lapply(verts, diet_from_init, run.dir, this.run, pprey_mat)
# test_verts <- c("DVR","FDF","HAP","MRO","MVR","POP","RAT","SB","SP")
# purrr::map(test_verts, possibly(diet_from_init,NA))
}
}
