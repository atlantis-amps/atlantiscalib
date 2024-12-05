#' Get group types
#'
#' @param fungrouplist
#' @param this.path
#'
#' @return
#' @export
#'
#' @examples
get_groups <- function(fungrouplist, this.path){

grp_list <- list()

  fgrps <- fungrouplist

  grp_list[['fgrps']] <- fgrps

  fun_grps <- fgrps %>%
    dplyr::mutate(Code=GroupType)

  # set up a functional group types table
  vertebrate_groups <- fun_grps %>%
    dplyr::filter(GroupType%in%c("FISH","SHARK","BIRD","MAMMAL")) %>%
    dplyr::mutate(BiomassType="vertebrate")

  plankton_groups <- fun_grps %>%
    dplyr::filter(GroupType %in% c("PWN",'CEP','LG_ZOO','MED_ZOO','SM_ZOO','LG_PHY','SM_PHY')) %>%
    dplyr::mutate(BiomassType="plankton")

  bottom_groups <- fun_grps %>%
    dplyr::filter(GroupType %in% c("MOB_EP_OTHER",'SED_EP_FF','SED_EP_OTHER','SEAGRASS','PHYTOBEN')) %>%
    dplyr::mutate(BiomassType="2D")

  other_groups <- fun_grps %>%
    dplyr::filter(GroupType %in% c("LG_INF","MICROPHTYBENTHOS","SED_BACT","PL_BACT","SM_INF","CARRION","LAB_DET","REF_DET"))%>%
    dplyr::mutate(BiomassType="other")

  biomass_groups <- dplyr::bind_rows(vertebrate_groups,plankton_groups,bottom_groups,other_groups)

  # add to grps df
  grps <- fun_grps %>%
    dplyr::left_join(biomass_groups) %>%
    dplyr::filter(IsTurnedOn==1)

  grp_list[['grps']] <- grps

  pred_groups <- fgrps %>% dplyr::filter(!GroupType %in% c("SM_PHY","CARRION","LAB_DET","PL_BACT","SED_BACT","PHYTOBEN","SEAGRASS")) %>%
    dplyr::filter(IsTurnedOn==1 & IsPredator==1) %>%
    dplyr::select(name, Code, longname)

  grp_list[['pred_groups']] <- pred_groups

  return(grp_list)
}
