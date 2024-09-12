#' Plot diet for predators
#'
#' @param predator
#'
#' @return
#' @export
#'
#' @examples
diet_from_init <- function(predator) {

  # This function takes a predator and its favorite 5 prey by ontogenetic stage (per PPREY matrix) and makes some plots:
  # Horizontal overlap (as the lowest value between s of predator and prey) - how do we handle stages and seasons?
  # vertical overlap
  # gape size overlap by age class (take size of pred, size of prey )
  # the whole concept of favorite prey items needs some thought
  #
  print(paste("Extracting info for", predator))
  # make a folder
  dir.create(paste("diets_from_init", this_run, predator, sep="/"), recursive = T)

  predator_name <- grps %>% filter(Code == predator) %>% pull(Name) # need this for the netcdf variables

  # pull pprey for this predator
  this_pprey <- pprey_mat %>%
    dplyr::filter(Predator == predator) %>%
    dplyr::select(-label) %>%
    tidyr::pivot_longer(cols = -c(Predator, PredatorAgeClass, PreyAgeClass), names_to = "Prey", values_to = "pprey") %>%
    dplyr::mutate(pprey = as.numeric(pprey)) #%>%
  #filter(Prey %in% verts)

  # make a plot of the predation according to pprey
  d_df <- this_pprey %>%
    dplyr::group_by(Predator, PredatorAgeClass, PreyAgeClass) %>%
    dplyr::mutate(tot = sum(pprey),
                  prop = pprey / tot) %>%
    dplyr::filter(prop > 0) %>%
    dplyr::arrange(Predator, PredatorAgeClass, PreyAgeClass, desc(prop)) %>%
    dplyr::mutate(cumprop = cumsum(prop)) %>%
    dplyr::ungroup()

  d_df <- d_df %>%
    dplyr::group_by(PredatorAgeClass) %>%
    dplyr::filter(if(min(cumprop) < .95) cumprop < .95 else TRUE) %>%
    dplyr::ungroup()


  this_pal <- grDevices::colorRampPalette(brewer.pal(12, "Paired"))

  d_plot <- d_df %>%
    ggplot2::ggplot()+
    ggplot2::geom_bar(ggplot2::aes(x = PreyAgeClass, y = prop, fill = Prey), stat = "identity")+
    ggplot2::scale_fill_manual(values = this_pal(length(unique(d_df$Prey)))) +
    ggplot2::theme_bw()+
    ggplot2::labs(x="",y="prop from PPREY",title = paste0(predator, "'s PPREY preferences (proportional < 0.95)"))+
    ggplot2::facet_grid(~PredatorAgeClass)

  d_file <- paste("diets_from_init", this_run, predator, "d.png", sep="/")

  ##########################################################################################
  # HORIZONTAL OVERLAP
  # pull horizontal distributions
  # of the predator
  s_pred <- s %>% dplyr::filter(species == predator)

  # need to do this starting from predator life stage
  pred_stages <- s_pred %>% pull(stage) %>% unique()
  s_overlap <- data.frame()
  for(i in 1:length(pred_stages)){

    #print(paste("pred_stage",i))

    fav_prey_df <- this_pprey %>%
      dplyr::filter(PredatorAgeClass == pred_stages[i]) %>%
      dplyr::group_by(Predator, PredatorAgeClass, PreyAgeClass) %>%
      dplyr::arrange(desc(pprey)) %>%
      dplyr::filter(pprey > 0) %>%
      dplyr::slice_head(n = 5) %>% # keep only the 5 favorite prey species
      ungroup()

    fav_prey <- fav_prey_df %>% dplyr::pull(Prey) %>% unique()

    if(length(fav_prey)==0) {next}

    # pull dists of the favorite prey for this life stage of the predator
    s_prey <- s %>% dplyr::filter(species %in% fav_prey)

    for(j in 1:length(fav_prey)){ # prey items eaten by this life stage of this predator

      #print(paste("fav_prey",j))

      prey_stages <- fav_prey_df %>% dplyr::filter(Prey == fav_prey[j]) %>% dplyr::pull(PreyAgeClass) %>% unique()

      # now subset s_prey data set to the fav_prey[j] that this loop iteration is doing
      this_s_prey <- s_prey %>% filter(species == fav_prey[j])

      if(nrow(this_s_prey)==0) {next}

      # for biomass pools we only have adult distributions.
      # To make the code work, duplicate it so that there are juvenile distributions for biomass pools
      # these will be identical to the "adult" distributions
      if(length(unique(this_s_prey$stage)) == 1){
        this_s_prey <- dplyr::rbind(this_s_prey,
                                    this_s_prey %>% dplyr::mutate(stage = "juvenile"))
      }

      for(k in 1:length(prey_stages)){ # life stages of this prey for the life stage of the predator

        #print(paste("prey_stage",k))

        #print(paste("stage", k))

        # seasons have different lengths for different species so we need to handle that
        # pick the one that is shortest and replicate each vector as necessary
        seasons_pred <- s_pred %>% dplyr::filter(stage == pred_stages[i]) %>% dplyr::pull(season) %>% unique()
        seasons_prey <- this_s_prey %>% dplyr::filter(stage == prey_stages[k]) %>% dplyr::pull(season) %>% unique()

        if(length(seasons_pred) >= length(seasons_prey)) {
          seasons <- seasons_pred

          # pad that from seasonal to monthly
          this_s_prey <- dplyr::bind_rows(replicate(length(seasons_pred)/length(seasons_prey),
                                                    this_s_prey %>% dplyr::filter(species == fav_prey[j]), simplify = FALSE))
          this_s_prey <- this_s_prey %>%
            dplyr::arrange(stage, b, season) %>%
            dplyr::mutate(new_season = rep(1:length(seasons), length.out = n())) %>%
            dplyr::select(-season) %>%
            dplyr::rename(season = new_season)

          this_s_pred <- s_pred

        } else { # other way round if seasons of the predator are fewer than seasons of the prey
          seasons <- seasons_prey

          # pad that from seasonal to monthly
          this_s_pred <- dplyr::bind_rows(replicate(length(seasons_prey)/length(seasons_pred), s_pred, simplify = FALSE))
          this_s_pred <- this_s_pred %>%
            dplyr::arrange(stage, b, season) %>%
            dplyr::mutate(new_season = rep(1:12, length.out = n())) %>%
            dplyr::select(-season) %>%
            dplyr::rename(season = new_season)

          this_s_prey <- this_s_prey
        }

        # now loop through seasons
        for (l in 1:length(seasons)) {

          #print(paste("season", l))

          s_pred_tmp <- this_s_pred %>%
            dplyr::filter(stage == pred_stages[i], season == seasons[l]) %>%
            dplyr::rename(pred = species, pred_stage = stage, s_pred = s)

          s_prey_tmp <- this_s_prey %>%
            dplyr::filter(stage == prey_stages[k], season == seasons[l]) %>%
            dplyr::rename(prey = species, prey_stage = stage, s_prey = s)

          # join
          s_overlap_tmp <- s_pred_tmp %>%
            dplyr::left_join(s_prey_tmp, by = c("season","b")) %>%
            dplyr::rowwise() %>%
            mutate(lo = min(c(s_pred, s_prey))) %>%
            ungroup() %>%
            mutate(overlap = sum(lo, na.rm = T)) %>% # a value of 0 means no overlap, 1 means complete overlap (identical dists, expected for cannibalism)
            dplyr::select(season, b, pred, pred_stage, prey, prey_stage, lo, overlap)

          # now bind into one dataframe for plotting
          s_overlap <- rbind(s_overlap, s_overlap_tmp)

        }
      }
    }

  }

  # if at this stage s_overlap has no rows it means that this group eats no prey and we should break the function
  if(nrow(s_overlap)==0){stop("This group eats nothing")}

  # drop na's and adjust the stages for facetting
  s_overlap <- s_overlap %>%
    drop_na(prey) %>%
    mutate(pred_stage = paste("predator", pred_stage, sep = ":"),
           prey_stage = paste("prey", prey_stage, sep = ":"))

  # make a plot per prey per season
  p <- list()
  idx <- 0
  for(i in 1:length(unique(s_overlap$prey))){

    this_prey <- unique(s_overlap$prey)[i]

    #print(this_prey)

    s_overlap_tmp <- s_overlap %>% filter(prey == this_prey)

    for(j in 1:length(unique(s_overlap_tmp$season))){

      this_season <- unique(s_overlap_tmp$season)[j]

      #print(this_season)

      # idx <- (i -1) * length(unique(s_overlap_tmp$season)) + j # make index for the list of plots
      idx <- idx + 1

      #print(idx)

      p[[idx]] <- amps_sf %>%
        dplyr::select(box_id) %>%
        dplyr:full_join(s_overlap_tmp, by = c("box_id" = "b")) %>%
        dplyr:filter(prey == unique(s_overlap_tmp$prey), season == unique(s_overlap_tmp$season)[j]) %>%
        ggplot4::ggplot()+
        ggplot4::geom_sf(ggplot4::aes(fill = lo)) +
        ggplot4::scale_fill_viridis() +
        ggplot4::theme_bw() +
        ggplot4::geom_text(ggplot4::aes(x=Inf,y=Inf,hjust=1,vjust=1,label=round(overlap,3)), color = "red")+
        ggplot4::labs(x="",y="",fill="Lower s", title = paste(predator,"eating",this_prey,"in season",this_season))+
        ggplot4::facet_grid(prey_stage~pred_stage)
    }
  }

  s_length <- length(p)

  s_plot <- patchwork::wrap_plots(p, ncol = 4) # 4 columns arrange the plots by seasons

  s_file <- paste("diets_from_init", this_run, predator, "horiz_overlap.png", sep="/")

  # ggsave(s_file, s_plot, width = 20, height = s_length, limitsize = FALSE)

  ##########################################################################################
  # VERTICAL OVERLAP
  # pull vertical distributions
  # of the predator
  v_pred <- v %>% dplyr::filter(species == predator)
  v_overlap <- data.frame()
  for(i in 1:length(pred_stages)){

    fav_prey_df <- this_pprey %>%
      dplyr::filter(PredatorAgeClass == pred_stages[i]) %>%
      dplyr::group_by(Predator, PredatorAgeClass, PreyAgeClass) %>%
      dplyr::arrange(desc(pprey)) %>%
      dplyr::filter(pprey > 0) %>%
      dplyr::slice_head(n = 5) %>%
      ungroup()

    fav_prey <- fav_prey_df %>% dplyr::pull(Prey) %>% unique()

    if(length(fav_prey)==0) {next}

    # pull vertical dists of the favorite prey for this life stage of the predator
    v_prey <- v %>% dplyr::filter(species %in% fav_prey)
    for(j in 1:length(fav_prey)){ # prey items eaten by this life stage of this predator

      prey_stages <- fav_prey_df %>% dplyr::filter(Prey == fav_prey[j]) %>% dplyr::pull(PreyAgeClass) %>% unique()

      for(k in 1:length(prey_stages)){ # life stages of this prey for the life stage of the predator

        v_pred_tmp <- v_pred %>%
          dplyr::filter(stage == pred_stages[i]) %>%
          dplyr::rename(pred = species, pred_stage = stage, v_pred = v)

        v_prey_tmp <- v_prey %>% dplyr::filter(species == fav_prey[j], stage == prey_stages[k]) %>%
          dplyr::rename(prey = species, prey_stage = stage, v_prey = v)

        # join
        v_overlap_tmp <- v_pred_tmp %>%
          dplyr::left_join(v_prey_tmp, by = c("time","z")) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(lo = min(c(v_pred, v_prey))) %>%
          dplyr::group_by(time) %>%
          dplyr::mutate(overlap = sum(lo)) %>% # a value of 0 means no overlap, 1 means complete overlap (identical dists, expected for cannibalism)
          dplyr::ungroup() %>%
          dplyr::select(time, z, pred, pred_stage, prey, prey_stage, lo, overlap)


        # now bind into one dataframe for plotting
        v_overlap <- rbind(v_overlap, v_overlap_tmp)

      }
    }

  }

  # drop na's and adjust the stages for facetting
  v_overlap <- v_overlap %>%
    drop_na(prey) %>%
    dplyr::mutate(pred_stage = paste("predator", pred_stage, sep = ":"),
                  prey_stage = paste("prey", prey_stage, sep = ":"))

  p <- list()
  for(i in 1:length(unique(v_overlap$prey))){

    this_prey <- unique(v_overlap$prey)[i]

    vp <- v_overlap %>%
      dplyr::filter(prey == unique(v_overlap$prey)[i])

    p[[i]] <- vp %>%
      ggplot2::ggplot()+
      ggplot2::geom_tile(ggplot2::aes(x = time, y = factor(-z), fill = lo)) +
      ggplot2::scale_fill_viridis() +
      ggplot2::theme_bw() +
      ggplot2::geom_text(aes(x=time,y=Inf,hjust=1,vjust=1,label=round(overlap,2)), color = "red")+
      ggplot2::labs(x="", y="z (1 = surface)", fill="Lower VERT", title = paste(predator,"eating",this_prey))+
      ggplot2::facet_grid(prey_stage~pred_stage)

  }

  v_plot <- patchwork::wrap_plots(p, ncol = 2) # 4 columns arrange the plots by seasons

  v_file <- paste("diets_from_init", this_run, predator, "vert_overlap.png", sep="/")

  ##########################################################################################
  # GAPE SIZE OVERLAP
  # ONLY DO THIS FOR VERTEBRATE PREY!
  # pull gape size information of the predator
  g_pred <- g %>% dplyr::filter(species == predator)

  # pull weight-at-age information from initial conditions
  # of the predator
  fg_atts <- grps %>% dplyr::filter(Code==predator)

  if(fg_atts$BiomassType!="vertebrate") stop("weight at age only for vertebrates.")

  #Extract from the init.nc file the appropriate reserve N time series variables
  resN_vars <- tidync::hyper_vars(init) %>% # all variables in the .nc file active grid
    dplyr::filter(grepl("_ResN",name)) %>% # filter for reserve N
    dplyr::filter(grepl(predator_name,name)) # filter for specific functional group

  #Extract from the output .nc file the appropriate structural N time series variables
  strucN_vars <- tidync::hyper_vars(init) %>% # all variables in the .nc file active grid
    dplyr::filter(grepl("_StructN",name)) %>% # filter for structural N
    dplyr::filter(grepl(predator_name,name)) # filter for specific functional group

  # Use pmap with only the variable names
  resN <- purrr::map(resN_vars$name, get_nc_fillval) %>% bind_rows()
  strucN <- purrr::map(strucN_vars$name, get_nc_fillval) %>% bind_rows()

  # combine, get total mgN, and convert to g
  weight <- resN %>%
    dplyr::left_join(strucN, by = c("species","age")) %>%
    dplyr::mutate(totN = value.x+value.y,
                  weight_g = totN * 20 * 5.7 / 1000) %>%
    dplyr::select(species,age,weight_g)

  # add upper and lower limit from gape size
  weight <- weight %>%
    dplyr::mutate(low = weight_g * (g_pred %>% filter(limit == "low") %>% pull(g)),
                  high = weight_g * (g_pred %>% filter(limit == "high") %>% pull(g)))

  # add stage and species code
  pred_weight <- weight %>%
    dplyr::left_join(grps %>% dplyr::select(Code, Name), by = c("species"="Name")) %>%
    dplyr::left_join(agemat, by = c("Code"="species")) %>%
    dplyr::mutate(age = age -1,
                  stage = ifelse(age < agemat, "juvenile", "adult")) %>%
    dplyr::select(species, stage, age, low, high)

  # now for the prey species, but by predator stage?
  eaten <- data.frame()
  for(i in 1:length(pred_stages)){

    fav_prey_df <- this_pprey %>%
      dplyr::filter(Prey %in% verts) %>% # ONLY VERTS
      dplyr::filter(PredatorAgeClass == pred_stages[i]) %>%
      dplyr::group_by(Predator, PredatorAgeClass, PreyAgeClass) %>%
      dplyr::arrange(desc(pprey)) %>%
      dplyr::filter(pprey > 0) %>%
      dplyr::slice_head(n = 5) %>%
      ungroup()

    fav_prey <- fav_prey_df %>% dplyr::select(Prey) %>% distinct()

    if(nrow(fav_prey)==0) {next}

    prey_names <- fav_prey %>%
      dplyr::left_join(grps %>% dplyr::select(Code, Name), by = c("Prey"="Code")) %>%
      dplyr::pull(Name)

    pattern <- paste(prey_names, collapse = "|") # Create a single pattern string

    resN_vars <- tidync::hyper_vars(init) %>% # all variables in the .nc file active grid
      dplyr::filter(grepl("_ResN",name)) %>% # filter for reserve N
      dplyr::filter(grepl(pattern,name)) # filter for specific functional group

    strucN_vars <- tidync::hyper_vars(init) %>% # all variables in the .nc file active grid
      dplyr::filter(grepl("_StructN",name)) %>% # filter for structural N
      dplyr::filter(grepl(pattern,name)) # filter for specific functional group

    # Use pmap with only the variable names
    resN <- purrr::map(resN_vars$name, get_nc_fillval) %>% dplyr::bind_rows()
    strucN <- purrr::map(strucN_vars$name, get_nc_fillval) %>% dplyr::bind_rows()

    # combine, get total mgN, and convert to g
    weight <- resN %>%
      dplyr::left_join(strucN, by = c("species","age")) %>%
      dplyr::mutate(totN = value.x+value.y,
             weight_g = totN * 20 * 5.7 / 1000) %>%
      dplyr::select(species,age,weight_g)

    # add stage and species code
    weight <- weight %>%
      dplyr::left_join(grps %>% dplyr::select(Code, Name), by = c("species"="Name")) %>%
      dplyr::left_join(agemat, by = c("Code"="species")) %>%
      dplyr::mutate(age = age -1,
             stage = ifelse(age < agemat, "juvenile", "adult")) %>%
      dplyr::select(species, stage, age, weight_g)

    # get age classes for this stage of the predator
    pred_weight_tmp <- pred_weight %>%
      dplyr::filter(stage == pred_stages[i])

    pred_ages <- pred_weight_tmp %>% dplyr::pull(age)

    # we need to do this by prey life stage or else they'll show in the table
    # do it by prey
    # and by life stage of the prey as informed in fav_prey_df
    for(j in 1:nrow(fav_prey)){

      this_prey <- fav_prey[j,1] %>% dplyr::pull()

      # identify stages of the prey for this predator stage
      prey_stages <- fav_prey_df %>% dplyr::filter(Prey == this_prey) %>% dplyr::pull(PreyAgeClass)

      for(k in 1:length(prey_stages)){

        prey_weight_tmp <- weight %>%
          dplyr::filter(species == prey_names[j], stage == prey_stages[k])

        # loop over predator's age classes

        for(l in 1:length(pred_ages)){

          lo <- pred_weight_tmp[l,]$low
          hi <- pred_weight_tmp[l,]$high

          ages_eaten <- prey_weight_tmp %>%
            dplyr::mutate(eaten = ifelse(between(weight_g, lo, hi), 1, 0)) %>%
            dplyr::filter(eaten > 0) %>%
            dplyr::pull(age)


          eaten_tmp <- tidyr::tibble(predator,
                              "stage" = pred_stages[i],
                              "age" = pred_ages[l],
                              "prey" = fav_prey[j,1] %>% pull(Prey),
                              "eaten_prey_ages" = list(ages_eaten))

          eaten <- rbind(eaten, eaten_tmp)

        }


      }


    }

  }


  eaten <- eaten %>% dplyr::group_by(predator,stage,age,prey) %>%
    dplyr::summarize(eaten_prey_ages = list(sort(unlist(eaten_prey_ages))))

  # need to split juvenile table from adult table
  # Function to replace numeric(0) with NA
  replace_numeric0_with_NA <- function(x) {
    if(is.list(x)) {
      lapply(x, function(y) if(length(y) == 0) "" else y)
    } else {
      x
    }
  }

  for(i in 1:length(pred_stages)){

    # rearrange and write out a table
    eaten_wide <- eaten %>%
      dplyr::filter(stage == pred_stages[i]) %>%
      dplyr::pivot_wider(id_cols = c(predator,stage,age), names_from = prey, values_from = eaten_prey_ages)

    # Apply this function to each column of the data frame
    eaten_wide[] <- lapply(eaten_wide, replace_numeric0_with_NA)

    # sort
    eaten_wide <- eaten_wide %>% arrange(age)

    # Create a kable table
    gape_kable <- eaten_wide %>%
      knitr::kable(format = "markdown", booktabs = TRUE)

    # Save the table as PDF
    # Using R Markdown
    # For this, you'll need to put your table creation code in an R Markdown document
    # and then knit the document to PDF.

    yaml_header <- paste0(
      "---\n",
      "title: '", predator, "s favorite vertebrate prey items (with age class)'\n",
      "output: \n",
      "  pdf_document:\n",
      "    latex_engine: xelatex\n",
      "header-includes:\n",
      "  - '\\usepackage{graphicx}'\n",
      "  - '\\usepackage{grffile}'\n",
      "  - '\\tiny' # Set the font size\n",
      "geometry: landscape\n",
      "---\n"
    )

    g_file <- paste("diets_from_init", this_run, predator, paste0("gape_table_", pred_stages[i], ".Rmd"), sep="/")
    cat(yaml_header, gape_kable, sep="\n", file=g_file)
    rmarkdown::render(g_file, output_format = "pdf_document")

  }


  ##########################################################################################
  # write everything out
  # diet preferences
  ggplot4::ggsave(d_file, d_plot, width = 8, height = 7)

  # horizontal distributions
  ggplot4::ggsave(s_file, s_plot, width = 22, height = s_length*1.1, limitsize = FALSE)

  # vertical distributions
  ggplot4::ggsave(v_file, v_plot, width = 10, height = 12)

}
