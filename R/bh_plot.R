#' Create Beverton-Holt curves
#'
#' @param this.nc
#' @param fg.table
#' @param bio.prm
#' @param prm.modify
#' @param runs.modify
#' @param run.dir
#'
#' @return pdf output curves
#' @export
#'
#' @examples
bh_plot <- function(this.nc, fg.table, bio.prm, prm.modify, runs.modify, run.dir){


  these.runs <- unique(prm.modify[prm.modify$run_no %in% runs.modify,]$run_name)


  folder.paths <- paste0(run.dir,"/",these.runs,"/outputFolder")
  print(paste("Analyze these runs"))
  print(these.runs)
  folder.num <- 1:length(folder.paths)

  for(eachnum in folder.num){

    this.run <- these.runs[eachnum]
    this.path <- folder.paths[eachnum]

    print(this.path)

  nc_file <- ncdf4::nc_open(here::here("data-raw",this.nc))
  names(fg.table)[1] <- "species"


  # List of species with BH recrutment
  # For simplicity, it plots BH relationship for all fish, BUT fish can use other reproduction types
  # Fish using other method have alpha = 99 --> selection done before plotting
  #Species list
  species_list <- fg.table$name[fg.table$GroupType=="FISH"]
  # Max age list
  age_list <- fg.table$NumCohorts[fg.table$GroupType=="FISH"]
  # Code list
  code_list <- fg.table$species[fg.table$GroupType=="FISH"]


  # Maturity age

  bio.lines <- readLines(here::here("data-raw",bio.prm))
  pattern = paste0('_age_mat')
  bio.lines.id = grep(pattern,bio.lines)
  mat_data = bio.lines[bio.lines.id]

  list.mat.sp  <- substr((word(mat_data)), 1, 3)
  list.mat.age <- as.numeric(word(mat_data, -1))
  list.mat.age <- list.mat.age[list.mat.sp%in%code_list]
  list.mat.sp <- list.mat.sp[list.mat.sp%in%code_list]

  # Initialize a list to store SSB and abundance in # at age 1
  total_biomass_list <- list()
  y1_abundance_list <- list()


  # Loop through each species that retrieves SSB and abundance in # at age 1
  for (species in species_list) {
    # Initialize abundance, st and res variable

    species_abundance <- 0
    age_classes <- paste0(rep(species,    as.numeric(age_list[species_list == species])),
                          1:as.numeric(age_list[species_list == species]),
                          rep("_Nums",as.numeric(age_list[species_list == species])))

    st_classes <- paste0(rep(species,    as.numeric(age_list[species_list == species])),
                         1:as.numeric(age_list[species_list == species]),
                         rep("_StructN",as.numeric(age_list[species_list == species])))

    res_classes <- paste0(rep(species,    as.numeric(age_list[species_list == species])),
                          1:as.numeric(age_list[species_list == species]),
                          rep("_ResN",as.numeric(age_list[species_list == species])))

    code.sp <- code_list[match(species, species_list)]
    age.mat <- list.mat.age[match(code.sp, list.mat.sp)]

    print(age.mat)
    # Store the age 1
    y1_abundance_list[[species]] <- sum(ncvar_get(nc_file, age_classes[1]),  na.rm = T)

    print("select only mature age class, accounting for the case age.mat = 1")
    # select only mature age class, accounting for the case age.mat = 1
    if (age.mat>1){ age_classes <- age_classes[-(1:(age.mat-1))]}
    # Loop through each age class
    for (i in 1:length(age_classes)) {
      # To refine
      print(i)
      # Check if the variable exists in the file
      if (age_classes[i] %in% names(nc_file$var)) {

        res.i <- ncdf4::ncatt_get(nc_file, res_classes[i])$`_FillValue`
        st.i <- ncdf4::ncatt_get(nc_file, st_classes[i])$`_FillValue`

        abundance_per_cell <- ncdf4::ncvar_get(nc_file, age_classes[i])
        total_abundance <- sum(abundance_per_cell,  na.rm = T)
        species_abundance <- species_abundance + total_abundance*(res.i + st.i)

      } else {
        warning(paste("Variable", age_class, "not found in the NetCDF file."))
      }
    }

    total_biomass_list[[species]] <- species_abundance

  }
  nc_close(nc_file)

  print("Second stage plot BH curves")


  # Définir une gamme de valeurs pour le stock
  stock_range <- seq(0, 1e7, length.out = 100)  # Ajuster l'intervalle et le nombre de points selon les besoins

  # Les données alpha et beta en provenance du prm file

  pattern = paste0('BHalpha_')
  bio.lines.id = grep(pattern,bio.lines)
  alpha_data = bio.lines[bio.lines.id]


  pattern = paste0('BHbeta_')
  bio.lines.id = grep(pattern,bio.lines)
  beta_data = bio.lines[bio.lines.id]

  pattern = paste0('_age_mat')
  bio.lines.id = grep(pattern,bio.lines)
  mat_data = bio.lines[bio.lines.id]


  # Extract beta and alpha
  alpha_df <- extract_values(alpha_data)
  beta_df <- extract_values(beta_data)
  names(alpha_df)[names(alpha_df) == "values"] <- "values_alpha"
  names(beta_df)[names(beta_df) == "values"] <- "values_beta"

  # Merge
  df <- merge(alpha_df, beta_df, by = "species")
  df <- df[df$values_alpha!=99,]
  df <- merge(df, fg.table[,c(1,4)], by = "species")

  # Data frame to store the curves
  curve_data <- data.frame()

  for (i in 1:nrow(df)) {
    print(i)
    species <- df$species[i]
    print(species)
    alpha <- df$values_alpha[i]
    beta <- df$values_beta[i]
    dim(total_biomass_list)
    print(total_biomass_list)
    total <- total_biomass_list[names(total_biomass_list)==df$name[i]][[1]]

    stock_range <- seq(0, 3*total, length.out = 100)
    recruitment <- beverton_holt(stock_range, alpha, beta)

    curve_data <- rbind(curve_data, data.frame(species = species, stock = stock_range, recruitment = recruitment))


  }

  curve_data_global <- curve_data %>%
    dplyr::left_join(fg.table, by = "species") %>%
    dplyr::select(longname, species, stock, recruitment)

  legend.rows <- ceiling(length(unique(curve_data_sp$longname))/3)

  # Global plot
  global.plot <- ggplot2::ggplot(curve_data_global, aes(x = stock, y = recruitment, color = longname)) +
    geom_line() +
    scale_x_log10() +
    scale_y_log10() +
    labs(title = "Stock-Recruitment curve",
         x = "Stock (mg N)",
         y = "Recruitment (#)") +
    theme_minimal() +
    ggplot2::scale_y_continuous(labels = scales::scientific) +
    ggplot2::scale_x_continuous(labels = scales::scientific) +
  theme(legend.position='bottom') +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = legend.rows, ncol=3))

  globalplotname <- "bh_curve_global.pdf"

  ggplot2::ggsave(globalplotname, plot = global.plot, device ="pdf", path = this.path, width = 24, height = 26, units = "cm")


  curve_data <- c()

  for (i in 1:nrow(df)) {
    print(i)
    species <- df$species[i]
    alpha <- df$values_alpha[i]
    beta <- df$values_beta[i]
    total <- total_biomass_list[names(total_biomass_list)==df$name[i]][[1]]
    first.y <- y1_abundance_list[names(y1_abundance_list)==df$name[i]][[1]]
    stock_range <- seq(0, 1.1*max(unlist(total_biomass_list)), length.out = 100)

    # Generate the data
    recruitment <- beverton_holt(stock_range, alpha, beta)

    name.fg <- df$name[i]
    # Save it
    curve_data <- rbind(curve_data, data.frame(species = species, stock = stock_range, recruitment = recruitment, name = df$name[i]))

  }

  curve_data_sp <- curve_data %>%
    dplyr::left_join(fg.table, by = "species") %>%
    dplyr::select(longname, species, stock, recruitment)

  # Calculate the number of pages with 9 panels per page
  n_pages <- ceiling(
    length(unique(curve_data$species))/ 12
  )

  print(n_pages)

  for (i in seq_len(n_pages)) {

    print(i)

    # species plot
    sp.plot <- ggplot2::ggplot(curve_data_sp, ggplot2::aes(x = stock, y = recruitment)) +
      ggplot2::geom_line(color = "#2c7bb6") +
      ggforce::facet_wrap_paginate(~ longname, ncol = 3, nrow = 4, page = i, scales= "free") +
      ggplot2::labs(title = "Beverton Holt curves",
                    x = "Stock",
                    y = "Recruitment") +
      ggplot2::theme_minimal() +
      ggplot2::scale_y_continuous(labels = scales::scientific) +
      ggplot2::scale_x_continuous(labels = scales::scientific)


    globalplotname <- paste0("bh_curve_sp_",i,".pdf")

   # ggplot2::ggsave(globalplotname, plot = sp.plot, device ="pdf", path = this.path, width = 21, height = 29, units = "cm")


  }



  # The curve + abundance age 1 + SSB
  curve_data_sp_ab <- c()

  for (i in 1:nrow(df)) {
    species <- df$species[i]
    alpha <- df$values_alpha[i]
    beta <- df$values_beta[i]
    total <- total_biomass_list[names(total_biomass_list)==df$name[i]][[1]]
    first.y <- y1_abundance_list[names(y1_abundance_list)==df$name[i]][[1]]
    stock_range <- seq(0, 3*total, length.out = 100)
    # Create the curve
    recruitment <- beverton_holt(stock_range, alpha, beta)
    yintercept <- beverton_holt(total, alpha, beta)
    print(species)
    print(alpha/first.y)
    # Save it
    curve_data_sp_ab <- rbind(curve_data_sp_ab, data.frame(species = species, stock = stock_range, recruitment = recruitment, alpha = alpha, beta = beta, total = total, yintercept= yintercept, firsty= first.y))

  }

  rm(alpha, beta, total, first.y, stock_range, recruitment, yintercept)

  curve_data_sp_ab <- curve_data_sp_ab %>%
    dplyr::left_join(fg.table, by = "species") %>%
    dplyr::select(longname, species, stock, recruitment, alpha, beta, total, yintercept, firsty) %>%
    dplyr::mutate(longname = as.factor(longname), SSB = "SSB", Y1 = "Y1")


  # Calculate the number of pages with 9 panels per page
  n_pages <- ceiling(
    length(unique(curve_data$species))/ 12
  )

  print(n_pages)

  for (i in seq_len(n_pages)) {

    print(i)

    # Plot it
    sp.ab.plot <- ggplot2::ggplot(curve_data_sp_ab, ggplot2::aes(x = stock, y = recruitment)) +
      ggplot2::geom_line(color = "#2c7bb6") +
      ggforce::facet_wrap_paginate(~ longname, ncol = 3, nrow = 4, page = i, scales= "free") +
      ggplot2::labs(x = "Stock",
                    y = "Recruitment") +
      ggplot2::theme_minimal() +
      ggplot2::geom_vline(data=curve_data_sp_ab, ggplot2::aes(xintercept=total), linetype="dashed",
                          color = "#d7191c") +
      ggplot2::geom_hline(data=curve_data_sp_ab, ggplot2::aes(yintercept=yintercept), linetype="dashed",
                          color = "#d7191c") +
      ggplot2::geom_hline(data=curve_data_sp_ab, ggplot2::aes(yintercept=firsty), linetype="dashed",
                          color = "#1a9641") +
      ggplot2::scale_y_continuous(labels = scales::scientific) +
      ggplot2::scale_x_continuous(labels = scales::scientific) +
      ggplot2::labs(title = "Red line = SSB",
                    subtitle = "Green line = Y1"
      )+
      ggplot2::theme(plot.title = ggplot2::element_text(size = 16, color = "#d7191c"),
                     plot.subtitle = ggplot2::element_text(size = 16, color = "#1a9641")
      )

    spabplotname <- paste0("bh_curve_sp_ab_",i,".pdf")

    ggplot2::ggsave(spabplotname, plot = sp.ab.plot, device ="pdf", path = this.path, width = 21, height = 29, units = "cm")


  }

  print("Combining pdf diet plots")
  pdf.list <- list.files(path=paste0(run.dir,"/",this.run), pattern="bh_curve.*\\.pdf$", full.names = TRUE, recursive = TRUE)
  qpdf::pdf_combine(pdf.list, output = paste0(run.dir,"/",this.run,"/","outputFolder/",this.run,"_BH_plots", ".pdf"))
  file.remove(pdf.list)

  # Ratio alpha/beta
  # for (i in 1:length(df$values_beta)){
  #   print(df$Name[i])
  #   print(df$values_alpha[i]/df$values_beta[i])
  # }

} # end folder
}
