bh_plot <- function(nc_path, nc_file, fg.table, bio.prm){

nc_file <- ncdf4::nc_open(here:here("data-raw",this.nc))
names(fg.table)[1] <- "species"


# List of species with BH recrutment
# For simplicity, it plots BH relationship for all fish, BUT fish can use other reproduction types
# Fish using other method have alpha = 99 --> selection done before plotting
#Species list
species_list <- fg.table$Name[fg.table$GroupType=="FISH"]
# Max age list
age_list <- fg.table$NumCohorts[fg.table$GroupType=="FISH"]
# Code list
code_list <- fg.table$species[fg.table$GroupType=="FISH"]


# Maturity age

bio.lines <- readLines(bio.prm)
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

  # Store the age 1
  y1_abundance_list[[species]] <- sum(ncvar_get(nc_file, age_classes[1]),  na.rm = T)


  # select only mature age class, accounting for the case age.mat = 1
  if (age.mat>1){ age_classes <- age_classes[-(1:(age.mat-1))]}
  # Loop through each age class
  for (i in 1:length(age_classes)) {
    # To refine

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




# Définir une gamme de valeurs pour le stock
stock_range <- seq(0, 1e7, length.out = 100)  # Ajuster l'intervalle et le nombre de points selon les besoins

# Les données alpha et beta en provenance du prm file

bio.lines <- readLines(bio.prm)
bio.lines = bio.lines
pattern = paste0('BHalpha_')
bio.lines.id = grep(pattern,bio.lines)
alpha_data = bio.lines[bio.lines.id]


pattern = paste0('BHbeta_')
bio.lines.id = grep(pattern,bio.lines)
beta_data = bio.lines[bio.lines.id]

pattern = paste0('_age_mat')
bio.lines.id = grep(pattern,bio.lines)
mat_data = bio.lines[bio.lines.id]



# extract species names and param value
extract_values <- function(data) {
  species <- sapply(data, function(x) strsplit(x, "   ")[[1]][1])
  values <- sapply(data, function(x) as.numeric(strsplit(x, "   ")[[1]][2]))
  species <- sapply(species, function(x) strsplit(x, "_")[[1]][2])

  return(data.frame(species = species, values = values))
}

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
  species <- df$species[i]
  alpha <- df$values_alpha[i]
  beta <- df$values_beta[i]
  total <- total_biomass_list[names(total_biomass_list)==df$Name[i]][[1]]

  stock_range <- seq(0, 3*total, length.out = 100)
  recruitment <- beverton_holt(stock_range, alpha, beta)

  curve_data <- rbind(curve_data, data.frame(species = species, stock = stock_range, recruitment = recruitment))


}


# Global plot
ggplot2::ggplot(curve_data, ggplot2::aes(x = stock, y = recruitment, color = species)) +
  ggplot2::geom_line() +
  ggplot2::scale_x_log10() +
  ggplot2::scale_y_log10() +
  ggplot2::labs(title = "Stock-Recruitement curve",
       x = "Stock (mg N)",
       y = "Recruitment (#)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "right")

# Plot per species in a pdf
# The curve only
pdf("BH.pdf", height = 9, width = 9)
empty_plot <- grid.rect(gp = gpar(col = NA), draw = FALSE)
plots <- list()
curve_data <- c()
for (i in 1:nrow(df)) {
  species <- df$species[i]
  alpha <- df$values_alpha[i]
  beta <- df$values_beta[i]
  total <- total_biomass_list[names(total_biomass_list)==df$Name[i]][[1]]
  first.y <- y1_abundance_list[names(y1_abundance_list)==df$Name[i]][[1]]
  stock_range <- seq(0, 1.1*max(unlist(total_biomass_list)), length.out = 100)

  # Generate the data
  recruitment <- beverton_holt(stock_range, alpha, beta)

  # Save it
  curve_data <- rbind(curve_data, data.frame(species = species, stock = stock_range, recruitment = recruitment))

  # Plot it
  p <- ggplot2::ggplot(curve_data[curve_data$species == species, ], ggplot2::aes(x = stock, y = recruitment)) +
    ggplot2::geom_line(color = "cyan4") +
    ggplot2::labs(title = paste(df$Name[i]),
         x = "Stock",
         y = "Recrutement") +
    ggplot2::theme_minimal()


  plots[[species]] <- p
}

do.call("grid.arrange", c(plots[1:9], ncol = 3))
do.call("grid.arrange", c(plots[10:18], ncol = 3))
do.call("grid.arrange", c(plots[19:27], ncol = 3))
do.call("grid.arrange", c(plots[28:36], ncol = 3))
do.call("grid.arrange", c(plots[37:38], list(empty_plot),list(empty_plot),
                          list(empty_plot),
                          list(empty_plot),
                          list(empty_plot), ncol = 3))


# The curve + abundance age 1 + SSB
plots <- list()
curve_data <- c()
for (i in 1:nrow(df)) {
  species <- df$species[i]
  alpha <- df$values_alpha[i]
  beta <- df$values_beta[i]
  total <- total_biomass_list[names(total_biomass_list)==df$Name[i]][[1]]
  first.y <- y1_abundance_list[names(y1_abundance_list)==df$Name[i]][[1]]
  stock_range <- seq(0, 3*total, length.out = 100)
  # Create the curve
  recruitment <- beverton_holt(stock_range, alpha, beta)
  print(species)
  print(alpha/first.y)
  # Save it
  curve_data <- rbind(curve_data, data.frame(species = species, stock = stock_range, recruitment = recruitment))

  # Plot it
  p <- ggplot2::ggplot(curve_data[curve_data$species == species, ], ggplot2::aes(x = stock, y = recruitment)) +
    ggplot2::geom_line(color = "cyan4") +
    ggplot2::labs(title = paste(df$Name[i]),
         x = "Stock",
         y = "Recrutement") +
    ggplot2::theme_minimal()+
    ggplot2::geom_vline(xintercept=total, linetype="dashed",
               color = "red")+
    ggplot2::geom_hline(yintercept=beverton_holt(total, alpha, beta), linetype="dashed",
               color = "red")+
    ggplot2::geom_hline(yintercept=first.y, linetype="dashed",
               color = "green3")+
    ggplot2::annotate("text", x = total, y = max(recruitment), label = "SSB", vjust = -0.1, hjust = -0.5, color = "red", angle = 90) +
    ggplot2::annotate("text", x = 0, y = first.y, label = "Y1", vjust = 1.5, hjust = -0.1, color = "green3")

  plots[[species]] <- p
}


# Add figures to the pdf
do.call("grid.arrange", c(plots[1:9], ncol = 3))
do.call("grid.arrange", c(plots[10:18], ncol = 3))
do.call("grid.arrange", c(plots[19:27], ncol = 3))
do.call("grid.arrange", c(plots[28:36], ncol = 3))
do.call("grid.arrange", c(plots[37:38], list(empty_plot),list(empty_plot),
                          list(empty_plot),
                          list(empty_plot),
                          list(empty_plot), ncol = 3))

dev.off()

# Ratio alpha/beta
# for (i in 1:length(df$values_beta)){
#   print(df$Name[i])
#   print(df$values_alpha[i]/df$values_beta[i])
# }

}
