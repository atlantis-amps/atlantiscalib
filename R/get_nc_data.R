#' Get nc data
#'
#' @param eachgroup each vertebrate group
#' @param thisncfile nc file
#' @param fungrouplist functional group file
#' @param runtime run length
#' @param maxtimestep max time step
#'
#' @return thissp.data
#' @export
#'
#' @examples
get_nc_data <- function(eachgroup,thisncfile, fungrouplist, runtime, maxtimestep){

  print(paste("Analyzing this group",eachgroup))

  this.sprow <- fungrouplist %>%
    dplyr::filter(name==eachgroup)

  print(this.sprow)

  group.ages <- paste(eachgroup,1:this.sprow$NumCohorts,sep="")

  print(group.ages)

  #make names for nc variables

  varlist <- c("_ResN","_Nums","_StructN","_Wage")

  var.listdata <- list()

  for(eachvar in 1:length(varlist)){

    eachvarlist <- varlist[eachvar]

    print(eachvarlist)

    name.var <- paste(group.ages,eachvarlist,sep="")

    variable.type <- gsub("_","",eachvarlist)

    if(eachvarlist == "_ResN" | eachvarlist == "_StructN" | eachvarlist == "_Wage") {

      for(eachage in 1:length(name.var)) {

        if(eachvarlist == "_Wage"){

          eachvarlist = "_ResN"
          name.var <- paste(group.ages,eachvarlist,sep="")
          variable.type <- gsub("_","",eachvarlist)
          eachvarlist = "_Wage"
        }

        eachvariable <- name.var[eachage]
        print(eachvariable)

        thisData <- RNetCDF::var.get.nc(thisncfile, eachvariable)

        if(eachvarlist == "_Wage") {

          thisData<-thisData*20*5.7*(3.65/2.65)/1000000

          variable.type = "Wage"
        }

        print(dim(thisData))
        thisData[thisData==0]<-NA  # Replace 0's with NA
        thisData <- thisData[1:7,2:89,1:(maxtimestep/outputfrequency)]
        thisDataMeanMg <-apply(thisData,3,mean,na.rm = TRUE) #Get mean size over time, averaging over depth and location

        thisY <-tibble::tibble(variable=thisDataMeanMg/thisDataMeanMg[1]) %>%  # Normalize by initial value
          dplyr::mutate(time = 1:nrow(.), age = eachage, group = eachvariable, variable_type= variable.type, code = this.sprow$Code)

        if(eachvarlist == "_Wage") {

          thisY <-tibble::tibble(variable=thisDataMeanMg) %>%  # Normalize by initial value
            dplyr::mutate(time = 1:nrow(.), age = eachage, group = eachvariable, variable_type= variable.type, code = this.sprow$Code)

        }

        listname <- paste(thisY$group[1],"_",thisY$variable_type[1],sep="")
        var.listdata[[listname]] <- thisY

      }

    } else if (eachvarlist == "_Nums") {

      for(eachage in 1:length(name.var)) {

        eachvariable <- name.var[eachage]
        print(eachvariable)

        thisData <- RNetCDF::var.get.nc(thisncfile, eachvariable)
        thisData[thisData==0]<-NA  # Replace 0's with NA
        print(dim(thisData))
        thisData <- thisData[1:7,2:89,1:(maxtimestep/outputfrequency)]
        thisDataMeanMg <-apply(thisData,3,mean,na.rm = TRUE) #Get mea]
        #thisData <- thisData[1:7,2:89,1:51] #use this for 10 year runs
        thisDataNums<-apply(thisData,3,sum,na.rm = TRUE)#Get nums over time, summing over depth and location
        thisY <- tibble::tibble(variable=thisDataNums) %>%  # Normalize by initial value
          dplyr::mutate(time = 1:nrow(.), age = eachage, group = eachvariable, variable_type= variable.type, code = this.sprow$Code)

        var.listdata[[eachvariable]] <- thisY

      }

    }
  }



  thissp.data <- var.listdata %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(code = this.sprow$Code, longname = this.sprow$longname) %>%
    dplyr::rename(atlantis_group = group) %>%
    dplyr::mutate(Year=(time)/365)


  print(paste("Done with group",eachgroup))


  return(thissp.data)
}
