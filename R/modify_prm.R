#' Modify biology prm
#'
#' @param eachrun run number from Google sheet
#' @param sheet.id Google sheet id
#' @param sheet.name Google sheet name
#' @param base.dir Base directory to copy
#' @param run.dir Run storage
#' @param prm.modify Google sheet with parameter changes
#' @param prm.name Name of prm file
#'
#' @return
#' @export
#'
#' @examples
modify_prm <- function(eachrun, sheet.id, sheet.name, base.dir, run.dir, prm.modify, prm.name){

#run that will be created
row.modify <- prm.modify %>%
  dplyr::filter(run_no %in% eachrun)

print(row.modify)
#directory name
dir.modify <- row.modify %>%
  dplyr::distinct(run_name) %>%
  dplyr::pull(run_name) %>%
  paste0(run.dir,"/",.)

print(paste("Creating run",dir.modify))

if(!dir.exists(dir.modify)){

  #make new directory
  system(paste0("mkdir ", here::here(dir.modify)), wait = TRUE)

  #copy basedir but not subdirectories
  # cp dir1/* dir2
  system(paste0("cp ", here::here(base.dir),"/* ", here::here(dir.modify)), wait = TRUE)

}

#read biology prm from new folder
biology.prm <- readLines(paste0(dir.modify,"/",prm.name))

#first change parameters that are not vectors

if(row.modify$is_vector==0){

  print(row.modify)

  #get full line for parameter
  this.nonvec.line <- biology.prm[grep(paste0("^",row.modify$parameter_name),biology.prm)]
  if(grepl("\\t",this.nonvec.line)) this.nonvec.line <- gsub("\\t","   ", this.nonvec.line)
  print(this.nonvec.line)
  #find line number of the parameter
  line.num <- grep(paste0("^",row.modify$parameter_name),biology.prm)

  this.num.line <- this.nonvec.line %>%
    stringr::str_split(.,"   ") %>%
    unlist %>%
    as.numeric

  #add existing and new prm parameter values
  row.modify$original_prm <- this.num.line[!is.na(this.num.line)]

  row.modify$new_prm <- row.modify$original_prm * row.modify$scaler

  print(row.modify)
  #write to googlesheet
  googlesheets4::range_write(sheet.id, data = row.modify, range = paste0("A",eachrun+1), col_names = FALSE)

  biology.prm[line.num] <- paste(row.modify$parameter_name,row.modify$new_prm, sep = "   ")
}


# change parameters that are vectors
if(row.modify$is_vector==1){

  #get full line for parameter
  this.salm.line <- biology.prm[grep(paste0("^",row.modify$parameter_name," "),biology.prm)]
  if(grepl("\\t",this.salm.line)) this.salm.line <- gsub("\\t","   ", this.salm.line)
  print(this.salm.line)
  #find line number of the parameter
  line.num <- grep(paste0("^",row.modify$parameter_name),biology.prm)

  #read line where vector is located
  this.vec.line <- biology.prm[line.num+1]
  if(grepl("\\t",this.vec.line)) this.vec.line <- gsub("\\t","", this.vec.line)
  vec.line <- line.num+1


  if(this.vec.line==" " | this.vec.line==""| this.vec.line=="   " | this.vec.line=="         "){

    this.vec.line <- biology.prm[line.num+2]
    vec.line <- line.num+2

    if(this.vec.line==" " | this.vec.line==""){

      this.vec.line <- biology.prm[line.num+3]
      vec.line <- line.num+3

    }

  }

#multiply parameter by scaler

this.num.line <- this.vec.line %>%
  stringi::stri_trim() %>%
  stringr::str_split(.,"   ") %>%
    unlist %>%
    as.numeric

  new.param <- this.num.line*row.modify$scaler

   biology.prm[vec.line] <- paste0(new.param, collapse = "   ")

  #convert parameters to text for record keeping
  #write to googlesheet
  row.modify$original_prm <- paste(as.character(this.num.line), collapse=" ")
  row.modify$new_prm <- paste(as.character(new.param), collapse=" ")
  print(row.modify)

  googlesheets4::range_write(sheet.id, data = row.modify, range = paste0("A",eachrun+1), col_names = FALSE)



} # end vector

#write modified biology prm file
writeLines(biology.prm, con = paste0(dir.modify,"/",prm.name))

}
