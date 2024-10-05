#' Modify biology prm
#'
#' @param eachrun run number from Google sheet
#' @param sheet.id Google sheet id
#' @param sheet.name Google sheet name
#' @param run.dir Run storage
#' @param prm.modify Google sheet with parameter changes
#' @param prm.name Name of prm file
#' @param atlantis.run Name of run file
#'
#' @return
#' @export
#'
#' @examples
modify_prm <- function(eachrun, sheet.id, sheet.name, run.dir, prm.modify, prm.name, atlantis.run){

  print(eachrun)
#run that will be created
row.modify <- prm.modify %>%
  dplyr::filter(run_no %in% eachrun)

print(row.modify)
#directory name
dir.modify <- row.modify %>%
  dplyr::distinct(run_name) %>%
  dplyr::pull(run_name) %>%
  paste0(run.dir,"/",.)

base.dir <- row.modify %>%
  dplyr::distinct(based_on_run)

print(paste("Creating run",dir.modify))

if(!dir.exists(dir.modify)){

  #make new directory
  system(paste0("mkdir ", here::here(dir.modify)), wait = TRUE)

  #copy basedir but not subdirectories
  # cp dir1/* dir2
  system(paste0("cp ", here::here(base.dir),"/* ", here::here(dir.modify)), wait = TRUE)

}

#read run file from new folder
run.prm <- readLines(paste0(dir.modify,"/",atlantis.run))
#get full line for parameter
this.nonvec.line <- run.prm[grep(paste0("^tstop"),run.prm)]
print(this.nonvec.line)
extra.text <- "   # Stop time after the given period  18255   30yr:  10960   25 yr: 9125   20yr: 7300  15 yr: 5500  10yr:3700  5yr:  1850  3yr: 1100  101yr-- 37000 Test run: 18255"
#find line number of the parameter
line.num <- grep(paste0("^tstop"),run.prm)
new.run.val <- paste0("tstop      	",row.modify$run_length_days[1], " day ",extra.text)
newrun.prm <- gsub(this.nonvec.line, new.run.val, run.prm)
writeLines(newrun.prm, con = paste0(dir.modify,"/",atlantis.run))


#read biology prm from new folder
biology.prm <- readLines(paste0(dir.modify,"/",prm.name))

#first change parameters that are not vectors

row.num <- 1:nrow(row.modify)

for(thisrow in row.num) {

  print(thisrow)
  eachrow <- row.modify[thisrow,]

if(eachrow$is_vector==0){

  print(eachrow)

  #get full line for parameter
  this.nonvec.line <- biology.prm[grep(paste0("^",eachrow$parameter_name),biology.prm)]
  if(grepl("\\t",this.nonvec.line)) this.nonvec.line <- gsub("\\t","   ", this.nonvec.line)
  print(this.nonvec.line)
  #find line number of the parameter
  line.num <- grep(paste0("^",eachrow$parameter_name),biology.prm)

  this.num.line <- this.nonvec.line %>%
    stringr::str_split(.,"   ") %>%
    unlist %>%
    as.numeric

  #add existing and new prm parameter values
  eachrow$original_prm <- this.num.line[!is.na(this.num.line)]

  eachrow$new_prm <- eachrow$original_prm * eachrow$scaler

  print(eachrow)
  #write to googlesheet
  googlesheets4::range_write(sheet.id, data = eachrow, range = paste0("A",(eachrow$index+1)), col_names = FALSE, sheet = sheet.name)

  biology.prm[line.num] <- paste(eachrow$parameter_name,eachrow$new_prm, sep = "   ")
}


# change parameters that are vectors
if(eachrow$is_vector==1){

  #get full line for parameter
  this.salm.line <- biology.prm[grep(paste0("^",eachrow$parameter_name," "),biology.prm)]
  if(grepl("\\t",this.salm.line)) this.salm.line <- gsub("\\t","   ", this.salm.line)
  print(this.salm.line)
  #find line number of the parameter
  line.num <- grep(paste0("^",eachrow$parameter_name),biology.prm)

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

  new.param <- this.num.line*eachrow$scaler

   biology.prm[vec.line] <- paste0(new.param, collapse = "   ")

  #convert parameters to text for record keeping
  #write to googlesheet
  eachrow$original_prm <- paste(as.character(this.num.line), collapse=" ")
  eachrow$new_prm <- paste(as.character(new.param), collapse=" ")
  print(eachrow)

  googlesheets4::range_write(sheet.id, data = eachrow, range = paste0("A",(eachrow$index+1)), col_names = FALSE, sheet = sheet.name)



} # end vector

#write modified biology prm file
writeLines(biology.prm, con = paste0(dir.modify,"/",prm.name))

}
}
