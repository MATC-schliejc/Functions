#------------------------------------------------------------------#
#  Purpose: UW-Platteville Functions       
#  Requestee: John Schliesmann                        
#  
#  Created by: John Schliesmann
#  Created Date: 6/21/2017
# 
#------------------------------------------------------------------#

library(snakecase)

#

#------------------- Scripts Using these functions ----------------#

##~~~~~~~~~~~~~~~~~}  INDEX OF FUNCTIONS  {~~~~~~~~~~~~~~~~~~~~~~~## 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#  formatSql
#  retainNext  (using PASS data)
#  percent
#  properCase
#  trimWhiteSpace

#----------------------- Helpful Code tidbits ---------------------#

#mutate(n=n(),seq=cumsum(n)/n())
#rm(list=ls())   # Removes all objects
#options(digits=2,scipen=999)
#options(java.parameters = "-Xmx4g" )



##~~~~~~~~~~~~~~~~~~~~~}  formatXl {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~## 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#DESCRIPTION:
#         Formats input fil results to match period as space column naming conventions
#REQUIRMENTS:
#         1. Input is results from oracle database sql
#USAGE:  df <- formatXl(inport file function))

formatXl <- function(df,type='snake') {
  cat('Depricated:  Use colFormat() instead\n')
 if(type == 'old'){
  names(df)  <- sapply(names(df),properCase)
  names(df) <- gsub(" ", ".", names(df))
  return(df)
}
else if(type=='snake'){
  names(df)  <- tolower(names(df))
  names(df) <- gsub(" ", "_", names(df))
  names(df) <- gsub("\\.", "_", names(df))
  return(df)
}
else return(df)
}


colFormat <- function(df,type='snake') {

  if(type == 'old'){
    names(df)  <- sapply(names(df),properCase)
    names(df) <- gsub(" ", ".", names(df))
    return(df)
  }
  else if(type=='snake'){
    names(df)  <- tolower(names(df))
    names(df) <- gsub(" ", "_", names(df))
    names(df) <- gsub("\\.", "_", names(df))
    return(df)
  }
  else return(df)
}
##~~~~~~~~~~~~~~~~~~~~~}  returnNext {~~~~~~~~~~~~~~~~~~~~~~~~~~~~## 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#REQUIRMENTS:
#         1. must pass in a dataframe
#         2. column named: Person.Id,term_code
#         3. term_code must include Fall and Spring for term duration to work
#
returnedNext <- function(df,duration='year'){
  # duration accepts Year or Term
  # Requeires a dataframe with Person.Id and term_code
  
  
  if(!(tolower(duration) %in% c('year','term')))
  {
    warning("Duration needs to be year or term.  Defaulting to year.")
    #duration <- 'year'
  }
  if(!('person_id' %in% colnames(df)) | !('term_code' %in% colnames(df))){
    stop("person_id or term_code column not found.")
    return(0)
  } else {}
  
  df$term_code <- as.integer(df$term_code)
  
  #modulus of term code post 1999: 20 = Fall, 0=Spring, 10=Summer, 25=wintrim
  #dataframe r will list the next term/year
  if(tolower(duration) == 'term'){
    r <- df %>% select(person_id,term_code) %>% 
      mutate(next_time = ifelse(term_code %% 30 == 0, term_code+20,
                                ifelse(term_code %% 30 == 20,term_code+10,0)),
             returned = 'Yes') %>%
      unique()
  } else {          
    r <- df %>% select(person_id,term_code) %>% mutate(next_time = term_code + 30,returned = 'Yes') %>% unique()
  }   
  #this merge identifies who returned
  r2 <- suppressWarnings(merge(r,r,by.x=c('person_id','next_time'),by.y=c('person_id','term_code'), all.x=TRUE))
  r3 <- r2[,c(1,3,6)] 
  r3[is.na(r3$returned.y),'returned.y'] <- 'No'
  names(r3)[names(r3) == 'returned.y'] <- ifelse(tolower(duration) == 'term','returned_next_term','returned_next_year')
  
  Return <- merge(df,r3, by=c('person_id','term_code'), all.x=TRUE)
  Return$term_code <- str_pad(Return$term_code,4,pad='0')
  rm(list=ls(pattern="^r"))
  
  return(Return)  
} 

##~~~~~~~~~~~~~~~~~~~~~}  percent {~~~~~~~~~~~~~~~~~~~~~~~~~~~~~## 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#DESCRIPTION:
#         adds a percent to the end of a value
#REQUIRMENTS:
#         1. Input is results from oracle database sql
#USAGE:   df %>% summarise(x = (percent(sum(Year.1)/sum(Year.0))))

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

##~~~~~~~~~~~~~~~~~~~~~}  properCase {~~~~~~~~~~~~~~~~~~~~~~~~~~~~## 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#REQUIRMENTS:
#         1. Pass in a character vector
#USAGE:  df$column <- sapply(df$column,properCase)

properCase <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        sep="", collapse=" ")
}

##~~~~~~~~~~~~~~~~~~~~~}  trimWhiteSpace {~~~~~~~~~~~~~~~~~~~~~~~~## 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#REQUIRMENTS:
#         1. Pass in a character vector
#USAGE:  df$column <- trimWhiteSpace(df$column)

trimWhiteSpace <- function (x) {gsub("^\\s+|\\s+$", "", x)}

##~~~~~~~~~~~~~~~~~~~~~}  replaceAllNa {~~~~~~~~~~~~~~~~~~~~~~~~## 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#REQUIRMENTS:
#         1. Pass in a dataframe
#USAGE:   %>% replaceAllNa() 
#TODO:  add default date... say 01/01/1900
 
replaceAllNa <- function(x){
   df <- x %>%
     mutate_if(is.integer, funs(replace(., is.na(.), 0))) %>%
     mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
     mutate_if(is.character, funs(replace(., is.na(.), '')))
   return(df)
}


##~~~~~~~~~~~~~~~~~~~~~}  Calculates Age using birthdate {~~~~~~~~~~~~~~~~~~~~~~~~## 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#http://blog.jsonbecker.com/2013/06/calculating-age-in-r.html
age_calc <- function(dob, enddate=Sys.Date(), units='months'){
  if (!inherits(dob, "Date") | !inherits(enddate, "Date"))
    stop("Both dob and enddate must be Date class objects")
  start <- as.POSIXlt(dob)
  end <- as.POSIXlt(enddate)

  years <- end$year - start$year
  if(units=='years'){
    result <- ifelse((end$mon < start$mon) | 
                      ((end$mon == start$mon) & (end$mday < start$mday)),
                      years - 1, years)    
  }else if(units=='months'){
    months <- (years-1) * 12
    result <- months + start$mon
  }else if(units=='days'){
    result <- difftime(end, start, units='days')
  }else{
    stop("Unrecognized units. Please choose years, months, or days.")
  }
  return(result)
}

#~~~~~~~~~~~~ Second Age option ~~~~~~~~~~~~~~~~~~~~####

age <- function(date_from, date_to = today(), units = "years", floor = TRUE) {
     library(lubridate)
     if (!inherits(date_from, "Date") | !inherits(date_to, "Date"))
        stop("Both dates must be Date class objects")

  dur = interval(date_from, date_to) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(dur)))
  return(dur)
}

#~~~~~~~~~~~~ Loading Multiple Files ~~~~~~~~~~~~~~~~~~~~####
#read_plus <- function(flnm) {
#    read_csv(flnm) %>% 
#        mutate(filename = flnm)
#}
#
#tbl_with_sources <-
#    list.files(pattern = "*.csv", 
#               full.names = T) %>% 
#    map_df(~read_plus(.))

#~~~~~~~~~~~~ Remove Duplicates ~~~~~~~~~~~~~~~~~~~~####
#  It requires a grouping function prior to calling
removeDuplicates <- function(x){
   df <- x %>%
     mutate(n=n(),seq=cumsum(n)/n()) %>%
     filter(seq == 1) %>%
     select(-n,-seq)
   return(df)
}

#~~~~~~~~~~~~ get Count and Sequence ~~~~~~~~~~~~~~~####
#  It requires a grouping function prior to calling
getSequence <- function(x){
   df <- x %>%
     mutate(n=n(),seq=cumsum(n)/n())
   return(df)
}


#~~~~~~~~~~~~ Extract Info from WTCS Reports ~~~~~~~~~~~~~~~####
extractPgm307 <- function(file_name){
  # Function: converts the PGM307.xls WTCS report to a data friendly format
  # @PARMA file_name: Full path to the PGM307.xls 

  if(file.exists(file_name)){
    df <- read_excel(file_name,skip = 10) %>%
      select(program = 1,
             program_title = 2,
             cip_code = 8) %>%
      na.omit() %>%
      separate(program_title, c('program_title','remove','program_cluster','program_pathway'),sep = '[\n\r]') %>%
      mutate(program_number = str_replace_all(program, "-", ""),
             program_cluster = str_remove(program_cluster,'CLUSTER: '),
             program_pathway = str_remove(program_pathway,'PATHWAY: ')) %>%
      select(program,
             program_number,
             cip_code,
             program_title,
             program_cluster,
             program_pathway)
    return(df)
  }else {stop('File not Found: Check path and file name.\n')}
  
}