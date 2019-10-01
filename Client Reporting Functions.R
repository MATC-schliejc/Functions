# ---------------------------------------------------------------------#
#  Purpose: Client Reporting System Functions
#  Requestee:
#
#  Created by: John Schliesmann
#  Created Date: 1/8/2019
#
#  Modifications:
#       5/23/19 - renamed loadCRS to loadWTCS. the function now builds all WICS file formats
#       5/23/19 - in Record Layout, record_type was relabeled record_id
#

#           
#
#  Notes:
#        1. Update CUSTOM MODIFICATIONS before loading new data set
#
#  ToDo:
#        1. How to handle new record layouts?  ie. homeless filed in the S1
#
# ----------------------------------------------------------------------#


#### ~~~~~~~~ Maintaining Layouts ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#
####  Notes: 
#        1. As WTCS updates the file layout of the CRS upload the 
#               {Record Layout.xlsx} & {Record Layout.RDS} will need 
#               to be updated.
#        2. The master file is located in WTCS_CRS gitlab website

#        Uncomment below code to use
    #layout  <- read_excel('S:\\RESEARCH\\55_John Schliesmann\\Record_Layouts.xlsx')
    #write_rds(layout,'S:\\RESEARCH\\09_WTCS Reports\\WTCS_CRS\\Record_Layouts.rds')        




#### ~~~~~~~~ loadCrs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
####
####  Requirments:
#        1. spreadsheet with WIX file configurations
#           S:\RESEARCH\09_WTCS Reports\WTCS_Automation\Record Layout.xlsx   (last updated 4/2019)
#        2. ulitizes RDS file of requirment #1 stored in S:\\RESEARCH\\09_WTCS Reports\\WTCS_CRS
####  Parameters:
#       file_name -- string of file name with extension.  can include path if file is not in working directory
#       rcd -- record id(s) to be processed.  It expects uppercase and will default to all record types.
####  Notes:
#        see commented function below this one
#        DO NOT assign this function to a object eg( S1 <- loadWTCS(file,rcd = 'S1')  )


loadWTCS <- function(file_name,rcd = 'client'){
  library(tidyverse)
  
  valid_record_id <- c('S1','S2','S3','S4','S5','S6','S7','S8','S9','C','X1','X2','X3') 
  
  if(rcd[1] == 'client') { rcd <- c('S1','S2','S3','S4','S5','S6','S7','S8','S9') }
  
  #--- ARGS ERROR CHECKS -------------------------##
  
  if(any(rcd %in% valid_record_id)==FALSE) {stop("Invalid Record(s) Selected.  \nPlease choose any combination of: 'S1','S2','S3','S4','S5','S6','S7','S8','S9','C','X1','X2','X3'")}
  
  if(!file.exists(file_name)){ 
    cat('The file does not exist.\n')

    default_path <- 'S:\\RESEARCH\\09_WTCS Reports\\WTCS_CRS\\'
    default_files <- list.files(path=default_path,
                                pattern="^WIX.CLIENT",full.names = FALSE,recursive = TRUE)
    
    if(!length(default_files)) { stop("The file cannot be found in the this directory") }
    
    #dirs <- dirname(default_files) #get the directory names of these (for grouping)
    #lastfile <- tapply(default_files,dirs,function(v) v[which.max(file.mtime(v))])  #find the last file in each directory (i.e. latest modified time)
    lastfile <- default_files[which.max(file.mtime(paste0(default_path,default_files)))]
    cat('\nLoading',lastfile,'\n\n')
    replace <- 'y'
#    replace <- readline(prompt=paste("Would you like to use",lastfile,"(y/n):" ))
    if(replace == 'y' | replace == 'Y') { 
      file_name <- paste0(default_path,lastfile) 
      }
    else { stop('No File found to process')}
    
  } else {cat('Processing:',file_name,'\n\n')}
  
  #-----------------------------------------------##
  
  fn <- 'S:\\RESEARCH\\09_WTCS Reports\\WTCS_CRS\\_RECORD LAYOUTS.RDS'
  layout <- readRDS(fn)
  cat('Using layouts in:',fn,'\n\n')
  
  #--- SUB FUNCTIONS -----------------------------##
    
  ### getWidths
  #      requires: Record Layout.rds must be read into an object called layout
  #      args:     record_id
  #      returns:  vector of numeric values corresponding to each field for the record_id
  
  getWidths <- function(x){ layout %>% filter(record_id == x) %>% pull(width) }
  
  ### getFields
  #      requires: Record Layout.rds must be read into an object called layout
  #      args:     record_id
  #      returns:  vector of string values corresponding to each field for the record_id
  getFields <- function(x){ layout %>% filter(record_id == x) %>% pull(data_element_name) }
  
  ### getColType
  #      requires: Record Layout.rds must be read into an object called layout
  #      args:     record_id
  #      returns:  string corresponding to each field type for the record_id
  getColType <- function(x){ paste0(rep('c',nrow(layout %>% filter(record_id == x))), collapse = "") }
  
  
  #--- MAIN FUNCTION -----------------------------##

  
  rcd_list <- list()
  suppressWarnings(
    for(i in 1:length(rcd)){
      cat("Processing",rcd[i],"Record\n")
      rcd_list[i] <- list (read_fwf(file_name,
                                    na = ' ',
                                    fwf_widths(c(getWidths(rcd[i])),
                                               c(getFields(rcd[i]))),
                                    getColType(rcd[i])) %>% 
                             filter(record_id == rcd[i]))
    }
  )
  
  
  names(rcd_list) <- tolower(rcd)
  cat('Moving',length(names(rcd_list)),'dataframe to global environment.')
  list2env(rcd_list ,.GlobalEnv)
  return(TRUE)
}

#### ~~~~~~~~ findValues ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
####
####  Requirments:


findValues <- function(df,col_name,new_col_name,...){
####  Parameters:
#       df -- the dataframe with concatenated vlaues
#       col_name -- is an existing column in df
#       new_col_name -- boolean column binded to df
#       ... -- any values to find (currently limited to 2 digit values)
  
  #ex: s3 %>% findValues('program_fee_codes','pfc_n','34','35')
  
  col_name <- ensym(col_name)
  args <- unlist(list(...))
  
  split_col <- function(string, size){
    pat <- paste0('(?<=.{',size,'})')
    strsplit(string, pat, perl=TRUE)
  }
  
  cat('Finding', col_name,': ',args,'\n')
  
  df2 <- df %>%
    mutate(pfc = split_col(!!enquo(col_name),2)) %>%
    unnest() %>%
    filter(pfc %in% args)
  
  if(nrow(df2) > 0){
    suppressMessages(
      df %>%
        left_join(df2) %>%
        mutate(!!new_col_name := ifelse(is.na(pfc),FALSE,TRUE)) %>%
        select(-pfc)
    )
   } else {
       cat('No values found for:',col_name,': ',args,'\n')
       df %>% mutate(!!new_col_name := FALSE)
       }
}

academicYear <- function(year,type = 'start') {
  if(type == 'start'){
    return(as.Date(paste0('05-29-',year-1),'%m-%d-%Y'))
  }
  else if(type == 'end'){
    return(as.Date(paste0('05-30-',year),'%m-%d-%Y'))
  }
  else { stop('Invalid Date Type')}
  
}

getDualEnroll <- function(){
  suppressMessages(
  de <- s1 %>%
    left_join(s3, by = c('student_id','fiscal_year')) %>%
    filter(recognized_credit_code %in% c('1A','1B','8A','8B','9A','9B','9C','9D','9G','9H','9K')) %>%
    select(student_id,
           fiscal_year,
           year_graduated_high_school,
           highest_grade_completed_at_enroll,
           highest_cred_received_at_enroll,
           recognized_credit_code
    ) %>%
    mutate(recognized_credit_code = paste0('rcc_',recognized_credit_code),
           is_dual_enrolled = TRUE,
           val = TRUE) %>%
    unique() %>%
    spread(recognized_credit_code,val,fill = FALSE) %>%
    unique()
  )
  return(de)
}


#### ~~~~~~~~ xwalks ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
####
####  Requirments:
#        1. 
#
#     Valid values:
#       1. "credit code" -- provides descriptions for valid recognized credit codes
#       2. "test topic"  -- provides descriptions for test topics. ie Reading, Math, etc
#       3. "test name"   -- provides descriptions for the name of the tests
#       4. "fire courses" - provides flag to id fire & hazmat reinbursement eligible courses
#

getXwalk <- function(xwalk = 'none'){
  
  if (xwalk == 'none') {
    cat('         credit code
         test topic
         test name
         fire courses\n')
        stop()
  }
  else if (xwalk == 'credit code') {  
    df <- data.frame(recognized_credit_code = c('1A','1B','8A','8B','9B','9C','9D','9G','9H','9K'),
                     recognized_credit_code_descr = c('1A - Advanced Standing Credit',
                                                      '1B - Advanced Standing Credit - Youth Apprenticeship',
                                                      '8A - Transcripted Credit',
                                                      '8B - Transcripted Credit - Youth Apprenticeship',
                                                      '9B - Youth Apprenticeship',
                                                      '9C - Start College Now / Youth Options',
                                                      '9D - Standard Enrollment',
                                                      '9G - Compulsory Edu. / 118.15 Contract',
                                                      '9H - 38.14(3) Contract',
                                                      '9K - Course Options (deprecated)'),
                     stringsAsFactors = FALSE)
  }
  else if (xwalk == 'aid code') {  
    df <- data.frame(course_aid_code = c('10','20','30','31','32','42','47','50','60','73','74','75','76','77','78'),
                     course_aid_code_descr = c('10 - Associate Degree (approved curriculum)',
                                                      '20 - Associate Degree (not approved curriculum)',
                                                      '30 - Short Term Technical Diploma',
                                                      '31 - One Year Technical Diploma',
                                                      '32 - Two year Technical Diploma',
                                                      '42 - Vocational Adult',
                                                      '47 - Occupational Adult',
                                                      '50 - Apprenticeship',
                                                      '60 - Community Service',
                                                      '73 - Beginning Adult Basic Edu.',
                                                      '74 - Beginning Adult Basic Edu.',
                                                      '75 - English Lang. Learning',
                                                      '76 - Adult & Youth HS/HSED/GED',
                                                      '77 - Developmental',
                                                      '78 - Remedial'),
                     stringsAsFactors = FALSE)
  }
  else if (xwalk == 'test topic'){
    df <-  data.frame(test_topic = c('03','04','06','11','12','13','14','15','16'),
                      test_topic_descr = c('03 - Reading',
                                           '04 - Language',
                                           '06 - Math',
                                           '11 - Speaking-Listening',
                                           '12 - Reading-Writing',
                                           '13 - Speaking',
                                           '14 - Listening',
                                           '15 - Reading',
                                           '16 - Writing'),
                      stringsAsFactors = FALSE)
  }
  else if (xwalk == 'test name'){
    df <- data.frame(test_name = c('030','031','032','033','034','035','036','037','040','041','042','043','101','102','103','104','105','106','107','108','109','110'),
                     test_name_descr = c('TABE-CLAS-E-A1',
                                         'TABE-CLAS-E-A2',
                                         'TABE-CLAS-E-B1',
                                         'TABE-CLAS-E-B2',
                                         'TABE-CLAS-E-C1',
                                         'TABE-CLAS-E-C2',
                                         'TABE-CLAS-E-D1',
                                         'TABE-CLAS-E-D2',
                                         'BEST-Literacy-B',
                                         'BEST-Literacy-C',
                                         'BEST-Literacy-D',
                                         'BEST-Plus',
                                         'TABE-L-11',
                                         'TABE-L-12',
                                         'TABE-E-11',
                                         'TABE-E-12',
                                         'TABE-M-11',
                                         'TABE-M-12',
                                         'TABE-D-11',
                                         'TABE-D-12',
                                         'TABE-A-11',
                                         'TABE-A-12'),
                     stringsAsFactors = FALSE) 
  }
  else if (xwalk == 'fire courses'){
    #these are not all elgible courses, only MATC listed courses
    df <- data.frame(course_aid_code = rep('47',8),
                     course_instructional_area = rep('503',8),
                     course_sequence = c('420','430','443','447','450','463','464','480'),
                     fire_hazmat_eligible_course = rep(TRUE,8),
                     stringsAsFactors = FALSE)
    
  }
  else if (xwalk == 'locations'){
    df <- data.frame(course_location = c('13','14','15','16','17','18','19','20','21','23','50','51','52','53','55','56','57','59','60','61','63','67','80','85','87','89','90','99'),
                     location_name = c('OCL 38.14',
                                       'CEWD / Work Place Learning Site',
                                       'CEWD / Personal Professional Site',
                                       'MATC Mitchell Field',
                                       'MATC TV Tower Site',
                                       'Short-term V/A Prof Dev',
                                       'Correctional Institutions',
                                       'Oak Creek Campus',
                                       'South Milwaukee',
                                       'Greendale',
                                       'Mequon Campus',
                                       'Brown Deer Center',
                                       'Nicolet Center',
                                       'Whitefish Bay Center',
                                       'Germantown Center',
                                       'Port Washington Center',
                                       'Shorewood Center',
                                       'North Shore Day Center',
                                       'Milwaukee',
                                       'Marshall Center',
                                       'Walkers Point Education Center',
                                       'HayMarket Square',
                                       'West Allis Campus',
                                       'Nathan Hale Center',
                                       'Wauwatosa West Center',
                                       'West Allis Central Center',
                                       'Out of District',
                                       'Reserved for use with Location of Learning Code')
    )
  }
  else { df <- 0 }
  
  return(df)
}
  
#### ~~~~~~~~ Rebuild Record Layouts ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
####
####  Requirments:
#        1. spreadsheet with record layout configurations
#           live copy: gitlab.com, project: WTCS_CRS (last updated 2/2019)
#           backup copy: S:\RESEARCH\55_John Schliesmann\Record Layout.xlsx 
#        2. As new records are added loadCrs needs to be updated with new record id.
####  Parameters:
#       file_name -- string of file name with extension.  can include path if file is not in working directory
####  

rebuild_layout <- function() {
  library(tidyverse)

  filename <- 'S:\\RESEARCH\\09_WTCS Reports\\WTCS_CRS\\WTCS Data Tables\\Record_Layouts.xlsx'
  
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  x <- lapply(x, as.data.frame)
  names(x) <- sheets
  saveRDS(dplyr::bind_rows(x),paste0('S:\\RESEARCH\\09_WTCS Reports\\WTCS_CRS\\_RECORD LAYOUTS_',format(Sys.time(), "%m-%d-%Y"),'.RDS'))
  cat('RDS layout is updated.\nRemember to add date modified to old _RECORD LAYOUT.RDS\nAnd remove date from new _RECORD LAYOUT.RDS')
}

#### ~~~~~~~~ Rebuild WIX Upload ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####


  
  # ---------------------------------------------------------------------#
  #  Purpose: rebuild wix file for state upload
  #  Requestee:
  #
  #  Created by: John Schliesmann
  #  Created Date: 1/10/2019
  #
  #  Requirments:
  #        1. s# data frames can only include columns to be written to output file
  #        2. output file must be encoded in UTF-8 format and (i think) end with  CRLF characters
  #  Notes:
  #        1. source for creating fwf file is cmadden@edisonohio.edu 
  #           https://www.airweb.org/eAIR/techtips/Pages/Importing-Exporting-Fixed-Data.aspx
  
  # ----------------------------------------------------------------------#
  

WixUpload <- function(){
  
  library(lubridate)
  
  #~~~~~~~~~~~~~~ USER EDITS ~~~~~~~~~~~~~~~~~~~~~####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
  
  #setwd('C:\\Users\\schliejc\\Downloads\\')
  setwd('S:\\RESEARCH\\09_WTCS Reports\\WTCS_CRS\\')
  
  fy <- if(month(Sys.Date()) > 8) {year(Sys.Date()) +1}
        else {year(Sys.Date())}
  
  #set file name of WIX
  output_file_name <- paste0('CLI09full',fy,'-test_',format(Sys.Date(), '%m-%d-%Y'),".txt")
  #setwd('S:\\RESEARCH\\09_WTCS Reports\\WTCS File Downloads\\')
  
  if(file.exists(output_file_name)) {
    cat(output_file_name, 'File already exists and will be deleted\n')
    file.remove(output_file_name)
  }
  #~~~~~~~~~~~~~~ REBUILD WIX FILE ~~~~~~~~~~~~~~~####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
  # ----- S1 -------------------------------------####
  
  cat('Writing S1 to file\n')
  s1[is.na(s1)] <- ''
  final <- c(sprintf(paste0(rep("%s",ncol(s1)),collapse = ''),
                     str_trunc(str_pad(s1$record_id,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s1$district_number,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s1$fiscal_year,4,'right'),4,'right',ellipsis=''),
                     str_trunc(str_pad(s1$student_id,9,'right'),9,'right',ellipsis=''),
                     str_trunc(str_pad(s1$blank_s1_1,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s1$veteran,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s1$first_gen,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s1$blank_s1_2,19,'right'),19,'right',ellipsis=''),
                     str_trunc(str_pad(s1$ability_to_benefit,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s1$homeless_individual,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s1$foster_care_youth,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s1$child_of_active_duty_vet,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s1$single_parent,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s1$displaced_homemaker,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s1$blank_s1_3,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s1$county_code,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s1$municipality_code,3,'right'),3,'right',ellipsis=''),
                     str_trunc(str_pad(s1$wtcs_resident_district,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s1$limited_english_proficiency,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s1$work_status_at_enroll,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s1$highest_grade_completed_at_enroll,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s1$academically_disadvantaged,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s1$economically_disadvantaged,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s1$person_with_disability,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s1$blank_s1_4,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s1$highest_cred_received_at_enroll,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s1$blank_s1_5,21,'right'),21,'right',ellipsis=''),
                     str_trunc(str_pad(s1$high_school_attended,8,'right'),8,'right',ellipsis=''),
                     str_trunc(str_pad(s1$year_graduated_high_school,4,'right'),4,'right',ellipsis=''),
                     str_trunc(str_pad(s1$incarcerated,1,'right'),1,'right',ellipsis='')
  ))
  
  
  #check to insure rows are 100 character long
  which(nchar(final) != 100)  
  
  write.table(as.data.frame(final), output_file_name,col.names = FALSE, row.names = FALSE, quote = FALSE)
  rm(final)
  
  # ----- S2 -------------------------------------####
  cat('Writing S2 to file\n')
  s2[is.na(s2)] <- ''
  
  final <- c(sprintf(paste0(rep("%s",ncol(s2)),collapse = ''),
                     str_trunc(str_pad(s2$record_id,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s2$district_number,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s2$fiscal_year,4,'right'),4,'right',ellipsis=''),
                     str_trunc(str_pad(s2$student_id,9,'right'),9,'right',ellipsis=''),
                     str_trunc(str_pad(s2$blank_s2_1,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s2$economic_indicators,8,'right'),8,'right',ellipsis=''),
                     str_trunc(str_pad(s2$exclusions,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s2$blank_s2_2,16,'right'),16,'right',ellipsis=''),
                     str_trunc(str_pad(s2$ebc_long_term_unempolyed,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s2$ebc_exhausting_tanf,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s2$ebc_foster_care_youth,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s2$ebc_homeless_runaway,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s2$ebc_ex_offender,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s2$ebc_low_income,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s2$ebc_migrant_seasonal_worker,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s2$facility_type,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s2$blank_s2_3,9,'right'),9,'right',ellipsis=''),
                     str_trunc(str_pad(s2$hour_of_service_begining,4,'right'),4,'right',ellipsis=''),
                     str_trunc(str_pad(s2$hour_of_service_intermediate,4,'right'),4,'right',ellipsis=''),
                     str_trunc(str_pad(s2$hour_of_service_advanced,4,'right'),4,'right',ellipsis=''),
                     str_trunc(str_pad(s2$blank_s2_4,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s2$basic_edu_student_goals,10,'right'),10,'right',ellipsis=''),
                     str_trunc(str_pad(s2$blank_s2_5,14,'right'),14,'right',ellipsis='')
  ))
  
  
  which(nchar(final) != 100)  
  
  write.table(as.data.frame(final), output_file_name,col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
  rm(final)
  
  # ----- S3 -------------------------------------####
  cat('Writing S3 to file\n')
  s3[is.na(s3)] <- ''
  s3$rowname <- NULL
  
  final <- c(sprintf(paste0(rep("%s",ncol(s3)),collapse = ''),
                     str_trunc(str_pad(s3$record_id,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s3$district_number,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s3$fiscal_year,4,'right'),4,'right',ellipsis=''),
                     str_trunc(str_pad(s3$student_id,9,'right'),9,'right',ellipsis=''),
                     str_trunc(str_pad(s3$course_enroll_date,6,'right'),6,'right',ellipsis=''),
                     str_trunc(str_pad(s3$course_aid_code,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s3$course_instructional_area,3,'right'),3,'right',ellipsis=''),
                     str_trunc(str_pad(s3$course_sequence,3,'right'),3,'right',ellipsis=''),
                     str_trunc(str_pad(s3$blank_s3_1,3,'right'),3,'right',ellipsis=''),
                     str_trunc(str_pad(s3$course_location,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s3$course_credits,4,'right'),4,'right',ellipsis=''),
                     str_trunc(str_pad(s3$program_fee_codes,6,'right'),6,'right',ellipsis=''),
                     str_trunc(str_pad(s3$course_section,5,'right'),5,'right',ellipsis=''),
                     str_trunc(str_pad(s3$fire_dept_id,5,'right'),5,'right',ellipsis=''),
                     str_trunc(str_pad(s3$course_grade,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s3$course_completion_status,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s3$basic_edu_attendance_hours,4,'right'),4,'right',ellipsis=''),
                     str_trunc(str_pad(s3$recognized_credit_code,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s3$nonstandard_delivery_code,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s3$institution_transferring_credit,4,'right'),4,'right',ellipsis=''),
                     str_trunc(str_pad(s3$location_of_learning,4,'right'),4,'right',ellipsis=''),
                     str_trunc(str_pad(s3$semester,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s3$blank_s3_2,11,'right'),11,'right',ellipsis=''),
                     str_trunc(str_pad(s3$wat_grant_number,11,'right'),11,'right',ellipsis='')
  ))
  
  
  which(nchar(final) != 100)  
  
  write.table(as.data.frame(final), output_file_name,col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
  rm(final)
  
  # ----- S4 -------------------------------------####
  cat('Writing S4 to file\n')
  s4[is.na(s4)] <- ''
  
  final <- c(sprintf(paste0(rep("%s",ncol(s4)),collapse = ''),
                     str_trunc(str_pad(s4$record_id,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s4$district_number,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s4$fiscal_year,4,'right'),4,'right',ellipsis=''),
                     str_trunc(str_pad(s4$student_id,9,'right'),9,'right',ellipsis=''),
                     str_trunc(str_pad(s4$system_office_grant_nbr,11,'right'),11,'right',ellipsis=''),
                     str_trunc(str_pad(s4$service_hours_for_period,4,'right'),4,'right',ellipsis=''),
                     str_trunc(str_pad(s4$blank_s4_1,4,'right'),4,'right',ellipsis=''),
                     str_trunc(str_pad(s4$incarcerated_release_date,10,'right'),10,'right',ellipsis=''),
                     str_trunc(str_pad(s4$continuing_in_grant_activity,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s4$service_provided_codes,12,'right'),12,'right',ellipsis=''),
                     str_trunc(str_pad(s4$employment_codes,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s4$training_outcome_codes,6,'right'),6,'right',ellipsis=''),
                     str_trunc(str_pad(s4$blank_s4_2,10,'right'),10,'right',ellipsis=''),
                     str_trunc(str_pad(s4$district_cbo_site,6,'right'),6,'right',ellipsis=''),
                     str_trunc(str_pad(s4$grant_exit_date,10,'right'),10,'right',ellipsis=''),
                     str_trunc(str_pad(s4$blank_s4_3,8,'right'),8,'right',ellipsis='')           
  ))
  
  which(nchar(final) != 100)  
  
  write.table(as.data.frame(final), output_file_name,col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
  rm(final)
  
  # ----- S5 -------------------------------------####
  cat('Writing S5 to file\n')
  s5[is.na(s5)] <- ''
  
  final <- c(sprintf(paste0(rep("%s",ncol(s5)),collapse = ''),
                     str_trunc(str_pad(s5$record_id,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s5$district_number,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s5$fiscal_year,4,'right'),4,'right',ellipsis=''),
                     str_trunc(str_pad(s5$student_id,9,'right'),9,'right',ellipsis=''),
                     str_trunc(str_pad(s5$client_last_name,26,'right'),26,'right',ellipsis=''),
                     str_trunc(str_pad(s5$client_former_last_name,13,'right'),13,'right',ellipsis=''),
                     str_trunc(str_pad(s5$client_first_name,15,'right'),15,'right',ellipsis=''),
                     str_trunc(str_pad(s5$client_middle_initial,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s5$birth_date,10,'right'),10,'right',ellipsis=''),
                     str_trunc(str_pad(s5$sex,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s5$ethnic_code_hispanic,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s5$race_american_indian_alaskan_native,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s5$race_asian,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s5$race_black,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s5$race_white,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s5$race_pacific_islander,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s5$blank_s5_1,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s5$ssn,9,'right'),9,'right',ellipsis='')   
  ))
  
  which(nchar(final) != 100)  
  
  write.table(as.data.frame(final), output_file_name,col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
  rm(final)
  
  # ----- S6 -------------------------------------####
  cat('Writing S6 to file\n')
  s6[is.na(s6)] <- ''
  
  final <- c(sprintf(paste0(rep("%s",ncol(s6)),collapse = ''),
                     str_trunc(str_pad(s6$record_id,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s6$district_number,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s6$fiscal_year,4,'right'),4,'right',ellipsis=''),
                     str_trunc(str_pad(s6$staff_id,9,'right'),9,'right',ellipsis=''),
                     str_trunc(str_pad(s6$course_aid_code,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s6$course_instructional_area,3,'right'),3,'right',ellipsis=''),
                     str_trunc(str_pad(s6$course_sequence,3,'right'),3,'right',ellipsis=''),
                     str_trunc(str_pad(s6$course_section,5,'right'),5,'right',ellipsis=''),
                     str_trunc(str_pad(s6$course_location,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s6$semester,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s6$no_matching_fte_code,2,'right'),2,'right',ellipsis='')
  ))
  
  which(nchar(final) != 36)  
  
  write.table(as.data.frame(final), output_file_name,col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
  rm(final)
  
  # ----- S7 -------------------------------------####
  cat('Writing S7 to file\n')
  s7[is.na(s7)] <- ''
  s7$rowname <- NULL
  
  final <- c(sprintf(paste0(rep("%s",ncol(s7)),collapse = ''),
                     str_trunc(str_pad(s7$record_id,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s7$district_number,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s7$fiscal_year,4,'right'),4,'right',ellipsis=''),
                     str_trunc(str_pad(s7$student_id,9,'right'),9,'right',ellipsis=''),
                     str_trunc(str_pad(s7$blank_s7_1,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s7$test_date,8,'right'),8,'right',ellipsis=''),
                     str_trunc(str_pad(s7$blank_s7_2,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s7$test_topic,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s7$test_name,3,'right'),3,'right',ellipsis=''),
                     str_trunc(str_pad(s7$abe_grade_level,4,'right'),4,'right',ellipsis=''),
                     str_trunc(str_pad(s7$ell_nrs_level,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s7$scale_score,3,'right'),3,'right',ellipsis=''),
                     str_trunc(str_pad(s7$completed_ell_instructional_level,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s7$advanced_an_efl_level,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s7$blank_s7_3,44,'right'),44,'right',ellipsis=''),
                     str_trunc(str_pad(s7$district_use,12,'right'),12,'right',ellipsis='')
  ))
  
  which(nchar(final) != 100)  
  
  write.table(as.data.frame(final), output_file_name,col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
  rm(final)
  
  # ----- S8 -------------------------------------####
  cat('Writing S8 to file\n')
  s8[is.na(s8)] <- ''
  
  final <- c(sprintf(paste0(rep("%s",ncol(s8)),collapse = ''),
                     str_trunc(str_pad(s8$record_id,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s8$district_number,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s8$fiscal_year,4,'right'),4,'right',ellipsis=''),
                     str_trunc(str_pad(s8$student_id,9,'right'),9,'right',ellipsis=''),
                     str_trunc(str_pad(s8$semester,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s8$post_911_supplemental_payment,7,'right'),7,'right',ellipsis=''),
                     str_trunc(str_pad(s8$post_911_federal_received,7,'right'),7,'right',ellipsis=''),
                     str_trunc(str_pad(s8$blank_s8_1,67,'right'),67,'right',ellipsis='')
  ))
  
  which(nchar(final) != 100)  
  
  write.table(as.data.frame(final), output_file_name,col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
  rm(final)
  
  # ----- S9 -------------------------------------####
  cat('Writing S9 to file\n')
  s9[is.na(s9)] <- ''
  
  final <- c(sprintf(paste0(rep("%s",ncol(s9)),collapse = ''),
                     str_trunc(str_pad(s9$record_id,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s9$district_number,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s9$fiscal_year,4,'right'),4,'right',ellipsis=''),
                     str_trunc(str_pad(s9$student_id,9,'right'),9,'right',ellipsis=''),
                     str_trunc(str_pad(s9$program_aid_code,2,'right'),2,'right',ellipsis=''),
                     str_trunc(str_pad(s9$program_instructional_area,3,'right'),3,'right',ellipsis=''),
                     str_trunc(str_pad(s9$program_sequence,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s9$blank_s9_1,6,'right'),6,'right',ellipsis=''),
                     str_trunc(str_pad(s9$program_graduate,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s9$technical_skill_attainment_assessment,1,'right'),1,'right',ellipsis=''),
                     str_trunc(str_pad(s9$blank_s9_2,69,'right'),69,'right',ellipsis='')
  ))
  
  which(nchar(final) != 100)  
  
  write.table(as.data.frame(final), output_file_name,col.names = FALSE, row.names = FALSE, quote = FALSE, append = TRUE)
  rm(final)
  
  #~~~~~~~ SCRIPT TO PRODUCE STR_TRUNC CODE ~~~~~~####
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
  
  WixLayout <- function(x) {
    # Used to rebuild the WIXUpload() function to match record layout.
    
    layout <- readRDS('S:\\RESEARCH\\09_WTCS Reports\\WTCS_CRS\\_RECORD LAYOUTS.RDS')
    # true version of Record_Layout.xlsx is found in gitlab.com user:schliejc@matc.edu
      # x = record_id
    y <- tolower(x)  #conforms to R record object
    rcd <- layout %>% filter(record_id == x)
    paste0("str_trunc(str_pad(",y,'$',rcd$data_element_name,',',rcd$width,",'right'),",rcd$width,",'right',ellipsis=''),")
  }
  # write.csv(WixLayout('S3'),'temp_layout.csv',row.names = FALSE)
  # # #edit temp_layout.csv in notepad++ to remove " and last comma
  # # #or open in excel, copy to R scrpit and remove last comma 
  # # Then copy script into the equalivant record id section above 

}

#### ~~~~~~~~ Get Historical Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
####
####  Notes:
####       This loads all the historical records into a list of lists
####       It also converts year & credit to integer and dates to date type
####  
#
getHistoricalData <- function(){
  l <- list()
  for(i in 1:9){
    cat(paste0('Getting Historical S',i,'.RDS\n'))
    df <- readRDS(paste0('S:\\RESEARCH\\09_WTCS Reports\\WTCS Historical Data\\Historical S',i,'.RDS'))
    df[df == ''] <- NA
    df$fiscal_year <- as.integer(df$fiscal_year)
    if(i==3){df$course_credits <- as.integer(df$course_credits)/100}
    if(i==4){df$grant_exit_date <- as.Date(df$grant_exit_date, "%m/%d/%Y")}
    if(i==5){df$birth_date <- as.Date(df$birth_date, "%m/%d/%Y")}
    if(i==7){df$test_date <- as.Date(df$test_date, "%m%d%Y")}
    
    l[i] <- list(df)
    rm(df)
  }
  
  rcd <- c('S1','S2','S3','S4','S5','S6','S7','S8','S9')  #this is how record_id appears in each file
  names(l) <- tolower(rcd)    #lower case is the R data frame object
  
  return(l)
}
#### ~~~~~~~~ Finalize Upload & Archive ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

# copy the last WIX.CLIENT file,
# copy the last _RECORD LAYOUTS file,
# copy the _TSA Program List
# copy Client Reporting Functions.r

#save.image(file = "WTCS Upload - FY2019.RData")
updateClientRds <- function(loc){
  
  
  #Use this to update the RDS files after the FY data is finalized.
  #historical data should be located in S:\\RESEARCH\\09_WTCS Reports\\WTCS Historical Data
  #this requires the last WIX.CLIENT file as well as the final WTCS upload
  
  #updateClientRds'S:\\RESEARCH\\09_WTCS Reports\\WTCS Historical Data\\_WTCS Uploads\\CLI09full2019-prod_08-14-2019_final.txt')
  
  source('S:\\RESEARCH\\55_John Schliesmann\\Client Reporting Functions.R')
  
  loadWTCS('new',rcd = 'client')
  
  demographics <- s1 %>% 
    left_join(s5) %>%
    select(student_id,
           fiscal_year,
           birth_date_org = birth_date,
           year_graduated_high_school_org = year_graduated_high_school)
  
  rm(list = ls(pattern = "s[0-9]"))
  
  loc <- 'S:\\RESEARCH\\09_WTCS Reports\\WTCS Historical Data\\_WTCS Uploads\\CLI09full2019-prod_08-14-2019_final.txt'
  #Get official WTCS upload
  loadWTCS(loc,rcd = 'client')  #location of latest WTCS final upload
  FISCAL_YEAR <- unlist(unique(max(s1$fiscal_year)))
  
  #rebuild birthdates and year graduated hs 
  s1 <- s1 %>%
    left_join(demographics) %>%
    mutate(year_graduated_high_school = ifelse(year_graduated_high_school == 'XXXX',
                                               year_graduated_high_school_org,
                                               year_graduated_high_school)) %>%
    select(-birth_date_org,-year_graduated_high_school_org)
  
  s5 <- s5 %>%
    left_join(demographics) %>%
    mutate(birth_date = ifelse(birth_date == '',
                               birth_date_org,
                               birth_date),
           ssn = '') %>%
    select(-birth_date_org,-year_graduated_high_school_org)
  
  
  new = mget(ls(pattern = "s[0-9]")) 
  
  rm(list = ls(pattern = "s[0-9]"))
  rcd <- c('S1','S2','S3','S4','S5','S6','S7','S8','S9')  #this is how record_id appears in each file
  names(new) <- tolower(rcd)    #lower case is the R data frame object
  
  
  setwd('S:\\RESEARCH\\09_WTCS Reports\\WTCS Historical Data')
  for(i in 1:9){
    file.copy(paste0('Historical S',i,'.RDS'),paste0('Z_save\\Historical S',i,' ',format(Sys.Date(), '%m-%d-%Y'),'.RDS'))
    cat(paste0('Merging Historical S',i,'.RDS\n'))
    
    old <- readRDS(paste0('Historical S',i,'.RDS'))
    merged <- old %>% bind_rows(new[i])
    saveRDS(merged, paste0('Historical S',i,'.RDS'))
    rm(old,merged)
    
    #--- create excel (this has not been tested)
    cat("\nWriting record id:",rcd, " to Excel\n")
    
    wb <- createWorkbook()
    ws <- addWorksheet(wb,rcd[i])
    writeData(wb,ws,new[i])
    saveWorkbook(wb,
                 paste0('S:\\RESEARCH\\09_WTCS Reports\\WTCS Historical Data\\',rcd,'_Record by FY\\',rcd,' - ',FISCAL_YEAR,'.xlsx'),
                 overwrite = FALSE)
    rm(wb,ws,tmp)
  }
}

