
LabJs_device <- function(data){
  
  # load/ install required packages:
  packages= c("stringi", "uaparserjs", "dplyr") # list of used packages:
  
  for(i in 1:length(packages)){
    
    if(packages[i] %in% rownames(installed.packages())==FALSE){
      install.packages(packages[i])
      library(packages[i], character.only=TRUE)
    }else{
      library(packages[i], character.only=TRUE)
    }
  }
  
  # Extract code containing meta tag

  I<- data[which(!is.na(data$meta)),]
  setup= I$meta

  
  
  result= NULL
  for(i in 1:length(setup)){ ### iterate for each subject
    t= NULL
    main= unlist(strsplit(setup[i], '",'))
    
    labjs_version= unlist(strsplit(main[1], ":"))[2]
    labjs_version= gsub("[^0-9A-Za-z///' ]","'" , labjs_version ,ignore.case = TRUE)
  
    
    screen= unlist(strsplit(main[9], ','))
    
    screen_width= unlist(strsplit(screen[2], ':'))[2]
    screen_height= unlist(strsplit(screen[3], ':'))[2]
    
    device_pixel_ratio= unlist(strsplit(screen[8], ':'))[2]
    
    platform= unlist(strsplit(main[5], ':'))[2]
    
    
    #### parse meta:
    out1<- dplyr::glimpse(ua_parse(setup[i]))
    browser= out1$ua.family
    browser_version= paste(out1$ua.major, out1$ua.minor, out1$ua.patch, sep= '.')
    OS= out1$os.family
    # OS_version= out1$os.major
    
    t<- data.frame(labjs_version, browser,  screen_width, screen_height, device_pixel_ratio,
                   platform, browser, browser_version, OS)
    result<- rbind(result, t)
  }
  
  result$device_pixel_ratio<- str_extract(result$device_pixel_ratio, "\\d+\\.*\\d*")
  result$subject= 1: nrow(result)
  
  
return(result)
  
}
