# This is just somewhere to store the marathon names and urls of interest
marathonList <- tibble::tibble(type = base::rep(c("us", "international"), each =10),
                               id = c("ING New York City", "Bank of America Chicago", "Boston", "Marine Corps", "Honolulu",
                                      "Walt Disney World", "Los Angeles", "Rock n Roll San Diego", "Medtornic Twin Cities", "Portland",
                                      "London", "Berlin", "Tokyo", "Paris", "Dubai",
                                      "Amsterdam", "Toronto", "Frankfurt", "Rotterdam", "Rome"),
                               siteURL = paste0("https://www.", c("nyrr.org/tcsnycmarathon", "chicagomarathon.com", "baa.org", "marinemarathon.com", "honolulumarathon.org/?s=registration",
                                                                  "rundisney.com/events/disneyworld/disneyworld-marathon-weekend/events/marathon", "lamarathon.com", "runrocknroll.com/Events/San-Diego", "tcmevents.org/events/medtronic-twin-cities-marathon-weekend-2020/race/marathon", "portlandmarathon.com",
                                                                  "virginmoneylondonmarathon.com/en-gb", "bmw-berlin-marathon.com/en", "marathon.tokyo/en", "schneiderelectricparismarathon.com/en", "dubaimarathon.org",
                                                                  "tcsamsterdammarathon.nl/en", "torontowaterfrontmarathon.com", "frankfurt-marathon.com/en", "nnmarathonrotterdam.org", "runromethemarathon.com/en/home-en")))
# if you don't have pacman, run install.packages("pacman")
# pacman stands for Package Manager and lets you load libraries if they
# exist and if you haven't installed them yet, it installs them then loads them
# p_load == package load
pacman::p_load(magrittr)
# Note that I don't like to load libraries into our environment because conflicts 
# can easily arise and I like to have everything explicitly stated to reduce unnecessary
# bloat. One exception to this occurs when I need access to the pipe operator. If I just 
# need the %>% operator, I'll load {dplyr}, if I need the %<>% operator, I'll load {magrittr}

# Also, {base} does not have to be explicitly called but I like to keep track
# of where every function is coming from.

# Lets create a space to store all the data from Chicago. After looking at the 
# source, I think 11 columns should be good to get all the info we're after
chicagoData <- base::matrix(ncol = 11)

# The chicago data is presented as a searchable list, the following url segments 
# allow us to input search parameters YEAR, PAGE, and SEX
chicagoPageURL <- c("https://results.chicagomarathon.com/","/?page=", "&event=MAR&lang=EN_CAP&num_results=1000&pid=list&search%5Bsex%5D=")

# Chicago Marathon has data from 1996-2019 we can index over all of these years
for(year in 1996:2019){
  # Sex isn't an extractable element from any of the data presentation methods so we have
  # to do a search on it and then we'll apply the search index as a sex column later
  for(sex in c("M","F")){
    # Open the first page in the search category and find the total number of pages
    page <- base::paste0(chicagoPageURL[1],
                         as.character(year),
                         chicagoPageURL[2],
                         "1",
                         chicagoPageURL[3], 
                         sex)
    
    pageCount <- page %>%
      # parse the html into an xml format
      xml2::read_html() %>%
      # The '.' indicates the class (inspecting the page reveals that the page buttons use the class = pagination)
      rvest::html_nodes('.pagination') %>% 
      # Extract the raw text from the page buttons
      rvest::html_text() %>%
      # The last page is the one between ... and >
      qdapRegex::rm_between("...", ">", extract = T) %>%
      # Store it as an integer so we can index up to it
      base::as.integer()
    # Index through each page number for that year/sex combination
    for(pageNumber in base::seq_len(pageCount)){
      # Create the search url
      page <- base::paste0(chicagoPageURL[1],
                           as.character(year),
                           chicagoPageURL[2],
                           as.character(pageNumber),
                           chicagoPageURL[3],
                           sex)
      # Get all the text from each row (inspecting the page shows that each element uses a class = list-field XXX)
      pageData <- xml2::read_html(page) %>%
        rvest::html_nodes(".list-field") %>%
        rvest::html_text()
      
      # Create a matrix from the parsed data, the first 8 elements are column headings
      pageMatrix <- base::matrix(pageData[-c(1:8)], byrow=T, ncol = 8)
      
      # Create sex, year, race, and country columns from the index information and the name column
      sexCol <- base::rep(sex, nrow(pageMatrix))
      yearCol <- base::rep(year, nrow(pageMatrix))
      raceCol <- base::rep("Chicago", base::nrow(pageMatrix))
      countryCol<- pageMatrix[,3] %>%
        qdapRegex::rm_between("(", ")", extract = T) %>%
        base::as.matrix()
      
      # Remove the country info from the name column and create a separate name column
      # Combine all the columns now into a singular matrix
      pageClean <- as.matrix(lapply(pageMatrix[,3], FUN = function(x) base::gsub("\\s\\(.*", "", x))) %>%
        base::cbind(yearCol, pageMatrix[,-c(3,8)], sexCol, raceCol, countryCol)
      
      # Name the columns
      base::colnames(pageClean) <- c("name", "yearcol",pageData[c(1:2,4:7)], "sex", "race","country")
      
      # Combine the info from this specific page with the data from the other pages
      chicagoData %<>% base::rbind(pageClean)
    }
  }
}

# This took a really long time to run, best to save the data to our local machine for recall later
save(chicagoData, file = "chicagoData.RData")
  
