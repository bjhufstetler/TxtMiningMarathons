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
# This thing takes a while to run, let's give ourselves 
# a pretty way to see it's progress
Monitor <- function(year, sex, pageNumber, pageCount)
{ 
  sumryStat <- c(year, sex, pageNumber, pageCount)
  sumryStat <- base::format(sumryStat, digits = 0)
  base::cat(paste("Progress | Year = ", sumryStat[1], 
                  "| Sex =", sumryStat[2],
                  "| Page =", sumryStat[3],
                  " of ", sumryStat[4]))
  base::cat("\n")
  utils::flush.console()
}


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
# allow us to input search parameters YEAR, PAGE, and SEX (note that the data is stored differently in 2019 than the others)
chicagoPageURL <- c("https://results.chicagomarathon.com/2019/?pid=list&num_results=1000&lang=EN_CAP",
                    "https://chicago-history.r.mikatiming.com/2018/?pid=list&num_results=1000&lang=EN_CAP&event_main_group=",
                    "&page=",
                    "&search%5Bsex%5D=")

# Chicago Marathon has data from 1996-2019 we can index over all of these years
for(year in 2008:2019){
  # Sex isn't an extractable element from any of the data presentation methods so we have
  # to do a search on it and then we'll apply the search index as a sex column later
  yearURL <- base::ifelse(year == 2019, 
                          chicagoPageURL[1], 
                          base::paste0(chicagoPageURL[2], as.character(year)))
  for(sex in c("M","W")){
    # Open the first page in the search category and find the total number of pages
    page <- base::paste0(yearURL,
                         chicagoPageURL[3],
                         "1",
                         chicagoPageURL[4], 
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
      page <- base::paste0(yearURL,
                           chicagoPageURL[3],
                           as.character(pageNumber),
                           chicagoPageURL[4],
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
      Monitor(year, sex, pageNumber, pageCount)
    }
  }
}

# Oops, accidentally put col in the name yearcol, not too late, let's replace it
base::colnames(chicagoData) <- c("name", "year",pageData[c(1:2,4:7)], "sex", "race","country")

# Clean up this mess we've made
rm(countryCol, 
   pageClean, 
   pageMatrix, 
   chicagoPageURL, 
   page, 
   pageCount, 
   pageData, 
   pageNumber, 
   raceCol, 
   sex, 
   sexCol, 
   year, 
   yearCol,
   yearURL,
   monitor)

# This took a really long time to run, best to save the data to our local machine for recall later
base::save(chicagoData, file = base::file.path("R", "data", "chicagoData.RData"))

###############################################################################################
# Ok, now that we have the data, let's do something with it
# First things first, let's convert the data into a tibble so we can use some tidyverse functions
dataTibble <- tibble::tibble(year = base::unlist(chicagoData[-1,colnames(chicagoData)=="year"]),
                             sex = base::unlist(chicagoData[-1,colnames(chicagoData)=="sex"]))

# Lets get some of the summary stats, how many people of each gender participated each year
# Then plot it
dataTibble %>%
  # Analyze the data in year groups
  dplyr::group_by(year) %>%
  # Count the number of people of each sex were present in each year group
  dplyr::count(sex) %>%
  # Plot the data in a count vs year line graph, with each sex broken out
  ggplot2::ggplot(ggplot2::aes(x = year,
                               y = n,
                               color = sex)) +
  ggplot2::geom_line() +
  # Add a line at the date of the 2013 Boston Marathon
  ggplot2::geom_vline(xintercept = 2013.33, 
                      color = "red", 
                      linetype = "dashed") +
  # Clean up the x axis to make it more readable
  ggplot2::scale_x_continuous(breaks=c(2008:2019)) +
  # Rotate the x axis labels
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))


  
