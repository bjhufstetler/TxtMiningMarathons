marathonList <- tibble::tibble(type = base::rep(c("us", "international"), each =10),
                               id = c("ING New York City", "Bank of America Chicago", "Boston", "Marine Corps", "Honolulu",
                                      "Walt Disney World", "Los Angeles", "Rock n Roll San Diego", "Medtornic Twin Cities", "Portland",
                                      "London", "Berlin", "Tokyo", "Paris", "Dubai",
                                      "Amsterdam", "Toronto", "Frankfurt", "Rotterdam", "Rome"),
                               siteURL = paste0("https://www.", c("nyrr.org/tcsnycmarathon", "chicagomarathon.com", "baa.org", "marinemarathon.com", "honolulumarathon.org/?s=registration",
                                                                  "rundisney.com/events/disneyworld/disneyworld-marathon-weekend/events/marathon", "lamarathon.com", "runrocknroll.com/Events/San-Diego", "tcmevents.org/events/medtronic-twin-cities-marathon-weekend-2020/race/marathon", "portlandmarathon.com",
                                                                  "virginmoneylondonmarathon.com/en-gb", "bmw-berlin-marathon.com/en", "marathon.tokyo/en", "schneiderelectricparismarathon.com/en", "dubaimarathon.org",
                                                                  "tcsamsterdammarathon.nl/en", "torontowaterfrontmarathon.com", "frankfurt-marathon.com/en", "nnmarathonrotterdam.org", "runromethemarathon.com/en/home-en")))
