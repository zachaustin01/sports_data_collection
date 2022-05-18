library(httr)

# F5 Odds
f5_odds <- function(){

  dk_link <- "https://sportsbook-us-in.draftkings.com//sites/US-IN-SB/api/v4/eventgroups/88670847/categories/729?format=json"

  test <- GET(dk_link,
              add_headers(
                "Host" = "sportsbook-us-in.draftkings.com",
                "User-Agent" = "DKWeb",
                "Accept" = "application/json",
                "Accept-Language" = "en-US,en;q=0.9",
                "Accept-Encoding" = "gzip, deflate, br",
                "Referer" = "https://sportsbook.draftkings.com/",
                "Content-Type" = "application/json",
                "Origin" = "https://sportsbook.draftkings.com",
                "DNT" = "1",
                "Connection" = "keep-alive",
                "Sec-Fetch-Dest" ="empty",
                "Sec-Fetch-Mode" ="cors",
                "Sec-Fetch-Site" ="same-site",
                "TE" = "trailers"))

  json_return <- jsonlite::fromJSON(content(test, as = "text"))

  # Note: need to change "7" here to whatever list object is populated automatically
  pre_f5_odds = bind_rows(Filter(Negate(is.null), json_return[["eventGroup"]][["offerCategories"]][["offerSubcategoryDescriptors"]])[[1]][["offerSubcategory"]][["offers"]][[1]])

    f5_odds = bind_rows(bind_rows(pre_f5_odds)$outcomes) %>%
    select(providerOfferId,oddsAmerican,line,label)

  mid_events = pre_f5_odds %>%
    select(
      providerEventId,
      providerOfferId
    )

  f5_events = json_return[['eventGroup']][['events']] %>%
    select(providerEventId,teamShortName1,teamShortName2) %>%
    left_join(mid_events,by='providerEventId') %>%
    left_join(f5_odds,by='providerOfferId') %>%
    select(teamShortName1,teamShortName2,line,label,oddsAmerican) %>%
    mutate(Odds = as.integer(oddsAmerican)) %>%
    filter(!is.na(line)) %>%
    mutate(teamShortName1 = ifelse(teamShortName1=="SFG","SF",teamShortName1),
           teamShortName2 = ifelse(teamShortName2=="SFG","SG",teamShortName2))

  # Check for double headers
  prior_teams = ""
  prior_teams_list = c()
  for(r in 1:nrow(f5_events)){

    row = f5_events[r,]

    teams = paste0(row$teamShortName1,row$teamShortName2)
    if(teams!=prior_teams){
      # New batch of lines

      if(teams %in% prior_teams_list){
        # Already lines available, so this is for second game
        # Rename team names in big dataframe
        f5_events[r,"teamShortName1"] = paste0(f5_events[r,"teamShortName1"],".1")
        f5_events[r,"teamShortName2"] = paste0(f5_events[r,"teamShortName2"],".1")
        teams = paste0(paste0(f5_events[r,"teamShortName1"],".1"),
                       paste0(f5_events[r,"teamShortName2"],".1"))

      }

      prior_teams_list = append(prior_teams_list,prior_teams)

    }

    prior_teams = teams

  }


  return(f5_events)


}

#NRFI Odds

nrfi_odds <- function(){

  nrfi_link <- "https://sportsbook-us-in.draftkings.com//sites/US-IN-SB/api/v4/eventgroups/88670847/categories/1024?format=json"

  test <- GET(nrfi_link,
              add_headers(
                "Host" = "sportsbook-us-in.draftkings.com",
                "User-Agent" = "DKWeb",
                "Accept" = "application/json",
                "Accept-Language" = "en-US,en;q=0.9",
                "Accept-Encoding" = "gzip, deflate, br",
                "Referer" = "https://sportsbook.draftkings.com/",
                "Content-Type" = "application/json",
                "Origin" = "https://sportsbook.draftkings.com",
                "DNT" = "1",
                "Connection" = "keep-alive",
                "Sec-Fetch-Dest" ="empty",
                "Sec-Fetch-Mode" ="cors",
                "Sec-Fetch-Site" ="same-site",
                "TE" = "trailers"))

  nrfi_json_return <- jsonlite::fromJSON(content(test, as = "text"))

  pre_nrfi_odds = bind_rows(Filter(Negate(is.null), nrfi_json_return[["eventGroup"]][["offerCategories"]][["offerSubcategoryDescriptors"]])[[1]][["offerSubcategory"]][["offers"]][[1]])
  nrfi_odds = bind_rows(bind_rows(pre_nrfi_odds)$outcomes)

  off_events_mid = pre_nrfi_odds %>%
    select(
      providerEventId,
      providerOfferId
    )

  nrfi_events = nrfi_json_return[["eventGroup"]][["events"]] %>%
    select(providerEventId,teamShortName1,teamShortName2) %>%
    left_join(off_events_mid,by='providerEventId') %>%
    left_join(nrfi_odds,by='providerOfferId') %>%
    select(teamShortName1,teamShortName2,line,label,oddsAmerican) %>%
    mutate(Odds = as.integer(oddsAmerican)) %>%
    mutate(teamShortName1 = ifelse(teamShortName1=="SFG","SF",teamShortName1),
           teamShortName2 = ifelse(teamShortName2=="SFG","SG",teamShortName2))

  # Check for double headers
  prior_teams = ""
  prior_teams_list = c()
  for(r in 1:nrow(nrfi_events)){

    row = nrfi_events[r,]

    teams = paste0(row$teamShortName1,row$teamShortName2)
    if(teams!=prior_teams){
      # New batch of lines

      if(teams %in% prior_teams_list){
        # Already lines available, so this is for second game
        # Rename team names in big dataframe
        nrfi_events[r,"teamShortName1"] = paste0(nrfi_events[r,"teamShortName1"],".1")
        nrfi_events[r,"teamShortName2"] = paste0(nrfi_events[r,"teamShortName2"],".1")
        teams = paste0(paste0(nrfi_events[r,"teamShortName1"],".1"),
                       paste0(nrfi_events[r,"teamShortName2"],".1"))

      }

      prior_teams_list = append(prior_teams_list,prior_teams)

    }

    prior_teams = teams

  }

  return(nrfi_events)

}


batter_props <- function(){

  link <- "https://sportsbook-us-in.draftkings.com//sites/US-IN-SB/api/v4/eventgroups/88670847/categories/743/subcategories/6606?format=json"

  test <- GET(link,
              add_headers(
                "Host" = "sportsbook-us-in.draftkings.com",
                "User-Agent" = "DKWeb",
                "Accept" = "application/json",
                "Accept-Language" = "en-US,en;q=0.9",
                "Accept-Encoding" = "gzip, deflate, br",
                "Referer" = "https://sportsbook.draftkings.com/",
                "Content-Type" = "application/json",
                "Origin" = "https://sportsbook.draftkings.com",
                "DNT" = "1",
                "Connection" = "keep-alive",
                "Sec-Fetch-Dest" ="empty",
                "Sec-Fetch-Mode" ="cors",
                "Sec-Fetch-Site" ="same-site",
                "TE" = "trailers"))

  json_return <- jsonlite::fromJSON(content(test, as = "text"))



}

full_game <- function(){

  link = "https://sportsbook-us-in.draftkings.com//sites/US-IN-SB/api/v4/eventgroups/88670847/categories/493/subcategories/4519?format=json"

  test <- GET(link,
              add_headers(
                "Host" = "sportsbook-us-in.draftkings.com",
                "User-Agent" = "DKWeb",
                "Accept" = "application/json",
                "Accept-Language" = "en-US,en;q=0.9",
                "Accept-Encoding" = "gzip, deflate, br",
                "Referer" = "https://sportsbook.draftkings.com/",
                "Content-Type" = "application/json",
                "Origin" = "https://sportsbook.draftkings.com",
                "DNT" = "1",
                "Connection" = "keep-alive",
                "Sec-Fetch-Dest" ="empty",
                "Sec-Fetch-Mode" ="cors",
                "Sec-Fetch-Site" ="same-site",
                "TE" = "trailers"))

  json_return <- jsonlite::fromJSON(content(test, as = "text"))

  pre_odds = bind_rows(bind_rows(json_return[["eventGroup"]][["offerCategories"]][["offerSubcategoryDescriptors"]][[1]][["offerSubcategory"]][["offers"]][[1]])$outcomes)

  post_odds = pre_odds %>%
    mutate(line = ifelse(is.na(line),0,line))

  odds_events_mid = bind_rows(json_return[["eventGroup"]][["offerCategories"]][["offerSubcategoryDescriptors"]][[1]][["offerSubcategory"]][["offers"]][[1]]) %>%
    select(providerOfferId,providerEventId)

  odds_events = json_return[["eventGroup"]][["events"]] %>%
    select(providerEventId,teamShortName1,teamShortName2) %>%
    left_join(odds_events_mid,by='providerEventId') %>%
    left_join(post_odds,by='providerOfferId') %>%
    select(teamShortName1,teamShortName2,label,oddsAmerican,line) %>%
    mutate(Odds = as.integer(oddsAmerican))

  return(odds_events)


}
