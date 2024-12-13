### This file stores all kinds of helper functions from our different scripts


## check which packages need to be loaded/installed
# adapted from Jochem Tolsma
fpackage_check <- function(packages) {
  for (package in packages){
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    }
  }
}

## retrieve professor NARCIS IDs based on their NARCIS publication data,
## ORCID information, and name matching with OA results
professor_identifier_retriever <- function(narcis_id,
                                           pub_data,
                                           prof_data){
  
  # set all the identifiers to NA
  oa_ids_pubs <- NA
  oa_ids_names <- NA
  oa_ids_orcid <- NA
  prof_orcid <- NA
  prof_name <- NA
  prof_name_w_initials <- NA
  prof_name_w_initials_space <- NA
  prof_initials_wo_first_name <- NA
  prof_initials_wo_first_name_space <- NA
  prof_name_first_initials <- NA
  prof_name_first_initials_space <- NA
  prof_alternative_full_name <- NA
  
  # get the professor's narcis id anad get their publications based on this
  prof_pubs <- filter(pub_data, profile_id == narcis_id)
  
  # get their name
  prof_name_details <- filter(prof_data, profile_id == narcis_id)
  prof_orcid <- prof_name_details$ORCID
  prof_name <- paste(prof_name_details$first, 
                     prof_name_details$last)
  
  # get the name, initials, etc combinations
  # convert the special characters
  prof_name_details[, 4:11] <- iconv(prof_name_details[, 4:11], to='ASCII//TRANSLIT')
  
  # get first/last name
  prof_name <- paste(
    prof_name_details$first, 
    prof_name_details$last)
  prof_name <- paste0("^",
                      prof_name,
                      "$")
  # get their initials, drop their name in the brackets
  prof_initials <- str_squish(str_split_i(prof_name_details$initialen, "\\(", 1))
  # but some profs might have a roepnaam which is not the name they use
  # to sign their papers (the brackets will contain the roepnaam, but the
  # initials will be containing the actual first name)
  # get an additional option w/o the first name at all
  prof_initials_wo_first_name <- paste(str_split(prof_initials, "\\.")[[1]], collapse = ".")
  # and space them out
  prof_initials_wo_first_name_space <- paste(str_split(prof_initials, "\\.")[[1]], collapse = ". ")
  
  # get the initials
  prof_initials <- str_split(prof_initials, "\\.")[[1]][-c(1, length(str_split(prof_initials, "\\.")[[1]]))]
  prof_initials_no_space <- paste0(paste(prof_initials, collapse = "."), ".")
  prof_initials_space <- paste0(paste(prof_initials, collapse = ". "), ".")
  # professor first name, initials, last name
  prof_name_w_initials <-  tolower(paste(prof_name_details$first, 
                                         prof_initials_no_space,
                                         prof_name_details$last))
  # tidy up
  prof_name_w_initials <- paste0("^", 
                                 str_replace(str_replace(prof_name_w_initials, " \\. ", " "), "\\.\\.", " "),
                                 "$")
  
  # professor first name, initials, last name, with spaces between the initials
  prof_name_w_initials_space <-  tolower(paste(
    prof_name_details$first, 
    prof_initials_space,
    prof_name_details$last))
  
  # tidy up
  prof_name_w_initials_space <- paste0("^", 
                                       str_replace(str_replace(prof_name_w_initials_space, " \\. ", " "), "\\.\\.", " "),
                                       "$")
  
  # just the first letter of the name + initials
  # with spacing
  first_initials_space <- paste(paste0(substring(prof_name_details$first, 1, 1), "."),
                                prof_initials_space)
  
  prof_name_first_initials_space <- tolower(paste(first_initials_space, 
                                                  prof_name_details$last))
  # tidy up
  prof_name_first_initials_space <- paste0("^", 
                                           str_replace(str_replace(prof_name_first_initials_space, " \\. ", " "), "\\.\\.", "."),
                                           "$")
  
  # w/o spacing
  first_initials <- str_remove_all(paste(paste0(substring(prof_name_details$first, 1, 1), 
                                                "."),
                                         prof_initials_space), " ")
  
  prof_name_first_initials <- tolower(paste(first_initials, 
                                            prof_name_details$last))
  
  # tidy up
  prof_name_first_initials <- paste0("^", 
                                     str_replace(str_replace(prof_name_first_initials, " \\. ", " "), "\\.\\.", " "),
                                     "$")
  
  # for some professors, we manually find their actual name vs their roepnaam in NARCIS
  ### redacted for privacy.
  
  # get the DOI list without the NAs
  doi_list <- prof_pubs$DOI[!is.na(prof_pubs$DOI)]
  prof_alternative_full_name <- NA
  
  # try to get the IDs
  if (all(is.na(oa_ids_pubs))){
    # if any DOIs in our list:
    if (length(doi_list)>0){
      
      # call OpenAlex to get their works based on the DOIs
      # do it in batches (currently 25)
      batch_size <- 25
      batches <- ceiling(length(doi_list)/batch_size)
      prof_works_oa <- data.frame(matrix(NA, ncol = 16, nrow = 0))
      begin_batch <- 1
      end_batch <- batch_size
      # initialize an empty data frame
      prof_works_oa_batch <- data.frame(matrix(NA, ncol = 16, nrow = 0))
      # loop through the batches
      for(i in 1:batches){
        # get the first batch
        # try, if error, just stop
        try(
          prof_works_oa_batch <- oa_fetch(
            entity = "works",
            doi = doi_list[begin_batch:end_batch]), 
          silent = TRUE
        )
        # when binding, some columns might not be there
        # so to address that, check the columns
        # if the new batch has less (but only after the first batch was processed, thus i > 1)
        if (i > 1 & length(which(!colnames(prof_works_oa) %in% colnames(prof_works_oa_batch))) > 0){
          # add the missing columns filled with NAs
          prof_works_oa_batch[colnames(prof_works_oa)[which(! colnames(prof_works_oa) %in% colnames(prof_works_oa_batch))]] <- NA
          # rearrange the columns
          prof_works_oa_batch <- prof_works_oa_batch[colnames(prof_works_oa)]
        }
        # if the new batch has more (but only after the first batch was processed, thus i > 1)
        if (i > 1 & length(which(! colnames(prof_works_oa_batch) %in% colnames(prof_works_oa))) > 0){
          # add the missing columns filled with NAs
          prof_works_oa[colnames(prof_works_oa_batch)[which(! colnames(prof_works_oa_batch) %in% colnames(prof_works_oa))]] <- NA
          # rearrange the columns
          prof_works_oa <- prof_works_oa[colnames(prof_works_oa_batch)]
        }
        
        # bind to the rest
        prof_works_oa <- rbind(prof_works_oa,
                               prof_works_oa_batch)
        
        # get the remaining batch size, and go to the end if less than batch size, 
        # and go for another round if more than batch size
        if (length(doi_list)-end_batch <= batch_size){
          end_batch <- end_batch + length(doi_list)-end_batch
        }else{
          end_batch <- end_batch + batch_size
        }
        # set the beginning to the beginning + batch size
        begin_batch <- begin_batch + batch_size
      }
      
      # and now loop through the authors of these papers in OpenAlex to get info
      # on the author ids OpenAlex has on them
      prof_oaid_info <- data.frame(matrix(NA, nrow = 0, ncol = 13))
      # loop through the papers we retrieved, get the DOIs and author lists and try to match the
      # author to those we miss
      if (nrow(prof_works_oa) > 0 & !all(is.na(prof_works_oa))){
        for (i in 1:nrow(prof_works_oa)){
          author_info <- c()
          info <- prof_works_oa[i, c("id","display_name", "doi")]
          doi_in_question <- str_remove(info$doi, "https://doi.org/")
          # if found and one person
          authors <- prof_works_oa[i, "author"][[1]][[1]]
          if (class(authors) != "logical"){
            
            # remove special characters
            authors$au_display_name <- iconv(authors$au_display_name, to='ASCII//TRANSLIT')
            
            # find the author with a matching name
            authors$match <- grepl(prof_name, tolower(authors$au_display_name))
            # check with initials as well
            authors$match <- ifelse(authors$match == FALSE,
                                    grepl(prof_name_w_initials, tolower(authors$au_display_name)),
                                    authors$match)
            # and spaced out initials
            authors$match <- ifelse(authors$match == FALSE,
                                    grepl(prof_name_w_initials_space, tolower(authors$au_display_name)),
                                    authors$match)
            # and no first name, but only initials and also only spaced out initials
            authors$match <- ifelse(authors$match == FALSE,
                                    grepl(prof_name_first_initials, tolower(authors$au_display_name)),
                                    authors$match)
            
            authors$match <- ifelse(authors$match == FALSE,
                                    grepl(prof_name_first_initials_space, tolower(authors$au_display_name)),
                                    authors$match)
            
            # if nothing still, initials without the first name
            authors$match <- ifelse(authors$match == FALSE,
                                    grepl(prof_initials_wo_first_name, tolower(authors$au_display_name)),
                                    authors$match)
            
            authors$match <- ifelse(authors$match == FALSE,
                                    grepl(prof_initials_wo_first_name_space, tolower(authors$au_display_name)),
                                    authors$match)   
            # and their alternative first name we manually correct for
            authors$match <- ifelse(authors$match == FALSE,
                                    grepl(prof_alternative_full_name, tolower(authors$au_display_name)),
                                    authors$match)   
            
            # fetch the author, if there is a match
            author <- filter(authors, match == TRUE)
            
            if (nrow(author) == 1){
              # remove the "match" column
              author_info <- author[, -12]
              # bind with the narcis ID for later
              author_info$profile_id <- narcis_id
            }
          }
          
          prof_oaid_info <- rbind(prof_oaid_info,
                                  author_info)
        }
        
        # make sure the empty oa_id did not make it in here, because then we get stuck
        # in an endless loop (or fetch things we do not want to, burdening the API)
        if (nrow(prof_oaid_info) > 0){
          prof_oaid_info <- filter(prof_oaid_info, au_id != "https://openalex.org/A9999999999")
          # get all oa ids of the author
          oa_ids_pubs <- as.character(unique(prof_oaid_info$au_id))
        }
      }
    }
  }
  
  # if no ID found:
  if (all(is.na(oa_ids_orcid))){
    # if no DOIs in our list, check if there is an ORCID we can use
    prof_orcid <- prof_name_details$ORCID
    # if there's an orcid
    if (!is.na(prof_orcid)){
      # fetch professor info from ORCIDs
      prof_info <- oa_fetch(
        entity = "authors", 
        orcid = prof_orcid)
      
      if (!is.null(prof_info)){
        # make sure the no name ID is not in here
        prof_info <- filter(prof_info, 
                            id != "https://openalex.org/A9999999999")
        # now retrieve OA IDs from this
        oa_ids_orcid <- as.character(unique(prof_info$id))
      }
    }
  }
  
  # if oa_ids still NA
  oa_ids_by_name <- as.data.frame(matrix(NA, nrow = 0, ncol = 17))
  if (all(is.na(oa_ids_names))){
    # search by name
    oa_au_result <- oa_fetch("author",
                             search = paste(prof_name_details$first, 
                                            prof_name_details$achternaam),
                             last_known_institution.country_code = "NL")
    
    # if no results, try an alternative name if present
    if(!is.null(nrow(oa_au_result)) & !is.na(prof_alternative_full_name)){
      oa_au_result <- oa_fetch("author",
                               search = prof_alternative_full_name,
                               last_known_institution.country_code = "NL")
    }
    # if still no results, try with extensive additional name displays:
    
    # if any results, see which names are an exact match
    if(!is.null(nrow(oa_au_result))){
      
      oa_au_result$display_name <- iconv(oa_au_result$display_name , to='ASCII//TRANSLIT')
      
      # find the author with a matching name
      oa_au_result$match <- grepl(prof_name, tolower(oa_au_result$display_name))
      # check with initials as well
      oa_au_result$match <- ifelse(oa_au_result$match == FALSE,
                                   grepl(prof_name_w_initials, tolower(oa_au_result$display_name)),
                                   oa_au_result$match)
      # and spaced out initials
      oa_au_result$match <- ifelse(oa_au_result$match == FALSE,
                                   grepl(prof_name_w_initials_space, tolower(oa_au_result$display_name)),
                                   oa_au_result$match)
      # and no first name, but only initials and also only spaced out initials
      oa_au_result$match <- ifelse(oa_au_result$match == FALSE,
                                   grepl(prof_name_first_initials, tolower(oa_au_result$display_name)),
                                   oa_au_result$match)
      
      oa_au_result$match <- ifelse(oa_au_result$match == FALSE,
                                   grepl(prof_name_first_initials_space, tolower(oa_au_result$display_name)),
                                   oa_au_result$match)
      
      # if nothing still, initials without the first name
      oa_au_result$match <- ifelse(oa_au_result$match == FALSE,
                                   grepl(prof_initials_wo_first_name, tolower(oa_au_result$display_name)),
                                   oa_au_result$match)
      
      oa_au_result$match <- ifelse(oa_au_result$match == FALSE,
                                   grepl(prof_initials_wo_first_name_space, tolower(oa_au_result$display_name)),
                                   oa_au_result$match)   
      # and their alternative first name we manually correct for
      oa_au_result$match <- ifelse(oa_au_result$match == FALSE,
                                   grepl(prof_alternative_full_name, tolower(oa_au_result$display_name)),
                                   oa_au_result$match)   
      
      oa_au_result <- filter(oa_au_result, 
                             ids != "https://openalex.org/A9999999999")
      # fetch the OA IDs if there is a match
      prof_oa_ids <- filter(oa_au_result, match == TRUE)
      id_list <- c()
      
      if (nrow(prof_oa_ids)>0){
        oa_ids_names <- prof_oa_ids$ids
        # if a list, only retain the OA IDs:
        if(class(oa_ids_names) == "list"){
          oa_ids_names <- c()
          for (j in 1:nrow(prof_oa_ids)){
            id <- prof_oa_ids$ids[[j]]['openalex']
            oa_ids_names <- c(oa_ids_names, id)
          }
          oa_ids_names <- unname(oa_ids_names)
        }
      }
    }
  }
  # if still nothing
  if (all(is.na(oa_ids_names))){
    # try to find OA ID matches by looking at alternative OA name spellings as well
    prof_name_details <- filter(profs_full, profile_id == narcis_id)
    
    oa_au_result <- oa_fetch("author",
                             search = paste(prof_name_details$first, 
                                            prof_name_details$achternaam))
    
    # unlist the results
    if (!all(is.na(oa_au_result))){
      display_name_ids <- data.frame(matrix(NA, nrow = 0, ncol = 2))
      for (j in 1:nrow(oa_au_result)){
        display_names <- unlist(oa_au_result$display_name_alternatives[j])
        ids <- unlist(oa_au_result$ids[j])['openalex']
        row <- cbind.data.frame("display_name" = display_names,
                                "oa_id" = ids)
        rownames(row) <- c()
        display_name_ids <- rbind(display_name_ids,
                                  row, row.names = NULL)
      }
      
      display_name_ids$display_name <- iconv(display_name_ids$display_name , to='ASCII//TRANSLIT')
      
      # find the author with a matching name
      display_name_ids$match <- grepl(prof_name, tolower(display_name_ids$display_name))
      # check with initials as well
      display_name_ids$match <- ifelse(display_name_ids$match == FALSE,
                                       grepl(prof_name_w_initials, tolower(display_name_ids$display_name)),
                                       display_name_ids$match)
      # and spaced out initials
      display_name_ids$match <- ifelse(display_name_ids$match == FALSE,
                                       grepl(prof_name_w_initials_space, tolower(display_name_ids$display_name)),
                                       display_name_ids$match)
      # and no first name, but only initials and also only spaced out initials
      display_name_ids$match <- ifelse(display_name_ids$match == FALSE,
                                       grepl(prof_name_first_initials, tolower(display_name_ids$display_name)),
                                       display_name_ids$match)
      
      display_name_ids$match <- ifelse(display_name_ids$match == FALSE,
                                       grepl(prof_name_first_initials_space, tolower(display_name_ids$display_name)),
                                       display_name_ids$match)
      
      # if nothing still, initials without the first name
      display_name_ids$match <- ifelse(display_name_ids$match == FALSE,
                                       grepl(prof_initials_wo_first_name, tolower(display_name_ids$display_name)),
                                       display_name_ids$match)
      
      display_name_ids$match <- ifelse(display_name_ids$match == FALSE,
                                       grepl(prof_initials_wo_first_name_space, tolower(display_name_ids$display_name)),
                                       display_name_ids$match)   
      # and their alternative first name we manually correct for
      display_name_ids$match <- ifelse(display_name_ids$match == FALSE,
                                       grepl(prof_alternative_full_name, tolower(display_name_ids$display_name)),
                                       display_name_ids$match) 
      
      # get the matches
      display_name_ids <- filter(display_name_ids,
                                 match == TRUE)
      
      # extract
      oa_ids_names <- display_name_ids$oa_id
    }
  }
  # bind into a list
  prof_search_output <- list()
  prof_search_output['narcis_id'] <- narcis_id
  prof_search_output['oa_ids_pubs'] <- list(oa_ids_pubs)  
  prof_search_output['oa_ids_orcid'] <- list(oa_ids_orcid)
  prof_search_output['oa_ids_names'] <- list(oa_ids_names)
  return(prof_search_output)
}


## retrieve professor data based on their OpenAlex IDs

professor_pub_info_retriever <- function(prof_oa_ids,
                                         narcis_id,
                                         pub_data,
                                         prof_data){
  
  # get the professor's narcis id and get their publications based on this
  prof_pubs <- filter(pub_data, profile_id == narcis_id)
  
  # get the professor's OA IDs list
  oa_ids <- filter(prof_oa_ids,
                   profile_id == narcis_id)$oa_id
  
  # pull all the data related to these OA ids
  # if ID found:
  if (length(oa_ids)>0){
    # get all papers from OA using these ids:
    prof_all_works_oa <- oa_fetch(
      entity = "works", 
      author.id = oa_ids)
    
    if (!all(is.na(prof_all_works_oa))){
      
      # deduplicate based on IDs
      prof_all_works_oa$duplicate <- duplicated(prof_all_works_oa[, c("id")])
      prof_all_works_oa <- filter(prof_all_works_oa,
                                  duplicate == FALSE)
      
      # paste the OA ID and the Narcis ID
      prof_all_works_oa$oa_id <- paste(oa_ids, collapse = ", ")
      prof_all_works_oa$profile_id <- narcis_id
      
      
      # unnest the authors of works
      prof_all_works_oa_unlist <- unnest(prof_all_works_oa, "author")
      # get the paper list for the profs themselves
      prof_themselves <- filter(prof_all_works_oa_unlist, au_id %in% oa_ids)
      # and a list of their coauthors
      coauthors <- filter(prof_all_works_oa_unlist, ! au_id %in% oa_ids)
      
      
      # unneest the professor's own publication data
      prof_themselves <- unnest(prof_themselves, "counts_by_year", names_sep = "_")
      
      prof_themselves_concepts <- unnest(prof_themselves, "concepts", names_sep = "_")
      
      
      # writing this out to a database
      # professor grants
      if ("grants" %in% colnames(prof_themselves)){
        if (dbExistsTable(con, "oa_prof_grants")){
          # check fields in the existing table
          fields <- dbListFields(con, "oa_prof_grants")
          # if not all fields there
          if(!all(fields %in% colnames(prof_themselves_grants))){
            n_missing <- which(!fields %in% colnames(prof_themselves_grants))
            padding <- data.frame(matrix(NA, ncol = length(n_missing), nrow=1))
            colnames(padding) <- fields[which(!fields %in% colnames(prof_themselves_grants))]
            prof_themselves_grants <- bind_cols(prof_themselves_grants,
                                                padding)
            prof_themselves_grants <- prof_themselves_grants[fields]
          }
          # only leave these fields in
          prof_themselves_grants <-  prof_themselves_grants %>%
            select(all_of(fields))
          dbAppendTable(con, "oa_prof_grants", prof_themselves_grants, row.names=NULL, append=TRUE)
        }else{
          prof_themselves_grants <- prof_themselves_grants %>%
            select(-ids, -referenced_works, -related_works, -duplicate, -concepts)%>%
            filter(!is.na(au_id))
          dbWriteTable(con, "oa_prof_grants", prof_themselves_grants, row.names=FALSE, append=TRUE)
        }
      }
      
      
      # professor publications
      if (dbExistsTable(con, "oa_prof_pubs")){
        # check fields in the existing table
        fields <- dbListFields(con, "oa_prof_pubs")
        # if not all fields there
        if(!all(fields %in% colnames(prof_themselves))){
          n_missing <- which(!fields %in% colnames(prof_themselves))
          padding <- data.frame(matrix(NA, ncol = length(n_missing), nrow=1))
          colnames(padding) <- fields[which(!fields %in% colnames(prof_themselves))]
          prof_themselves <- bind_cols(prof_themselves,
                                       padding)
          prof_themselves <- prof_themselves[fields]
        }
        # only leave these fields in
        prof_themselves <-  prof_themselves %>%
          select(all_of(fields))
        
        dbAppendTable(con, "oa_prof_pubs", prof_themselves, row.names=NULL, append=TRUE)
      }else{
        prof_themselves <- prof_themselves %>%
          select(-ids, -referenced_works, -related_works, -duplicate, -concepts, -grants)%>%
          filter(!is.na(au_id))
        dbWriteTable(con, "oa_prof_pubs", prof_themselves, row.names=FALSE, append=TRUE)
      }
      # professor concepts
      if (dbExistsTable(con, "oa_prof_concepts")){
        # check fields in the existing table
        fields <- dbListFields(con, "oa_prof_concepts")
        # if not all fields there
        if(!all(fields %in% colnames(prof_themselves_concepts))){
          n_missing <- which(!fields %in% colnames(prof_themselves_concepts))
          padding <- data.frame(matrix(NA, ncol = length(n_missing), nrow=1))
          colnames(padding) <- fields[which(!fields %in% colnames(prof_themselves_concepts))]
          prof_themselves_concepts <- bind_cols(prof_themselves_concepts,
                                                padding)
          prof_themselves_concepts <- prof_themselves_concepts[fields]
        }
        # only leave these fields in
        prof_themselves_concepts <-  prof_themselves_concepts %>%
          select(all_of(fields))
        
        dbAppendTable(con, "oa_prof_concepts", prof_themselves_concepts, row.names=NULL, append=TRUE)
      }else{
        prof_themselves_concepts <- prof_themselves_concepts %>%
          select(-ids, -referenced_works, -related_works, -duplicate, -grants)%>%
          filter(!is.na(au_id))
        dbWriteTable(con, "oa_prof_concepts", prof_themselves_concepts, row.names=FALSE, append=TRUE)
      }
      
      # tidy up the dataframe on coauthors, if any info is there
      if (!all(is.na((coauthors)))){
        if (nrow(coauthors) > 0){
          # add the author id info just in case
          coauthors$oa_id <- paste(oa_ids, collapse = ", ")
          coauthors$profile_id <- narcis_id
          
          # professor coauthor info
          if (dbExistsTable(con, "oa_coauthor_info")){
            # check fields in the existing table
            fields <- dbListFields(con, "oa_coauthor_info")
            # if not all fields there
            if(!all(fields %in% colnames(coauthors))){
              n_missing <- which(!fields %in% colnames(coauthors))
              padding <- data.frame(matrix(NA, ncol = length(n_missing), nrow=1))
              colnames(padding) <- fields[which(!fields %in% colnames(coauthors))]
              coauthors <- bind_cols(coauthors,
                                     padding)
              coauthors <- coauthors[fields]
            }
            # only leave these fields in
            coauthors <-  coauthors %>%
              select(all_of(fields))
            
            dbAppendTable(con, "oa_coauthor_info", coauthors, row.names=NULL, append=TRUE)
          }else{
            coauthors <- coauthors %>%
              select(-concepts, -counts_by_year, -ids, 
                     -referenced_works, -related_works, 
                     -duplicate, -grants)%>%
              filter(!is.na(au_id))
            dbWriteTable(con, "oa_coauthor_info", coauthors, row.names=FALSE, append=TRUE)
          }
        }
      }
    }
  }
}


## gender information from genderize.io
genderize_io_function <- function(name,
                                  api_key = "",
                                  country_id = ""){
  # call the api using the name
  name_call <- paste0("https://api.genderize.io?name=", name)
  
  # if country ID provided
  if (country_id != ""){
    name_call <- paste0(name_call, "&country_id=", toupper(country_id))
  }
  # if API key provided
  if (api_key !=""){
    name_call <- paste0(name_call, "&apikey=", api_key, "")
  }
  output <- fromJSON(txt=name_call)
  # if no gender predicted
  if(is.null(output[["gender"]])){
    output[["gender"]] <- NA
  }
  output <- data.frame(output)
  return(output)
}


## altmetric explorer API calling function using the ORCID
altmetric_api_orcid_caller <- function(orcid,
                                       api_secret,
                                       api_key,
                                       endpoint = c("research_outputs",
                                                    "attention",
                                                    "demographics",
                                                    "mentions",
                                                    "mention_sources",
                                                    "journals")){
  if (! endpoint %in% c("research_outputs",
                        "attention",
                        "demographics",
                        "mentions",
                        "mention_sources",
                        "journals")){
    stop('Please select one of the following endpoints: "research_outputs", "attention",
    "demographics", "mentions",  "mention_sources", "journals"')
  }
  
  filter_pt1 <- paste0('orcid|', orcid)
  filter_pt2 <- paste(filter_pt1, "scope|all", sep = "|")
  #filters = 'orcid|0000-0003-0800-5271|scope|all'
  digest <- hmac(api_secret, filter_pt2,
                 algo = c("sha1"),
                 serialize = FALSE)
  
  
  # build up the url by inserting an ORCID
  if (endpoint == "research_outputs"){
    url_pt1a <- paste0("https://www.altmetric.com/explorer/api/research_outputs?digest=", digest)
    url_pt1b <- paste0(url_pt1a, "&filter%5Borcid%5D=")
    url_pt1 <- paste0(url_pt1b, orcid)
  } else{
    # for other endpoints, specify the end point
    url_pt1a <- "https://www.altmetric.com/explorer/api/research_outputs/"
    url_pt1b <- paste0(paste0(url_pt1a, endpoint), "?digest=")
    url_pt1c <- paste0(url_pt1b, digest)
    url_pt1d <- paste0(url_pt1c, "&filter%5Borcid%5D=")
    url_pt1 <- paste0(url_pt1d, orcid)
  }
  # first, get the first page with a 100 results
  url_pt2 <- paste0(paste0("&filter%5Bscope%5D=all&key=", api_key), "&page%5Bnumber%5D=1")
  
  
  url_build <- paste0(url_pt1, url_pt2)
  
  # fetch the output
  output <- fromJSON(txt=url_build)
  
  # fetch the dataframe we need
  dataframe_output <- output$data
  
  # get the total number of results
  results_total <- output$meta$response$`total-results`
  # since we get results by a 100, get the number of calls we need (minus the one we made)
  calls_remaining <- ceiling(results_total/25)-1
  
  # if more calls remaining, call the api again, getting the correponding pages:
  if (calls_remaining > 0){
    for (call in seq_len(calls_remaining)){
      # build the url for each next page
      page <- call + 1
      url_pages <- paste0(paste0("&filter%5Bscope%5D=all&key=", api_key), "&page%5Bnumber%5D=")
      url_pages <- paste0(url_pages, page)
      url_build <- paste0(url_pt1, url_pages)
      page_output <- fromJSON(txt=url_build)
      dataframe_page_output <- page_output$data
      
      dataframe_output <- dataframe_output %>%
        dplyr::bind_rows(dataframe_page_output)
    }
  }
  return(dataframe_output)
}


## Altmetric details API Retriever function (publication list with DOIs as input)

altmetric_mention_retriever <- function(api_key,
                                        doi,
                                        include_twitter = FALSE){
  
  # elements to make a url that calls the API to get mention data
  doi_call_url <- "https://api.altmetric.com/v1/fetch/doi/"
  api_key_url <- paste0("?key=", api_key)
  exclude_twitter_url <- "&exclude_sources=twitter"
  # get the doi without the url part
  doi_url <- str_remove(doi$doi[1], "https://doi.org/")
  # work id
  work_id <- doi$id[1]
  # if there is a doi:
  if (!is.na(doi_url)){
    # make the url for the api call
    api_call <- paste(doi_call_url, doi_url, api_key_url, sep = "")
    
    # exclude twitter? if so, add:
    if (include_twitter == FALSE){
      api_call <- paste(api_call, exclude_twitter_url, sep = "")
    }
    # empty DOI output object, in case try fails
    doi_output <- NA
    # and call the api
    try(doi_output <- fromJSON(txt=api_call), silent = TRUE)
    # now, check if this doi has any mentions in the media
    if ("posts" %in% names(doi_output)){
      mentions <- doi_output[["posts"]]
      if (length(mentions)>0){
        # if news:
        if ("news" %in% names(mentions)){
          news <- mentions[["news"]]
          news <- unnest(news, author, names_sep = "_")
          news$id <- work_id
          if (dbExistsTable(con, "altmetric_pub_att_news")){
            # check fields in the existing table
            fields <- dbListFields(con, "altmetric_pub_att_news")
            # if not all fields there
            if(!all(fields %in% colnames(news))){
              n_missing <- which(!fields %in% colnames(news))
              padding <- data.frame(matrix(NA, ncol = length(n_missing), nrow=1))
              colnames(padding) <- fields[which(!fields %in% colnames(news))]
              news <- bind_cols(news,
                                padding)
              news <- news[fields]
            }
            # only leave these fields in
            news <-  news %>%
              select(all_of(fields))
            dbAppendTable(con, "altmetric_pub_att_news", news, row.names=NULL, append=TRUE)
          }else{
            news <- news %>%
              select(-citation_ids)
            dbWriteTable(con, "altmetric_pub_att_news", news, row.names=FALSE, append=TRUE)
          }
        }
        
        if ("wikipedia" %in% names(mentions)){
          wiki <- mentions[["wikipedia"]]
          wiki <- unnest(wiki, author, names_sep = "_")
          wiki$id <- work_id
          
          if (dbExistsTable(con, "altmetric_pub_att_wiki")){
            # check fields in the existing table
            fields <- dbListFields(con, "altmetric_pub_att_wiki")
            # if not all fields there
            if(!all(fields %in% colnames(wiki))){
              n_missing <- which(!fields %in% colnames(wiki))
              padding <- data.frame(matrix(NA, ncol = length(n_missing), nrow=1))
              colnames(padding) <- fields[which(!fields %in% colnames(wiki))]
              wiki <- bind_cols(wiki,
                                padding)
              wiki <- wiki[fields]
            }
            # only leave these fields in
            wiki <-  wiki %>%
              select(all_of(fields))
            dbAppendTable(con, "altmetric_pub_att_wiki", wiki, row.names=NULL, append=TRUE)
          }else{
            wiki <-  wiki %>%
              select(-citation_ids)
            dbWriteTable(con, "altmetric_pub_att_wiki", wiki, row.names=FALSE, append=TRUE)
          }
        }
        
        if ("reddit" %in% names(mentions)){
          reddit <- mentions[["reddit"]]
          reddit <- unnest(reddit, author, names_sep = "_")
          reddit$id <- work_id
          
          if (dbExistsTable(con, "altmetric_pub_att_reddit")){
            # check fields in the existing table
            fields <- dbListFields(con, "altmetric_pub_att_reddit")
            # if not all fields there
            if(!all(fields %in% colnames(reddit))){
              n_missing <- which(!fields %in% colnames(reddit))
              padding <- data.frame(matrix(NA, ncol = length(n_missing), nrow=1))
              colnames(padding) <- fields[which(!fields %in% colnames(reddit))]
              reddit <- bind_cols(reddit,
                                  padding)
              reddit <- reddit[fields]
            }
            # only leave these fields in
            reddit <-  reddit %>%
              select(all_of(fields))
            dbAppendTable(con, "altmetric_pub_att_reddit", reddit, row.names=NULL, append=TRUE)
          }else{
            reddit <-  reddit %>%
              select(-citation_ids)
            dbWriteTable(con, "altmetric_pub_att_reddit", reddit, row.names=FALSE, append=TRUE)
          }
        }
        
        if ("blogs" %in% names(mentions)){
          blogs <- mentions[["blogs"]]
          blogs <- unnest(blogs, author, names_sep = "_")
          blogs$id <- work_id
          
          if (dbExistsTable(con, "altmetric_pub_att_blogs")){
            # check fields in the existing table
            fields <- dbListFields(con, "altmetric_pub_att_blogs")
            # if not all fields there
            if(!all(fields %in% colnames(blogs))){
              n_missing <- which(!fields %in% colnames(blogs))
              padding <- data.frame(matrix(NA, ncol = length(n_missing), nrow=1))
              colnames(padding) <- fields[which(!fields %in% colnames(blogs))]
              blogs <- bind_cols(blogs,
                                 padding)
              blogs <- blogs[fields]
            }
            # only leave these fields in
            blogs <-  blogs %>%
              select(all_of(fields))
            dbAppendTable(con, "altmetric_pub_att_blogs", blogs, row.names=NULL, append=TRUE)
          }else{
            blogs <- blogs %>%
              select(-citation_ids)
            dbWriteTable(con, "altmetric_pub_att_blogs", blogs, row.names=FALSE, append=TRUE)
          }
        }
        
        if ("policy" %in% names(mentions)){
          policy <- mentions[["policy"]]
          policy <- unnest(policy, source)
          policy$id <- work_id
          if ("author" %in% colnames(policy)){
            policy <- unnest(policy, author)
          }
          
          if (dbExistsTable(con, "altmetric_pub_att_policy")){
            # check fields in the existing table
            fields <- dbListFields(con, "altmetric_pub_att_policy")
            # if not all fields there
            if(!all(fields %in% colnames(policy))){
              n_missing <- which(!fields %in% colnames(policy))
              padding <- data.frame(matrix(NA, ncol = length(n_missing), nrow=1))
              colnames(padding) <- fields[which(!fields %in% colnames(policy))]
              policy <- bind_cols(policy,
                                  padding)
              policy <- policy[fields]
            }
            # only leave these fields in
            policy <-  policy %>%
              select(all_of(fields))
            dbAppendTable(con, "altmetric_pub_att_policy", policy, row.names=NULL, append=TRUE)
          }else{
            policy <- policy %>%
              select(-citation_ids, -collections)
            dbWriteTable(con, "altmetric_pub_att_policy", policy, row.names=FALSE, append=TRUE)
          }
        }
        
        if ("twitter" %in% names(mentions)){
          twitter <- mentions[["twitter"]]
          twitter <- unnest(twitter, author)
          twitter$id <- work_id
          if (dbExistsTable(con, "altmetric_pub_att_twitter")){
            # check fields in the existing table
            fields <- dbListFields(con, "altmetric_pub_att_twitter")
            # if not all fields there
            if(!all(fields %in% colnames(twitter))){
              n_missing <- which(!fields %in% colnames(twitter))
              padding <- data.frame(matrix(NA, ncol = length(n_missing), nrow=1))
              colnames(padding) <- fields[which(!fields %in% colnames(twitter))]
              twitter <- bind_cols(twitter,
                                   padding)
              twitter <- twitter[fields]
            }
            # only leave these fields in
            twitter <-  twitter %>%
              select(all_of(fields))
            dbAppendTable(con, "altmetric_pub_att_twitter", twitter, row.names=NULL, append=TRUE)
          }else{
            twitter <- twitter %>%
              select(-citation_ids)
            dbWriteTable(con, "altmetric_pub_att_twitter", twitter, row.names=FALSE, append=TRUE)
          }
        }
      }
    }
  }
}


##  The same function, but for robustness checks
altmetric_mention_retriever_rob <- function(api_key,
                                            doi,
                                            include_twitter = FALSE){
  
  # elements to make a url that calls the API to get mention data
  doi_call_url <- "https://api.altmetric.com/v1/fetch/doi/"
  api_key_url <- paste0("?key=", api_key)
  exclude_twitter_url <- "&exclude_sources=twitter"
  # get the doi without the url part
  doi_url <- str_remove(doi$doi[1], "https://doi.org/")
  # work id
  work_id <- doi$id[1]
  # if there is a doi:
  if (!is.na(doi_url)){
    # make the url for the api call
    api_call <- paste(doi_call_url, doi_url, api_key_url, sep = "")
    
    # exclude twitter? if so, add:
    if (include_twitter == FALSE){
      api_call <- paste(api_call, exclude_twitter_url, sep = "")
    }
    # empty DOI output object, in case try fails
    doi_output <- NA
    # and call the api
    try(doi_output <- fromJSON(txt=api_call), silent = TRUE)
    # now, check if this doi has any mentions in the media
    if ("posts" %in% names(doi_output)){
      mentions <- doi_output[["posts"]]
      if (length(mentions)>0){
        # if news:
        if ("news" %in% names(mentions)){
          news <- mentions[["news"]]
          news <- unnest(news, author, names_sep = "_")
          news$id <- work_id
          if (dbExistsTable(con, "rob_altmetric_pub_att_news")){
            # check fields in the existing table
            fields <- dbListFields(con, "rob_altmetric_pub_att_news")
            # if not all fields there
            if(!all(fields %in% colnames(news))){
              n_missing <- which(!fields %in% colnames(news))
              padding <- data.frame(matrix(NA, ncol = length(n_missing), nrow=1))
              colnames(padding) <- fields[which(!fields %in% colnames(news))]
              news <- bind_cols(news,
                                padding)
              news <- news[fields]
            }
            # only leave these fields in
            news <-  news %>%
              select(all_of(fields))
            dbAppendTable(con, "rob_altmetric_pub_att_news", news, row.names=NULL, append=TRUE)
          }else{
            news <- news %>%
              select(-citation_ids)
            dbWriteTable(con, "rob_altmetric_pub_att_news", news, row.names=FALSE, append=TRUE)
          }
        }
        
        if ("blogs" %in% names(mentions)){
          blogs <- mentions[["blogs"]]
          blogs <- unnest(blogs, author, names_sep = "_")
          blogs$id <- work_id
          
          if (dbExistsTable(con, "rob_altmetric_pub_att_blogs")){
            # check fields in the existing table
            fields <- dbListFields(con, "rob_altmetric_pub_att_blogs")
            # if not all fields there
            if(!all(fields %in% colnames(blogs))){
              n_missing <- which(!fields %in% colnames(blogs))
              padding <- data.frame(matrix(NA, ncol = length(n_missing), nrow=1))
              colnames(padding) <- fields[which(!fields %in% colnames(blogs))]
              blogs <- bind_cols(blogs,
                                 padding)
              blogs <- blogs[fields]
            }
            # only leave these fields in
            blogs <-  blogs %>%
              select(all_of(fields))
            dbAppendTable(con, "rob_altmetric_pub_att_blogs", blogs, row.names=NULL, append=TRUE)
          }else{
            blogs <- blogs %>%
              select(-citation_ids)
            dbWriteTable(con, "rob_altmetric_pub_att_blogs", blogs, row.names=FALSE, append=TRUE)
          }
        }
      }
    }
  }
}


## Altmetric details API retriever - only for tweets (publication list with DOIs as input)
altmetric_twitter_retriever <- function(api_key,
                                        publication_list){
  
  # elements to make a url that calls the API to get mention data
  doi_call_url <- "https://api.altmetric.com/v1/fetch/doi/"
  api_key_url <- paste0("?key=", api_key)
  
  # get the publication list
  oa_pubs <- publication_list
  # generate a dataframe to hold the output
  twitter_attention_df <- data.frame(matrix(NA, ncol = 5, nrow = 0))
  colnames(twitter_attention_df) <- c("id",  "license",
                                      "citation_ids", "tweeter_id", "tweet_id")
  
  # if not empty:
  if (!all(is.na(oa_pubs))){
    # loop, for each doi, and:
    for (i in 1:nrow(oa_pubs)){
      # get the doi without the url part
      doi_url <- str_remove(oa_pubs$doi[i], "https://doi.org/")
      # if there is a doi:
      if (!is.na(doi_url)){
        # make the url for the api call
        api_call <- paste(doi_call_url, doi_url, api_key_url, "&include_sources=twitter", sep="") 
        
        # empty DOI output object, in case try fails
        doi_output <- NA
        # and call the api
        try(doi_output <- fromJSON(txt=api_call), silent = TRUE)
        # now, check if this doi has any mentions in the media
        if(!all(is.na(doi_output))){
          if ("posts" %in% names(doi_output)){
            if ("twitter" %in% names(doi_output[["posts"]])){
              tweet_info <- doi_output[["posts"]][["twitter"]]
              tweet_info <- unnest(tweet_info, cols = c(citation_ids, author))
              # combine this with relevant publication info
              pub_info_combi <- data.frame(oa_pubs[i, c("id")])
              colnames(pub_info_combi) <- "id"
              tweet_info_full <- bind_cols(pub_info_combi, tweet_info)
              
              # check if all relevant columns there, if not, pad
              if(!all(colnames(twitter_attention_df) %in% colnames(tweet_info_full))){
                n_missing <- which(! colnames(twitter_attention_df) %in% colnames(tweet_info_full))
                padding <- data.frame(matrix(NA, ncol = length(n_missing), nrow=1))
                colnames(padding) <- colnames(twitter_attention_df)[which(!colnames(twitter_attention_df) %in% colnames(tweet_info_full))]
                tweet_info_full <- bind_cols(tweet_info_full,
                                             padding)
                tweet_info_full <- tweet_info_full[colnames(twitter_attention_df)]
              }
              
              
              twitter_attention_df <- rbind(twitter_attention_df,
                                            tweet_info_full)
            }
          }
        }
      }
    }
    # return the list of mentions dataframe
    return(twitter_attention_df)
  }
}


## Compile coauthor panel and append it to the author panel per profile_id:
coauthor_data_compiler <- function(profile_id,
                                   prof_panel,
                                   prof_coauthor_matching,
                                   prof_coauthor_panel,
                                   prof_coauthor_info_w_gender){
  
  current_profile_id <- profile_id
  # get info on this prof
  prof_panel <- filter(prof_panel,
                       profile_id == current_profile_id)
  
  # get into on this prof's coauthors only for the years in which they coauthored
  prof_coauthors <- filter(prof_coauthor_matching, 
                           profile_id == current_profile_id & publication_year >= 1974)
  # now, extract the statistics on these coauthors from out coauthor panel
  prof_coauthor_data <- filter(prof_coauthor_panel,
                               id %in% prof_coauthors$au_id)
  
  # and select the relevant coauthors to check gender composition per paper
  prof_coauthors_oa_gender <- filter(prof_coauthor_info_w_gender,
                                     profile_id == current_profile_id)
  
  # get coauthor_d + year variable to filter on
  prof_coauthors$au_year <- paste0(prof_coauthors$au_id, "_", prof_coauthors$publication_year)
  prof_coauthor_data$au_year <- paste0(prof_coauthor_data$id, "_", prof_coauthor_data$year)
  
  prof_coauthor_data_rel <- filter(prof_coauthor_data,
                                   au_year %in% prof_coauthors$au_year)
  
  # get yearly professor coauthor data breakdown
  prof_coauthor_data_year <- prof_coauthor_data_rel %>%
    group_by(year)%>%
    summarise_at(vars(count_pubs:coa_attn_twitter_by_total), sum, na.rm = TRUE)
  
  prof_coauthor_data_rel$gender <- ifelse(is.na(prof_coauthor_data_rel$gender), 
                                          "unknown", 
                                          prof_coauthor_data_rel$gender)
  # gender breakdown
  prof_coauthor_gender_breakdown <- prof_coauthor_data_rel %>%
    group_by(gender, year)%>%
    summarise(n = n())%>%
    pivot_wider(names_from = gender, values_from = n)%>%
    replace(is.na(.), 0)
  
  if (!"unknown" %in% colnames(prof_coauthor_gender_breakdown)){
    prof_coauthor_gender_breakdown$unknown <- 0
  }
  
  if (!"m" %in% colnames(prof_coauthor_gender_breakdown)){
    prof_coauthor_gender_breakdown$m <- 0
  }
  
  if (!"w" %in% colnames(prof_coauthor_gender_breakdown)){
    prof_coauthor_gender_breakdown$w <- 0
  }
  
  prof_coauthor_gender_breakdown <- prof_coauthor_gender_breakdown[c("year", "m", "unknown", "w")]
  
  colnames(prof_coauthor_gender_breakdown)[which(colnames(prof_coauthor_gender_breakdown) == "m")] <- "coa_m"
  colnames(prof_coauthor_gender_breakdown)[which(colnames(prof_coauthor_gender_breakdown) == "w")] <- "coa_w"
  colnames(prof_coauthor_gender_breakdown)[which(colnames(prof_coauthor_gender_breakdown) == "unknown")] <- "coa_u"
  
  # combined coauthor yearly summary
  prof_coauthor_data_year <- merge(prof_coauthor_data_year,
                                   prof_coauthor_gender_breakdown,
                                   by = "year")
  
  # tidy up the colnames
  colnames(prof_coauthor_data_year)[2:9] <- paste0("coa_", colnames(prof_coauthor_data_year)[2:9])
  
  # and get a lagged equivalent
  prof_coauthor_data_year_lag <- prof_coauthor_data_year %>%
    arrange(year)%>%
    mutate_at(vars(contains('coa_')), lag)
  
  colnames(prof_coauthor_data_year_lag)[-1] <- paste0(colnames(prof_coauthor_data_year)[-1], "_l")
  
  # merge the two together and
  # get cumulative info on coauthors for the professor in question 
  prof_coauthor_data_all <- merge(prof_coauthor_data_year,
                                  prof_coauthor_data_year_lag,
                                  by = "year")
  
  # if we want cumulative, we first need to remove duplicate coauthor counts, keeping 
  # only the most recent
  years <- unique(prof_coauthor_data_rel$year)
  colnames_cumulative <- c("prof_tot_count_pubs",
                           "prof_tot_cited_by",
                           "prof_tot_count_pubs_total_oa",
                           "prof_tot_cited_by_total_oa",
                           "prof_tot_cited_by_before_2012",
                           "prof_tot_count_pubs_before_2012",
                           "prof_tot_cited_by_total_all",
                           "prof_tot_count_pubs_total_all",     
                           "prof_tot_coa_attn_twitter_by",
                           "prof_tot_coa_attn_news_by",
                           "prof_tot_coa_attn_blog_by",
                           "prof_tot_coa_attn_news_by_total",
                           "prof_tot_coa_attn_blog_by_total",
                           "prof_tot_coa_attn_twitter_by_total",
                           "prof_tot_unique_coa_m",
                           "prof_tot_unique_coa_u",
                           "prof_tot_unique_coa_w")
  
  colnames_cumulative_all <- c("year", "prof_tot_count_pubs", "prof_tot_cited_by", 
                               "prof_tot_count_pubs_total_oa", "prof_tot_cited_by_total_oa", 
                               "prof_tot_cited_by_before_2012", "prof_tot_count_pubs_before_2012",
                               "prof_tot_cited_by_total_all",  "prof_tot_count_pubs_total_all", 
                               "prof_tot_coa_attn_twitter_by",  "prof_tot_coa_attn_news_by", 
                               "prof_tot_coa_attn_blog_by", "prof_tot_coa_attn_news_by_total", 
                               "prof_tot_coa_attn_blog_by_total", "prof_tot_coa_attn_twitter_by_total",
                               "prof_tot_unique_coa_m",
                               "prof_tot_unique_coa_u",
                               "prof_tot_unique_coa_w",
                               "prof_tot_count_pubs_l", "prof_tot_cited_by_l", "prof_tot_count_pubs_total_oa_l",
                               "prof_tot_cited_by_total_oa_l",  "prof_tot_cited_by_before_2012_l", 
                               "prof_tot_count_pubs_before_2012_l", "prof_tot_cited_by_total_all_l", 
                               "prof_tot_count_pubs_total_all_l", "prof_tot_coa_attn_twitter_by_l", 
                               "prof_tot_coa_attn_news_by_l",   "prof_tot_coa_attn_blog_by_l",   
                               "prof_tot_coa_attn_news_by_total_l", "prof_tot_coa_attn_blog_by_total_l",
                               "prof_tot_coa_attn_twitter_by_total_l",
                               "prof_tot_unique_coa_m_l",
                               "prof_tot_unique_coa_u_l",
                               "prof_tot_unique_coa_w_l",
                               "share_w_coa_all",
                               "share_w_coa_known",
                               "share_w_coa_all_l",
                               "share_w_coa_known_l")
  
  prof_coauthor_cumulative_data_all <- as.data.frame(matrix(NA, ncol = length(colnames_cumulative_all), nrow = 0))
  colnames(prof_coauthor_cumulative_data_all) <- colnames_cumulative_all
  
  # loop  through the years to accumulate this
  # additionally, get the average gender composition
  # of author's papers in that year
  for (year_in_question in years){
    prof_coauthor_data_year <- filter(prof_coauthor_data_rel,
                                      year <= year_in_question)
    
    if (nrow(prof_coauthor_data_year) > 0){
      
      prof_coauthor_data_year <- prof_coauthor_data_year %>%
        group_by(id)%>%
        slice(which.max(year))%>%
        ungroup()
      
      cumulative_year <- prof_coauthor_data_year %>%
        select(-year)%>%
        summarise(across(where(is.numeric), \(x) sum(x, na.rm = T), .names = "prof_tot_{.col}"))
      
      cumulative_year_gender <- prof_coauthor_data_year %>%
        group_by(gender)%>%
        summarise(n = n())%>%
        pivot_wider(names_from = gender, values_from = n)%>%
        replace(is.na(.), 0)
      
      if(! "unknown" %in% colnames(cumulative_year_gender)){
        cumulative_year_gender$unknown <- 0
      }
      if(! "m" %in% colnames(cumulative_year_gender)){
        cumulative_year_gender$m <- 0
      }
      if(! "w" %in% colnames(cumulative_year_gender)){
        cumulative_year_gender$w <- 0
      }
      cumulative_year_gender <- cumulative_year_gender[c("m", "unknown", "w")]
      colnames(cumulative_year_gender) <- c("m", "u", "w")
      
      colnames(cumulative_year_gender) <- paste0("prof_tot_unique_coa_", colnames(cumulative_year_gender))
      
      cumulative_year$year <- year_in_question
      
      cumulative_year <- cbind(cumulative_year,
                               cumulative_year_gender)
      
      cumulative_year <- cumulative_year %>% relocate(year)
      
      prof_coauthor_data_year_lag <- filter(prof_coauthor_data_rel,
                                            year <= year_in_question - 1)
    } else {
      cumulative_year <- as.data.frame(matrix(0, ncol = length(colnames_cumulative), nrow = 1))
      colnames(cumulative_year) <- colnames_cumulative
      cumulative_year$year <- year_in_question
    }
    
    prof_coauthor_data_year_lag <- filter(prof_coauthor_data_rel,
                                          year <= year_in_question - 1)
    
    if (nrow(prof_coauthor_data_year_lag) > 0){
      prof_coauthor_data_year_lag <- prof_coauthor_data_year_lag %>%
        group_by(id)%>%
        slice(which.max(year))%>%
        ungroup()
      
      cumulative_year_lag <- prof_coauthor_data_year_lag %>%
        select(-year)%>%
        summarise(across(where(is.numeric), \(x) sum(x, na.rm = T), .names = "prof_tot_{.col}_l"))
      
      cumulative_year_gender_lag <- prof_coauthor_data_year_lag %>%
        group_by(gender)%>%
        summarise(n = n())%>%
        pivot_wider(names_from = gender, values_from = n)%>%
        replace(is.na(.), 0)
      
      if(! "unknown" %in% colnames(cumulative_year_gender_lag)){
        cumulative_year_gender_lag$unknown <- 0
      }
      if(! "m" %in% colnames(cumulative_year_gender_lag)){
        cumulative_year_gender_lag$m <- 0
      }
      if(! "w" %in% colnames(cumulative_year_gender_lag)){
        cumulative_year_gender_lag$w <- 0
      }
      cumulative_year_gender_lag <- cumulative_year_gender_lag[c("m", "unknown", "w")]
      colnames(cumulative_year_gender_lag) <- paste0(c("m", "u", "w"), "_l")
      colnames(cumulative_year_gender_lag) <- paste0("prof_tot_unique_coa_", colnames(cumulative_year_gender_lag))
      
      cumulative_year_lag$year <- year_in_question
      
      cumulative_year_lag <- cbind(cumulative_year_lag,
                                   cumulative_year_gender_lag)
      
    }else{
      cumulative_year_lag <- as.data.frame(matrix(NA, ncol = length(colnames_cumulative), nrow = 1))
      colnames(cumulative_year_lag) <- paste0(colnames_cumulative, "_l")
      cumulative_year_lag$year <- year_in_question
    }
    
    # merge cumulatives and lagged cumulatives
    cumulative_all <- merge(cumulative_year,
                            cumulative_year_lag,
                            by = "year")
    
    # add in gender composition
    
    ## gender composition
    gender_composition_paper <- prof_coauthors_oa_gender %>%
      filter(publication_year == year_in_question)%>%
      group_by(profile_id, id)%>%
      summarise(n_w = sum(inferred_gender == "w"),
                n_m = sum(inferred_gender == "m"),
                n_u = sum(inferred_gender == "unknown"))%>%
      mutate(share_w_all = n_w / (n_w + n_m + n_u),
             share_w_known = n_w / (n_w + n_m))%>%
      select(profile_id, id, share_w_all, share_w_known)%>%
      group_by(profile_id)%>%
      reframe(share_w_coa_all = round(mean(share_w_all, na.rm = TRUE), 3),
              share_w_coa_known = round(mean(share_w_known, na.rm = TRUE), 3))
    
    if (nrow(gender_composition_paper) == 0){
      gender_composition_paper <- cbind.data.frame(profile_id = current_profile_id,
                                                   share_w_coa_all = NA,
                                                   share_w_coa_known = NA)
    }
    
    ## gender composition lagged
    gender_composition_paper_lag <- prof_coauthors_oa_gender %>%
      filter(publication_year == year_in_question - 1)%>%
      group_by(profile_id, id)%>%
      summarise(n_w = sum(inferred_gender == "w"),
                n_m = sum(inferred_gender == "m"),
                n_u = sum(inferred_gender == "unknown"))%>%
      mutate(share_w_all = n_w / (n_w + n_m + n_u),
             share_w_known = n_w / (n_w + n_m))%>%
      select(profile_id, id, share_w_all, share_w_known)%>%
      group_by(profile_id)%>%
      summarise(share_w_coa_all = round(mean(share_w_all, na.rm = TRUE), 3),
                share_w_coa_known = round(mean(share_w_known, na.rm = TRUE), 3))
    
    if (nrow(gender_composition_paper_lag) == 0){
      gender_composition_paper_lag <- cbind.data.frame(profile_id = current_profile_id,
                                                       share_w_coa_all = NA,
                                                       share_w_coa_known = NA)
    }
    
    colnames(gender_composition_paper_lag) <- paste0(colnames(gender_composition_paper_lag), "_l")
    
    ## gender composition combined
    gender_composition_prof_year <- cbind(gender_composition_paper,
                                          gender_composition_paper_lag[, -1])
    
    cumulative_all <- cbind(cumulative_all,
                            gender_composition_prof_year[, -1])
    
    
    cumulative_all <- cumulative_all %>%
      relocate(year)
    
    prof_coauthor_cumulative_data_all <- rbind.data.frame(prof_coauthor_cumulative_data_all,
                                                          cumulative_all)
    
    
  }
  
  
  
  # merge prof info with coauthor data
  prof_panel_merge <- merge(prof_panel,
                            prof_coauthor_data_all,
                            by = "year",
                            all.x = TRUE)
  
  prof_panel_merge <- merge(prof_panel_merge,
                            prof_coauthor_cumulative_data_all,
                            by = "year",
                            all.x = TRUE)
  
  return(prof_panel_merge)
}


# Regression analysis functions

# this function takes individual field datasets,
# and estimates the linear model, gets the predicted values per gender,
# and then compares the predictions between genders


# the same, but with scopus fields
lm_fitter_cl_robust_scopus <- function(panel_dataset,
                                       lm_formula_list,
                                       year_cutoff_upper = 2023,
                                       year_cutoff_lower = 1973){
  
  all_predictions <- data.frame(matrix(ncol = 7, nrow = 0))
  all_models <- data.frame(matrix(ncol = 10, nrow = 0))
  all_comparisons <- data.frame(matrix(ncol = 6, nrow = 0))
  
  fields <- c("phys", "life", "health", "soc_sci", "arts")
  life <- filter(prof_panel_filter, overall_adj_domain == "Life Sciences")
  phys <- filter(prof_panel_filter, overall_adj_domain == "Physical Sciences")
  soc_sci <- filter(prof_panel_filter, overall_adj_domain == "Social Sciences")
  arts <- filter(prof_panel_filter, overall_adj_domain == "Arts and Humanities")
  health <- filter(prof_panel_filter, overall_adj_domain == "Health Sciences")
  
  for (field in fields){
    #print(field)
    lm_model <- NA
    
    field_dataset <- get(field)
    
    field_dataset <- filter(field_dataset,
                            year <= year_cutoff_upper & year >= year_cutoff_lower)
    
    for (lm_formula in lm_formula_list){
      
      lm_model <- lm(lm_formula,
                     data = field_dataset) 
      
      # cluster-robust SEs
      coef_test_output <- coeftest(lm(lm_formula,
                                      data = field_dataset), vcov = vcovCL, cluster = field_dataset$profile_id)
      model_result <- coef_test_output[,] %>% 
        as.data.frame() %>% 
        rownames_to_column(var = "term")
      model_result$lower_ci <- as.data.frame(confint(coef_test_output,
                                                     level = 0.95))[,1]
      model_result$upper_ci <- as.data.frame(confint(coef_test_output,
                                                     level = 0.95))[,2]
      
      # extracting R squared
      model_result[nrow(model_result)+1, 1] <- "R^2"
      model_result[nrow(model_result), 2:ncol(model_result)] <- round(summary(lm_model)$r.squared,3)
      model_result$field <- field
      model_result$covariate <- trimws(str_split_i(lm_formula, "~", 1))
      # binding field models together
      all_models <- rbind(all_models,
                          model_result)
      # prediction for our plots
      prediction <- predict_response(lm_model, c("inferred_gender"), vcov_fun = "vcovCL", 
                                     vcov_type = "HC0",
                                     vcov.args = list(cluster = field_dataset$profile_id))
      # check if we get CIs for our prediction
      if(!"conf.low" %in% colnames(prediction)){
        # if not, pad this with NA and CIs identical to the prediction
        prediction$std.error <- NA
        prediction$conf.low <- prediction$predicted
        prediction$conf.high <- prediction$predicted
        prediction <- prediction[,c("x", "predicted", "std.error", "conf.low", "conf.high", "group" )]
      }
      
      prediction$field <- field
      
      
      # binding the predictions for all fields together
      prediction$covariate <- trimws(str_split_i(lm_formula, "~", 1))
      all_predictions <- rbind(all_predictions,
                               prediction)
      
      # comparing whether the predictions for men and women are significantly different
      gender_comparison <- test_predictions(lm_model, c("inferred_gender"), vcov_fun = "vcovCL")
      gender_comparison$field <- field
      gender_comparison$covariate <- trimws(str_split_i(lm_formula, "~", 1))
      
      all_comparisons <- rbind(all_comparisons,
                               gender_comparison)
      
      
      
      
      #print(field)
      
      
    }
  }
  # adding some stars
  all_models$stars <- ifelse(all_models$`Pr(>|t|)` <= 0.001, "***",
                             ifelse(all_models$`Pr(>|t|)` <= 0.001, "**",
                                    ifelse(all_models$`Pr(>|t|)` <= 0.05, "*",
                                           ifelse(all_models$`Pr(>|t|)` <= 0.1, ".", ""))))
  
  all_comparisons$stars <- ifelse(all_comparisons$`p.value` <= 0.001, "***",
                                  ifelse(all_comparisons$`p.value` <= 0.001, "**",
                                         ifelse(all_comparisons$`p.value` <= 0.05, "*",
                                                ifelse(all_comparisons$`p.value` <= 0.1, ".", ""))))
  
  # tidying up and rounding the results
  all_comparisons$group1 <- str_split_i(all_comparisons$inferred_gender, "-", 1)
  all_comparisons$group2 <- str_split_i(all_comparisons$inferred_gender, "-", 2)
  all_comparisons$p_rounded <- round(all_comparisons$p.value, 4)
  
  
  all_models$field <- factor(all_models$field,
                             levels = c("phys",
                                        "life",
                                        "health",
                                        "soc_sci",
                                        "arts"))
  
  
  all_predictions$field <- factor(all_predictions$field,
                                  levels = c("phys",
                                             "life",
                                             "health",
                                             "soc_sci",
                                             "arts"))
  
  all_comparisons$field <- factor(all_comparisons$field,
                                  levels = c("phys",
                                             "life",
                                             "health",
                                             "soc_sci",
                                             "arts"))
  
  
  
  output_list <- list(all_models,
                      all_predictions,
                      all_comparisons)
  
  return(output_list)
}


## Function that can fit a poisson or a logistic regression

glm_fitter_cl_robust_scopus <- function(panel_dataset,
                                 formula_list,
                                 year_cutoff_upper = 2023,
                                 year_cutoff_lower = 1974,
                                 reg_family = c("poisson", "binomial")){
  
  all_models <- data.frame(matrix(ncol = 10, nrow = 0))
  
  fields <- c("phys", "life", "health", "soc_sci", "arts")
  life <- filter(prof_panel_filter, overall_adj_domain == "Life Sciences")
  phys <- filter(prof_panel_filter, overall_adj_domain == "Physical Sciences")
  soc_sci <- filter(prof_panel_filter, overall_adj_domain == "Social Sciences")
  arts <- filter(prof_panel_filter, overall_adj_domain == "Arts and Humanities")
  health <- filter(prof_panel_filter, overall_adj_domain == "Health Sciences")
  
  for (field in fields){
    #print(field)
    reg_model <- NA
    coef_test_output <- NA
    
    field_dataset <- get(field)
    
    field_dataset <- filter(field_dataset,
                            year <= year_cutoff_upper & year >= year_cutoff_lower)
    
    for (model_formula in formula_list){
      #print(model_formula)
      
      if (reg_family == "poisson"){
        
        reg_model <- glm(model_formula,
                         data = field_dataset,
                         family = 'poisson') 
        
        # cluster-robust SEs
        
        coef_test_output <- coeftest(reg_model, vcov = vcovCL, cluster = reg_model$data$profile_id)
        model_result <- coef_test_output[,] %>% 
          as.data.frame() %>% 
          rownames_to_column(var = "term")
        model_result$lower_ci <- as.data.frame(confint(coef_test_output,
                                                       level = 0.95))[,1]
        model_result$upper_ci <- as.data.frame(confint(coef_test_output,
                                                       level = 0.95))[,2]
      }
      
      if (reg_family == "binomial"){
        
        reg_model <- glm(model_formula,
                         data = field_dataset,
                         family = 'binomial') 
        
        coef_test_output <- coeftest(reg_model, type = "HC0", cluster = reg_model$data$profile_id)
        model_result <- coef_test_output[,] %>% 
          as.data.frame() %>% 
          rownames_to_column(var = "term")
        model_result$lower_ci <- as.data.frame(confint(coef_test_output,
                                                       level = 0.95))[,1]
        model_result$upper_ci <- as.data.frame(confint(coef_test_output,
                                                       level = 0.95))[,2]
        
        model_result[nrow(model_result)+1, 1] <- "R^2"
        pseudo_r <- "no convergence"
        try(pseudo_r <- round(PseudoR2(reg_model),3))
        model_result[nrow(model_result), 2:ncol(model_result)] <- pseudo_r
        
      }
      
      # tidy up
      model_result$field <- field
      model_result$covariate <- trimws(str_split_i(model_formula, "~", 1))
      # binding field models together
      all_models <- rbind(all_models,
                          model_result)
      
    }
  }
  # adding some stars
  all_models$stars <- ifelse(all_models$`Pr(>|z|)` <= 0.001, "***",
                             ifelse(all_models$`Pr(>|z|)` <= 0.001, "**",
                                    ifelse(all_models$`Pr(>|z|)` <= 0.05, "*",
                                           ifelse(all_models$`Pr(>|z|)` <= 0.1, ".", ""))))
  
  all_models$field <- factor(all_models$field,
                             levels = c("phys",
                                        "life",
                                        "health",
                                        "soc_sci",
                                        "arts"))
  
  
  output_list <- list(all_models)
  
  return(output_list)
}

## FE and RE regression
fe_re_fitter_cl_robust_scopus <- function(panel_dataset,
                                   lm_formula_list,
                                   year_cutoff_upper = 2023,
                                   year_cutoff_lower = 1973,
                                   index = c("profile_id", "year"),
                                   fe_re = c("fe", "re")){
  
  all_models <- data.frame(matrix(ncol = 10, nrow = 0))
  
  fields <- c("phys", "life", "health", "soc_sci", "arts")
  life <- filter(prof_panel_filter, overall_adj_domain == "Life Sciences")
  phys <- filter(prof_panel_filter, overall_adj_domain == "Physical Sciences")
  soc_sci <- filter(prof_panel_filter, overall_adj_domain == "Social Sciences")
  arts <- filter(prof_panel_filter, overall_adj_domain == "Arts and Humanities")
  health <- filter(prof_panel_filter, overall_adj_domain == "Health Sciences")
  
  for (field in fields){
    plm_model <- NA
    
    field_dataset <- get(field)
    
    field_dataset <- filter(field_dataset,
                            year <= year_cutoff_upper & year >= year_cutoff_lower)
    
    for (lm_formula in lm_formula_list){
      
      if (fe_re == "fe"){
        
        plm_model <- plm(lm_formula, 
                         data = field_dataset,
                         index = index, 
                         model = "within", 
                         effect = "twoways")
      }
      
      if (fe_re == "re"){
        plm_model <- plm(lm_formula,
                         data = field_dataset,
                         index = index, 
                         model = "random")
        
      }
      
      # cluster-robust SEs
      coef_test_output <- coeftest(lm(lm_formula,
                                      data = field_dataset), vcov = vcovCL)
      model_result <- coef_test_output[,] %>% 
        as.data.frame() %>% 
        rownames_to_column(var = "term")
      model_result$lower_ci <- as.data.frame(confint(coef_test_output,
                                                     level = 0.95))[,1]
      model_result$upper_ci <- as.data.frame(confint(coef_test_output,
                                                     level = 0.95))[,2]
      
      # extracting R squared
      model_result[nrow(model_result)+1, 1] <- "R^2"
      model_result[nrow(model_result), 2:ncol(model_result)] <- round(summary(plm_model)$r.squared,3)
      model_result$field <- field
      model_result$covariate <- trimws(str_split_i(lm_formula, "~", 1))
      # binding field models together
      all_models <- rbind(all_models,
                          model_result)
      
    }
  }
  # adding some stars
  all_models$stars <- ifelse(all_models$`Pr(>|t|)` <= 0.001, "***",
                             ifelse(all_models$`Pr(>|t|)` <= 0.001, "**",
                                    ifelse(all_models$`Pr(>|t|)` <= 0.05, "*",
                                           ifelse(all_models$`Pr(>|t|)` <= 0.1, ".", ""))))
  
  all_models$field <- factor(all_models$field,
                             levels = c("phys",
                                        "life",
                                        "health",
                                        "soc_sci",
                                        "arts"))
  
  
  output_list <- list(all_models)
  
  return(output_list)
}

# this function tidies up regression outputs for three
# outcome variables and arranges them for easy writing
# into a CSV file
neat_regression_table <- function(
    table_1,
    table_2,
    table_3){
  
  table_1_neat <- table_1 %>%
    arrange(field)%>%
    filter(!grepl("year)", term))%>%
    select(field, term:`Std. Error`, stars)
  
  colnames(table_1_neat) <- c("field", 
                              "term",
                              "coef_printed",
                              "se_printed",
                              "sig_printed")
  
  table_2_neat <- table_2 %>%
    arrange(field)%>%
    filter(!grepl("year)", term))%>%
    select(field, term:`Std. Error`, stars)
  
  colnames(table_2_neat) <- c("field", 
                              "term",
                              "coef_online",
                              "se_online",
                              "sig_online")
  
  table_3_neat <- table_3 %>%
    arrange(field)%>%
    filter(!grepl("year)", term))%>%
    select(field, term:`Std. Error`, stars)
  
  colnames(table_3_neat) <- c("field", 
                              "term",
                              "coef_twitter",
                              "se_twitter",
                              "sig_twitter")
  
  three_models_table <- merge(table_1_neat,
                              table_2_neat,
                              by = c("field", 
                                     "term"),
                              all.x = TRUE,
                              all.y = TRUE)
  
  three_models_table <- merge(three_models_table,
                              table_3_neat,
                              by = c("field", 
                                     "term"),
                              all.x = TRUE,
                              all.y = TRUE)
  
  
  three_models_table$term <- ifelse(three_models_table$term %in% c("news_all_l",
                                                                   "news_all_l_log",
                                                                   "alt_online_all_l",
                                                                   "alt_online_all_l_log",
                                                                   "alt_twitter_l",
                                                                   "alt_twitter_l_log"),
                                    "t_min_1",
                                    three_models_table$term)
  
  
  three_models_table$term <- factor(three_models_table$term,
                                    levels = c(
                                      "inferred_genderw",
                                      "cited_by_total_all_l",
                                      "cited_by_total_all_l_log",
                                      "news_all_total_l",
                                      "news_all_total_l_log",
                                      "alt_online_all_total_l",
                                      "alt_online_all_total_l_log",
                                      "alt_twitter_total_l",
                                      "alt_twitter_total_l_log",
                                      "coa_tot_cited_by_total_l",
                                      "coa_tot_cited_by_total_l_log",
                                      "coa_online_all_total_l",
                                      "coa_online_all_total_l_log",
                                      "coa_tot_online_all_total_l",
                                      "coa_tot_online_all_total_l_log",
                                      "coa_twitter_total_l",
                                      "coa_twitter_total_l_log",
                                      "coa_tot_twitter_total_l",
                                      "coa_tot_twitter_total_l_log",
                                      "cited_by_l",
                                      "cited_by_l_log",
                                      "coa_tot_cited_by_l",
                                      "coa_tot_cited_by_l_log",
                                      "coa_online_all_l",
                                      "coa_online_all_l_log",
                                      "coa_tot_online_all_l",
                                      "coa_tot_online_all_l_log",
                                      "coa_twitter_l",
                                      "coa_twitter_l_log",
                                      "coa_tot_twitter_l",
                                      "coa_tot_twitter_l_log",
                                      "t_min_1",
                                      "news_all_l",
                                      "news_all_l_log",
                                      "alt_online_all_l",
                                      "alt_online_all_l_log",
                                      "alt_twitter_l",
                                      "alt_twitter_l_log",
                                      "years_since_first_pub",
                                      "as.factor(any_grant_l)1",
                                      "(Intercept)",
                                      "R^2"
                                    ))
  
  
  three_models_table <- three_models_table %>%
    mutate(across(where(is.numeric), round, 5))%>%
    mutate(term = recode(term,
                         'alt_online_all_l' = "Online attention (t-1)",
                         'alt_online_all_total_l' = "Total online attention (t-1)",
                         'alt_twitter_l' = "Twitter/X attention (t-1)",
                         'alt_twitter_total_l' = "Total Twitter/X attention (t-1)",
                         'cited_by_total_all_l' = "Total citations (t-1)",
                         'as.factor(any_grant_l)1' = "Received an NWO/ERC grant (t-1)",
                         'coa_online_all_total_l' = "Coauthors' total online attention total (t-1)",
                         'coa_tot_online_all_total_l' = "Coauthors' total online attention total (t-1)",
                         'coa_tot_cited_by_total_l' = "Coauthors' total citations (t-1)",
                         'coa_twitter_total_l' = "Coauthors' total Twitter/X attention (t-1)",
                         'coa_tot_twitter_total_l' = "Coauthors' total Twitter/X attention (t-1)",
                         'years_since_first_pub' = "Years since first publication",
                         'inferred_genderw' = "Inferred gender (reference: man)",
                         'news_all_l' = "Printed news attention (t-1)",
                         'news_all_total_l' = "Total printed news attention (t-1)",
                         'alt_online_all_l_log' = "Online attention (log, t-1)",
                         'alt_online_all_total_l_log' = "Total online attention (log, t-1)",
                         'alt_twitter_l_log' = "Twitter/X attention (log, t-1)",
                         'alt_twitter_total_l_log' = "Total Twitter/X attention (log, t-1)",
                         'cited_by_total_all_l_log' = "Total citations (log, t-1)",
                         't_min_1' = "Dependent variable (t-1)",
                         'coa_online_all_total_l_log' = "Coauthors' total online attention total (log, t-1)",
                         'coa_tot_online_all_total_l_log' = "Coauthors' total online attention total (log, t-1)",
                         'coa_tot_cited_by_total_l_log' = "Coauthors' total citations (log, t-1)",
                         'coa_twitter_total_l_log' = "Coauthors' total Twitter/X attention (log, t-1)",
                         'coa_tot_twitter_total_l_log' = "Coauthors' total Twitter/X attention (log, t-1)",
                         'news_all_l_log' = "Printed news attention (log, t-1)",
                         'news_all_total_l_log' = "Total printed news attention (log, t-1)",
                         'cited_by_l' = "Citations (t-1)",
                         'coa_tot_online_all_l' = "Coauthors' online attention (t-1)",
                         'coa_tot_cited_by_l' = "Coauthors' citations (t-1)",
                         'coa_tot_twitter_l' = "Coauthors' Twitter/X attention (t-1)",
                         'cited_by_all_l_log' = "Total citations (log, t-1)",
                         'coa_online_all_l_log' = "Coauthors' online attention (log, t-1)",
                         'coa_tot_cited_by_l_log' = "Coauthors' citations (log, t-1)",
                         'coa_twitter_l_log' = "Coauthors' Twitter/X attention (log, t-1)"
    ),
    field = recode(field,
                   'stem' = "STEM",
                   'medicine' = "Medicine",
                   'soc_sci' = "Social science",
                   'arts' = "Arts & Humanities"))%>%
    arrange(field, term)%>%
    select(field:coef_printed, sig_printed, se_printed, 
           coef_online, sig_online, se_online,
           coef_twitter, sig_twitter, se_twitter)
  
  three_models_table$se_printed <- ifelse(!is.na(three_models_table$se_printed),
                                          paste0("(", as.character(three_models_table$se_printed), ")"),
                                          three_models_table$se_printed)
  
  three_models_table$se_online <- ifelse(!is.na(three_models_table$se_online),
                                         paste0("(", as.character(three_models_table$se_online), ")"),
                                         three_models_table$se_online)
  
  three_models_table$se_twitter <- ifelse(!is.na(three_models_table$se_twitter),
                                          paste0("(", as.character(three_models_table$se_twitter), ")"),
                                          three_models_table$se_twitter)
  
  three_models_table$field <- ifelse(three_models_table$term == "Inferred gender (reference: man)",
                                     as.character(three_models_table$field),
                                     "")
  
  return(three_models_table)
  
}

## neat regressions for scopus classification
# this function tidies up regression outputs for three
# outcome variables and arranges them for easy writing
# into a CSV file
neat_regression_table_scopus <- function(
    table_1,
    table_2,
    table_3,
    fe ="no"){
  
  table_1_neat <- table_1 %>%
    arrange(field)%>%
    filter(!grepl("year)", term))%>%
    select(field, term:`Std. Error`, stars)
  
  colnames(table_1_neat) <- c("field", 
                              "term",
                              "coef_printed",
                              "se_printed",
                              "sig_printed")
  
  table_2_neat <- table_2 %>%
    arrange(field)%>%
    filter(!grepl("year)", term))%>%
    select(field, term:`Std. Error`, stars)
  
  colnames(table_2_neat) <- c("field", 
                              "term",
                              "coef_online",
                              "se_online",
                              "sig_online")
  
  table_3_neat <- table_3 %>%
    arrange(field)%>%
    filter(!grepl("year)", term))%>%
    select(field, term:`Std. Error`, stars)
  
  colnames(table_3_neat) <- c("field", 
                              "term",
                              "coef_twitter",
                              "se_twitter",
                              "sig_twitter")
  
  three_models_table <- merge(table_1_neat,
                              table_2_neat,
                              by = c("field", 
                                     "term"),
                              all.x = TRUE,
                              all.y = TRUE)
  
  three_models_table <- merge(three_models_table,
                              table_3_neat,
                              by = c("field", 
                                     "term"),
                              all.x = TRUE,
                              all.y = TRUE)
  
  
  three_models_table$term <- ifelse(three_models_table$term %in% c("news_all_l",
                                                                   "news_all_l_log",
                                                                   "alt_online_all_l",
                                                                   "alt_online_all_l_log",
                                                                   "alt_twitter_l",
                                                                   "alt_twitter_l_log"),
                                    "t_min_1",
                                    three_models_table$term)
  
  
  three_models_table$term <- factor(three_models_table$term,
                                    levels = c(
                                      "inferred_genderw",
                                      "cited_by_total_all_l",
                                      "cited_by_total_all_l_log",
                                      "news_all_total_l",
                                      "news_all_total_l_log",
                                      "alt_online_all_total_l",
                                      "alt_online_all_total_l_log",
                                      "alt_twitter_total_l",
                                      "alt_twitter_total_l_log",
                                      "coa_tot_cited_by_total_l",
                                      "coa_tot_cited_by_total_l_log",
                                      "coa_online_all_total_l",
                                      "coa_online_all_total_l_log",
                                      "coa_tot_online_all_total_l",
                                      "coa_tot_online_all_total_l_log",
                                      "coa_twitter_total_l",
                                      "coa_twitter_total_l_log",
                                      "coa_tot_twitter_total_l",
                                      "coa_tot_twitter_total_l_log",
                                      "cited_by_l",
                                      "cited_by_l_log",
                                      "coa_tot_cited_by_l",
                                      "coa_tot_cited_by_l_log",
                                      "coa_online_all_l",
                                      "coa_online_all_l_log",
                                      "coa_tot_online_all_l",
                                      "coa_tot_online_all_l_log",
                                      "coa_twitter_l",
                                      "coa_twitter_l_log",
                                      "coa_tot_twitter_l",
                                      "coa_tot_twitter_l_log",
                                      "t_min_1",
                                      "news_all_l",
                                      "news_all_l_log",
                                      "alt_online_all_l",
                                      "alt_online_all_l_log",
                                      "alt_twitter_l",
                                      "alt_twitter_l_log",
                                      "years_since_first_pub",
                                      "as.factor(any_grant_l)1",
                                      "(Intercept)",
                                      "R^2"
                                    ))
  
  
  three_models_table <- three_models_table %>%
    mutate(across(where(is.numeric), round, 5))%>%
    mutate(term = recode(term,
                         'alt_online_all_l' = "Online attention (t-1)",
                         'alt_online_all_total_l' = "Total online attention (t-1)",
                         'alt_twitter_l' = "Twitter/X attention (t-1)",
                         'alt_twitter_total_l' = "Total Twitter/X attention (t-1)",
                         'cited_by_total_all_l' = "Total citations (t-1)",
                         'as.factor(any_grant_l)1' = "Received an NWO/ERC grant (t-1)",
                         'coa_online_all_total_l' = "Coauthors' total online attention total (t-1)",
                         'coa_tot_online_all_total_l' = "Coauthors' total online attention total (t-1)",
                         'coa_tot_cited_by_total_l' = "Coauthors' total citations (t-1)",
                         'coa_twitter_total_l' = "Coauthors' total Twitter/X attention (t-1)",
                         'coa_tot_twitter_total_l' = "Coauthors' total Twitter/X attention (t-1)",
                         'years_since_first_pub' = "Years since first publication",
                         'inferred_genderw' = "Inferred gender (reference: man)",
                         'news_all_l' = "Printed news attention (t-1)",
                         'news_all_total_l' = "Total printed news attention (t-1)",
                         'alt_online_all_l_log' = "Online attention (log, t-1)",
                         'alt_online_all_total_l_log' = "Total online attention (log, t-1)",
                         'alt_twitter_l_log' = "Twitter/X attention (log, t-1)",
                         'alt_twitter_total_l_log' = "Total Twitter/X attention (log, t-1)",
                         'cited_by_total_all_l_log' = "Total citations (log, t-1)",
                         't_min_1' = "Dependent variable (t-1)",
                         'coa_online_all_total_l_log' = "Coauthors' total online attention total (log, t-1)",
                         'coa_tot_online_all_total_l_log' = "Coauthors' total online attention total (log, t-1)",
                         'coa_tot_cited_by_total_l_log' = "Coauthors' total citations (log, t-1)",
                         'coa_twitter_total_l_log' = "Coauthors' total Twitter/X attention (log, t-1)",
                         'coa_tot_twitter_total_l_log' = "Coauthors' total Twitter/X attention (log, t-1)",
                         'news_all_l_log' = "Printed news attention (log, t-1)",
                         'news_all_total_l_log' = "Total printed news attention (log, t-1)",
                         'cited_by_l' = "Citations (t-1)",
                         'coa_tot_online_all_l' = "Coauthors' online attention (t-1)",
                         'coa_tot_cited_by_l' = "Coauthors' citations (t-1)",
                         'coa_tot_twitter_l' = "Coauthors' Twitter/X attention (t-1)",
                         'cited_by_all_l_log' = "Total citations (log, t-1)",
                         'coa_online_all_l_log' = "Coauthors' online attention (log, t-1)",
                         'coa_tot_cited_by_l_log' = "Coauthors' citations (log, t-1)",
                         'coa_twitter_l_log' = "Coauthors' Twitter/X attention (log, t-1)"
    ),
    field = recode(field,
                   'phys' = "Physical Sciences", 
                   'life' = "Life Sciences", 
                   'health' = "Health Sciences", 
                   'soc_sci' = "Social Sciences", 
                   'arts' = "Arts & Humanities"))%>%
    arrange(field, term)%>%
    select(field:coef_printed, sig_printed, se_printed, 
           coef_online, sig_online, se_online,
           coef_twitter, sig_twitter, se_twitter)
  
  three_models_table$se_printed <- ifelse(!is.na(three_models_table$se_printed),
                                          paste0("(", as.character(three_models_table$se_printed), ")"),
                                          three_models_table$se_printed)
  
  three_models_table$se_online <- ifelse(!is.na(three_models_table$se_online),
                                         paste0("(", as.character(three_models_table$se_online), ")"),
                                         three_models_table$se_online)
  
  three_models_table$se_twitter <- ifelse(!is.na(three_models_table$se_twitter),
                                          paste0("(", as.character(three_models_table$se_twitter), ")"),
                                          three_models_table$se_twitter)
  
  if (fe == "no"){
    
    three_models_table$field <- ifelse(three_models_table$term == "Inferred gender (reference: man)",
                                       as.character(three_models_table$field),
                                       "")
  }
  if (fe == "yes"){
    three_models_table$field <- ifelse(three_models_table$term == "Total citations (t-1)",
                                       as.character(three_models_table$field),
                                       "")
  }
  return(three_models_table)
  
}
