library(tidyverse)
library(tictoc)

messagef <- function(...) message(sprintf(...))
printf <- function(...) print(sprintf(...))

LIQ_items_map <- c("LIQ.item1" = "LIQ.fav",
                   "LIQ.item2" = "LIQ.read_prose", 
                   "LIQ.item3" = "LIQ.read_lyrics",
                   "LIQ.item4" = "LIQ.read_plays", 
                   "LIQ.item5" = "LIQ.creative_writing", 
                   "LIQ.item6" = "LIQ.weekly_read", 
                   "LIQ.item7" = "LIQ.weekly__write", 
                   "LIQ.item8" = "LIQ.peak_time_read",
                   "LIQ.item9" = "LIQ.peak_time_write")

get_parameters <- function(data, input, keep_pseudo_na = T, var_data){
  
  vars <- c("x" = input$bv_variable1, "y" = input$bv_variable2)
  var_info1 <- var_data %>% filter(variable == input$bv_variable1)
  var_info2 <- var_data %>% filter(variable == input$bv_variable2)
  sub_type <- sprintf("%s-%s", substr(var_info1$type, 1, 3), substr(var_info2$type, 1, 3))  
  list(vars = vars, sub_type = sub_type)
}


split_multi_entry <- function(entry){
  if(length(entry) == 1){
    ret <- str_replace_all(entry, "',", "@") %>% 
      str_replace_all("'", "") %>% 
      str_split("@") %>% 
      unlist()        
  }    
  else{
    ret <- NULL
  }
  ret
}

join_rows <- function(data){
  ids <- data %>% count(p_id) %>% filter(n > 1) %>% pull(p_id)
  ret <- data %>% filter(!(p_id %in% ids))
  fixed_rows <- 
    map_dfr(ids, function(i){
    tmp <- data %>% filter(p_id == i)
    completed <- which(tmp$session.complete == TRUE)
    if(length(completed) == 0){
      tmp  <- tmp[nrow(tmp),]   
    }
    else{
      tmp <- tmp[max(completed), ]  
    }
    tmp
  })
  ret %>% bind_rows(fixed_rows) 
}

get_test_duration <- function(start_time, end_time, num_restarts){
  if(length(num_restarts) > 1){
    map2_dbl(start_time, end_time, function(st, et){
      get_test_duration(st, et, 0)
    }) %>% sum()
  }
  if(num_restarts > 0) {
    return(NA)
  }
  difftime(end_time, start_time, units = "mins") %>% as.numeric()
}

parse_PARALA_results <- function(res){
  
  ret <- 
    map_dfc(names(res), function(test){
      #messagef("Parsing test %s", test)
      if(test %in% c("RAT", "MDT", "MIQ")){
        #tic("1")
        ret <- 
          res[[test]][!str_detect(names(res[[test]]), "^q")] %>% 
          as_tibble() %>% 
          #select(!starts_with("q")) %>% 
          set_names(sprintf("%s.%s", test, names(.) %>% 
                              tolower() %>% 
                              str_replace(" ", "_")))
        #toc()
        ret
      }
      else if(test == "LIQ"){
        #tic("2")
        tmp <- mpipoet:::parse_LIQ_results(res)
        #toc()
        #browser()
        tmp
      }
      else if(test == "results"){
        print(test$results)
      }
      else if(test == "SRP"){
        tmp <- res[[test]] %>% as.data.frame(stringsAsFactors = F)
        var_names <- c("SRP.technical_problems", "SRP.technical_problems_desc", "SRP.focus", "SRP.user_feedback")
        if(length(tmp) != 4){
          var_names <- c("SRP.technical_problems", "SRP.technical_problems_desc", "SRP.focus")
        }
        tmp[[1]] <- c("no", "yes")[as.integer(tmp[[1]])]
        names(tmp) <- var_names
        tmp
      }
      else if(test %in% c("PRF", "WMM", "SRP")){
        #tic("2")
        tmp <- res[[test]]
        
        ret <- 
          map_dfc(1:length(tmp), function(i){
          tmp <- tmp[[i]] %>% as.data.frame(stringsAsFactors = F)
          if(test == "PRF"){
            professions <- c("literature", "linguistic", "music")
            names(tmp) <- c(sprintf("PRF.%s", professions[i]), sprintf("PRF.%s_desc", professions[i]))
          }
          else{
            names(tmp) <- c("WMM.wants_take_part", "WMM_prolific_id")
          }
          tmp[[1]] <- c("no", "yes")[as.integer(tmp[[1]])]
          tmp
        })
        #toc()
        ret
      }
      else{
        #tic("5")
        if(length(res[[test]]) != length(unique(names(res[[test]])))){
          messagef("Found duplicated names in %s", test)
          res[[test]] <- res[[test]][unique(names(res[[test]]))]
        }
        data <- 
          res[[test]][!str_detect(names(res[[test]]), "^q|^items|^points")] %>% 
          as_tibble() %>% 
          #select(-starts_with("q"), -starts_with("items"), -starts_with("points")) %>% 
          set_names(sprintf("%s.%s", test, names(.) %>% tolower() %>% str_replace_all(" ", "_")))
        data <- data[!duplicated(data), ]
        #toc()
        data
      }
    })
  if("session" %in% names(res)){
    if(length(ret$session.time_started) > 1){
      browser()
    }
    test_duration <- get_test_duration(ret$session.time_started, ret$session.current_time, ret$session.num_restarts)
    ret <- ret %>% 
      mutate(p_id = session.p_id, 
             session.test_duration_min = test_duration) %>% 
      select(-session.p_id, 
             -session.pilot, 
             -session.current_time, 
             -session.language, 
             -session.num_restarts) %>% 
      select(p_id, everything())
  }
  ret
}

cache_locked <- FALSE

read_cache <- function(cache_dir = g_cache_dir){
  cache_file <- file.path(cache_dir, "cache.rds")
  if(!file.exists(cache_file)){
    return(tibble())
  }
  readRDS(cache_file)
}

save_cache <- function(data, cache_dir = cache_dir){
  if(!cache_locked){
    cache_locked <- TRUE
    saveRDS(data, file.path(cache_dir, "cache.rds"))
    cache_locked <- FALSE
  }
}

delete_cache <- function(cache_dir = g_cache_dir){
  unlink(file.path(cache_dir, "cache.rds"), recursive = T)
}

g_bad_ids <- c()
update_cache <- function(result_dir = g_result_dir, cache_dir = g_cache_dir){
  messagef("Caching  data from <%s> to <%s>", result_dir, cache_dir)
  if(!file.exists(cache_dir)){
    dir.create(cache_dir)  
  } 
  #browser()
  cache <- read_cache(cache_dir)
  
  cache_ids_complete <- c()
  if(nrow(cache) > 0){
    messagef("Read cache with %d lines and %d distinct ids", nrow(cache), n_distinct(cache$p_id))
    cache <- cache %>% 
      group_by(p_id) %>% 
      mutate(any_complete = any(session.complete)) %>% 
      ungroup()
    cache_ids_complete <- unique(cache[cache$any_complete,]$p_id)
  }
  result_files <- list.files(result_dir, pattern = "*.rds", full.names = T)
  l <- length(result_files)
  
  messagef("Found %d data files in <%s>", l, result_dir)
  new_files <- result_files
  if(length(cache_ids_complete) > 0 ){
    #browser()
    ids <- str_extract(result_files, "p_id=[a-z0-9]+") %>% str_replace("p_id=", "")
    complete <- str_extract(result_files, "complete=[a-z]+") %>% str_replace("complete=", "") %>% 
      toupper() %>% 
      as.logical()
    #browser()
    file_stats <- tibble(fname = result_files, 
                         p_id = ids, 
                         is_complete = complete) %>% 
      group_by(p_id) %>% 
      mutate(n_files = n(), 
             is_bad = FALSE,
             in_cache = FALSE) %>% 
      ungroup()
    file_stats[file_stats$p_id %in% g_bad_ids,]$is_bad <- TRUE
    file_stats[file_stats$p_id %in% cache_ids_complete,]$in_cache <- TRUE
    new_files <- file_stats %>% 
      filter(!is_bad, !in_cache) %>% 
      pull(fname)
    messagef("Found %d new data files in <%s>", length(new_files), result_dir)
  }
  if(length(new_files) == 0){
    messagef("No new files.")
    return(cache)
  }
  tic()
  new_data <- purrr::map(new_files, ~{
    readRDS(.x) %>% as.list()
    })
  #assign("res", new_data, globalenv())
  t <- toc(quiet = T)
  messagef("Reading RDS: %.3f s elapsed.", t$toc- t$tic)
  tic()
  ret <-
    map_dfr(new_data, function(res){
      tic()
      res <- tryCatch({parse_PARALA_results(res)}, 
                      error = function(e){
                        g_bad_ids <<- c(g_bad_ids, res$session$p_id)
                        messagef("#Bad ids: %d", length(g_bad_ids))
                        NULL
                        })
      t <- toc(quiet = T)
      #messagef("Parse single participants (%d entries): %.3f s elapsed.", length(res), t$toc- t$tic)
      res
    })
  t <- toc(quiet = T)
  
  messagef("Parsed %d participants: %.3f s elapsed.", nrow(ret), t$toc- t$tic)
  ret <- bind_rows(ret, cache) %>% 
    distinct(p_id, session.time_started, session.complete, .keep_all = T) 
  #browser()
  ret$any_complete <- NULL
  save_cache(ret, cache_dir)
  ret
}

read_data <- function(result_dir = g_result_dir){
  messagef("Setting up data from %s", result_dir)
  tic()
  results <- purrr::map(list.files(result_dir, pattern = "*.rds", full.names = T), ~{readRDS(.x) %>% as.list()})
  assign("res", results, globalenv())
  t <- toc(quiet = T)
  messagef("Reading RDS: %.3f s elapsed.", t$toc- t$tic)
  tic()
  
  ret <-
    map_dfr(results, function(res){
      tic()
      ret <- tryCatch({parse_PARALA_results(res)}, 
                      error = function(e){})
      t <- toc(quiet = T)
      #messagef("Parse single participants (%d entries): %.3f s elapsed.", length(res), t$toc- t$tic)
      ret
    })
  t <- toc(quiet = T)
  messagef("Parsed %d participants: %.3f s elapsed.", nrow(ret), t$toc- t$tic)
  ret
}

setup_workspace <- function(result_dir = g_result_dir, cache_dir = g_cache_dir){
  #master <- read_data(result_dir)
  master <- update_cache(result_dir, cache_dir)
  if(nrow(master) == 0){
    assign("master", tibble(), globalenv())
    return()
  }
  tic()
  master$DEG.gender[is.na(master$DEG.gender)] <- sample(1:4, size = sum(is.na(master$DEG.gender)), replace = T)
  master <- master %>% mutate(DEG.age = round(DEG.age/12), 
                              DEG.gender = factor(DEG.gender, 
                                                  levels = 1:4, 
                                                  labels = c("female", "male", "other", "rather not say")))
  
  master$DEG.age[is.na(master$DEG.age)] <- 99
  master <- master %>% 
    mutate(DEG.age_group = DEG.age > 30) %>% 
    mutate(DEG.age_group = factor(DEG.age_group, labels = c("<30", "30+"))) #%>% filter(!is.na(EDU.education))
  #names(master)[str_detect(names(master), "^LIQ")] <- LIQ_items_map
  master <- add_participant_selection(master)
  master <- master %>% mutate(match_id = p_id == trimws(WMM_prolific_id))
  t <- toc(quiet = T)
  messagef("Post processing: %.3f elapsed", t$toc - t$tic)
  assign("master", master, globalenv())
  master
}

update_workspace <- function(result_dir = g_result_dir){
  messagef("Callind update work_space")
  setup_workspace(result_dir, g_cache_dir)
}

get_correlations <- function(data, var_x, var_y, method = "pearson"){
  f <- as.formula(sprintf("~ %s + %s", var_x, var_y))
  ct <- cor.test(f, data = data, method = method)
  return(ct %>% broom::tidy())     
}

recode_na <- function(x, new_value = FALSE){
  x[is.na(x)] <- new_value
  x
}
  
na_to_false <- function(x){
  recode_na(x)
}

na_to_true <- function(x){
  recode_na(x, TRUE)
}

add_participant_selection <- function(data){
  no_take_part <- (data$WMM.wants_take_part == "no") %>% na_to_true()
  not_focussed <- (data$SRP.focus < 3) %>% na_to_true()
  knows_target_authors <- (as.numeric(data$AUT.author4) >2 &  as.numeric(data$AUT.author7) >2) %>% na_to_true()
  bad_reader <- (data$SLS.perc_correct_total < .5) %>% na_to_false()
  non_native_speaker <- (data$DEG.first_language != "de") %>% na_to_true()
  too_fast <- !is.na(data$session.test_duration_min) & data$session.test_duration_min < 30
  poem_pref <- c(1, 1, 2, 2, 2, 3, 3)[data$LIQ.pref_poetry] %>% recode_na(new_value = 0)
  poem_pref[poem_pref >= 3 & data$LIQ.poetry_reading_peak < 1] <-  2
  poem_pref[poem_pref <= 1 & data$LIQ.poetry_reading_peak >= 1] <-  2
  poem_pref[poem_pref < 1] <-  1
  status <- no_take_part | not_focussed | knows_target_authors | bad_reader | non_native_speaker | too_fast
  good_rhythm <- (data$RAT.ability > 0) %>% na_to_false()
  parala_group <- sprintf("poetry_%s.rhythm_%s", 
                          c("low", "medium", "high")[poem_pref],
                          c("low", "high")[good_rhythm + 1]
                          )
  #parala_group <- c("poetry_low.rhythml", "pl_rl", "pm_rl", "pm_rh", "ph_rl", "ph_rh") [(poem_pref -1) * 2 + as.integer(good_rhythm)  + 1]
  data %>% mutate(SEL.status = c("in", "out")[status + 1], SEL.group = parala_group)
}
