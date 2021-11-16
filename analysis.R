library(tidyverse)

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

parse_PARALA_results <- function(res){
  ret <- 
    map_dfc(names(res), function(test){
      #browser()
      if(test %in% c("RAT", "MDT", "MIQ")){
        res[[test]] %>% as_tibble() %>% select(!starts_with("q")) %>% set_names(sprintf("%s.%s", test, names(.) %>% tolower() %>% str_replace(" ", "_")))
      }
      else if(test == "SRP"){
        tmp <- res[[test]] %>% as.data.frame(stringsAsFactors = F)
        tmp[[1]] <- c("no", "yes")[as.integer(tmp[[1]])]
        names(tmp) <- c("SRP.technical_problems", "SRP.technical_problems_desc", "SRP.focus")
        tmp
      }
      else if(test %in% c("PRF", "WMM", "SRP")){
        tmp <- res[[test]]
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
      }
      else{
        data <- 
          res[[test]] %>% 
          as_tibble() %>% 
          select(-starts_with("q"), -starts_with("items"), -starts_with("points")) %>% 
          set_names(sprintf("%s.%s", test, names(.) %>% tolower() %>% str_replace_all(" ", "_")))
        data[!duplicated(data), ]
      }
    })
  if("session" %in% names(res)){
    ret <- ret %>% mutate(p_id = session.p_id) %>% select(-session.p_id, 
                                                          -session.pilot, 
                                                          -session.current_time, 
                                                          -session.language, 
                                                          -session.num_restarts) %>% 
      select(p_id, everything())
  }
  ret
}
read_data <- function(result_dir = "data/from_server"){
  messagef("Setting up data from %s", result_dir)
  
  results <- purrr::map(list.files(result_dir, pattern = "*.rds", full.names = T), ~{readRDS(.x) %>% as.list()})
  map_dfr(results, function(res){
    #browser()
    parse_PARALA_results(res)
  })
}
setup_workspace <- function(results = "data/results"){
  master <- read_data(results)
  master$DEG.gender[is.na(master$DEG.gender)] <- sample(1:4, size = sum(is.na(master$DEG.gender)), replace = T)
  master <- master %>% mutate(age = round(DEG.age/12), 
                              gender = factor(DEG.gender, 
                                              levels = 1:4, 
                                              labels = c("female", "male", "other", "rather not say")))
  
  master$age[is.na(master$age)] <- 99
  master <- master %>% 
    mutate(age_group = age > 30) %>% 
    mutate(age_group = factor(age_group, labels = c("<30", "30+"))) %>% filter(!is.na(EDU.education))
  names(master)[str_detect(names(master), "^LIQ")] <- LIQ_items_map
  assign("master", master, globalenv())
}

get_correlations <- function(data, var_x, var_y, method = "pearson"){
  f <- as.formula(sprintf("~ %s + %s", var_x, var_y))
  ct <- cor.test(f, data = data, method = method)
  return(ct %>% broom::tidy())     
}

