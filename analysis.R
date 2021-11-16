library(tidyverse)

messagef <- function(...) message(sprintf(...))
printf <- function(...) print(sprintf(...))

num_predictors <- c("GMS.general", "PIT.ability", "JAJ.ability", "IMI.earworm_frequency", "IMI.negative_valence",
                    "IMI.earworm_length", "IMI.earworm_duration", "age", "IMI.help", "IMI.movement", "IMI.earworm_removal")
cat_predictors <- c("gender", "music_studies")


dummy <- list()
dummy[["IMI"]] <- tibble(IMI.earworm_frequency = NA, 
                    IMI.help = NA, 
                    IMI.negative_valence = NA, 
                    IMI.movement = NA, 
                    IMI.personal_reflection = NA, 
                    IMI.earworm_length = NA, 
                    IMI.earworm_duration = NA, 
                    IMI.earworm_removal = NA)
dummy[["EWE"]] <- tibble(EWE.earworm = NA, 
                    EWE.frequency = NA, 
                    EWE.lyrics = NA, 
                    EWE.likingbefore = NA, 
                    EWE.likingafter = NA, 
                    EWE.trigger = NA, 
                    EWE.origin = NA, 
                    EWE.content = NA, 
                    EWE.innerform = NA, 
                    EWE.reproduction = NA, 
                    EWE.genre = NA, 
                    EWE.regularity = NA, 
                    EWE.complexity = NA, 
                    EWE.counterstrategies = NA)
dummy[["JAJ"]] <- tibble(JAJ.ability  = NA, JAJ.ability_sem  = NA, JAJ.num_items = NA)
dummy[["PIT"]] <- tibble(PIT.ability  = NA, PIT.ability_sem  = NA, PIT.num_items = NA)
dummy[["DEG"]] <- tibble(DEG.age = NA,   DEG.gender = NA)
dummy[["GMS"]] <- tibble(GMS.general = NA)
dummy[["results"]] <- tibble(results.music_studies = NA)

EWE_genres <- psyquest::psyquest_dict %>% 
  as.data.frame() %>% 
  filter(str_detect(key, "TEWE_0011_CHOICE")) %>% pull(en)

EWE_inner_forms <- psyquest::psyquest_dict %>% 
  as.data.frame() %>% 
  filter(str_detect(key, "TEWE_0009_CHOICE")) %>% pull(en)

parse_generic_entry <- function(q_entry, label){
  #browser()
  dummy_entry <- dummy[[label]]
  stopifnot(!is.null(dummy_entry))
  if(is.null(q_entry)){
    return(dummy_entry)
  }
  names <- names(q_entry)
  if(length(names) == 0){
    return(dummy_entry)
  }
  sum_data <- names[!stringr::str_detect(names, "q[0-9]+")]
  if(label == "EWE"){
    genres <- str_split(q_entry$InnerForm, ",") %>% unlist() %>% str_replace_all("\\'", "")
    #browser()   
    #in an earlier version of EWE the InnerForm und Genre fields got mixed up, fixed that here.
    if(length(intersect(genres, EWE_genres)) > 0){
      sum_data <- setdiff(c(sum_data, "q9"), "Genre")
      ret <- q_entry[sum_data]
      names(ret)[names(ret) == "InnerForm"] <- "Genre"
      names(ret)[names(ret) == "q9"] <- "InnerForm"
      ret$InnerForm <- EWE_inner_forms[as.numeric(ret$InnerForm)]
      ret$Lyrics <- c("lyrics", "no lyrics")[as.integer(q_entry$q3)]
      ret$Reproduction <- c("properly", "partially properly", "not properly", "not at all")[as.integer(q_entry$q10)]
      #ret$Lyrics <-      
      #printf("Genre 1: %s", ret$Genre)
    }
    else{
      ret <- q_entry[sum_data]
    }
  }
  else{
    ret <- q_entry[sum_data]
  }
  # if(label == "results"){
  #   browser()
  # }
  names(ret) <- sprintf("%s.%s", label, names(ret) %>% stringr::str_to_lower() %>% stringr::str_replace(" ", "_"))
  ret %>% tibble::as_tibble()
}

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
    completed <- which(tmp$complete == TRUE)
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

read_data <- function(result_dir = "data/from_server"){
  messagef("Setting up data from %s", result_dir)
  
  results <- purrr::map(list.files(result_dir, pattern = "*.rds", full.names = T), ~{readRDS(.x) %>% as.list()})
  #browser()
  purrr::map_dfr(results, function(x){
    #browser()
    names <- names(x)
    if(length(intersect(names, c("IMI", "EWE", "JAJ", "GMS", "DEG"))) != 5){
      return(NULL)
    }
    ret <- parse_generic_entry(x$IMI, "IMI")
  
    ret <- parse_generic_entry(x$EWE, "EWE") %>% bind_cols(ret)
    if(!is.character(ret$EWE.reproduction)){
      #for some unknown reason an EWE reproduction values was no an character but a number, fix that here
      #and pray to god.
      ret$EWE.reproduction <- c("properly", "partially properly", "not properly", "not at all")[as.integer(x$EWE$q10)]
      
    }
    
    JAJ <- parse_generic_entry(x$JAJ, "JAJ")
    attr(JAJ$JAJ.ability, "metadata") <- NULL
    ret <-  JAJ %>% bind_cols(ret)
    PIT <- parse_generic_entry(x$PIAT, "PIT")
    attr(PIT$PIT.ability, "metadata") <- NULL
    ret <-  PIT %>% bind_cols(ret)
    
    ret <- parse_generic_entry(x$GMS, "GMS") %>% bind_cols(ret )
    ret <- parse_generic_entry(x$DEG, "DEG") %>% bind_cols(ret )
    ret <- parse_generic_entry(x$results, "results") %>% bind_cols(ret )
    if("DEG.handedness" %in% names(ret)){
      return(NULL)
    }
    
    session_tibble <- x$session %>%  tibble::as_tibble() 
    ret <- ret %>% dplyr::bind_cols(session_tibble) %>% 
      dplyr::select(p_id, -pilot, -num_restarts, time_ended = current_time, everything())
    ret
    }) %>% 
    join_rows() %>% 
    dplyr::arrange(time_started)
}

music_studies_map <- c("Ja, ich studiere derzeit Musik" =  "Music Student",
                   "Ja, Ich studiere ein anderes Fach als Musik" =  "Other Student", 
                   "Nein" = "No Student")
parse_quoted_entry <- function(entry, sep = ","){
  parsed <- entry %>% str_split("'") 
  parsed <- 
    map(parsed, function(x){
      x <- x[nchar(x) > 0]
      x <- x[x != sep]
      x
  })
  base_set <- parsed %>% unlist() %>% unique() %>% sort()
  map_dfr(parsed, function(x){
      indicator <- rep(FALSE, length(base_set))
      names(indicator) <- base_set
      indicator[x] <- TRUE
      indicator %>% t() %>% as_tibble()
    })
}
EWE_counterstrategies_labels <- c(
  "I distract myself with other things." = "distract",
  "I do nothing." = "nothing",
  "I listen to other music." = "listen_other",
  "I listen to the music of which I have an earworm." = "listen_earworm",
  "I sing or play the music that has caught my ear." = "play_earworm",
  "I sing, play, or imagine other music." = "play_other",
  "None of the above." = "other")

EWE_trigger_labels <- c("A person, word, sound, or other occurrence in my environment reminded me of this music." = "environment", 
                        "I have no idea why this music came into my head." = "no_idea", 
                        "I recently heard this music." = "recently_heard", 
                        "I recently performed this music." = "recently_performed", 
                        "I was thinking about a future event and related it to this music." = "anticipation", 
                        "Other reasons" = "other", 
                        "This music reminds me of something from my past." = "past")

EWE_postprocessing <- function(data){
  data$EWE.complexity <- NULL
  data$EWE.frequency <- 6 - data$EWE.frequency 
  data$EWE.likingbefore <- 6 - data$EWE.likingbefore
  data$EWE.likingafter <- 6 - data$EWE.likingafter
  data$EWE.likingchange <- data$EWE.likingafter - data$EWE.likingbefore
  data$EWE.reproduction <- 5-   as.factor(data$EWE.reproduction) %>% as.integer()
  #data$EWE.complexity <- 6 - data$EWE.complexity
  counterstrats <- parse_quoted_entry(data$EWE.counterstrategies)
  names(counterstrats) <- sprintf("EWE.counterstrategy_%s", EWE_counterstrategies_labels[names(counterstrats)])
  data <- data %>% select(-EWE.counterstrategies) %>% bind_cols(counterstrats)
  genres <- data$EWE.genre %>% parse_quoted_entry() 
  names(genres) <-  names(genres) %>% tolower() %>% str_replace_all(" ", "_")
  names(genres) <- sprintf("EWE.genre_%s", names(genres))
  data <- data %>% select(-EWE.genre) %>% bind_cols(genres)
  trigger <- parse_quoted_entry(data$EWE.trigger)
  names(trigger) <- sprintf("EWE.trigger_%s", EWE_trigger_labels[names(trigger)])
  data <- data %>% select(-EWE.trigger) %>% bind_cols(trigger)
  
  data
}
setup_workspace <- function(results = "data/from_server"){
  master <- read_data(results)
  master <- master %>% mutate(age = round(DEG.age/12), 
                              gender = factor(DEG.gender, 
                                              levels = 1:4, 
                                              labels = c("female", "male", "other", "rather not say"))) %>% 
    rename(music_studies = results.music_studies) %>% 
    mutate(music_studies = music_studies_map[music_studies] %>% as.vector())
  master[nchar(master$EWE.earworm) == 0,]$EWE.earworm <- "NA"
  #dirty hack
  master <- EWE_postprocessing(master)
  master[is.na(master$music_studies),]$music_studies <- "Music Student"
  master <- master %>% 
    mutate(age_group = age > 30) %>% 
    mutate(age_group = factor(age_group, labels = c("<30", "30+"))) %>% 
    select(-pilot)
  assign("master", master, globalenv())
}

get_correlations <- function(data, var_x, var_y, method = "pearson"){
  f <- as.formula(sprintf("~ %s + %s", var_x, var_y))
  ct <- cor.test(f, data = data, method = method)
  return(ct %>% broom::tidy())     
}

get_pc_graph <- function(master, 
                         alpha = .05,   
                         charge = -120,
                         linkDistance = 100,
                         fontSize = 16,
                         opacityNoHover = .75){
  #require(Rgraphviz)
  
  red <- master
  cor_data <- cor(red, use = "pairwise.complete.obs")
  suffStat <- list(C = cor_data, n = nrow(red))
  pc.fit <- pcalg::pc(suffStat, indepTest = pcalg::gaussCItest, p = ncol(red), alpha = alpha)
  labels <- names(red)
  #adjm <- wgtMatrix(getGraph(pc.fit), transpose = FALSE)
  #ig_network <- graph_from_adjacency_matrix(adjm, mode = "directed", weighted = T)
  ig_network <- graph_from_graphnel(getGraph(pc.fit), name = TRUE, weight = TRUE, unlist.attrs = TRUE)
  d3_network <- igraph_to_networkD3(ig_network)
  d3_network$nodes$group <- 1
  d3_network$nodes$name <- labels
  for(i in 1:nrow(d3_network$links)){
    d3_network$links[i,]$value <-  50*abs(cor_data[d3_network$links[i,]$source + 1,  d3_network$links[i,]$target + 1])
  }
  sn <- forceNetwork(
    Links = d3_network$links, 
    Nodes = d3_network$nodes, 
    Source = 'source', 
    Target = 'target', 
    Value = "value",
    NodeID = 'name', 
    Group = "group", 
    fontSize = fontSize,
    opacityNoHover = opacityNoHover,
    bounded = F,
    zoom = T,
    charge = charge,
    linkDistance = linkDistance,
    arrows = TRUE,
    #Nodesize = "size",
    colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")
  )
  #q <- pcalg::iplotPC(pc.fit, labels = labels)
  #plot(pc.fit@graph, main = "", nodeAttrs = nAttrs, attrs = attrs)
  sn
}
beta_plot <- function(lm_model, order_by_size = F){
  if("lm" %in% class(lm_model)){
    lm_model <- lm_model %>% 
      broom::tidy()
  }
  lm_model <- lm_model %>% 
    filter(term != "(Intercept)") %>% 
    mutate(y_min = estimate  - 1.96*std.error, y_max = estimate  + 1.96*std.error, 
           sig = y_min > 0 | y_max < 0)
  
  if(order_by_size) 
    lm_model <- lm_model %>% mutate(term = factor(term) %>% fct_reorder(estimate, mean))
  if("N" %in%  names(lm_model)){
    q <- lm_model %>% 
      mutate(N_fact = factor(N)) %>% 
      ggplot(aes(x = term, y = estimate, colour = sig, group = N_fact)) 
    q <- q + geom_point(shape = 2, size = 2, position = position_dodge(width = 1)) 
    q <- q + geom_linerange(aes(ymin = y_min, ymax = y_max, colour = sig, group = N_fact), position = position_dodge(width = 1))
    q <- q + geom_text(aes(y = 2, x = 10 * (N - min(N))/max(N) + 2, label = sprintf("N = %s", N)), 
                       size = 3, colour ="black")                            
    q <- q + ylim(-1, 1)
  }
  else{
    q <- lm_model %>% ggplot(aes(x = term, y = estimate )) 
    q <- q + geom_point(shape = 2, size = 2, color = def_colour1) 
    q <- q + geom_linerange(aes(ymin = y_min, ymax = y_max))
  }
  q <- q + coord_flip()  
  q <- q + geom_hline(yintercept = 0, linetype = "dashed")
  q <- q + theme(legend.position = "none")
  q
}

get_model <- function(data, dv = "IMI.earworm_frequency", predictors = num_predictors, output_format = "raw", ...){
  output_format <- match.arg(output_format, c("raw","summary", "glance", "tidy", "sj", "jtools_tab", "jtools_plot"))
  predictors <- setdiff(predictors, dv)
  data <- data %>% select(all_of(c(dv, predictors))) %>%  mutate_if(is.numeric, scale)
  f <- sprintf("%s ~ .", dv) %>% as.formula()                      
  lm_tab <- lm(f, data = data)
  if(output_format == "summary"){
    lm_tab <- lm_tab %>% 
      summary()
  }
  if(output_format == "tidy"){
    lm_tab <- lm_tab %>% 
      broom::tidy()
  }
  if(output_format == "glance"){
    lm_tab <- lm_tab %>% 
      broom::glance()
  }
  if(output_format == "sj"){
    lm_tab <- lm_tab %>% 
      sjPlot::tab_model(...)
  }
  if(output_format == "jtools_tab"){
    lm_tab <- lm_tab %>% 
      jtools::summ(...)
  }
  if(output_format == "jtools_plot"){
    lm_tab <- lm_tab %>% 
      jtools::plot_summs(scale = T,  ...)
  }
  lm_tab
}
