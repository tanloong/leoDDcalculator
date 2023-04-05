rm(list = ls())
library(udpipe)
library(dplyr)
library(readr)

get_file_prefix <- function(file="") {
    basename <- gsub('^.*/', '', file)
    file_prefix <- gsub('\\.txt', '', basename)
    return(file_prefix)
}


parse_file <- function(file="", udmodel_eng="", texts_folder="") {
    # for the postag_lemma csv
    file_prefix <- get_file_prefix(file)
    ofilename <- paste0(file_prefix, '_raw_dependencies.csv')
    results_folder <- paste0(gsub('/$', '', texts_folder), '_results_dd/')

    if (!file.exists(file.path(results_folder, ofilename))) {
    myfile <- read_file(file)

    # postag and lemmatize the text
    pos <- udpipe_annotate(udmodel_eng,
                           myfile,
                           tagger = 'default',
                           parser = 'default')
    #glimpse(pos)

    # change it into a dataframe
    pos_df <- as.data.frame(pos)

    #glimpse(pos_df)

    #select columns
    pos_df_mini <- pos_df %>%
      select(#pid = paragraph_id,
        sid = sentence_id,
        token_id,
        head_token_id,
        token,
        #lemma,
        #xpos,
        dep_relation = dep_rel)

    #add a column that is the head token
    pos_df_mini$token_head <- pos_df_mini$token[match(pos_df_mini$head_token_id,
                                                      pos_df_mini$token_id)]

    pos_df_mini <- pos_df_mini %>%
      select(sid,
             token,
             token_head,
             dep_relation,
             token_id,
             head_token_id)

    #head(pos_df_mini)

    ########## write out files


    if (!dir.exists(results_folder)) { dir.create(results_folder) }
    options(warn=-1)

    #####
    # write out the postag lemma csv
    write_csv(pos_df_mini, paste0(results_folder, ofilename))
}
    else {
        pos_df_mini = read.csv(file.path(results_folder, ofilename))
    }
    return(pos_df_mini)
}

mdd_ndd_calculate <- function(language_model_folder = 'C:/',
                              texts_folder = 'C:/mytexts/'){

  df_mdd_ndd_all_files <- data.frame()

  # load the language model
  udmodel_eng <- udpipe_load_model(file = paste0(language_model_folder, 'english-ewt-ud-2.4-190531.udpipe'))
  
  files <- list.files(texts_folder, pattern=".*\\.txt", full.names = T)
  
  # read in files in the folder
  for (file in files){
    pos_df_mini = parse_file(file, udmodel_eng, texts_folder)

    ####### calculating mdd and ndd

    mdd_v <- vector() # holder of mdd of each sentence
    ndd_v <- vector() # holder of ndd of each sentence


    # split the df pos_df_mini by sentence
    pos_df_mini <- pos_df_mini %>%
      select(sid, dep_relation, token_id, head_token_id)

    pos_df_mini_list <- split(pos_df_mini,
                              pos_df_mini$sid)

    #pos_df_mini_list[[1]]

    for (i in 1:length(pos_df_mini_list)){
      df <- pos_df_mini_list[[i]]
      df$token_id <- as.integer(df$token_id)
      df$head_token_id <- as.integer(df$head_token_id)

      #glimpse(df)

      root_distance <- df %>%
        filter(dep_relation == 'root') %>%
        select(token_id)

      sent_length <- max(df$token_id)

      df_dd <- df %>%
        mutate(dd = abs(token_id - head_token_id))

      # remove root and punct
      df_dd_clean <- df_dd %>%
        filter(dep_relation != 'root') %>%
        filter(dep_relation != 'punct')

      #mdd of the sentence
      mdd_sent <- mean(df_dd_clean$dd)
      mdd_v <- c(mdd_v, mdd_sent)

      ndd_sent <- as.double(abs(log(mdd_sent/sqrt(sent_length * root_distance))))

      ndd_v <- c(ndd_v, ndd_sent)

    }

    mdd_v <- na.omit(mdd_v)
    ndd_v <- na.omit(ndd_v)

    mdd_file <- as.character(round(mean(mdd_v), 4))
    ndd_file <- as.character(round(mean(ndd_v), 4))

    file_prefix <- get_file_prefix(file)
    outdf_mdd_ndd <- data.frame(file_id = file_prefix,
                                mdd = mdd_file,
                                ndd = ndd_file)

    df_mdd_ndd_all_files <- bind_rows(df_mdd_ndd_all_files, outdf_mdd_ndd)
    options(warn=-1)

  }

  results_folder <- paste0(gsub('/$', '', texts_folder), '_results_dd/')
  write_csv(df_mdd_ndd_all_files, paste0(results_folder, '0mdd_ndd_results.csv'))
}
