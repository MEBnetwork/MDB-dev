library(DBI)
library(RPostgres)
library(getPass)

#### 0. Connection to the database ####
# @Bedassa Set your credentials below 

# dsn_database = "MDB"   #  name of your Database
# dsn_hostname = "localhost"  
# dsn_port = 5432      #  port number. e.g. 98939
# dsn_uid = "postgres"         # username.
# dsn_pwd <- getPass::getPass("Enter DSN Password: ")
# drv <- RPostgres::Postgres()

connec <- dbConnect(drv, 
                      dbname = dsn_database,
                      host = dsn_hostname, 
                      port = dsn_port,
                      user = dsn_uid, 
                      password = dsn_pwd)

#### 1. Functions ####
identify.fuzzy.matchs = function(pattern, vector, sensitivity = 1, min = 0.1){
  matching = data.frame(position = NA,priority = NA)[-1,]
  for(i in 0:10){
    test = agrep(pattern = pattern, x = vector, max.distance = i)
    match = 
      data.frame(
        position = ifelse(length(test)>0,
                          test,
                          0),
        priority = 1/(1+(i*sensitivity))
      ) |> 
      filter(position != 0) |>
      filter(!(position %in% matching$position))
    if(nrow(match)>0)
      matching = 
      matching |>
      add_row(
        match
      )
  }
  matching = 
    matching |> 
    mutate(match = vector[position]) |> 
   dplyr:: select(match, priority) |> 
    filter(priority >= min)
  return(matching)
}
 ###### 1.1. functions ####
#  update data vector entry of database

# update.database <- function(old.db, new.data) {
  
  
#   cat("\nDetails to be updated :\n")
#   print(old.db)
#   
#   
#   
#   cat("\nDetails to be checked:\n")
#   print(new.data)
#   
#   # Iterate through each field
#   for (field in names(old.db)) {
#     old_value <- as.character(old.db[[field]])
#     new_value <- as.character(new.data[[field]])
#     
#     cat(paste0("\nField: ", field, 
#                "\n  old Value: ", ifelse(is.na(old_value), "NA", old_value), 
#                "\n  new Value: ", ifelse(is.na(new_value), "NA", new_value), "\n"))
#     update <- readline(prompt = "Do you want to update this field with the suggested value? (y/n): ")
#     if (tolower(update) == "y") {
#       old.dbt[[field]] <- ifelse(is.na(new_value), old_value, old_value)
#       cat("Field updated.\n")
#     } else {
#       cat("Field remains unchanged.\n")
#     }
#   }
#   
#   return(old.db)
# }
# 
# updated.dbn <- update.database(old.db, new.data)
# 
# cat("\nUpdated Details:\n")
# print(updated_pers)

#### >> 1.0. Flags ####
create.flags = F
if(create.flags == T){
  flags.site = 
    read.csv('1-data/flag-combination.csv') |> 
    dplyr::select(sif_id = val, 
           sif_combination = flags) |> 
    mutate(sif_description = 'NOT DEFINED')
  
  app = sqlAppendTable(con = connec,
                       table = 'site_flags',
                       value = flags.site,
                       row.names = F)
  dbExecute(con = connec,
            statement = app)
  
  flags.loggers = 
    read.csv('1-data/flag-combination.csv') |> 
    dplyr::select(lgf_id = val, 
           lgf_combination = flags) |> 
    mutate(lgf_description = 'NOT DEFINED')
  
  app = sqlAppendTable(con = connec,
                       table = 'logger_flags',
                       value = flags.loggers,
                       row.names = F)
  dbExecute(con = connec,
            statement = app)
  
  flags.meta.ts = 
    read.csv('1-data/flag-combination.csv') |> 
    dplyr::select(mtf_id = val,
           mtf_combination = flags) |> 
    mutate(mtf_description = 'NOT DEFINED')
  
  app = sqlAppendTable(con = connec,
                       table = 'meta_ts_flags',
                       value = flags.meta.ts,
                       row.names = F)
  dbExecute(con = connec,
            statement = app)
  
  flags.clim.ts = 
    read.csv('1-data/flag-combination.csv') |> 
    dplyr::select(ctf_id = val, 
           ctf_combination = flags) |> 
    mutate(ctf_description = 'NOT DEFINED')
  
  app = sqlAppendTable(con = connec,
                       table = 'clim_ts_flags',
                       value = flags.clim.ts,
                       row.names = F)
  dbExecute(con = connec,
            statement = app)
}

#### >> 1.1 People ####
# pers.to.test = DATASETS$people
# people.list.db = tbl(connec, 'people') |> collect()

push.person = function(
    pers.to.test, 
    people.list.db){
  
  if(is.na(pers.to.test$ppl_coauthor_name)){
    pers.to.test$ppl_coauthor_name = paste(
      pers.to.test$ppl_firstname, 
      if(!is.na(pers.to.test$ppl_middlename_initials)){
        pers.to.test$ppl_middlename_initials
      },
      pers.to.test$ppl_lastname
    ) |> 
      str_replace_all('\\s+', ' ') # remove double spaces
  }
  
  if(!is.na(pers.to.test$ppl_orcid) & 
     length(people.list.db$ppl_orcid|>discard(is.na))>0){
    matchs.orcid = identify.fuzzy.matchs(pers.to.test$ppl_orcid,
                                         people.list.db$ppl_orcid,
                                         sensitivity = 1,
                                         min = 1)
  }else{matchs.orcid = data.frame(match = 'a',priority = 0)[-1,]}
  
  
  matchs.coauthor.name = identify.fuzzy.matchs(pers.to.test$ppl_coauthor_name, 
                                          people.list.db$ppl_coauthor_name,
                                          sensitivity = 1)
  
  matchs.lastname = identify.fuzzy.matchs(pers.to.test$ppl_lastname, 
                                          people.list.db$ppl_lastname,
                                          sensitivity = 2.5) 
  
  matchs.email = identify.fuzzy.matchs(pers.to.test$ppl_email, 
                                       people.list.db$ppl_email,
                                       sensitivity = 1) 
  
  if(length(people.list.db$ppl_second_email|>discard(is.na))>0){
    matchs.second.email = identify.fuzzy.matchs(pers.to.test$ppl_email, 
                                                people.list.db$ppl_second_email|>
                                                  discard(is.na),
                                                sensitivity = 1)
  }else{matchs.second.email = data.frame(match = 'a',priority = 0)[-1,]}
  
  if(!is.na(pers.to.test$ppl_second_email)){
    matchs.email.2 = identify.fuzzy.matchs(pers.to.test$ppl_second_email, 
                                           people.list.db$ppl_email |> 
                                             discard(is.na),
                                           sensitivity = 1) 
    matchs.email = matchs.email |>
      add_row(matchs.email.2)
    
    if(length(people.list.db$ppl_second_email|>discard(is.na))>0){
      matchs.second.email.2 = identify.fuzzy.matchs(pers.to.test$ppl_second_email, 
                                                    people.list.db$ppl_second_email |> 
                                                      discard(is.na),
                                                    sensitivity = 1)
      matchs.second.email = matchs.second.email |>
        add_row(matchs.second.email.2)
    } 
  }
  
  m = 
    right_join(people.list.db, 
               matchs.orcid, 
               by = c('ppl_orcid' = 'match')) |>
    add_row(
      right_join(people.list.db,
                 matchs.coauthor.name,
                 by = c('ppl_coauthor_name' = 'match'))) |>
    add_row(
      right_join(people.list.db,
                 matchs.lastname, 
                 by = c('ppl_lastname' = 'match'))) |>
    add_row(
      right_join(people.list.db,
                 matchs.email, 
                 by = c('ppl_email' = 'match'))) |>
    add_row(
      right_join(people.list.db,
                 matchs.second.email, 
                 by = c('ppl_second_email' = 'match'))) |>
    relocate(priority, .before = ppl_id) |> 
    arrange(desc(priority)) |> 
    distinct(ppl_id, .keep_all = T)
  
  if(nrow(m)>0){
    cat('There is/are match(es)\n\nPerson to add: \n')
    print.data.frame(pers.to.test |> data.frame(), row.names = F)
    cat("\nMatch(es) in the database:\n")
    print.data.frame(m |> data.frame(), row.names = F)
    cat('\n(Note:\nmatches are order by priority. 1 = full match -> 0.1 = low match')
  }else{
    cat('There is no match in the database, a new entry will be created\n')
    app = sqlAppendTable(con = connec,
                         table = 'people',
                         value = pers.to.test,
                         row.names = F)
    dbExecute(con = connec,
              statement = app)
    
    db.people.id = 
      tbl(connec, 'people') |>
      dplyr::select(ppl_id) |>
      collect() |>
      max()
    cat('The new entry was added to the database')
    return(
      pers.to.test |>
        mutate(ppl_id = db.people.id) |>
        relocate(ppl_id, .before = ppl_firstname)
    )
  }
  
  confirm.1 = 'n'
  while(confirm.1 != 'y'){
    action = 
      readline('What do you like to do? 
0: add to the database new entry
1: update a database entry
2: use a database entry')
    if(action %in% paste0(0:2)){
      cat(paste0("You will ", 
                 if(action == '0'){"add a new entry to the database"
                 }else if(action == '1'){'update the database entry'
                 }else if(action == '2'){'use the database entry'}
      ))
      confirm.1 = readline('Confirm your choice (y/n): \n')
    }else{
      cat('CHOICE NOT CONFORM')
    }
    }
    if(action == '0'){
      app = sqlAppendTable(con = connec,
                           table = 'people',
                           value = pers.to.test,
                           row.names = F)
      dbExecute(con = connec,
                statement = app)
      
      db.people.id = 
        tbl(connec, 'people') |>
        dplyr::select(ppl_id) |>
        collect() |>
        max()
      
      cat('The new entry was added to the database')
    }else{
      id.valid = F
      while(id.valid == F) {
        db.people.id = 
          readline(paste0('Which database entry do you like to ',
                          if(action == '1'){'update? '}else{'use? '},
                          '\nentrer the ppl_id from the database'))
        if(db.people.id %in% m$ppl_id){
          cat(paste0("You will use the ppl_id: ", db.people.id))
          confirm.2 = readline('Confirm your choice (y/n): \n')
          if(confirm.2 == 'y'){id.valid = T}
        }else{cat('CHOICE NOT CONFORM')}
      }
      if(action == 1){
        
        cat("Please run the update.database function and use the update.people function!")
      }
    }
  return(
    pers.to.test |>
      mutate(ppl_id = db.people.id) |>
      relocate(ppl_id, .before = ppl_firstname)
  )
}

push.people = 
  function(people.df){
    added =
      map_df(
      .x = 1:nrow(people.df),
      .f = ~push.person(
        pers.to.test = people.df[.x,],
        people.list.db = 
          tbl(connec, 'people') |> 
          collect()
      )
    ) 
    return(added)
  }

# people_added = push.people(people.df = DATASETS$people)

#### >> 1.2. Locations ####
# loc.to.test = DATASETS$location
# loc.list.db =
#   tbl(connec, 'locations') |>
#   collect()

push.loc = function(
    loc.to.test, 
    loc.list.db){
  
  m = loc.list.db[
    paste0(
    round(as.numeric(loc.list.db$loc_long),4),'_',
    round(as.numeric(loc.list.db$loc_lat),4)) ==
      paste0(
        round(as.numeric(loc.to.test$loc_long),4),'_',
        round(as.numeric(loc.to.test$loc_lat),4))
      ,] |> 
    mutate(priority = 1) |>
    relocate(priority, .before = loc_long)
  
  if(nrow(m)>0){
    cat('There is/are match(es)\n\nlocations to add: \n')
    print.data.frame(loc.to.test |> data.frame(), row.names = F)
    cat("\nMatch(es) in the database:\n")
    print.data.frame(m |> data.frame(), row.names = F)
    cat('\n(Note:\nmatches are order by priority. 1 = full match -> 0.1 = low match')
  }else{
    cat('There is no match in the database, a new entry will be created\n')
    app = sqlAppendTable(con = connec,
                         table = 'locations',
                         value = loc.to.test,
                         row.names = F)
    dbExecute(con = connec,
              statement = app)
    
    cat('The new entry was added to the database')
    return(
      loc.to.test
    )
  }
  
  confirm.1 = 'n'
  while(confirm.1 != 'y'){
    action = 
      readline('What do you like to do? 
1: update a database entry
2: use a database entry')
    if(action %in% paste0(1:2)){
      cat(paste0("You will ", 
                 if(action == '1'){'update the database entry'
                 }else if(action == '2'){'use the database entry'}
      ))
      confirm.1 = readline('Confirm your choice (y/n): \n')
    }else{
      cat('CHOICE NOT CONFORM')
    }
  }
    id.valid = F
    if(action == 1){
      
      cat("Please prepare the updated vector and use the update.people function!")
    }
  return(
    loc.to.test
  )
}

push.locations = 
  function(location.df){
    added =
      map_df(
        .x = 1:nrow(location.df),
        .f = ~push.loc(
          loc.to.test = location.df[.x,],
          loc.list.db = 
            tbl(connec, 'locations') |> 
            collect()
        )
      ) 
    return(added)
  }

# added.locations = push.locations(location.df = DATASETS$location)

#### >> 1.3. Experiments ####
# exp.to.test = DATASETS$experiments
# exp.list.db =
#   tbl(connec, 'experiments') |>
#   collect()

push.exp = function(
    exp.to.test, 
    exp.list.db){
  
  matchs.exp.name = identify.fuzzy.matchs(exp.to.test$exp_name, 
                                               exp.list.db$exp_name,
                                               sensitivity = 1)
  
  if(!is.na(exp.to.test$exp_doi) & 
     length(exp.list.db$exp_doi|>discard(is.na))>0){
    matchs.doi = identify.fuzzy.matchs(exp.to.test$exp_doi,
                                         exp.list.db$exp_doi,
                                         sensitivity = 1,
                                         min = 1)
  }else{matchs.doi = data.frame(match = 'a',priority = 0)[-1,]}
  
  m = 
    right_join(exp.list.db, 
               matchs.exp.name, 
               by = c('exp_name' = 'match')) |>
    add_row(
      right_join(exp.list.db,
                 matchs.doi,
                 by = c('exp_doi' = 'match'))) |>
    relocate(priority, .before = exp_id) |> 
    arrange(desc(priority)) |> 
    distinct(exp_id, .keep_all = T)
  
  if(nrow(m)>0){
    cat('There is/are match(es)\n\nExperiments to add: \n')
    print.data.frame(exp.to.test |> data.frame(), row.names = F)
    cat("\nMatch(es) in the database:\n")
    print.data.frame(m |> data.frame(), row.names = F)
    cat('\n(Note:\nmatches are order by priority. 1 = full match -> 0.1 = low match')
  }else{
    cat('There is no match in the database, a new entry will be created\n')
    app = sqlAppendTable(con = connec,
                         table = 'experiments',
                         value = exp.to.test,
                         row.names = F)
    dbExecute(con = connec,
              statement = app)
    
    db.exp.id = 
      tbl(connec, 'experiments') |>
      dplyr::select(exp_id) |>
      collect() |>
      max()
    cat('The new entry was added to the database')
    return(
      exp.to.test |>
        mutate(exp_id = db.exp.id) |>
        relocate(exp_id, .before = exp_name)
    )
  }
  
  confirm.1 = 'n'
  while(confirm.1 != 'y'){
    action = 
      readline('What do you like to do? 
0: add to the database new entry
1: update a database entry
2: use a database entry')
    if(action %in% paste0(0:2)){
      cat(paste0("You will ", 
                 if(action == '0'){"add a new entry to the database"
                 }else if(action == '1'){'update the database entry'
                 }else if(action == '2'){'use the database entry'}
      ))
      confirm.1 = readline('Confirm your choice (y/n): \n')
    }else{
      cat('CHOICE NOT CONFORM')
    }
  }
  if(action == '0'){
    app = sqlAppendTable(con = connec,
                         table = 'experiments',
                         value = exp.to.test,
                         row.names = F)
    dbExecute(con = connec,
              statement = app)
    
    db.exp.id = 
      tbl(connec, 'experiments') |>
      dplyr::select(exp_id) |>
      collect() |>
      max()
    
    cat('The new entry was added to the database')
  }else{
    id.valid = F
    while(id.valid == F) {
      db.exp.id = 
        readline(paste0('Which database entry do you like to ',
                        if(action == '1'){'update? '}else{'use? '},
                        '\nentrer the exp_id from the database'))
      if(db.exp.id %in% m$exp_id){
        cat(paste0("You will use the exp_id: ", db.exp.id))
        confirm.2 = readline('Confirm your choice (y/n): \n')
        if(confirm.2 == 'y'){id.valid = T}
      }else{cat('CHOICE NOT CONFORM')}
    }
    if(action == 1){
      cat("Please prepare the updated vector and use the update.people function!")
    }
  }
  return(
    exp.to.test |>
      mutate(exp_id = db.exp.id) |>
      relocate(exp_id, .before = exp_name)
  )
}

push.experiments = 
  function(experiment.df){
    added =
      map_df(
        .x = 1:nrow(experiment.df),
        .f = ~push.exp(
          exp.to.test = experiment.df[.x,],
          exp.list.db = 
            tbl(connec, 'experiments') |> 
            collect()
        )
      ) 
    return(added)
  }

# experiments_added = push.experiments(experiment.df = exp.to.test)

#### >> 1.4 Sites ####
# site.to.test = DATASETS$sites
# site.list.db = tbl(connec, 'sites') |> collect()

push.site = function(
    site.to.test, 
    site.list.db){
  
  site.to.test$exp_id = 
    experiments_added$exp_id[experiments_added$exp_name == site.to.test$exp_id] |>
    as.integer()
  
  site.list.db =
    site.list.db |>
    mutate(key = paste0('(',round(loc_long,4), ')-(', round(loc_lat,4),')'))
  
  site.to.test =
    site.to.test |> 
    mutate(key = paste0('(',round(loc_long,4), ')-(', round(loc_lat,4),')'))
  
  matchs.locations = site.list.db |> 
    filter(key %in% site.to.test$key)
  
  matchs.name = 
    identify.fuzzy.matchs(site.to.test$site_owner_id,
                          site.list.db$site_owner_id, 
                          sensitivity = 1)
  
  m = 
    matchs.locations |>
        mutate(priority = .9) |> 
    add_row(
      right_join(site.list.db,
                 matchs.name, 
                 by = c('site_owner_id' = 'match'))) |> 
    relocate(priority, .before = site_id) |> 
    dplyr::select(-key) |> 
    arrange(desc(priority)) |> 
    distinct(site_id, .keep_all = T)
  
  
  if(nrow(m)>0){
    cat('There is/are match(es)\n\nSites to add: \n')
    print.data.frame(site.to.test |> data.frame(), row.names = F)
    cat("\nMatch(es) in the database:\n")
    print.data.frame(m |> data.frame(), row.names = F)
    cat('\n(Note:\nmatches are order by priority. 1 = full match -> 0.1 = low match')
  }else{
    cat('There is no match in the database, a new entry will be created\n')
    app = sqlAppendTable(con = connec,
                         table = 'sites',
                         value = site.to.test |> select(-key),
                         row.names = F)
    dbExecute(con = connec,
              statement = app)
    
    db.site.id = 
      tbl(connec, 'sites') |>
      dplyr::select(site_id) |>
      collect() |>
      max()
    cat('The new entry was added to the database')
    return(
      site.to.test |>
        mutate(site_id = db.site.id) |>
        relocate(site_id, .before = exp_id) |> 
        select(-key)
    )
  }
  
  confirm.1 = 'n'
  while(confirm.1 != 'y'){
    action = 
      readline('What do you like to do? 
0: add to the database new entry
1: update a database entry
2: use a database entry')
    if(action %in% paste0(0:2)){
      cat(paste0("You will ", 
                 if(action == '0'){"add a new entry to the database"
                 }else if(action == '1'){'update the database entry'
                 }else if(action == '2'){'use the database entry'}
      ))
      confirm.1 = readline('Confirm your choice (y/n): \n')
    }else{
      cat('CHOICE NOT CONFORM')
    }
  }
  if(action == '0'){
    app = sqlAppendTable(con = connec,
                         table = 'sites',
                         value = site.to.test |> select(-key),
                         row.names = F)
    dbExecute(con = connec,
              statement = app)
    
    db.site.id = 
      tbl(connec, 'sites') |>
      dplyr::select(site_id) |>
      collect() |>
      max()
    
    cat('The new entry was added to the database')
  }else{
    id.valid = F
    while(id.valid == F) {
      db.site.id = 
        readline(paste0('Which database entry do you like to ',
                        if(action == '1'){'update? '}else{'use? '},
                        '\nentrer the site_id from the database'))
      if(db.site.id %in% m$site_id){
        cat(paste0("You will use the site_id: ", db.site.id))
        confirm.2 = readline('Confirm your choice (y/n): \n')
        if(confirm.2 == 'y'){id.valid = T}
      }else{cat('CHOICE NOT CONFORM')}
    }
    if(action == 1){
      cat("Please prepare the updated vector and use the update.people function!")
    }
  }
  return(
    site.to.test |>
      mutate(site_id = db.site.id) |>
      relocate(site_id, .before = exp_id) |> 
      select(-key)
  )
}

push.sites = 
  function(site.df){
    added =
      map_df(
        .x = 1:nrow(site.df),
        .f = ~push.site(
          site.to.test = site.df[.x,],
          site.list.db = 
            tbl(connec, 'sites') |> 
            collect()
        )
      ) 
    return(added)
  }

# sites_added = push.sites(site.to.test)

#### >> 1.5 Loggers ####
# logger.to.test = DATASETS$loggers
# logger.list.db =
#   tbl(connec, 'loggers') |>
#   collect()

push.logger = function(
    logger.to.test, 
    logger.list.db){
  
  matchs.logger.owner.id = 
    identify.fuzzy.matchs(logger.to.test$log_owner_id, 
                                          logger.list.db$log_owner_id,
                                          sensitivity = 1)
  
  if(!is.na(logger.to.test$log_serial_number) & 
     length(logger.to.test$log_serial_number|>discard(is.na))>0){
    matchs.serial.number = identify.fuzzy.matchs(logger.to.test$log_serial_number,
                                                 logger.list.db$log_serial_number,
                                       sensitivity = 1,
                                       min = 1)
  }else{matchs.serial.number = data.frame(match = 'a', priority = 0)[-1,]}
  
  m = 
    right_join(logger.list.db, 
               matchs.logger.owner.id, 
               by = c('log_owner_id' = 'match')) |>
    add_row(
      right_join(logger.list.db,
                 matchs.serial.number,
                 by = c('log_serial_number' = 'match'))) |>
    relocate(priority, .before = log_id) |> 
    arrange(desc(priority)) |> 
    distinct(log_id, .keep_all = T)
  
  if(nrow(m)>0){
    cat('There is/are match(es)\n\nLoggers to add: \n')
    print.data.frame(logger.to.test |> data.frame(), row.names = F)
    cat("\nMatch(es) in the database:\n")
    print.data.frame(m |> data.frame(), row.names = F)
    cat('\n(Note:\nmatches are order by priority. 1 = full match -> 0.1 = low match')
  }else{
    cat('There is no match in the database, a new entry will be created\n')
    app = sqlAppendTable(con = connec,
                         table = 'loggers',
                         value = logger.to.test,
                         row.names = F)
    dbExecute(con = connec,
              statement = app)
    
    db.log.id = 
      tbl(connec, 'loggers') |>
      dplyr::select(log_id) |>
      collect() |>
      max()
    cat('The new entry was added to the database')
    return(
      logger.to.test |>
        mutate(log_id = db.log.id) |>
        relocate(log_id, .before = lgf_id)
    )
  }
  
  confirm.1 = 'n'
  while(confirm.1 != 'y'){
    action = 
      readline('What do you like to do? 
0: add to the database new entry
1: update a database entry
2: use a database entry')
    if(action %in% paste0(0:2)){
      cat(paste0("You will ", 
                 if(action == '0'){"add a new entry to the database"
                 }else if(action == '1'){'update the database entry'
                 }else if(action == '2'){'use the database entry'}
      ))
      confirm.1 = readline('Confirm your choice (y/n): \n')
    }else{
      cat('CHOICE NOT CONFORM')
    }
  }
  if(action == '0'){
    app = sqlAppendTable(con = connec,
                         table = 'loggers',
                         value = logger.to.test,
                         row.names = F)
    dbExecute(con = connec,
              statement = app)
    
    db.logger.id = 
      tbl(connec, 'loggers') |>
      dplyr::select(log_id) |>
      collect() |>
      max()
    
    cat('The new entry was added to the database')
  }else{
    id.valid = F
    while(id.valid == F) {
      db.logger.id = 
        readline(paste0('Which database entry do you like to ',
                        if(action == '1'){'update? '}else{'use? '},
                        '\nentrer the log_id from the database'))
      if(db.logger.id %in% m$log_id){
        cat(paste0("You will use the log_id: ", db.logger.id))
        confirm.2 = readline('Confirm your choice (y/n): \n')
        if(confirm.2 == 'y'){id.valid = T}
      }else{cat('CHOICE NOT CONFORM')}
    }
    if(action == 1){
      cat("Please prepare the updated vector and use the update function!")
    }
  }
  return(
    logger.to.test |>
      mutate(log_id = db.logger.id) |>
      relocate(log_id, .before = lgf_id)
  )
}


push.loggers = 
  function(logger.df){
    added =
      map_df(
        .x = 1:nrow(logger.df),
        .f = ~push.logger(
          logger.to.test = logger.df[.x,],
          logger.list.db = 
            tbl(connec, 'loggers') |> 
            collect()
        )
      ) 
    return(added)
  }

# loggers_added = push.loggers(logger.df = logger.to.test)

#### >> 1.6 Meta_ts ####
# meta.ts.to.test = DATASETS$meta_ts
# meta.ts.list.db = tbl(connec, 'meta_ts') |> collect()

push.meta.ts = function(
    meta.ts.to.test, 
    meta.ts.list.db){

  meta.ts.to.test$site_id = 
    sites_added$site_id[sites_added$site_owner_id == meta.ts.to.test$site_id] |>
    as.integer()
  
  meta.ts.to.test$log_id = 
    loggers_added$log_id[loggers_added$log_owner_id == meta.ts.to.test$log_id] |>
    as.integer()
  
  # Controls
  matchs.name = 
    identify.fuzzy.matchs(meta.ts.to.test$mts_owner_id,
                          meta.ts.list.db$mts_owner_id, 
                          sensitivity = 1)
  
  m = 
    right_join(meta.ts.list.db,
                 matchs.name, 
                 by = c('mts_owner_id' = 'match')) |> 
    relocate(priority, .before = mts_id) |> 
    arrange(desc(priority)) |> 
    distinct(mts_id, .keep_all = T)
  
  
  if(nrow(m)>0){
    cat('There is/are match(es)\n\nmeta.ts to add: \n')
    print.data.frame(meta.ts.to.test |> data.frame(), row.names = F)
    cat("\nMatch(es) in the database:\n")
    print.data.frame(m |> data.frame(), row.names = F)
    cat('\n(Note:\nmatches are order by priority. 1 = full match -> 0.1 = low match')
  }else{
    cat('There is no match in the database, a new entry will be created\n')
    app = sqlAppendTable(con = connec,
                         table = 'meta_ts',
                         value = meta.ts.to.test,
                         row.names = F)
    dbExecute(con = connec,
              statement = app)
    
    db.meta.ts.id = 
      tbl(connec, 'meta_ts') |>
      dplyr::select(mts_id) |>
      collect() |>
      max()
    
    cat('The new entry was added to the database')
    return(
      meta.ts.to.test |>
        mutate(mts_id = db.meta.ts.id) |>
        relocate(mts_id, .before = site_id)
    )
  }
  
  confirm.1 = 'n'
  while(confirm.1 != 'y'){
    action = 
      readline('What do you like to do? 
0: add to the database new entry
1: update a database entry
2: use a database entry')
    if(action %in% paste0(0:2)){
      cat(paste0("You will ", 
                 if(action == '0'){"add a new entry to the database"
                 }else if(action == '1'){'update the database entry'
                 }else if(action == '2'){'use the database entry'}
      ))
      confirm.1 = readline('Confirm your choice (y/n): \n')
    }else{
      cat('CHOICE NOT CONFORM')
    }
  }
  if(action == '0'){
    app = sqlAppendTable(con = connec,
                         table = 'meta_ts',
                         value = meta.ts.to.test,
                         row.names = F)
    dbExecute(con = connec,
              statement = app)
    
    db.meta.ts.id = 
      tbl(connec, 'meta_ts') |>
      dplyr::select(mts_id) |>
      collect() |>
      max()
    
    cat('The new entry was added to the database')
  }else{
    id.valid = F
    while(id.valid == F) {
      db.meta.ts.id = 
        readline(paste0('Which database entry do you like to ',
                        if(action == '1'){'update? '}else{'use? '},
                        '\nentrer the meta.ts_id from the database'))
      if(db.meta.ts.id %in% m$mts_id){
        cat(paste0("You will use the meta.ts_id: ", db.meta.ts.id))
        confirm.2 = readline('Confirm your choice (y/n): \n')
        if(confirm.2 == 'y'){id.valid = T}
      }else{cat('CHOICE NOT CONFORM')}
    }
    if(action == 1){
      cat("Please prepare the updated vector and use the update.people function!")
    }
  }
  return(
    meta.ts.to.test |>
      mutate(mts_id = as.numeric(db.meta.ts.id)) |>
      relocate(mts_id, .before = site_id)
  )
}

push.meta.ts.s = 
  function(meta.ts.df){
    added =
      map_df(
        .x = 1:nrow(meta.ts.df),
        .f = ~{
          print(.x)
          push.meta.ts(
          meta.ts.to.test = meta.ts.df[.x,],
          meta.ts.list.db = 
            tbl(connec, 'meta_ts') |> 
            collect()
          )}
        ) 
    return(added)
  }

# meta_ts_added = push.meta.ts.s(meta.ts.to.test)

#### >> 1.7 clim_timeseries ####
# no control needed
# clim.ts.to.test = DATASETS$clim_ts
# clim.ts.list.db = tbl(connec, 'clim_ts') |> collect()

push.clim.ts = function(
    clim.ts.to.push, 
    clim.ts.list.db){
  
  action = 
    readline('Do you like to push the dataset? (y/n): \n')
  if(action == 'y'){
    
    for(i in 1:nrow(clim.ts.to.push)){
      clim.ts.to.test = clim.ts.to.push[i,]
      clim.ts.to.test$mts_id = tbl(connec, 'meta_ts') |> pull('mts_id') |> unique() |> sample(1)
      clim.ts.to.test = clim.ts.to.test |> select(mts_id , ctf_id ,
                                                  cts_timestamp_utc = cts_timestamp,
                                                  cts_value , cts_sensor_id , 
                                                  cts_bexis_id, cts_update)
      # clim.ts.to.test$mts_id[i] = 
      #   meta_ts_added$mts_id[meta_ts_added$mts_owner_id == clim.ts.to.test$mts_id[i]] |>
      #   as.integer()
      
      app = sqlAppendTable(con = connec,
                           table = 'clim_ts',
                           value = clim.ts.to.test,
                           row.names = F)
      dbExecute(con = connec,
                statement = app)
    }
    cat("Your data have been pushed to the database!")
  }else if(action == 'n'){
    cat("The data will not be updated!")
  }else{
    cat("CHOICE NOT CONFORM")
  }
}

# clim_ts_added = push.clim.ts(clim.ts.to.test)

# meta_ts_people
# vegetation_survey
# survey_people
# species_composition
# species

#### 2. Pushing the dataset ####
data.path = '1-data/bexis_download/'
listing.bexis = read.csv(paste0(data.path, 'listing-bexis.csv'))
list.files = list.files(data.path)
list.zip.files = list.files[grep('BEXIS.zip', list.files)]

i = 1 # Here add the dataset to push

DATASETS = 
  formate.dataset.to.sql(i=1, 
                         list.zip.files, 
                         listing.bexis, data.path) # Function from 2-4-1-3

people_added = push.people(people.df = DATASETS$people)

added.locations = push.locations(location.df = DATASETS$location)

experiments_added = push.experiments(experiment.df = DATASETS$experiments)

sites_added = push.sites(DATASETS$sites)

loggers_added = push.loggers(logger.df = DATASETS$loggers)

meta_ts_added = push.meta.ts.s(DATASETS$meta_ts)

clim_ts_added = push.clim.ts(DATASETS$clim_ts)
