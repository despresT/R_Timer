
timer <- function (x, week_graph = FALSE) {

    if(!x %in% c('on', 'off', 'pause', 'resume')) stop("x must be 'on', 'off', 'pause' or 'resume'")

    library(lubridate)
    library(data.table)
                                        #  library(digest)

    timer_table <- fread('timer_table.csv')

    last_state <- fread('last_state')



    if(x == last_state[, state] && Sys.Date() == last_state[, date]) {
        stop(paste0('You press two times timer("', x, '")'))
    }

        if(x == 'off' && last_state[, state] == 'on') {
        warning(paste0('You did not done any pause today?'))
    }



    if(dim( timer_table)[1] != 0) {
        timer_table[, date := as.Date(date)]
        timer_table[, working_time := as.numeric(working_time)]
        timer_table[, working_time := as.difftime(working_time, units =  unique(timer_table[, units]))]
        timer_table[, pause_time := as.numeric(pause_time)]
        timer_table[, pause_time := as.difftime(pause_time, units =  unique(timer_table[, units]))]
        timer_table[, resume_time := as.character(resume_time)]
        timer_table[, unclass_resume_time := as.numeric(unclass_resume_time)]

                                        #   timer_table[, end_time := format(ymd_hms(end_time),"%H:%M:%S")]
                                        # timer_table[, end_time := as.POSIXct(end_time, format = "%H:%M:%S")]

    }

                                        #  if(  digest(timer_table, algo = 'sha1') == "82aa84af8ce1c6264a09e6f7cbe194020e9573da") {
    if(dim( timer_table)[1] == 0) {
        timer_table <- NULL
    }

    if(x == 'on') {

        if( !is.null(timer_table)) {

            if(any(is.na(timer_table[ dim(timer_table)[1] - 1, ][, working_time]),
                   is.na(timer_table[ dim(timer_table)[1], ][, working_time]))) {
                stop(paste0('You press two times timer("on"). Need to rollback to row number ', dim(timer_table)[1] - 1))
            }
        }

        started_time <-   Sys.time()
        unclass_started_time <- unclass(started_time)

        today_table <- data.table(year = year(started_time), week = week(started_time), date = date(started_time), unclass_date = unclass_started_time ,
                                  starting_job_time = paste(hour(started_time),minute(started_time),second(started_time), sep=":"),
                                  unclass_end_time = as.numeric(NA),
                                  end_time = NA,
                                  working_time = as.difftime('NA', units = 'hours'),
                                  units = units(as.difftime('NA', units = 'hours')),
                                  work_threshold_per_week = NA,
                                  summary_work_time_in_week = NA,
                                  pause_time = as.difftime('NA', units = 'hours'),
                                  unclass_start_pause_time = NA,
                                  pause_time_real = NA,
                                  pause_added = FALSE,
                                  resume_time = NA,
                                  unclass_resume_time = as.numeric(NA)
                                  )

        timer_table <- rbind(timer_table, today_table)

    } else {

        if(x == 'pause') {

        if(!is.na(last(timer_table)[, working_time])) {
            stop(paste0('You press two times timer("', x, '"). Need to run again timer("', ifelse(x == "off", "on", "resume"), '")'))
        }


        pause_time1 <-  Sys.time()

        unclass_pause_time <- unclass( pause_time1)

        if(!any(last(timer_table[, date]) == date( pause_time1), last(timer_table[, date]) == date( pause_time1) - 1)) {
            stop('The Sys.time date is not matching the last row of "timer_table"')
        }

        working_time_off <-  difftime(as.POSIXlt(unclass_pause_time, origin = "1970-01-01"), as.POSIXlt(last(timer_table[, unclass_date]), origin = "1970-01-01"), units="hours")

        today_table_off <- data.table(year = last(timer_table[, year]), week = last(timer_table[, week]), date = last(timer_table[, date]), unclass_date = last(timer_table[, unclass_date]),
                                      starting_job_time =last(timer_table[,  starting_job_time]),
                                      unclass_end_time = NA,
                                      end_time =  paste(hour( pause_time1),minute( pause_time1),second( pause_time1), sep=":"),
                                      working_time = working_time_off,
                                      units = units(working_time_off),
                                      work_threshold_per_week = ifelse(timer_table[, sum(as.numeric(working_time), na.rm = T), by = 'week'][week == last(timer_table[, week])][, V1] <= 35, "below", "above"),
                                      summary_work_time_in_week = timer_table[, sum(as.numeric(working_time), na.rm = T), by = 'week'][week == last(timer_table[, week])][, V1],
                                      pause_time = as.difftime('NA', units = 'hours'),
                                      unclass_start_pause_time =  unclass_pause_time,
                                      pause_time_real = paste(hour(pause_time1),minute(pause_time1),second(pause_time1), sep=":"),
                                      pause_added = FALSE,
                                      resume_time = NA,
                                      unclass_resume_time = as.numeric(NA)
                                            )

        if(dim(timer_table)[1] == 1) {

            timer_table <- today_table_off
        } else {
            timer_table <- rbind(  timer_table[ 1:dim(timer_table)[1] - 1],  today_table_off)
        }
      #       timer_table[, work_hour_per_week := rep(timer_table[, sum(as.numeric(working_time)), by = 'week'][, V1] ,
      #                                       summary(as.factor(timer_table[, week])))]
      #
      #  timer_table[, work_threshold_per_week := rep(ifelse(timer_table[, sum(as.numeric(working_time)), by = 'week'][, V1] < 35, "below", "above"),
      #             summary(as.factor(timer_table[, week])))]
        }


        if(x == "resume" && last_state[, state]== 'pause') {
            resume_time <-  Sys.time()
            unclass_resume_time <- unclass(resume_time)
            pause_time1 <-  abs(difftime( as.POSIXlt(last(timer_table[, unclass_start_pause_time]), origin = "1970-01-01"), as.POSIXlt(unclass_resume_time, origin = "1970-01-01"), units="hours"))

            set(timer_table, i = dim(timer_table)[1], j = 'pause_time', value = pause_time1)
            set(timer_table, i = dim(timer_table)[1], j = 'resume_time', value = paste(hour(resume_time),minute(resume_time),second(resume_time), sep=":"))
            set(timer_table, i = dim(timer_table)[1], j = 'unclass_resume_time', value = unclass_resume_time)




        }

        if(x == 'off') {
            if(is.na(last(timer_table)[, working_time])) {
                stop(paste0('You press two times timer("', x, '"). Need to run again timer("', ifelse(x == "off", "on", "resume"), '")'))
            }


        end_time_off <-  Sys.time()

        unclass_end_time <- unclass(end_time_off)

        if(!any(last(timer_table[, date]) == date(end_time_off), last(timer_table[, date]) == date(end_time_off) - 1)) {
            stop('The Sys.time date is not matching the last row of "timer_table"')
        }

        working_time_off <-  difftime(as.POSIXlt(unclass_end_time, origin = "1970-01-01"), as.POSIXlt(last(timer_table[, unclass_date]), origin = "1970-01-01"), units="hours")

        working_time_without_pause <- working_time_off - (abs(difftime (as.POSIXlt(last(timer_table[, unclass_start_pause_time]), origin = "1970-01-01"), as.POSIXlt(last(timer_table[, unclass_resume_time]), origin = "1970-01-01"), units="hours")))



        today_table_off <- data.table(year = last(timer_table[, year]), week = last(timer_table[, week]), date = last(timer_table[, date]), unclass_date = last(timer_table[, unclass_date]),
                                      starting_job_time =last(timer_table[,  starting_job_time]),
                                      unclass_end_time = unclass_end_time,
                                      end_time =  paste(hour(end_time_off), minute(end_time_off), second(end_time_off), sep=":"),
                                      working_time = working_time_without_pause,
                                      units = units(working_time_without_pause),
                                      work_threshold_per_week = ifelse(timer_table[, sum(as.numeric(working_time), na.rm = T), by = 'week'][week == last(timer_table[, week])][, V1] <= 35, "below", "above"),
                                      summary_work_time_in_week = timer_table[, sum(as.numeric(working_time), na.rm = T), by = 'week'][week == last(timer_table[, week])][, V1],
                                      pause_time = last(timer_table[, pause_time]),
                                      unclass_start_pause_time = last(timer_table[, unclass_start_pause_time]),
                                      pause_time_real = last(timer_table[, pause_time_real]),
                                      pause_added = TRUE,
                                      resume_time = last(timer_table[, resume_time]),
                                      unclass_resume_time = last(timer_table[,  unclass_resume_time])
                                      )


        if(dim(timer_table)[1] == 1) {
            timer_table <- today_table_off
        } else {
            timer_table <- rbind(  timer_table[ 1:dim(timer_table)[1] - 1],  today_table_off)
        }

            }







    }


    timer_table[,  summary_work_time_in_week := rep(timer_table[, sum(as.numeric(working_time), na.rm = T), by = 'week'][, V1],
                                                 summary(as.factor(timer_table[, week])))]

    fwrite(timer_table, 'timer_table.csv')
    fwrite(data.table(state = x, date = Sys.Date()), file = 'last_state')

    if (week_graph == TRUE) {
        library(ggplot2)



   #  timer_table[, work_hour_per_week := rep(timer_table[, sum(as.numeric(working_time)), by = 'week'][, V1] ,
   #                                          summary(as.factor(timer_table[, week])))]
   #
   #     timer_table[, work_threshold_per_week := rep(ifelse(timer_table[, sum(as.numeric(working_time)), by = 'week'][, V1] < 35, "below", "above"),
   #                summary(as.factor(timer_table[, week])))]



        ggplot( timer_table, aes(as.factor(week),  as.numeric(working_time))) +
            geom_col( aes(fill = work_threshold_per_week)) +
                                        geom_hline(yintercept = 35, col = 'black') +
                                        scale_fill_manual(values = c('red', 'grey'))

        }


}








