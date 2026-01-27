# in the demo data, we assume each pen consists 4 automated nests

data(meta_demo)
data(hand_demo)
data(ani_info_demo)





######### find the best off time to adjust the autonest clock (optional) #########
time_cols <- c("Time_start", "Time_end")
hand_demo[, (time_cols) := lapply(.SD, as_hms), .SDcols = time_cols]
hand_demo[, Date := as.Date(Date, format = "%Y-%m-%d")]
collect_min <- as.numeric(median(hand_demo$Time_end - hand_demo$Time_start) / 60)

Nestnumbers <- sort(unique(meta_demo$Nestnumber))

ot_res <- find_best_ot_min(meta_demo, hand_demo, from = NULL, to = NULL,
                 hand_nest_colnames = c("Nest1", "Nest2", "Nest3", "Nest4"),
                 Nestnumbers = Nestnumbers,
                 num_per_pen = 4,
                 ot_min_range = seq(-100, 100, by = 50),
                 timezone = "Europe/Berlin", collect_min = collect_min,
                 fakeegg = TRUE)
ot_min <- ot_res[1,1]





######### prepare meta file ready for downstream analysis #########
meta_prep <- prep_data(meta_demo, ot_min = ot_min) 

## add col "pen" to group Nests
table(meta_prep$Nestnumber)
meta_prep <- Make_pen_number(meta_prep, Nestnumbers = Nestnumbers, num_per_pen = 4)
## filter out extra nests (pen = NA)
meta_prep <- meta_prep[!is.na(pen)]
table(meta_prep$pen)

## add filter flags: flagged = TRUE
meta_prep <- Add_filter_flag(meta_prep, 
                        start_time = 4, end_time = 21, 
                        thrd_laydiff = 22, timezone = "Europe/Berlin",
                        select_flags = c(1:5))
### check flagged eggsignal percentage
table(meta_prep[Eggsignal > 0, F_combined]) / nrow(meta_prep[Eggsignal > 0])





######### prepare merged egg counts (hand + autonest) for downstream analysis #########
negg <- get_good_hand_eggcount(meta_prep, hand_demo, from = NULL, to = NULL,
                                 hand_nest_colnames = c("Nest1", "Nest2", "Nest3", "Nest4"),
                                 Nestnumbers = Nestnumbers, 
                                 num_per_pen = 4,
                                 timezone = "Europe/Berlin", 
                                 collect_min = collect_min,
                                 fakeegg = TRUE)
## check proportion of matches between autonest filtered eggs and hand eggs
negg[, diff_ah := Nauto_filt - Nhand]
table(negg$diff_ah) / nrow(negg)

# subset and rename required columns
negg <- negg[, c("Date", "Pen", "Nest", "Nhand", "Nfloor", "dtm_hpre", "dtm_h"), with = FALSE]
setnames(negg, "dtm_hpre", "date_time_pre")
setnames(negg, "dtm_h", "date_time")





######### loop / parallel: pens & dates for egg imputation #########
pens <- sort(unique(meta_prep$pen))

CV = TRUE
type = c("assign", "prob")
for (norm_result in type){
  
  CV_res <- list()
  pen_res <- list()
  impeggs_res <- list()
  
  
  for(p in pens){
    
    pen_meta <- meta_prep[pen == p]
    
    # get imputed laying dates based on chickens laying pattern
    ani_impeggs <- list()
    for (i in unique(pen_meta[ani != 0, ani])) {
      
      res <- get_imputed_laydates(
        pen_meta = pen_meta, aniid = i, max_gap_hour = 200, 
        nstate = 4, max_iter = 20, stop_pegg = 8, min_pegg = 1.5, thrd_laydiffh = 24,
        plot = FALSE)
      
      ani_impeggs[[length(ani_impeggs) + 1]] <- res
    }
    ani_impeggs <- rbindlist(ani_impeggs, fill = TRUE)
    ## save
    impeggs_res[[length(impeggs_res) + 1]] <- ani_impeggs
    
    
    # output a list
    pen_assign <- process_pen(p, negg, meta_prep, ani_info_demo,
                              index_floornest = 99,
                              ani_impeggs, weight_imd = 1.2, factor_imd = 1.2, p_perc_dups = 50,
                              nearby_days = 2, penalty_weight = 0.3, max_penalty = 0.6, 
                              weight_nl = 0.8, factor_nl = 1,
                              weight_np = 0.2, factor_np = 1,
                              norm_result = norm_result)
    
    ## save
    pen_res[[length(pen_res) + 1]] <- pen_assign
    
    
    # for CV, output a data table
    if (CV == TRUE) {
      pen_trusted_dat <- pen_assign$pen_trusted
      
      pen_CV_res <- CV_pen(pen_trusted_dat, reps = 3, k = 5, seed = as.numeric(Sys.Date()),
                           p, negg, meta_prep, ani_info_demo,
                           index_floornest = 99,
                           ani_impeggs, weight_imd = 1.2, factor_imd = 1.2, p_perc_dups = 50,
                           nearby_days = 2, penalty_weight = 0.3, max_penalty = 0.6, 
                           weight_nl = 0.8, factor_nl = 1,
                           weight_np = 0.2, factor_np = 1,
                           norm_result = norm_result)
      
      ## save
      CV_res[[length(CV_res) + 1]] <- pen_CV_res
    }
    
  }
  
  # write all pen results by one of the normalising type (assign or prob)
  filepath <- sprintf("results_%s_%s.RData",norm_result,Sys.Date())
  filepath
  save(pen_res, CV_res, file = filepath)  
  
}

# write HMM imputed laying dates based on chickens laying pattern for reference 
from_d <- as.character(min(meta_prep$Date))
to_d <- as.character(max(meta_prep$Date))
filepath <- sprintf("HMM_%s_%s_%s.txt",Sys.Date(),from_d,to_d)
filepath
write.table(rbindlist(impeggs_res, fill = TRUE), filepath, quote = FALSE, row.names = FALSE, col.names = TRUE, sep = "\t")





######### a helper function to plot HMM imputed laying pattern of a single animal #########
plot_laying_ani(meta_prep[pen == 1], 
                aniid = 11,
                impute = TRUE,
                p = 1)





######### check results #########
types = c("prob","assign")
res_list <- list()
for (type in types) {
  file <- list.files(pattern = type)
  file <- sort(file, decreasing = TRUE)[1]
  print(file)
  # Load file into a temporary environment
  tmp_env <- new.env()
  load(file, envir = tmp_env)
  
  # Extract and save the loaded objects
  obj_names <- ls(tmp_env)
  loaded_objs <- mget(obj_names, envir = tmp_env)
  res_list[[type]] <- loaded_objs
}
str(res_list, max.level = 1)


# check CV
resCV <- rbindlist(lapply(names(res_list), function(type_name) {
  CV_res <- res_list[[type_name]]$CV_res
  
  tmp <- rbindlist(lapply(seq_along(CV_res), function(i) {
    dat <- CV_res[[i]]
    tab <- table(factor(dat$topx, levels = c(TRUE, FALSE))) / nrow(dat)
    data.table(pen = i, TF = names(tab), prop = as.numeric(tab))
  }))
  
  tmp[, type := type_name]
  tmp
}))

## plot CV
ggplot(resCV) +
  geom_point(aes(x = factor(pen), y = prop, color = TF), size = 2) +
  facet_wrap(~ type, nrow = 2) +
  ylim(c(0, 1)) +
  labs(title = sprintf("CV results"))

resCV[type == "assign" & TF == "TRUE", summary(prop)]
resCV[type == "prob" & TF == "TRUE", summary(prop)]


# check prob
respp <- rbindlist(lapply(names(res_list), function(type_name) {
  pen_res <- res_list[[type_name]]$pen_res
  
  tmp <- rbindlist(lapply(pen_res, function(pen) {
    pen$pen_priors
  }))
  
  tmpp <- tmp[, .(sum_prior = sum(prior)), by = ani]

  tmpp[, type := type_name]
  tmpp
}))

table(respp$type)

## plot prob
order <- respp[type == "assign"][order(sum_prior), unique(ani)]
other_ani <- setdiff(unique(respp$ani), order)
if (length(other_ani) > 0) {
  other_ani <- respp[ani %in% other_ani & type == "prob"][order(sum_prior), unique(ani)]
}
ani_levels <- c(order, other_ani)
respp$ani <- factor(respp$ani, levels = ani_levels)

ggplot(respp) +
  geom_point(aes(x = ani, y = sum_prior, color = type), size = 1.5, alpha = 0.7) +
  labs(title = "sum of assigned prior") 


# trusted
restt <- rbindlist(lapply(res_list[["prob"]]$pen_res, function(pen) {
  pen$pen_trusted
}))





######### get EN numbers #########
from_d <- as.character(min(restt$date))
to_d <- as.character(max(restt$date))
cat("Period: ",from_d,"to",to_d)
# imputed
t1 <- restt[, .N, by = ani]
t2 <- respp[type == "assign", .(ani, sum_prior)]
setnames(t2, "sum_prior","sum_prior_a")
t3 <- respp[type == "prob", .(ani, sum_prior)]
setnames(t3, "sum_prior","sum_prior_p")

eggnum <- merge(t1, t2, by = "ani", all = TRUE)
eggnum <- merge(eggnum, t3, by = "ani", all = TRUE)
eggnum[is.na(eggnum)] <- 0
eggnum[, EN_imp_a := N + sum_prior_a]
eggnum[, EN_imp_p := N + sum_prior_p]

# autonest
## raw
t1 <- meta_prep[Date >= from_d & Date <= to_d & Eggsignal > 0][
  ani != 0, .(EN = .N), by = ani
]
## + filter flags
t2 <- meta_prep[Date >= from_d & Date <= to_d & Eggsignal > 0 & F_combined != TRUE][
  , .(EN = .N), by = ani
]

# merge all EN
tab <- merge(eggnum, t1[, .(ani, EN)], by = "ani", all = TRUE)
setnames(tab, "EN", "EN_auto")
tab <- merge(tab, t2[, .(ani, EN)], by = "ani", all = TRUE)
setnames(tab, "EN", "EN_autofilt")

tab[is.na(tab)] <- 0
if (nrow(tab) != nrow(eggnum)) {warning("check data size")}
filepath <- sprintf("%s_EN_%s_%s.txt",Sys.Date(),from_d,to_d)
filepath
write.table(tab,
            filepath, col.names = TRUE, row.names = FALSE, quote = FALSE, sep = "\t")
