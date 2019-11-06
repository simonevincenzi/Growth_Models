gomp_vB_TMB_parall_validation_choice_rand_choice_cov.f = function(data_region_df = data_region_df, 
                                                             mark_all_pop_df = mark_all_pop_df,
                                                             age_cut = age_cut,cont = cont,  
                                                             data_compl = data_compl, 
                                                             rand_eff_n = rand_eff_n, 
                                                             linf_var = linf_var, 
                                                             k_var = k_var, 
                                                             t0_var = t0_var, 
                                                             mod_id = mod_id,
                                                             valid = 0,
                                                             seed = seed) {
  
  
  model_rsq = tibble(rsq_gam_train = NA)

  model_rsq$rsq_gam_train =  NA
  model_rsq$rsq_gam_test =  NA
  model_rsq$logRMSE_gam_test = NA
  model_rsq$logRMSE_gam_train = NA
  model_rsq$RMSE_gam_test = NA
  model_rsq$RMSE_gam_train = NA
  model_rsq$mean_error_gam_train = NA
  model_rsq$max_error_gam_test = NA
  model_rsq$id_gam = NA
  model_rsq$age_gam = NA
  model_rsq$model = paste("mod",rand_eff_n,"rand","l",linf_var,"k",k_var,"t0",t0_var, sep = "_")
  model_rsq$std = NA
  model_rsq$conv = NA
  model_rsq$AIC = NA
  model_rsq$func = "gomp"
  model_rsq$cont = cont
  model_rsq$n_pop = length(unique(data_region_df$Pop))
  
  
  
  data_growth = data_region_df  
    
    data_growth = data_growth %>%
    filter(., !is.na(Length), Length > 0, Month == 9) %>%
    arrange(Mark, Age)
  
  
    
  if (valid == 1) {
    
    data_growth = filter(data_growth, (is.na(mark_age_kept) & is.na(mark_valid)) | 
                           (mark_age_kept == 1 & mark_valid == 1))
    
    
    cohort_df = select(data_region_df, Mark, Cohort) %>% group_by(Mark) %>% 
      summarise(Cohort = Cohort[1])
    
    data_region_df = select(data_region_df, Mark, Length, Pop, Age, mark_age_kept,mark_valid)
    
    
    }  
  
  if (valid == 0) {
    
    cohort_df = select(data_region_df, Mark, Cohort) %>% group_by(Mark) %>% 
      summarise(Cohort = Cohort[1])
    
    data_region_df = select(data_region_df, Mark, Length, Pop, Age, mark_age_kept,mark_valid)
    
  }  
    
  # Define covariates (always Const, i.e. no covariates)
  
  ### Constant ####
  
  if (linf_var == "Const") {
    linf.formula =  ~ 1
    linf_ind_sd = 1
  }
  
  if (linf_var == "Species + Pop + Cohort") {
      linf.formula =  ~ Species + Pop + Cohort
      linf_ind_sd = 1
  }
    
    
  if (linf_var == "Species * Pop * Cohort") {
      linf.formula =  ~ Species * Pop * Cohort
      linf_ind_sd = 1
  }
    
  if (linf_var == "Pop") {
      linf.formula =  ~ Pop
      linf_ind_sd = 1
  } 
  
  if (linf_var == "Species + Pop") {
      linf.formula =  ~ Species + Pop
      linf_ind_sd = 1
  }
    
  if (linf_var == "Species * Pop") {
      linf.formula =  ~ Species * Pop
      linf_ind_sd = 1
  }
    
  if (linf_var == "Pop + Cohort") {
      linf.formula =  ~ Pop + Cohort
      linf_ind_sd = 1
  }
    
  if (linf_var == "Pop * Cohort") {
      linf.formula =  ~ Pop * Cohort
      linf_ind_sd = 1
  }
    
  if (linf_var == "Species") {
      linf.formula =  ~ Species
      linf_ind_sd = 1
  }
    
  if (linf_var == "Cohort") {
      linf.formula =  ~ Cohort
      linf_ind_sd = 1
  }
    
    
    
  if (k_var == "Const") {
      k.formula =  ~ 1
      k_ind_sd = 1
    }
    
  if (k_var == "Species + Pop + Cohort") {
      k.formula =  ~ Species + Pop + Cohort
      k_ind_sd = 1
    }
    
    
  if (k_var == "Species * Pop * Cohort") {
      k.formula =  ~ Species * Pop * Cohort
      k_ind_sd = 1
    }
    
  if (k_var == "Pop") {
      k.formula =  ~ Pop
      k_ind_sd = 1
    } 
    
  if (k_var == "Species + Pop") {
      k.formula =  ~ Species + Pop
      k_ind_sd = 1
    }
    
   if (k_var == "Species * Pop") {
      k.formula =  ~ Species * Pop
      k_ind_sd = 1
    }
    
  if (k_var == "Pop + Cohort") {
      k.formula =  ~ Pop + Cohort
      k_ind_sd = 1
    }
    
   if (k_var == "Pop * Cohort") {
      k.formula =  ~ Pop * Cohort
      k_ind_sd = 1
    }
    
   if (k_var == "Species") {
      k.formula =  ~ Species
      k_ind_sd = 1
    }
    
   if (k_var == "Cohort") {
      k.formula =  ~ Cohort
      k_ind_sd = 1
    }
    
    if (t0_var == "Const") {
      t0.formula =  ~ 1
      t0_ind_sd = 1
    }
    
    if (t0_var == "Species + Pop + Cohort") {
      t0.formula =  ~ Species + Pop + Cohort
      t0_ind_sd = 1
    }
    
    
    if (t0_var == "Species * Pop * Cohort") {
      t0.formula =  ~ Species * Pop * Cohort
      t0_ind_sd = 1
    }
    
    if (t0_var == "Pop") {
      t0.formula =  ~ Pop
      t0_ind_sd = 1
    } 
    
    if (t0_var == "Species + Pop") {
      t0.formula =  ~ Species + Pop
      t0_ind_sd = 1
    }
    
    if (t0_var == "Species * Pop") {
      t0.formula =  ~ Species * Pop
      t0_ind_sd = 1
    }
    
    if (t0_var == "Pop + Cohort") {
      t0.formula =  ~ Pop + Cohort
      t0_ind_sd = 1
    }
    
    if (t0_var == "Pop * Cohort") {
      t0.formula =  ~ Pop * Cohort
      t0_ind_sd = 1
    }
    
    if (t0_var == "Species") {
      t0.formula =  ~ Species
      t0_ind_sd = 1
    }
    
    if (t0_var == "Cohort") {
      t0.formula =  ~ Cohort
      t0_ind_sd = 1
    } 
      
  
  linf.mm = model.matrix(linf.formula,data=data_growth)
  k.mm = model.matrix(k.formula,data=data_growth)
  t0.mm = model.matrix(t0.formula,data = data_growth)
  
  X = linf.mm
  X_k0 = k.mm
  X_t0 = t0.mm
  
  
  n_linf_ind_sd = ncol(X)
  n_k_ind_sd = ncol(X_k0)
  
  
  
  
  
  
  
  #####
  ## ordering Marks (tag of titles)
  
  diffmark  = diff(as.numeric(data_growth$Mark))
  diffmark  = ifelse(diffmark==0,0,1)
  
  ### writes the dat file - remember that write_dat is in ADMButil.s
  
  ###It changed to dat_write
  
  
  #---------------------- TMB starts here
  
  res_df = tibble(days = NA, logRMSE = NA, 
                  RMSE = NA, MAE = NA, MaxAE = NA,
                  Rsq = NA,
                  worst_pred_season_title_id = NA,
                  worst_pred_season_desc = NA,
                  worst_pred_diff = NA,
                  cont = cont)
  
  
  member <- function(x,y) !is.na(match(x,y))  #useful function
  
  pred_df = data.frame(Mark = unique(data_growth$Mark),Age = NA, u = NA,v = NA,
                       w = NA, linf = NA,
                       k = NA,
                       t0 = NA,
                       pred_mean = NA,
                       pred_std = NA,
                       pred_lci = NA,
                       pred_uci = NA,
                       max_age = NA,
                       model = NA,
                       data_compl = NA,
                       mod_id = NA,
                       cont = NA,
                       AIC =  NA,
                       valid = NA,
                       Mark_cor = NA,
                       Pop = NA,
                       Length = NA,
                       mark_age_kept = NA,
                       mark_valid = NA,
                       func = NA,
                       n_pop = NA)
  
  
  require(TMB)
  if (file.exists("scripts/m_grow3_TMB_daily_gomp.o") == F){
    compile("scripts/m_grow3_TMB_daily_gomp.cpp")
  }
  dyn.load(dynlib("scripts/m_grow3_TMB_daily_gomp"))

  
  if (rand_eff_n == 3) {
    
    dat = list(linf_ind_sd = linf_ind_sd,
               n_linf_ind_sd = n_linf_ind_sd,
               k_ind_sd = k_ind_sd,
               n_k_ind_sd = n_k_ind_sd,
               n=nrow(data_growth),
               Length=1.0*data_growth$Length,
               age=1.0*data_growth$Age,
               total_marks=length(unique(data_growth$Mark)),
               start=c(1,which(diffmark>0)+1)-1,              # -1 because TMB is zero-indexed
               stop=c(which(diffmark>0),nrow(data_growth))-1,              # -1 because TMB is zero-indexed
               q=ncol(X),
               X=as(X, "dgTMatrix"),
               q_k0=ncol(X_k0),
               X_k0=as(X_k0, "dgTMatrix"),
               q_t0=ncol(X_t0),
               X_t0=as(X_t0, "dgTMatrix")
    )
    
    log_sigma_v_set = 0.8
    
    
    parameters=list(
      beta=rep(0.2,dat$q),
      beta_k0=c(0.5,rep(0.25,dat$q_k0-1)),
      beta_t0=rep(-1,dat$q_t0),
      log_sigma_u=-1.5,
      log_sigma_v=-1.5,
      log_sigma_w=-1.5,
      log_sigma=2*3.5,
      u=rep(0,dat$total_marks),
      v=rep(0,dat$total_marks),
      w=rep(0,dat$total_marks)
    )
    
    # Set parameter bounds (typically based on biological knowledge)
    L0 = list(
      beta=rep(-10,dat$q),
      beta_k0=rep(-40,dat$q_k0),
      beta_t0=rep(-10,dat$q_t0),
      log_sigma_u=-10,
      log_sigma_v=-10,
      log_sigma_w=-10,
      log_sigma=-10
    )  
    U0 = list(
      beta=rep(15,dat$q),
      beta_k0=rep(15,dat$q_k0),
      beta_t0=rep(15,dat$q_t0),
      log_sigma_u=1,
      #log_sigma_v=1,
      log_sigma_v=log_sigma_v_set,
      log_sigma_w=1,
      log_sigma=6
    )  
    
    # Set inactive parameters
    # map=list(log_sigma_w=factor(NA),w=factor(rep(NA,dat$total_marks)))
    
    # Remove inactive parameters from bounds
    #member <- function(x,y) !is.na(match(x,y))
    #L = unlist(L0[!member(names(L0),names(map))])
    #U = unlist(U0[!member(names(U0),names(map))])
    
    L = unlist(L0)
    U = unlist(U0)
    
    # Build the model
    obj <- MakeADFun(data=dat,parameters = parameters, random=c("u","v","w"),DLL="m_grow3_TMB_daily_gomp")
    opt = NULL
    
    # pred_df = data.frame(Mark = unique(data_growth$Mark),
    #                      
    #                      pred_mean = NA,
    #                      pred_std = NA,
    #                      pred_lci = NA,
    #                      pred_uci = NA)
    
    
    tmb_rep = data.frame()
    
    count_trials = 1
    beta_start_val = 0.2
    
    
    
    # Fit the model
    opt = tryCatch(
      {
        opt = nlminb(obj$par,obj$fn,obj$gr,lower=L,upper=U,control=list(eval.max=8000,iter.max=8000), verbose = F)
        
        #return(list(opt = opt, tmb_rep = tmb_rep))
        
      },
      error = function(e) {
        
        opt = data.frame(convergence = 2)
        return(opt)
        
      })
    
    
    
    
    if(opt$convergence > 0) {
      
      count_trials = count_trials + 1
      
      opt = tryCatch(
        {
          opt = nlminb(obj$par,obj$fn,obj$gr,lower=L,upper=U,control=list(eval.max=9000,iter.max=9000), verbose = F)
          
        },
        error = function(e) {
          
          opt = data.frame(convergence = 2)
          return(opt)
          
        })
      
      
    }
    
    if(opt$convergence > 0) {
      
      count_trials = count_trials + 1
      beta_start_val = 0.25
      
      parameters=list(
        beta=rep(0.25,dat$q),
        beta_k0=c(0.5,rep(0.25,dat$q_k0-1)),
        beta_t0=rep(-1,dat$q_t0),
        log_sigma_u=-1.5,
        log_sigma_v=-1.5,
        log_sigma_w=-1.5,
        log_sigma=2,
        u=rep(0,dat$total_marks),
        v=rep(0,dat$total_marks),
        w=rep(0,dat$total_marks)
      )
      
      # Set parameter bounds (typically based on biological knowledge)
      L0 = list(
        beta=rep(-10,dat$q),
        #beta=rep(0,dat$q),
        beta_k0=rep(-40,dat$q_k0),
        beta_t0=rep(-50,dat$q_t0),
        log_sigma_u=-10,
        log_sigma_v=-10,
        log_sigma_w=-10,
        log_sigma=-10
      )
      U0 = list(
        beta=rep(15,dat$q),
        beta_k0=rep(15,dat$q_k0),
        beta_t0=rep(15,dat$q_t0),
        log_sigma_u=1,
        #log_sigma_v=1,
        log_sigma_v=log_sigma_v_set,
        log_sigma_w=1,
        log_sigma=6
      )
      
      # Set inactive parameters
      #map=list(log_sigma_w=factor(NA),w=factor(rep(NA,dat$total_marks)))
      
      # Remove inactive parameters from bounds
      #member <- function(x,y) !is.na(match(x,y))
      #L = unlist(L0[!member(names(L0),names(map))])
      #U = unlist(U0[!member(names(U0),names(map))])
      
      L = unlist(L0)
      U = unlist(U0)
      
      # Build the model
      obj <- MakeADFun(data=dat,parameters = parameters, random=c("u","v","w"),DLL="m_grow3_TMB_daily_gomp")
      opt = NULL
      
      opt = tryCatch(
        {
          opt = nlminb(obj$par,obj$fn,obj$gr,lower=L,upper=U,control=list(eval.max=9000,iter.max=9000), verbose = F)
          
        },
        error = function(e) {
          
          opt = data.frame(convergence = 2)
          return(opt)
          
        })
      
      
    }
    
    
    if(opt$convergence > 0) {
      
      count_trials = count_trials + 1
      beta_start_val = 0.3
      
      parameters=list(
        beta=rep(0.3,dat$q),
        beta_k0=c(0.5,rep(0.25,dat$q_k0-1)),
        beta_t0=rep(-1,dat$q_t0),
        log_sigma_u=-1.5,
        log_sigma_v=-1.5,
        log_sigma_w=-1.5,
        log_sigma=2,
        u=rep(0,dat$total_marks),
        v=rep(0,dat$total_marks),
        w=rep(0,dat$total_marks)
      )
      
      # Set parameter bounds (typically based on biological knowledge)
      L0 = list(
        beta=rep(-10,dat$q),
        #beta=rep(0,dat$q),
        beta_k0=rep(-40,dat$q_k0),
        beta_t0=rep(-50,dat$q_t0),
        log_sigma_u=-10,
        log_sigma_v=-10,
        log_sigma_w=-10,
        log_sigma=-10
      )
      U0 = list(
        beta=rep(15,dat$q),
        beta_k0=rep(15,dat$q_k0),
        beta_t0=rep(15,dat$q_t0),
        log_sigma_u=1,
        #log_sigma_v=1,
        log_sigma_v=log_sigma_v_set,
        log_sigma_w=1,
        log_sigma=6
      )
      
      # Set inactive parameters
      #map=list(log_sigma_w=factor(NA),w=factor(rep(NA,dat$total_marks)))
      
      # Remove inactive parameters from bounds
      #member <- function(x,y) !is.na(match(x,y))
      #L = unlist(L0[!member(names(L0),names(map))])
      #U = unlist(U0[!member(names(U0),names(map))])
      
      L = unlist(L0)
      U = unlist(U0)
      
      # Build the model
      obj <- MakeADFun(data=dat,parameters = parameters, random=c("u","v","w"),DLL="m_grow3_TMB_daily_gomp")
      opt = NULL
      
      opt = tryCatch(
        {
          opt = nlminb(obj$par,obj$fn,obj$gr,lower=L,upper=U,control=list(eval.max=9000,iter.max=9000), verbose = F)
          
        },
        error = function(e) {
          
          opt = data.frame(convergence = 2)
          return(opt)
          
        })
      
      
    }
  } else if (rand_eff_n == 2) {
    
    dat = list(linf_ind_sd = linf_ind_sd,
               n_linf_ind_sd = n_linf_ind_sd,
               k_ind_sd = k_ind_sd,
               n_k_ind_sd = n_k_ind_sd,
               n=nrow(data_growth),
               Length=1.0*data_growth$Length,
               age=1.0*data_growth$Age,
               total_marks=length(unique(data_growth$Mark)),
               start=c(1,which(diffmark>0)+1)-1,              # -1 because TMB is zero-indexed
               stop=c(which(diffmark>0),nrow(data_growth))-1,              # -1 because TMB is zero-indexed
               q=ncol(X),
               X=as(X, "dgTMatrix"),
               q_k0=ncol(X_k0),
               X_k0=as(X_k0, "dgTMatrix"),
               q_t0=ncol(X_t0),
               X_t0=as(X_t0, "dgTMatrix")
    )
    
    log_sigma_v_set = 0.8
    
    
    parameters=list(
      beta=rep(0.2,dat$q),
      beta_k0=c(0.5,rep(0.25,dat$q_k0-1)),
      beta_t0=rep(-1,dat$q_t0),
      log_sigma_u=-1.5,
      log_sigma_v=-1.5,
      log_sigma_w=-1.5,
      log_sigma=2*3.5,
      u=rep(0,dat$total_marks),
      v=rep(0,dat$total_marks),
      w=rep(0,dat$total_marks)
    )
    
    # Set parameter bounds (typically based on biological knowledge)
    L0 = list(
      beta=rep(-10,dat$q),
      beta_k0=rep(-40,dat$q_k0),
      beta_t0=rep(-50,dat$q_t0),
      log_sigma_u=-10,
      log_sigma_v=-10,
      log_sigma_w=-10,
      log_sigma=-10
    )  
    U0 = list(
      beta=rep(10,dat$q),
      beta_k0=rep(50,dat$q_k0),
      beta_t0=rep(10,dat$q_t0),
      log_sigma_u=10,
      #log_sigma_v=1,
      log_sigma_v=log_sigma_v_set,
      log_sigma_w=10,
      log_sigma=10
    )  
    
    # Set inactive parameters
    map=list(log_sigma_w=factor(NA),w=factor(rep(NA,dat$total_marks)))
    
    # Remove inactive parameters from bounds
    member <- function(x,y) !is.na(match(x,y))
    L = unlist(L0[!member(names(L0),names(map))])
    U = unlist(U0[!member(names(U0),names(map))])
    
    
    # Build the model
    obj <- MakeADFun(data=dat,parameters = parameters, random=c("u","v"),DLL="m_grow3_TMB_daily_gomp", map = map)
    opt = NULL
    
    # pred_df = data.frame(Mark = unique(data_growth$Mark),
    #                      
    #                      pred_mean = NA,
    #                      pred_std = NA,
    #                      pred_lci = NA,
    #                      pred_uci = NA)
    # 
    
    
    
    
    tmb_rep = data.frame()
    
    count_trials = 1
    beta_start_val = 0.2
    
    
    
    # Fit the model
    opt = tryCatch(
      {
        opt = nlminb(obj$par,obj$fn,obj$gr,lower=L,upper=U,control=list(eval.max=8000,iter.max=8000), verbose = F)
        
        #return(list(opt = opt, tmb_rep = tmb_rep))
        
      },
      error = function(e) {
        
        opt = data.frame(convergence = 2)
        return(opt)
        
      })
    
    
    
    
    if(opt$convergence > 0) {
      
      count_trials = count_trials + 1
      
      opt = tryCatch(
        {
          opt = nlminb(obj$par,obj$fn,obj$gr,lower=L,upper=U,control=list(eval.max=9000,iter.max=9000), verbose = F)
          
        },
        error = function(e) {
          
          opt = data.frame(convergence = 2)
          return(opt)
          
        })
      
      
    }
    
    if(opt$convergence > 0) {
      
      count_trials = count_trials + 1
      beta_start_val = 0.25
      
      parameters=list(
        beta=rep(0.25,dat$q),
        beta_k0=c(0.5,rep(0.25,dat$q_k0-1)),
        beta_t0=rep(-1,dat$q_t0),
        log_sigma_u=-1.5,
        log_sigma_v=-1.5,
        log_sigma_w=-1.5,
        log_sigma=2,
        u=rep(0,dat$total_marks),
        v=rep(0,dat$total_marks),
        w=rep(0,dat$total_marks)
      )
      
      # Set parameter bounds (typically based on biological knowledge)
      L0 = list(
        beta=rep(-10,dat$q),
        #beta=rep(0,dat$q),
        beta_k0=rep(-40,dat$q_k0),
        beta_t0=rep(-10,dat$q_t0),
        log_sigma_u=-10,
        log_sigma_v=-10,
        log_sigma_w=-10,
        log_sigma=-10
      )
      U0 = list(
        beta=rep(1,dat$q),
        beta_k0=rep(5,dat$q_k0),
        beta_t0=rep(1,dat$q_t0),
        log_sigma_u=1,
        #log_sigma_v=1,
        log_sigma_v=log_sigma_v_set,
        log_sigma_w=1,
        log_sigma=6
      )
      
      # Set inactive parameters
      map=list(log_sigma_w=factor(NA),w=factor(rep(NA,dat$total_marks)))
      
      # Remove inactive parameters from bounds
      member <- function(x,y) !is.na(match(x,y))
      L = unlist(L0[!member(names(L0),names(map))])
      U = unlist(U0[!member(names(U0),names(map))])
      
      # Build the model
      obj <- MakeADFun(data=dat,parameters = parameters, random=c("u","v"),DLL="m_grow3_TMB_daily_gomp",map = map)
      opt = NULL
      
      opt = tryCatch(
        {
          opt = nlminb(obj$par,obj$fn,obj$gr,lower=L,upper=U,control=list(eval.max=9000,iter.max=9000), verbose = F)
          
        },
        error = function(e) {
          
          opt = data.frame(convergence = 2)
          return(opt)
          
        })
      
      
    }
    
    
    if(opt$convergence > 0) {
      
      count_trials = count_trials + 1
      beta_start_val = 0.3
      
      parameters=list(
        beta=rep(0.3,dat$q),
        beta_k0=c(0.5,rep(0.25,dat$q_k0-1)),
        beta_t0=rep(-1,dat$q_t0),
        log_sigma_u=-1.5,
        log_sigma_v=-1.5,
        log_sigma_w=-1.5,
        log_sigma=2,
        u=rep(0,dat$total_marks),
        v=rep(0,dat$total_marks),
        w=rep(0,dat$total_marks)
      )
      
      # Set parameter bounds (typically based on biological knowledge)
      L0 = list(
        beta=rep(-10,dat$q),
        #beta=rep(0,dat$q),
        beta_k0=rep(-40,dat$q_k0),
        beta_t0=rep(-10,dat$q_t0),
        log_sigma_u=-10,
        log_sigma_v=-10,
        log_sigma_w=-10,
        log_sigma=-10
      )
      U0 = list(
        beta=rep(1,dat$q),
        beta_k0=rep(5,dat$q_k0),
        beta_t0=rep(1,dat$q_t0),
        log_sigma_u=1,
        #log_sigma_v=1,
        log_sigma_v=log_sigma_v_set,
        log_sigma_w=1,
        log_sigma=6
      )
      
      # Set inactive parameters
      map=list(log_sigma_w=factor(NA),w=factor(rep(NA,dat$total_marks)))
      
      # Remove inactive parameters from bounds
      member <- function(x,y) !is.na(match(x,y))
      L = unlist(L0[!member(names(L0),names(map))])
      U = unlist(U0[!member(names(U0),names(map))])
      
      # Build the model
      obj <- MakeADFun(data=dat,parameters = parameters, random=c("u","v"),DLL="m_grow3_TMB_daily_gomp", map = map)
      opt = NULL
      
      opt = tryCatch(
        {
          opt = nlminb(obj$par,obj$fn,obj$gr,lower=L,upper=U,control=list(eval.max=9000,iter.max=9000), verbose = F)
          
        },
        error = function(e) {
          
          opt = data.frame(convergence = 2)
          return(opt)
          
        })
      
      
    }
  }
  
  
  saveRDS(opt, "opt.RDS")
  
  if(opt$convergence == 0) {
    
    
    #tmb_rep = summary(sdreport(obj))
    sd_out = sdreport(obj, getReportCovariance=FALSE)
    #sd_out = sdreport(obj,hessian.fixed=obj$he(opt$par), getReportCovariance=FALSE) 
    saveRDS(sd_out, "sd_out.RDS")
    tmb_rep = summary(sd_out)
    
    #tmb_rep = summary(sdreport(obj, getReportCovariance=FALSE))
     
    saveRDS(tmb_rep,"tmb_rep.RDS")
    
    
    tmb_rep_row = rownames(tmb_rep)
    rownames(tmb_rep) = NULL
    tmb_rep = as.data.frame(tmb_rep)
    tmb_rep$par = tmb_rep_row
    colnames(tmb_rep)[2]= "Std_error"
    #tmb_rep$country_desc = unique(data_growth$country_desc)
    #tmb_rep$content_subregion_desc = unique(data_growth$content_subregion_desc)
    tmb_rep = tmb_rep %>%
      mutate(.,snapshot_date = ymd(format(Sys.Date(), "%Y%m%d")))
    tmb_rep$snapshot_date = as.numeric(strftime(tmb_rep$snapshot_date,format = "%Y%m%d"))
    tmb_rep$count_trials = count_trials
    tmb_rep$beta_start_val = beta_start_val
    
    
    
    
    pred_days = 18
    
    days_df = data_growth %>%
      group_by(Mark) %>%
      summarise(max_age = max(Age))
    
    
    if (rand_eff_n == 3) {
    
    pred_df = data.frame(Mark = rep(unique(data_growth$Mark),each = pred_days),
                         year = rep(1:pred_days, length(unique(data_growth$Mark))),
                         u = rep(filter(tmb_rep,par == "u")$Estimate, each = pred_days),
                         v = rep(filter(tmb_rep,par == "v")$Estimate,each = pred_days),
                         w = rep(filter(tmb_rep,par == "w")$Estimate,each = pred_days),
                         linf = rep(filter(tmb_rep,par == "L_inf_vb")$Estimate,each = pred_days),
                         k = rep(filter(tmb_rep,par == "k_vb")$Estimate,each = pred_days),
                         t0 = rep(filter(tmb_rep,par == "t0_vb")$Estimate,each = pred_days),
                         pred_mean = filter(tmb_rep,par == "L_28")$Estimate,
                         pred_std = filter(tmb_rep,par == "L_28")$Std_error,
                         pred_lci = filter(tmb_rep,par == "L_28")$Estimate - 2*filter(tmb_rep,par == "L_28")$Std_error,
                         pred_uci = filter(tmb_rep,par == "L_28")$Estimate + 2*filter(tmb_rep,par == "L_28")$Std_error)
    
    }
    
    if (rand_eff_n == 2) {
      
      pred_df = data.frame(Mark = rep(unique(data_growth$Mark),each = pred_days),
                           year = rep(1:pred_days, length(unique(data_growth$Mark))),
                           u = rep(filter(tmb_rep,par == "u")$Estimate, each = pred_days),
                           v = rep(filter(tmb_rep,par == "v")$Estimate,each = pred_days),
                           pred_mean = filter(tmb_rep,par == "L_28")$Estimate,
                           pred_std = filter(tmb_rep,par == "L_28")$Std_error,
                           pred_lci = filter(tmb_rep,par == "L_28")$Estimate - 2*filter(tmb_rep,par == "L_28")$Std_error,
                           pred_uci = filter(tmb_rep,par == "L_28")$Estimate + 2*filter(tmb_rep,par == "L_28")$Std_error)
      
      
    }
    
    pred_df = left_join(pred_df,days_df,by="Mark")
    rm(days_df)
    
pred_df$model = paste("mod",rand_eff_n,"rand","l",linf_var,"k",k_var,"t0",t0_var, sep = "_")
pred_df$data_compl = data_compl
pred_df$mod_id = mod_id
pred_df$cont = cont
pred_df$AIC = TMBAIC(opt)
pred_df$valid = valid

pred_df = as.data.frame(pred_df) %>% 
  rename(.,Mark_ind = Mark,
         Age = year) %>% 
  left_join(.,mark_all_pop_df) %>% 
  rename(., Mark = Mark_ind) %>% 
  left_join(., data_region_df) %>% 
  left_join(., cohort_df)
    
    
####################

if (valid == 1) {

train_df = select(pred_df, Mark, pred_mean, Length, Pop, Age, Cohort,mark_age_kept,mark_valid) %>% 
  filter(., !is.na(Length)) %>% 
  filter(., (is.na(mark_age_kept) & is.na(mark_valid)) | 
           (mark_age_kept == 1 & mark_valid == 1))
  
train_df$type = "train"  
  

test_df = select(pred_df, Mark, pred_mean, Length, Pop, Age,Cohort, mark_age_kept,mark_valid) %>%
  filter(., !is.na(Length)) %>%
  anti_join(., train_df)
  
test_df$type =  "test"  }


if (valid == 0) {
  
  train_df = select(pred_df, Mark, pred_mean, Length, Pop, Age, Cohort,mark_age_kept,mark_valid) %>% 
    filter(., !is.na(Length)) 
  
  train_df$type = "train"  
  
  
  test_df = train_df
  
  test_df$type =  "test"  }




saveRDS(test_df,"test_df.RDS")
saveRDS(train_df,"train_df.RDS")

SSE.train_gam = sum((train_df$Length-train_df$pred_mean)^2, na.rm = T)
SST.train = sum((train_df$Length-mean(train_df$Length))^2, na.rm = T)
SSE.test_gam = sum((test_df$Length-test_df$pred_mean)^2, na.rm = T)
SST.test = sum((test_df$Length-mean(test_df$Length))^2, na.rm = T)

model_rsq$rsq_gam_train =  1-(SSE.train_gam/SST.train)
model_rsq$rsq_gam_test =  1-(SSE.test_gam/SST.test)






model_rsq$logRMSE_gam_test = Metrics::rmse(log(test_df$Length),
                                           log(test_df$pred_mean))

model_rsq$logRMSE_gam_train = Metrics::rmse(log(train_df$Length),
                                            log(train_df$pred_mean))


model_rsq$RMSE_gam_test = Metrics::rmse(test_df$Length, 
                                        test_df$pred_mean)

model_rsq$RMSE_gam_train = Metrics::rmse(train_df$Length, 
                                         train_df$pred_mean)

model_rsq$max_error_gam_test = max(abs(test_df$Length - 
                                         test_df$pred_mean))

model_rsq$mean_error_gam_train = mean(abs(train_df$Length - 
                                            train_df$pred_mean))




model_rsq$id_gam = test_df %>%
  mutate(.,max_error = abs(test_df$Length - 
                             test_df$pred_mean)) %>%
  arrange(., desc(max_error)) %>%
  top_n(.,1) %>%
  select(.,Mark) %>%
  as.numeric()

model_rsq$age_gam = test_df %>%
  mutate(.,max_error = abs(test_df$Length - 
                             test_df$pred_mean)) %>%
  arrange(., desc(max_error)) %>%
  top_n(.,1) %>%
  select(.,Age) %>%
  as.numeric()


model_rsq$model = paste("mod",rand_eff_n,"rand","l",linf_var,"k",k_var,"t0",t0_var, sep = "_")

model_rsq$std = ifelse(sum(is.na(pred_df$pred_std)) == 0, 1, 0)

model_rsq$conv = opt$convergence

model_rsq$AIC = TMBAIC(opt)

model_rsq$n_pop = length(unique(data_growth$Pop))




#######






 }    
  
  
  # pred_df = pred_df %>% 
  #   left_join(., cohort_df)
  pred_df$func = "gomp"
  pred_df$n_pop = length(unique(data_growth$Pop))
  
  if (!exists("test_df")) {
    
    test_df = data.frame("Mark" = NA, "pred_mean" = NA, "Length" = NA, "Pop" = NA,
                         "Age" = NA, "Cohort", "mark_age_kept" = NA, "mark_valid" = NA,
                         "type" = "test")
  }
  
  
  if (!exists("train_df")) {
    
    train_df = data.frame("Mark" = NA, "pred_mean" = NA, "Length" = NA, "Pop" = NA,
                          "Age" = NA, "Cohort" = NA, "mark_age_kept" = NA, "mark_valid" = NA,
                          "type" = "test")
  }
  
  test_df$model = paste("mod",rand_eff_n,"rand","l",linf_var,"k",k_var,"t0",t0_var, sep = "_")
  test_df$func = "vb"
  test_df$cont = cont
  
  train_df$model = paste("mod",rand_eff_n,"rand","l",linf_var,"k",k_var,"t0",t0_var, sep = "_")
  train_df$func = "gomp"
  train_df$cont = cont
  
  
  return(list("pred_df" = pred_df, "model_rsq" = model_rsq, 
              "test_df" = test_df, "train_df" = train_df, "opt" = opt))
  
  
}
