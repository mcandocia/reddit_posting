library(plyr)
library(dplyr)
library(reshape2)
library(lubridate)
library(glmnet)
library(ggplot2)
library(cetcolor)
library(scales)
library(stringr)

dow = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
hours_ = c(paste0(c(12,1:11), ' AM'), paste0(c(12, 1:11), ' PM'))
rev_frame <- function(x){
  x[,rev(colnames(x))]
}
paste_rev2 <- function(..., sep=" ", collapse=NULL) {
  arg <- c(lapply(list(...), rev), list(sep=sep, collapse=collapse))
  do.call(paste, arg)
}
apply(out, 1, paste_rev2, collapse=".")

default_weekhour_order = apply(expand.grid(hour=paste0(':hour',hours_), dow=paste0('day',dow)), 1, paste_rev2, collapse='')
n_weekhours = 24*7

shift_timezone <- function(x, offset=0){
  if (offset==0)
    return(x)
  if (offset > 0){
    #GMT + offset
    idx1 = (n_weekhours + 1 - offset):n_weekhours
    idx2 = 1:(n_weekhours - offset)
  }
  else{
    #GMT - offset
    idx1 = (1-offset):n_weekhours
    idx2 = 1:(-offset)
  }
  to = c(default_weekhour_order[idx1], default_weekhour_order[idx2])
  x = mapvalues(x, from=default_weekhour_order, to = to)
  x = factor(x, levels = default_weekhour_order)
  return(x)
}

fix_timestamp <- function(x) as.POSIXct(x, tz="GMT", origin='1970-01-01')

fix_hour <- function(x){
  values = c(paste0(c(12,1:11),' AM'), paste0(c(12, 1:11),' PM'))
  res = factor(values[x+1], levels=values)
  return(res)
}

clean_glmnet <- function(model, data, alpha=0.5, family='gaussian', ...){
  x = model.matrix(model, data=data, 
                   contrasts.arg = list(day=contrasts(data$day, contrasts=FALSE),
                                        hour=contrasts(data$hour, contrasts=FALSE))
  )
  y = data[,as.character(model)[2]]
  print(head(y))
  model = glmnet(x, y, family=family, alpha=alpha, ...)
  return(model)
}

construct_day_hour_effects <- function(model, offset=0){
  #offset is used to present data in different timezones; default is GMT
  if ('glmnet' %in% class(model)){
    betas = model$beta
    b = betas[,ncol(betas)]
    rn = rownames(betas)
  }
  else{
    #meh, I will just stick to clean_glm function
    stop("I am too lazy to implement this because glmnet is easier and faster now...")
    b= model$coefficients
    rn = names(b)
  }
  valid_names = grepl('day\\w+:hour.*', rn)
  valid_b = b[valid_names]
  valid_rn = rn[valid_names]
  if (offset != 0){
    valid_rn = shift_timezone(valid_rn, offset)
  }
  summary_frame = data.frame(weekday = gsub('day(\\w+):.*','\\1',valid_rn),
                             hour = gsub('.*:hour(.*)','\\1',valid_rn)) %>%
    mutate( hour = factor(hour, levels=hours_),
            weekday = factor(weekday, levels=dow),
            estimate=valid_b)
  return(summary_frame)
}

load_subreddits <- function(x, no_prefix=FALSE){
  files = dir('.')
  if (!no_prefix)
    files = files[grepl(paste0('.*_',x,'_p[0-9]{2}\\.csv'), files)]
  else
    files = files[grepl(paste0(x,'_p[0-9]{2}\\.csv'), files)]
  data_list = list()
  for (file in files){
    data_list[[file]] = read.csv(file, stringsAsFactors=FALSE)
  }
  data = do.call(rbind, data_list)
  names(data)[names(data)=='f0_'] = 'title_length'
  data = data %>% filter(!duplicated(id)) %>% mutate(created_utc=fix_timestamp(created_utc), #subreddit=factor(subreddit),
                                                     hour=fix_hour(hour(created_utc)), day=weekdays(created_utc), month=factor(month(created_utc)),
                                                     year=factor(year(created_utc)), logscore=log(1+score))
  domains = table(data$domain)
  top_domains = domains[domains > 50 + sqrt(nrow(data))/20]
  data = data %>% mutate(top_domain = factor(ifelse(domain %in% names(top_domains), domain, 'OTHER_DOMAIN'), levels=c('OTHER_DOMAIN', names(top_domains))),
                         day=factor(day), hour=factor(hour))
  return(data)
}

load_random <- function(invert=FALSE, months='all'){
  if (months=='all'){
    mregex = '.*'
  }
  else
    mregex = paste0('(',paste(months, collapse='|'),')')
  #invert = TRUE loads all non-random files
  files = dir('.')
  if (!invert)
    files = files[grepl(paste0(mregex, '_rsample_[0-9]{3}.csv'), files)]
  else
    files = files[grepl('.*_\\w+_p[0-9]{2}\\.csv', files)]
  data_list = list()
  for (file in files){
    data_list[[file]] = read.csv(file, stringsAsFactors=FALSE)
  }
  data = do.call(rbind, data_list)
  names(data)[names(data)=='f0_'] = 'title_length'
  data = data %>% filter(!duplicated(id)) %>% mutate(created_utc=fix_timestamp(created_utc), #subreddit=factor(subreddit),
                                                     hour=fix_hour(hour(created_utc)), day=weekdays(created_utc), month=factor(month(created_utc)),
                                                     year=factor(year(created_utc)), logscore=log(1+score))
  domains = table(data$domain)
  top_domains = domains[domains > 24*7 + sqrt(nrow(data))/15]
  subreddits = table(data$subreddit)
  top_subreddits = subreddits[subreddits > 24*7 + sqrt(nrow(data))/15]
  #print(top_subreddits)
  data = data %>% mutate(top_domain = factor(ifelse(domain %in% names(top_domains), domain, 'OTHER_DOMAIN'), levels=c('OTHER_DOMAIN', names(top_domains))),
                         top_subreddit = factor(ifelse(subreddit %in% names(top_subreddits), subreddit, 'OTHER_SUBREDDIT'), levels=c('OTHER_SUBREDDIT', names(top_subreddits))),
                         hour=factor(hour), day=factor(day))
  return(data)
}

if (FALSE){
 
  
  #load random sample
  randdata = load_random()
  
  rand_model = clean_glmnet(logscore ~ day:hour + top_domain + top_subreddit + title_length + over_18 + selftext_length, 
                            data=randdata)
  
  gc()
  
  rand_model_over50 =clean_glmnet(over50 ~ day:hour + top_domain + top_subreddit + title_length + over_18 + selftext_length, 
                                  data=randdata %>% mutate(over50 = 1*(score >= 50)), family='binomial')
  gc()
  
  rand_model_over500 =clean_glmnet(over500 ~ day:hour + top_domain + top_subreddit + title_length + over_18 + selftext_length, 
                                   data=randdata %>% mutate(over500 = 1*(score >= 500)), family='binomial')
  gc()
  rand_model_march = clean_glmnet(logscore ~ day:hour + top_domain + top_subreddit + title_length + over_18 + selftext_length, 
                                  data=randdata %>% filter(month==3))
  gc()
  rand_model_march_over50 =clean_glmnet(over50 ~ day:hour + top_domain + top_subreddit + title_length + over_18 + selftext_length, 
                                        data=randdata %>% filter(month==3) %>% mutate(over50 = 1*(score >= 50)), family='binomial')
  gc()
  
  rand_model_march_over500 =clean_glmnet(over500 ~ day:hour + top_domain + top_subreddit + title_length + over_18 + selftext_length, 
                                         data=randdata %>% filter(month==3) %>% mutate(over500 = 1*(score >= 500)), family='binomial')
  gc()
  rand_model_june= clean_glmnet(logscore ~ day:hour + top_domain + top_subreddit + title_length + over_18 + selftext_length, 
                                data=randdata %>% filter(month==6))
  gc()
  rand_model_june_over50 =clean_glmnet(over50 ~ day:hour + top_domain + top_subreddit + title_length + over_18 + selftext_length, 
                                       data=randdata%>% filter(month==6) %>% mutate(over50 = 1*(score >= 50)), family='binomial')
  gc()
  
  rand_model_june_over500 =clean_glmnet(over500 ~ day:hour + top_domain + top_subreddit + title_length + over_18 + selftext_length, 
                                        data=randdata %>% filter(month==6) %>% mutate(over500 = 1*(score >= 500)), family='binomial')
  gc()
  
  
  rand_model_params = construct_day_hour_effects(rand_model, offset=5) 
  rand_model_over50_params = construct_day_hour_effects(rand_model_over50, offset=5)
  rand_model_over500_params = construct_day_hour_effects(rand_model_over500, offset=5)
  
  grand_model_params = construct_day_hour_effects(rand_model, offset=0) 
  grand_model_over50_params = construct_day_hour_effects(rand_model_over50, offset=0)
  grand_model_over500_params = construct_day_hour_effects(rand_model_over500, offset=0)
  
  rand_model_march_params = construct_day_hour_effects(rand_model_march, offset=5) 
  rand_model_march_over50_params = construct_day_hour_effects(rand_model_march_over50, offset=5)
  rand_model_march_over500_params = construct_day_hour_effects(rand_model_march_over500, offset=5)
  
  rand_model_june_params = construct_day_hour_effects(rand_model_june, offset=5) 
  rand_model_june_over50_params = construct_day_hour_effects(rand_model_june_over50, offset=5)
  rand_model_june_over500_params = construct_day_hour_effects(rand_model_june_over500, offset=5)
  
  pname = 'diverging_bwr_55-98_c37_n256'
  pal = cet_pal(7, pname)
  
  ggplot(rand_model_params, aes(x=weekday, y=hour, fill=exp(estimate)-1)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect',colors=pal,label=percent)
  
  ggplot(rand_model_over50_params, aes(x=weekday, y=hour, fill=exp(estimate)-1)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect',colors=pal,label=percent)
  
  ggplot(rand_model_over500_params, aes(x=weekday, y=hour, fill=exp(estimate)-1)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect',colors=pal,label=percent)
  
  
  
  ggplot(rand_model_march_params, aes(x=weekday, y=hour, fill=exp(estimate)-1)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect',colors=pal,label=percent)
  
  ggplot(rand_model_march_over50_params, aes(x=weekday, y=hour, fill=exp(estimate)-1)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect',colors=pal,label=percent)
  
  ggplot(rand_model_march_over500_params, aes(x=weekday, y=hour, fill=exp(estimate)-1)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect',colors=pal,label=percent)
  
  
  
  ggplot(rand_model_june_params, aes(x=weekday, y=hour, fill=exp(estimate)-1)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect',colors=pal,label=percent)
  
  ggplot(rand_model_june_over50_params, aes(x=weekday, y=hour, fill=exp(estimate)-1)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect',colors=pal,label=percent)
  
  ggplot(rand_model_june_over500_params, aes(x=weekday, y=hour, fill=exp(estimate)-1)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect',colors=pal,label=percent)
  
  
  #let's look at defaults
  
  
  default_subreddits = c('announcements','Art','AskReddit','askscience','aww','blog','books','creepy','dataisbeautiful','DIY','Documentaries','EarthPorn','explainlikeimfive',
                         'food','funny','Futurology','gadgets','gaming','GetMotivated','gifs','history','IAmA','InternetIsBeautiful','Jokes','LifeProTips','listentothis',
                         'mildlyinteresting','movies','Music','news','nosleep','nottheonion','OldSchoolCool','personalfinance','philosophy','photoshopbattles','pics',
                         'science','Showerthoughts','space','sports','television','tifu','todayilearned','UpliftingNews','videos','worldnews')
  
  default_rand_model = clean_glmnet(logscore ~ day : hour + top_domain + top_subreddit + title_length + over_18 + selftext_length, 
                                    data=randdata %>% filter(subreddit %in% default_subreddits))
  
  default_rand_over50_model = clean_glmnet(over50 ~ day : hour + top_domain + top_subreddit + title_length + over_18 + selftext_length, 
                                           data=randdata %>% filter(subreddit %in% default_subreddits) %>% mutate(over50 = 1*(score >= 50)), family='binomial')
  
  default_rand_over500_model = clean_glmnet(over500 ~ day : hour + top_domain + top_subreddit + title_length + over_18 + selftext_length, 
                                            data=randdata %>% filter(subreddit %in% default_subreddits) %>% mutate(over500 = 1*(score >= 500)), family='binomial')
  
  gc()
  
  default_march_rand_model = clean_glmnet(logscore ~ day : hour + top_domain + top_subreddit + title_length + over_18 + selftext_length, 
                                          data=randdata %>% filter(subreddit %in% default_subreddits & month==3))
  
  default_march_rand_over50_model = clean_glmnet(over50 ~ day:hour + top_domain + top_subreddit + title_length + over_18 + selftext_length, 
                                                 data=randdata %>% filter(subreddit %in% default_subreddits & month==3) %>% mutate(over50 = 1*(score >= 50)), family='binomial')
  
  default_march_rand_over500_model = clean_glmnet(over500 ~ day:hour + top_domain + top_subreddit + title_length + over_18 + selftext_length, 
                                                  data=randdata %>% filter(subreddit %in% default_subreddits & month==3) %>% mutate(over500 = 1*(score >= 500)), family='binomial')
  
  gc()
  
  default_june_rand_model = clean_glmnet(logscore ~ day:hour + top_domain + top_subreddit + title_length + over_18 + selftext_length, 
                                         data=randdata %>% filter(subreddit %in% default_subreddits & month==6))
  
  default_june_rand_over50_model = clean_glmnet(over50 ~ day:hour + top_domain + top_subreddit + title_length + over_18 + selftext_length, 
                                                data=randdata %>% filter(subreddit %in% default_subreddits & month==6) %>% mutate(over50 = 1*(score >= 50)), family='binomial')
  
  default_june_rand_over500_model = clean_glmnet(over500 ~ day:hour + top_domain + top_subreddit + title_length + over_18 + selftext_length, 
                                                 data=randdata %>% filter(subreddit %in% default_subreddits & month==6) %>% mutate(over500 = 1*(score >= 500)), family='binomial')
  
  gc()
  
  d_a_rp = construct_day_hour_effects(default_rand_model, offset=5)
  d_a_r50p = construct_day_hour_effects(default_rand_over50_model, offset=5)
  d_a_r500p = construct_day_hour_effects(default_rand_over500_model, offset=5)
  
  gd_a_rp = construct_day_hour_effects(default_rand_model, offset=0)
  gd_a_r50p = construct_day_hour_effects(default_rand_over50_model, offset=0)
  gd_a_r500p = construct_day_hour_effects(default_rand_over500_model, offset=0)
  
  d_m_rp = construct_day_hour_effects(default_march_rand_model, offset=5)
  d_m_r50p = construct_day_hour_effects(default_march_rand_over50_model, offset=5)
  d_m_r500p = construct_day_hour_effects(default_march_rand_over500_model, offset=5)
  
  d_j_rp = construct_day_hour_effects(default_june_rand_model, offset=5)
  d_j_r50p = construct_day_hour_effects(default_june_rand_over50_model, offset=5)
  d_j_r500p = construct_day_hour_effects(default_june_rand_over500_model, offset=5)
  
  #allows for accurate labels with otherwise asymmetric parameter transformations
  wpercent <- function(x){
    percent(round(x*100)/100)
  }
  
  twpercent <- function(x){
    wpercent(exp(x)-1)
  }
  
  ggplot(d_a_rp, aes(x=weekday, y=hour, fill=estimate)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Change',colors=pal,label=twpercent, limits=c(-0.37,0.37), breaks = seq(-0.37,0.37,length.out=5)) + 
    ggtitle('Effect of Posting Time on Score for Default Subreddits', subtitle='US Central Time (GMT-5:00); based on random sample from 73,000 posts in March and June of 2017') + 
    geom_text(aes(label=twpercent(estimate))) +
    xlab('') + ylab('')
  
  ggplot(d_a_r50p, aes(x=weekday, y=hour, fill=estimate)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect',colors=pal,label=twpercent, limits=c(-1.6,1.6), breaks=seq(-1.6,1.6,length.out=5)) + 
    ggtitle('Effect of Posting Time on Chances of a Post Scoring at Least 50 for Default Subreddits', 
            subtitle='US Central Time (GMT-5:00); based on random sample from 73,000 posts in March and June of 2017') + 
    xlab('') + ylab('') + 
    geom_text(aes(label=twpercent(estimate)))
  
  ggplot(d_a_r500p, aes(x=weekday, y=hour, fill=pmax(-4.6, estimate))) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect',colors=pal,label=twpercent, limits=c(-4.6,2.5), breaks=seq(-4.6,2.5,length.out=5)) + 
    ggtitle('Effect of Posting Time on Chances of a Post Scoring at Least 500 for Default Subreddits', 
            subtitle='US Central Time (GMT-5:00); based on random sample from 73,000 posts in March and June of 2017') + 
    xlab('') + ylab('') + 
    geom_text(aes(label=twpercent(estimate)))
  
  
  ggplot(d_m_rp, aes(x=weekday, y=hour, fill=exp(estimate)-1)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect',colors=pal,label=percent)
  
  ggplot(d_m_r50p, aes(x=weekday, y=hour, fill=exp(estimate)-1)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect',colors=pal,label=percent)
  
  ggplot(d_m_r500p, aes(x=weekday, y=hour, fill=exp(estimate)-1)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect',colors=pal,label=percent)
  
  
  ggplot(d_j_rp, aes(x=weekday, y=hour, fill=exp(estimate)-1)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect',colors=pal,label=percent)
  
  ggplot(d_j_r50p, aes(x=weekday, y=hour, fill=exp(estimate)-1)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect',colors=pal,label=percent)
  
  ggplot(d_j_r500p, aes(x=weekday, y=hour, fill=exp(estimate)-1)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect',colors=pal,label=percent)
  
  ggplot(construct_day_hour_effects(default_rand_model, offset=5), aes(x=weekday, y=hour, fill=exp(estimate)-1)) + 
    geom_tile() + scale_fill_gradientn('Percent Change in Score', colors=cet_pal(5, 'fire'), label=percent) 
  
  ggplot(construct_day_hour_effects(rand_model, offset=0), aes(x=weekday, y=hour, fill=exp(estimate)-1)) + geom_tile() + 
    scale_fill_gradientn(colors=cet_pal(5, 'inferno'), label=percent)
  
  ggplot(rand_model_params_over500 %>% mutate(we), aes(x=weekday, y=hour, fill=exp(estimate)-1)) + geom_tile() + 
    scale_fill_gradientn(colors=cet_pal(5, 'inferno'), label=percent)
  
  
  #compound data
  asq <- function(x) sign(x) * sqrt(abs(x))
  compound_data = rbind(
    rand_model_params %>% mutate(sample='all subreddits', model='percent change', month='March & June'),
    rand_model_over50_params  %>% mutate(sample='all subreddits', model='score ≥ 50', month='March & June'),
    rand_model_over500_params  %>% mutate(sample='all subreddits', model='score ≥ 500', month='March & June'),
    
    rand_model_march_params %>% mutate(sample='all subreddits', model='percent change', month='March'),
    rand_model_march_over50_params %>% mutate(sample='all subreddits', model='score ≥ 50', month='March'),
    rand_model_march_over500_params %>% mutate(sample='all subreddits', model='score ≥ 500', month='March'),
    
    rand_model_june_params %>%  mutate(sample='all subreddits', model='percent change', month='June'),
    rand_model_june_over50_params %>%  mutate(sample='all subreddits', model='score ≥ 50', month='June'),
    rand_model_june_over500_params %>%  mutate(sample='all subreddits', model='score ≥ 500', month='June'),
    
    d_a_rp %>% mutate(sample='default subreddits', model='percent change', month='March & June'),
    d_a_r50p  %>% mutate(sample='default subreddits', model='score ≥ 50', month='March & June'),
    d_a_r500p  %>% mutate(sample='default subreddits', model='score ≥ 500', month='March & June'),
    
    d_m_rp %>% mutate(sample='default subreddits', model='percent change', month='March'),
    d_m_r50p %>% mutate(sample='default subreddits', model='score ≥ 50', month='March'),
    d_m_r500p %>% mutate(sample='default subreddits', model='score ≥ 500', month='March'),
    
    d_j_rp %>%  mutate(sample='default subreddits', model='percent change', month='June'),
    d_j_r50p %>%  mutate(sample='default subreddits', model='score ≥ 50', month='June'),
    d_j_r500p %>%  mutate(sample='default subreddits', model='score ≥ 500', month='June')
  ) %>% group_by(model) %>% mutate(model_scaled_estimate = scale(estimate)) %>% ungroup()
  
  ggplot(compound_data, aes(x=weekday, y=hour, fill=asq(model_scaled_estimate))) + geom_tile() + 
    scale_fill_gradientn('Effect Strength\n', colors=pal) + 
    facet_grid(month+sample ~ model) + theme_void() + 
    ggtitle("Visual Comparison of Post Score Models", subtitle='Effect strength based on square root of regression parameters normalized by model (this detail isn\'t too important)') + 
    theme(axis.title=element_text())  +
    theme(plot.margin = unit(c(1,1,1,1), "cm"))
  
  #
  ggplot(d_a_rp, aes(x=weekday, y=hour, fill=estimate)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Change\n',colors=pal,label=twpercent, limits=c(-0.37,0.37), breaks = seq(-0.37,0.37,length.out=5)) + 
    ggtitle('Effect of Posting Time on Score for Default Subreddits', subtitle='US Central Time (GMT-5:00); based on random sample from 73,000 posts in March and June of 2017') + 
    geom_text(aes(label=twpercent(estimate))) +
    xlab('') + ylab('')
  
  ggplot(d_a_r50p, aes(x=weekday, y=hour, fill=estimate)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect\n',colors=pal,label=twpercent, limits=c(-1.6,1.6), breaks=seq(-1.6,1.6,length.out=5)) + 
    ggtitle('Effect of Posting Time on Odds of a Post Scoring at Least 50 for Default Subreddits', 
            subtitle='US Central Time (GMT-5:00); based on random sample from 73,000 posts in March and June of 2017') + 
    xlab('') + ylab('') + 
    geom_text(aes(label=twpercent(estimate)))
  
  ggplot(d_a_r500p, aes(x=weekday, y=hour, fill=pmax(-4.6, estimate))) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect\n',colors=pal,label=twpercent, limits=c(-1.7,1.7), breaks=seq(-1.7,1.7,length.out=5)) + 
    ggtitle('Effect of Posting Time on Odds of a Post Scoring at Least 500 for Default Subreddits', 
            subtitle='US Central Time (GMT-5:00); based on random sample from 73,000 posts in March and June of 2017') + 
    xlab('') + ylab('') + 
    geom_text(aes(label=twpercent(estimate)))
  
  
  ggplot(rand_model_params, aes(x=weekday, y=hour, fill=estimate)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Change\n',colors=pal,label=twpercent, limits=c(-0.16,0.16), breaks = seq(-0.16,0.16,length.out=5)) + 
    ggtitle('Effect of Posting Time on Score for All Subreddits', subtitle='US Central Time (GMT-5:00); based on random sample from 717,000  posts in March and June of 2017') + 
    geom_text(aes(label=twpercent(estimate))) +
    xlab('') + ylab('')
  
  ggplot(rand_model_over50_params, aes(x=weekday, y=hour, fill=estimate)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect\n',colors=pal,label=twpercent, limits=c(-0.5,0.5), breaks=seq(-0.5,0.5,length.out=5)) + 
    ggtitle('Effect of Posting Time on Odds of a Post Scoring at Least 50 for All Subreddits', 
            subtitle='US Central Time (GMT-5:00); based on random sample from 717,000  posts in March and June of 2017') + 
    xlab('') + ylab('') + 
    geom_text(aes(label=twpercent(estimate)))
  
  ggplot(rand_model_over500_params, aes(x=weekday, y=hour, fill=pmax(-4.6, estimate))) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect\n',colors=pal,label=twpercent, limits=c(-1.08,1.08), breaks=seq(-1.08,1.08,length.out=5)) + 
    ggtitle('Effect of Posting Time on Odds of a Post Scoring at Least 500 for All Subreddits', 
            subtitle='US Central Time (GMT-5:00); based on random sample from 717,000  posts in March and June of 2017') + 
    xlab('') + ylab('') + 
    geom_text(aes(label=twpercent(estimate)))
  
  
  #now GMT version of plots
  ggplot(gd_a_rp, aes(x=weekday, y=hour, fill=estimate)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Change\n',colors=pal,label=twpercent, limits=c(-0.37,0.37), breaks = seq(-0.37,0.37,length.out=5)) + 
    ggtitle('Effect of Posting Time on Score for Default Subreddits', subtitle='Greenwich Mean Time; based on random sample from 73,000 posts in March and June of 2017') + 
    geom_text(aes(label=twpercent(estimate))) +
    xlab('') + ylab('') 
  
  ggplot(gd_a_r50p, aes(x=weekday, y=hour, fill=estimate)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect\n',colors=pal,label=twpercent, limits=c(-1.6,1.6), breaks=seq(-1.6,1.6,length.out=5)) + 
    ggtitle('Effect of Posting Time on Odds of a Post Scoring at Least 50 for Default Subreddits', 
            subtitle='Greenwich Mean Time; based on random sample from 73,000 posts in March and June of 2017') + 
    xlab('') + ylab('') + 
    geom_text(aes(label=twpercent(estimate)))
  
  ggplot(gd_a_r500p, aes(x=weekday, y=hour, fill=pmax(-4.6, estimate))) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect\n',colors=pal,label=twpercent, limits=c(-1.7,1.7), breaks=seq(-1.7,1.7,length.out=5)) + 
    ggtitle('Effect of Posting Time on Odds of a Post Scoring at Least 500 for Default Subreddits', 
            subtitle='Greenwich Mean Time; based on random sample from 73,000 posts in March and June of 2017') + 
    xlab('') + ylab('') + 
    geom_text(aes(label=twpercent(estimate)))
  
  
  ggplot(grand_model_params, aes(x=weekday, y=hour, fill=estimate)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Change\n',colors=pal,label=twpercent, limits=c(-0.16,0.16), breaks = seq(-0.16,0.16,length.out=5)) + 
    ggtitle('Effect of Posting Time on Score for All Subreddits', subtitle='Greenwich Mean Time; based on random sample from 717,000  posts in March and June of 2017') + 
    geom_text(aes(label=twpercent(estimate))) +
    xlab('') + ylab('')
  
  ggplot(grand_model_over50_params, aes(x=weekday, y=hour, fill=estimate)) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect\n',colors=pal,label=twpercent, limits=c(-0.5,0.5), breaks=seq(-0.5,0.5,length.out=5)) + 
    ggtitle('Effect of Posting Time on Odds of a Post Scoring at Least 50 for All Subreddits', 
            subtitle='Greenwich Mean Time; based on random sample from 717,000 posts in March and June of 2017') + 
    xlab('') + ylab('') + 
    geom_text(aes(label=twpercent(estimate)))
  
  ggplot(grand_model_over500_params, aes(x=weekday, y=hour, fill=pmax(-4.6, estimate))) + 
    geom_tile() + 
    scale_fill_gradientn('Percent Effect\n',colors=pal,label=twpercent, limits=c(-1.08,1.08), breaks=seq(-1.08,1.08,length.out=5)) + 
    ggtitle('Effect of Posting Time on Odds of a Post Scoring at Least 500 for All Subreddits', 
            subtitle='Greenwich Mean Time; based on random sample from 717,000 posts in March and June of 2017') + 
    xlab('') + ylab('') + 
    geom_text(aes(label=twpercent(estimate)))
  
  
  #look at domains
  summarize_domain_effects <- function(model, top=10, bottom=10, exclude_self=TRUE){
    prefix = 'top_domain'
    beta = model$beta[,ncol(model$beta)]
    bnames = names(beta)
    relevance = grepl(paste0(prefix, '.*'), bnames)
    if (exclude_self)
      relevance = relevance & !grepl('.*self\\..*', bnames)
    relevant_bnames = gsub(paste0(prefix,'(.*)'),'\\1',bnames[relevance])
    vals = beta[relevance]
    ordered = order(vals)
    ordered_vals = vals[ordered]
    ordered_names = relevant_bnames[ordered]
    n_elements = length(vals)
    topindexes = (n_elements-(top-1)):n_elements
    botindexes = 1:bottom
    top_frame = data.frame(domain=ordered_names[topindexes], 
                           estimate=ordered_vals[topindexes],
                           ranking='best')
    bot_frame = data.frame(domain=ordered_names[botindexes],
                           estimate=ordered_vals[botindexes],
                           ranking='worst')
    tdf = rbind(top_frame, bot_frame)
    tdf$domain = factor(as.character(tdf$domain), levels = as.character(tdf$domain)[order(tdf$estimate)], ordered=TRUE)
    return(tdf)
  }
  
  all_pct_domains = summarize_domain_effects(rand_model, top=12,bottom=12)
  all_50_domains = summarize_domain_effects(rand_model_over50)
  all_500_domains = summarize_domain_effects(rand_model_over500)
  
  def_pct_domains = summarize_domain_effects(default_rand_model)
  def_50_domains = summarize_domain_effects(default_rand_over50_model)
  def_500_domains = summarize_domain_effects(default_rand_over500_model)
  
  ggplot(all_pct_domains, aes(x=domain, y=(exp(estimate)-1), fill=ranking)) + 
    geom_bar(stat='identity') + coord_flip() +
    scale_y_continuous('Expected Percent Change', label=wpercent) +
    geom_text(aes(x=domain, y=exp(estimate)-1 + 0.4*(1-2*(estimate<0)),
                  label=twpercent(estimate)), size=5) +
    theme(plot.margin = unit(c(1,1,1,1), "cm")) +
    guides(fill=FALSE) + ggtitle("Expected Change in Score Based on Domain of Submission",
                                 subtitle="excluding self-posts, and when compared to less-frequently submitted domains \nbased on 717,000 reddit posts from March and June 2017; top and bottom 12 domains featured")
  
  #the rest of these tell a similar story
  ggplot(all_50_domains  %>% filter(estimate>0), aes(x=domain, y=(exp(estimate)-1))) + 
    geom_bar(stat='identity') + coord_flip() +
    scale_y_continuous('Expected Percent Change', label=wpercent) +
    geom_text(aes(x=domain, y=exp(estimate)-1 + 1.2*(1-2*(estimate<0)),
                  label=twpercent(estimate)), size=5)+
    theme(plot.margin = unit(c(1,1,1,1), "cm"))
  
  ggplot(all_500_domains  %>% filter(estimate>0), aes(x=domain, y=(exp(estimate)-1))) + 
    geom_bar(stat='identity') + coord_flip() +
    scale_y_continuous('Expected Percent Change', label=wpercent) +
    geom_text(aes(x=domain, y=exp(estimate)-1 + 1.34*(1-2*(estimate<0)),
                  label=twpercent(estimate)), size=5)+
    theme(plot.margin = unit(c(1,1,1,1), "cm"))
  
  
  ggplot(def_pct_domains, aes(x=domain, y=(exp(estimate)-1), fill=ranking)) + 
    geom_bar(stat='identity') + coord_flip() +
    scale_y_continuous('Expected Percent Change', label=wpercent) +
    geom_text(aes(x=domain, y=exp(estimate)-1 + 0.8*(1-2*(estimate<0)),
                  label=twpercent(estimate)), size=5)+
    theme(plot.margin = unit(c(1,1,1,1), "cm"))
  
  ggplot(def_50_domains  %>% filter(estimate>0), aes(x=domain, y=(exp(estimate)-1))) + 
    geom_bar(stat='identity') + coord_flip() +
    scale_y_continuous('Expected Percent Change', label=wpercent) +
    geom_text(aes(x=domain, y=exp(estimate)-1 + 0.9*(1-2*(estimate<0)),
                  label=twpercent(estimate)), size=5)+
    theme(plot.margin = unit(c(1,1,1,1), "cm"))
  
  ggplot(def_500_domains %>% filter(estimate>0), aes(x=domain, y=(exp(estimate)-1))) + 
    geom_bar(stat='identity') + coord_flip() +
    scale_y_continuous('Expected Percent Change', label=wpercent) +
    geom_text(aes(x=domain, y=exp(estimate)-1 + 3.1*(1-2*(estimate<0)),
                  label=twpercent(estimate)), size=5)+
    theme(plot.margin = unit(c(1,1,1,1), "cm"))
  
  #combine default/all 50/500 into one for comparison
  #factor leveling may not work, though...nope, doesn't work very well...
  #DO NOT USE
  combined_domains = rbind(
    all_50_domains %>% mutate(subreddits='All Subreddits', threshold='≥50'),
    all_500_domains %>% mutate(subreddits='All Subreddits', threshold='≥500'),
    def_50_domains %>% mutate(subreddits='Default Subreddits', threshold='≥50'),
    def_500_domains %>% mutate(subreddits='Default Subreddits', threshold='≥500')
  )
  ggplot(combined_domains, aes(x=domain, y=exp(estimate)-1)) + 
    geom_bar(stat='identity') + 
    coord_flip() + facet_grid(threshold ~ subreddits)
  
  
  
  
  
  #look at default subreddits
  summarize_subreddit_effects <- function(model, top=12, bottom=12){
    prefix = 'top_subreddit'
    beta = model$beta[,ncol(model$beta)]
    bnames = names(beta)
    relevance = grepl(paste0(prefix, '.*'), bnames)
    relevant_bnames = gsub(paste0(prefix,'(.*)'),'\\1',bnames[relevance])
    vals = beta[relevance]
    ordered = order(vals)
    ordered_vals = vals[ordered]
    ordered_names = relevant_bnames[ordered]
    n_elements = length(vals)
    topindexes = (n_elements-(top-1)):n_elements
    botindexes = 1:bottom
    top_frame = data.frame(subreddit=ordered_names[topindexes], 
                           estimate=ordered_vals[topindexes],
                           ranking='best')
    bot_frame = data.frame(subreddit=ordered_names[botindexes],
                           estimate=ordered_vals[botindexes],
                           ranking='worst')
    tdf = rbind(top_frame, bot_frame)
    tdf$subreddit = factor(as.character(tdf$subreddit), levels = as.character(tdf$subreddit)[order(tdf$estimate)], ordered=TRUE)
    return(tdf)
  }
  all_pct_subreddits = summarize_subreddit_effects(rand_model, top=50)
  all_50_subreddits = summarize_subreddit_effects(rand_model_over50)
  all_500_subreddits = summarize_subreddit_effects(rand_model_over500)
  
  def_pct_subreddits = summarize_subreddit_effects(default_rand_model)
  def_50_subreddits = summarize_subreddit_effects(default_rand_over50_model)
  def_500_subreddits = summarize_subreddit_effects(default_rand_over500_model)
  
  ggplot(all_pct_subreddits %>% filter(ranking=='best'), aes(x=subreddit, y=(exp(estimate)-1), fill=estimate)) + 
    geom_bar(stat='identity') + coord_flip() +
    scale_y_continuous('Expected Percent Change', label=wpercent) +
    geom_text(aes(x=subreddit, y=exp(estimate)-1 + 0.3*(1-2*(estimate<0)),
                  label=twpercent(estimate)), size=3) +
    theme(plot.margin = unit(c(1,1,1,1), "cm")) + 
    scale_fill_gradientn('Estimated\nEffect',colors=cet_pal(5, 'inferno'), label=twpercent) +
    ggtitle("Expected Change in Submission Score Based on Subreddit", 
            subtitle="when compared to subreddits less frequently submitted to\nbased on sample of 717,000 posts from March and June of 2017")
    
  
  ggplot(all_50_subreddits  %>% filter(estimate>0), aes(x=subreddit, y=(exp(estimate)-1))) + 
    geom_bar(stat='identity') + coord_flip() +
    scale_y_continuous('Expected Percent Change', label=wpercent) +
    geom_text(aes(x=subreddit, y=exp(estimate)-1 + 0.4*(1-2*(estimate<0)),
                  label=twpercent(estimate)), size=5)+
    theme(plot.margin = unit(c(1,1,1,1), "cm"))
  
  ggplot(all_500_subreddits  %>% filter(estimate>0), aes(x=subreddit, y=(exp(estimate)-1),
                                                         fill=estimate)) + 
    geom_bar(stat='identity') + coord_flip() +
    scale_y_continuous('Expected Percent Change', label=wpercent, limits = c(0,1000)) +
    geom_text(aes(x=subreddit, y=exp(estimate)-1 + 4.2*(1-2*(estimate<0)),
                  label=twpercent(estimate), hjust=0), size=5)+
    theme(plot.margin = unit(c(1,1,1,1), "cm")) + 
    scale_fill_gradientn('Percent Change', colors=cet_pal(7, 'inferno'), label=twpercent) +
    ggtitle("Estimated Effect of Subreddit on Odds of Post Score Being ≥ 500", 
            subtitle="small group size and many variables in logistic model result in overfit variables\n(so don't trust these values!!!)")
  
  #look at T_D and DIB
  #The_Donald
  
  td = load_subreddits('TheDonald', no_prefix=TRUE)
  td_model = clean_glmnet(logscore ~ day:hour + top_domain + title_length, data=td, alpha=0.5)
  td_summary = construct_day_hour_effects(td_model, offset=5)
  gtd_summary = construct_day_hour_effects(td_model, offset=0)
  
  ggplot(td_summary, aes(x=weekday, y=hour, fill=estimate)) + geom_tile() + 
    scale_fill_gradientn('Percent Change', colors=pal, label=twpercent, limits=c(-1.21,1.21)) + 
    geom_text(aes(x=weekday, y=hour, label=twpercent(estimate))) + 
    ggtitle('Estimated Effect of Posting Time on Expected Score for /r/The_Donald Submissions', 
            subtitle='US Central Time (GMT-5:00); Based on 141,000 submissions from June 2017 ') + 
    xlab('') + ylab('')
  ggplot(gtd_summary, aes(x=weekday, y=hour, fill=estimate)) + geom_tile() + 
    scale_fill_gradientn('Percent Change', colors=pal, label=twpercent, limits=c(-1.21,1.21)) + 
    geom_text(aes(x=weekday, y=hour, label=twpercent(estimate))) + 
    ggtitle('Estimated Effect of Posting Time on Expected Score for /r/The_Donald Submissions', 
            subtitle='Greenwich Mean Time;Based on 141,000 submissions from June 2017') + 
    xlab('') + ylab('')
  
  #looking at some counts, but not really interesting...
  td_tbl = table(td$day, td$hour)
  ggplot(melt(td_tbl), aes(x=Var1, y=Var2, fill=value)) + geom_tile()
  
  #let's look at dataisbeautiful
  dib = load_subreddits('dataisbeautiful')
  glm_dib_model = clean_glmnet(logscore ~ day:hour + month + top_domain + title_length, data=dib, alpha=0.5)
  dib_summary = construct_day_hour_effects(glm_dib_model, offset=5)
  gdib_summary = construct_day_hour_effects(glm_dib_model, offset=0)
  
  ggplot(dib_summary, aes(x=weekday, y=hour, fill=estimate)) + geom_tile() + 
    scale_fill_gradientn('Percent Change', colors=pal, label=twpercent, limits=c(-0.64,0.64)) + 
    geom_text(aes(x=weekday, y=hour, label=twpercent(estimate))) + 
    ggtitle('Estimated Effect of Posting Time on Expected Score for /r/DataIsBeautiful Submissions', 
            subtitle='US Central Time (GMT-5:00); Based on 16,350 submissions from January-June 2017 and October 2016') + 
    xlab('') + ylab('')
  ggplot(gdib_summary, aes(x=weekday, y=hour, fill=estimate)) + geom_tile() + 
    scale_fill_gradientn('Percent Change', colors=pal, label=twpercent, limits=c(-0.64,0.64)) + 
    geom_text(aes(x=weekday, y=hour, label=twpercent(estimate))) + 
    ggtitle('Estimated Effect of Posting Time on Expected Score for /r/DataIsBeautiful Submissions', 
            subtitle='Greenwich Mean Time; Based on 16,350 submissions from January-June 2017 and October 2016') + 
    xlab('') + ylab('')
  #kinda choppy

}