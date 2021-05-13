##### STEP 1: Set up libraries/constants #####

## Load libraries, set working directory
pacman::p_load(tidyverse, tigris, tidycensus, viridis, sf, mapview, spdep, furrr, lubridate, scales, leaflet, leafsync, plainview)

# library(arcgisbinding)
# arc.check_product()

options(
  tigris_use_cache = T,
  tigris_class = "sf"
)

## Initiate multicore processing
plan(multisession)
set.seed(1)

## Load ACS variables to assist in finding appropriate variables
acs19 <- load_variables(2019, "acs5", cache = TRUE)

## Define Portland county MSAs
pdx_msa_definition <- c('41005', '41009', '41051', '41067', '41071', '53011', '53059')
pdxcnty <- substr(pdx_msa_definition, start = 3, stop = 5)

##### STEP 2: Define functions #####

## Define spatial statistic function
get_gistar <- function(df, lang_origin = "lep_chinese", var = "estimate", method = "k", k.neighbor = 5, distance = 0.4) {
  
  ## create working dataframe from dataset and filter for appropriate variable
  wdf <- df %>% 
    st_transform(4269) %>% 
    ungroup() %>%
    filter(variable == lang_origin,
           !is.na(!!var)) 
  
  ## Determine the 50th percentile of proportions, estimates and densities
  higher_share <- wdf %>%
    filter(share > 0) %>% 
    st_drop_geometry() %>% 
    ungroup() %>% 
    summarize(q = quantile(share, c(0.5), na.rm = T)) %>%
    pull(q)
  
  higher_estimate <- wdf %>%
    filter(estimate > 0) %>% 
    st_drop_geometry() %>% 
    ungroup() %>% 
    summarize(q = quantile(estimate, c(0.5), na.rm = T)) %>%
    pull(q)
  
  higher_density <- wdf %>%
    filter(density > 0) %>% 
    st_drop_geometry() %>% 
    ungroup() %>% 
    summarize(q = quantile(density, c(0.5), na.rm = T)) %>%
    pull(q)
  
  ## Make Spatial object from working df
  wdf.sp <- as(wdf, "Spatial")
  
  if(method == "d") { # distance
    d <- dnearneigh(coordinates(wdf.sp), d1 = 0, d2 = distance, longlat = TRUE)
    spatial_weights <- nb2listw(include.self(d)) ## the include.self() function makes it Gi* instead of just Gi
  } 
  
  if(method == "k") { # k-nearest neighbor
    k <- knearneigh(coordinates(wdf.sp), k = k.neighbor, longlat = TRUE)
    spatial_weights <- nb2listw(include.self(knn2nb(k)))
  } 
  
  if(method == "q") { # queen
    spatial_weights <- nb2listw(include.self(poly2nb(wdf.sp)))
  } 
  
  if(method == "r") { # rook
    spatial_weights <- nb2listw(include.self(poly2nb(wdf.sp, queen = FALSE)))
  }
  
  ## Calculate local G for each tract
  varvec <- (wdf%>%select(!!var) %>% st_set_geometry(NULL) %>% as.data.frame(.))[,]
  g <- localG(varvec, spatial_weights)
  wdf$g <- g
  
  ## If this is performed on estimates (raw numbers), then filter for 
  ## - local G that's 2 standard deviations above average
  ## - OR higher-than-median estimates that have a population density over 85 per square mile
  ## - AND non-zero estimates
  if(var == "estimate") {
    wdf <- wdf %>%
      filter(g >= 1.96 | (estimate > higher_estimate & density > 85), estimate != 0) %>% 
      summarize() %>% st_transform(2913)
  }
  
  ## If this is performed on shares (percentage of population), then filter for 
  ## - local G that's 2 standard deviations above average
  ## - OR higher-than-median shares that have a population density over 40 per square mile
  ## - AND non-zero estimates
  if(var == "share") {
    wdf <- wdf %>%
      filter(g >= 1.96 | (share > higher_share & density > 40), estimate != 0) %>% 
      summarize() %>% st_transform(2913)
  }
  
  ## If this is performed on density (people per square mile), then filter for 
  ## - local G that's 2 standard deviations above average
  ## - OR higher-than-median population density that is at least over 85 per square mile
  ## - AND non-zero estimates
  if(var == "density") {
    wdf <- wdf %>%
      filter(g >= 1.96 | (density > higher_density & density > 85), estimate != 0) %>% 
      summarize() %>% st_transform(2913)
  }
  
  return(wdf)
}

## Function to crunch localG based on variety of inputs for languages 
crunch <- function(method_name, var, method = "k", k.neighbor = 5, distance = 0.4) {
  df <- furrr::future_map(.x = langs, .f = get_gistar, .progress = TRUE,
                          df = languages, var = var, method = method, k.neighbor = k.neighbor, distance = distance) %>%
    setNames(langs) %>% do.call(rbind.data.frame, .) %>%
    rownames_to_column(., "language") %>% separate(., language, c("language", NA), sep = "[.]") %>%
    mutate(method = method_name)
  return(df)
}

## Function to crunch localG based on variety of inputs for languages 
crunch2 <- function(method_name, var, method = "k", k.neighbor = 5, distance = 0.4) {
  df <- furrr::future_map(.x = birthplaces, .f = get_gistar, .progress = TRUE,
                          df = birthplace, var = var, method = method, k.neighbor = k.neighbor, distance = distance) %>%
    setNames(birthplaces) %>% do.call(rbind.data.frame, .) %>%
    rownames_to_column(., "birthplace") %>% separate(., birthplace, c("birthplace", NA), sep = "[.]") %>%
    mutate(method = method_name)
  return(df)
}

##### STEP 3: Analyze LEP language data #####

## Define list of LEP language variables using `acs19` object to find variable names
lep_languages <- c('Spanish' = 'C16001_005', 'French Haitian' = 'C16001_008', 
                   'Germanic' = 'C16001_011', 'Slavic' = 'C16001_014', 
                   'Other Indo-European languages' = 'C16001_017', 'Korean' = 'C16001_020', 
                   'Chinese' = 'C16001_023', 'Vietnamese' = 'C16001_026', 
                   'Tagalog' = 'C16001_029', 'Other Asian languages' = 'C16001_032', 
                   'Arabic' = 'C16001_035', 'Other languages' = 'C16001_038',
                   'ttl_speakers_base' = 'C16001_001')

## Pull LEP data into long format, filter for MSA, calculate shares 
## and densities and interpret reliability of data
languages <- get_acs(geography = "tract",
                     variables = lep_languages,
                     state = c("OR", "WA"),
                     county = pdxcnty,
                     survey = "acs5",
                     year = 2019,
                     cache_table = TRUE,
                     geometry = TRUE)  %>%
  filter(substr(GEOID, 1, 5) %in% pdx_msa_definition) %>%
  mutate(area = as.numeric(st_area(st_transform(.,2913))/27880000)) %>%
  group_by(GEOID) %>%
  left_join(., st_drop_geometry(select(filter(., variable == "ttl_speakers_base"), GEOID, denom_est = estimate, denom_moe = moe)),
            by = "GEOID") %>%
  mutate(cv = moe / 1.645 / estimate,
         reliability = case_when(cv > 0.4 ~ "3. Not reliable", 
                                 cv >= 0.2 & cv < 0.4 ~ "2. Use caution",
                                 cv < 0.2 ~ "1. Reliable"),
         share = estimate / denom_est,
         share_moe = moe_prop(num = estimate, 
                              denom = denom_est, 
                              moe_num = moe, 
                              moe_denom = denom_moe),
         share_cv = share_moe / 1.645 / share,
         share_reliability = case_when(cv > 0.4 ~ "3. Not reliable", 
                                       cv >= 0.2 & cv < 0.4 ~ "2. Use caution",
                                       cv < 0.2 ~ "1. Reliable"),
         density = estimate / area,
  ) %>% ## Share of total speakers
  filter(!(is.na(share))) %>%
  select(GEOID:moe, cv:share_reliability, density, area, geometry, -c(cv, share_cv)) # reorder columns

## List of language names to iterate over
langs <- c('Arabic', 'Chinese', 'French Haitian', 'Germanic', 'Korean', 
           'Other Asian languages', 'Other Indo-European languages', 'Other languages', 'Slavic', 'Spanish', 
           'Tagalog', 'Vietnamese')

## Genearte 6 different language overlays based on shares, estimates and densities using 2 spatial statistical approaches
k5_estimate <- crunch("k5_estimate", var = "estimate", method = "k", k.neighbor = 5)
k5_share <- crunch("k5_share", var = "share", method = "k", k.neighbor = 5)
k5_density <- crunch("k5_density", var = "density", method = "k", k.neighbor = 5)
d1_estimate <- crunch("d1_estimate", var = "estimate", method = "d", distance = 1.6)
d1_share <- crunch("d1_share", var = "share", method = "d", distance = 1.6)
d1_density <- crunch("d1_density", var = "density", method = "d", distance = 1.6)

## Combine all methods into one
lep_methods <- rbind(k5_estimate, k5_share, k5_density, d1_estimate, d1_share, d1_density)

## Dissolve the methods by language names
lep_dissolved <- lep_methods %>% group_by(language) %>% summarize() %>% st_transform('EPSG:2913')



##### STEP 4: Analyze birthplace for foreign-born population for select countires #####

## Define list of countries for which we have flagged critical languages that don't appear in ACS LEP table C16001
place_of_birth <- c("Nepal" = "B05006_062", "Ethiopia" = "B05006_094", "Somalia" = "B05006_096",
                    "Romania" = "B05006_041", "Burma (Karen)" = "B05006_068", "Japan" = "B05006_053",
                    "Ukraine" = "B05006_044", "Eritrea" = "B05006_093", "Thailand" = "B05006_075",
                    "Laos" = "B05006_071", "Hong Kong (Cantonese)" = "B05006_051", "Cambodia" = "B05006_069",
                    "Russia" = "B05006_042", "Micronesia (Chuukese)" = "B05006_127",
                    "total" = "B01001_001") # Use total population instead of foreign-born population as denominator

## Pull birthplace data into long format, filter for MSA, calculate shares 
## and densities and interpret reliability of data
birthplace <- get_acs(geography = "tract",
                      variables = place_of_birth,
                      state = c("OR", "WA"),
                      county = pdxcnty,
                      survey = "acs5",
                      year = 2019,
                      # output = 'wide',
                      cache_table = TRUE,
                      geometry = TRUE)  %>%
  filter(substr(GEOID, 1, 5) %in% pdx_msa_definition) %>%
  mutate(area = as.numeric(st_area(st_transform(.,2913))/27880000)) %>%
  group_by(GEOID) %>%
  left_join(., st_drop_geometry(select(filter(., variable == "total"), GEOID, denom_est = estimate, denom_moe = moe)),
            by = "GEOID") %>%
  mutate(cv = moe / 1.645 / estimate,
         reliability = case_when(cv > 0.4 ~ "3. Not reliable", 
                                 cv >= 0.2 & cv < 0.4 ~ "2. Use caution",
                                 cv < 0.2 ~ "1. Reliable"),
         share = estimate / denom_est,
         share_moe = moe_prop(num = estimate, 
                              denom = denom_est, 
                              moe_num = moe, 
                              moe_denom = denom_moe),
         share_cv = share_moe / 1.645 / share,
         share_reliability = case_when(cv > 0.4 ~ "3. Not reliable", 
                                       cv >= 0.2 & cv < 0.4 ~ "2. Use caution",
                                       cv < 0.2 ~ "1. Reliable"),
         density = estimate / area,
  ) %>% ## Share of total speakers
  filter(!(is.na(share))) %>%
  select(GEOID:moe, cv:share_reliability, density, area, geometry, -c(cv, share_cv))

## List of country names to iterate over
birthplaces <- c("Nepal", "Ethiopia", "Somalia", "Romania", "Burma (Karen)", "Japan", "Ukraine", "Eritrea",
                 "Thailand", "Laos", "Hong Kong (Cantonese)", "Micronesia (Chuukese)", "Cambodia", "Russia")

## Genearte 6 different birthplace overlays based on shares, estimates and densities using 2 spatial statistical approaches
k5_estimate_bpl <- crunch2("k5_estimate", var = "estimate", method = "k", k.neighbor = 5)
k5_share_bpl <- crunch2("k5_share", var = "share", method = "k", k.neighbor = 5)
k5_density_bpl <- crunch2("k5_density", var = "density", method = "k", k.neighbor = 5)
d1_estimate_bpl <- crunch2("d1_estimate", var = "estimate", method = "d", distance = 1.6)
d1_share_bpl <- crunch2("d1_share", var = "share", method = "d", distance = 1.6)
d1_density_bpl <- crunch2("d1_density", var = "density", method = "d", distance = 1.6)


## Combine all methods into one
bpl_methods <- rbind(k5_estimate_bpl, k5_share_bpl, k5_density_bpl, d1_estimate_bpl, d1_share_bpl, d1_density_bpl)

## Dissolve the methods by language names
bpl_dissolved <- bpl_methods %>% group_by(birthplace) %>% summarize() %>% st_transform('EPSG:2913')


### Combine both LEP languages and birthplace countries
lep_and_birthplace_method_overlays <- rbind(
  lep_dissolved %>% rename(language_or_birthplace = language),
  bpl_dissolved %>% rename(language_or_birthplace = birthplace)
) %>% st_transform(4326)

sf::st_crs(lep_and_birthplace_method_overlays) <- 4326

## Combine base LEP and birthplace data
lep_and_birthplaces <- rbind(languages, birthplace) %>% st_transform(4326)
sf::st_crs(lep_and_birthplaces) <- 4326

saveRDS(lep_and_birthplace_method_overlays, "app/LEP-immigrant-communities/data/lep_and_birthplace_method_overlays_2019.rds")
saveRDS(lep_and_birthplaces, "app/LEP-immigrant-communities/data/lep_and_birthplaces_2019.rds")

### Optional: Display map grid of a particular language/country
# bpl_lep_map <- function(target){
#   sync(
#     mapview(filter(lep_and_birthplaces,variable == target), zcol = "estimate", layer.name = paste0(target,"-#")),
#     mapview(filter(lep_and_birthplaces,variable == target), zcol = "share", layer.name = paste0(target,"-%")),
#     mapview(filter(lep_and_birthplaces,variable == target), zcol = "density", layer.name = paste0(target,"-density")),
#     mapview(filter(lep_and_birthplace_method_overlays, language_or_birthplace == target))
#   )
# }

# lep_and_birthplaces %>% 
#   # arrange(variable) %>%
#   filter(!(variable %in% c("ttl_speakers_base", "total"))) %>% 
#   pull(variable) %>% unique()
# 
# bpl_lep_map("Laos")


##### STEP 5: Append overlay data #####

choices_lang_bpl <- lep_and_birthplaces %>%
  filter(!(variable %in% c("ttl_speakers_base", "total", "French Haitian",
                           "Germanic", "Other Indo-European languages",
                           "Other Asian languages", "Other languages"))) %>%
  pull(variable) %>% unique()

overlay_appended <- map_df(.x = choices_lang_bpl, .f = ~
                             {
                               overlay <- lep_and_birthplace_method_overlays %>%
                                 filter(language_or_birthplace == .x) %>%
                                 st_transform(2913) %>%
                                 st_buffer(., -10) %>% # Negative buffer to account for sliver geometries
                                 st_transform(4326)
                               
                               sf::st_crs(overlay) <- 4326
                               
                               tracts_in_overlay <- lep_and_birthplaces %>%
                                 filter(variable == .x) %>%
                                 st_join(., overlay) %>%
                                 filter(language_or_birthplace == .x) %>%
                                 select(GEOID, language_or_birthplace)
                               
                               lep_and_birthplaces %>%
                                 filter(variable == .x) %>%
                                 mutate(in_overlay = ifelse(GEOID %in% tracts_in_overlay$GEOID, TRUE, FALSE))
                             })

saveRDS(overlay_appended, "app/LEP-immigrant-communities/data/lep_and_birthplaces_w_overlay_2019.rds") 


### Optional: Write to fgdb ####
# library(arcgisbinding)
# arc.check_product()
# 
# overlay_appended <- st_transform(overlay_appended, 'EPSG:2913')
# sf::st_crs(overlay_appended) <- 2913
# arc.write("data/data.gdb/lep_and_birthplaces_w_overlay_2019", overlay_appended, overwrite = TRUE)
