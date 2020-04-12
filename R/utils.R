# utils
# Private helper functions for 2speed package
# that do not fit into the routes, grids or maps collections


#helper to put European names first - assumes 4-letter ICAO code
isEur <- function(x) substr(x,1,1) %in% c("E","L")

#put ADEP ADES together with European first
concat_ADEPADES <- function(ADEP, ADES, unidirectional=FALSE){
  #vector adep ades is ok
  sep <- if_else(unidirectional, ">", "<>")
  AP2 <- case_when(
    isEur(ADEP) & isEur(ADES) ~ paste(pmin(ADEP,ADES),pmax(ADEP,ADES),sep=sep),
    isEur(ADEP) ~ paste(ADEP,ADES,sep=sep),
    isEur(ADES) ~ paste(ADES,ADEP,sep=sep),
    TRUE ~ paste(pmin(ADEP,ADES),pmax(ADEP,ADES),sep=sep))
}



#copy a list of attributes from one dataset to another
copy_attr <- function(from, to, atts){
  lapply(atts, function(x) {
    if (is.null(attr(from,x))){
      warning("Warning: Attribute",x,"not found to copy.")
    }
    attr(to,x) <- attr(from,x)
    p <- parent.env(environment()) #need to pass the value back to 'to' in the copy_attr environment
    assign("to",to,p)
  })
  return(to)
}

#for vectors adep, ades, make AP2 list (ie including long lat)
make_AP2 <- function(adep, ades, ap, ...){
  stopifnot(length(adep)==length(ades))

  data.frame(ADEP=adep, ADES=ades, stringsAsFactors = FALSE) %>%
    left_join(ap %>% select(APICAO, long, lat) %>% rename(ADEP=APICAO, from_long=long, from_lat=lat),
              by="ADEP") %>%
    left_join(ap %>% select(APICAO, long, lat) %>% rename(ADES=APICAO, to_long=long, to_lat=lat),
              by="ADES") %>%
    mutate(AP2 = concat_ADEPADES(ADEP, ADES, ...),
           gcdistance_km = distGeo(c(from_long, from_lat),
                                   c(to_long, to_lat))/1000)
}


# projection function - for more readable code in full_map
# in utils because used both in maps and elsewhere
# crs=54030 Robinson works well, but avoid cropping - a classic
# for the Pacific crs=CRS("+proj=robin +lon_0=150 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
prj <- function(x,crs=crs_map) {
  st_transform(x, crs=crs, quiet=FALSE)
}


#rename a dataset in an environment, replacing *one* find with replace
#returns ds not found error - but does the job!
ren_subst <- function(ds,find_str,replace_str,in_env){
  # ds is a string
  #copy with new name
  assign(sub(find_str,replace_str,ds),
         get(ds, in_env), in_env)
  #get rid of old one
  rm(ds, envir=in_env)
  TRUE
}


#wrapper for geosphere::gcIntermediate that returns an sfc object
st_gcIntermediate <- function(crs, ...){
  #not vector-clever for n, which is (single) integer
  #starts with 4326 - any old lat long and then transform to the required crs
  st_transform(
    st_sfc(st_linestring(gcIntermediate(...)),crs=4326),
    crs)
}


