bb <- c(78, 95, 60, 27)
idf <- tt %>% 
  filter(is.element(dep, bb), jour > "2020-08-31") 
idf$dep <- as.factor(idf$dep)
bb <- c(78, 95, 60, 27)
pop <- c(1395000,1105464,780000,541054)
dpt <- data.frame(bb,pop)
names(dpt) <- c("dep", "pop")
dpt$dep <- as.factor(dpt$dep)
idf <- left_join(idf, dpt, by = "dep") %>% 
  mutate(hosp = incid_hosp*100000/pop)


idf %>% 
  ggplot() +
  aes(x= jour, y = hosp, col = dep) +
  geom_point() +
  geom_smooth()

somx <- function(x){
  zz <- 1
  if (x>0){
    zz <- x + somx(x-1)
  }
  return(zz)
}