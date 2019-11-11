

# calculates the manhattan distance between two picked up products to compair it against the walked distance
calc.short.dist<- function(aisles, product.hits, input.data){
  product.hits$short.dist<- numeric(nrow(product.hits))
  product.hits$walk.dist<- numeric(nrow(product.hits))
  main.aisles<- aisles %>% filter(type == "main") %>% mutate(zmin= zmin, zmax= zmax) 
  mid.points<- (main.aisles$zmax+main.aisles$zmin)/2
  if(nrow(product.hits)>1)
    for(i in 1:(nrow(product.hits)-1)){
      if(product.hits$aisles.name[i]==product.hits$aisles.name[i+1]){
        product.hits$short.dist[i]<-abs(product.hits$z[i]-product.hits$z[i+1])
        product.hits$walk.dist[i]<- sum(input.data$dist[product.hits$time.index[i]:product.hits$time.index[i+1]])
      }else
      {
        dist.to.midle.1<-mid.points-product.hits$z.pos[i]
        dist.to.midle.2<-mid.points-product.hits$z.pos[i+1]
        product.hits$short.dist[i]<- min(abs(dist.to.midle.1)+abs(dist.to.midle.2))+
          abs(product.hits$x.pos[i]-product.hits$x.pos[i+1])
        product.hits$walk.dist[i]<- sum(input.data$dist[product.hits$time.index[i]:product.hits$time.index[i+1]])
        
      }
    }
  product.hits$dist.frac<- product.hits$walk.dist/product.hits$short.dist
  return(product.hits)
  
}
# from all hits finds if the product is a target product or related or wrong
filter.product.hits<- function(targets, hits){
  # give indexes of hitted items that are also targets
  hit.target<- lapply(hits$product, function(x) str_detect(x,targets$productname)) %>% lapply(function(x)any(x))%>% 
    unlist()
  
  false.hits<- hits%>% filter(!hit.target)
  # give logical indexes of related products when a product was not correct
  hit.related<- lapply(false.hits$product, function(x) lapply( targets$alternativename, function(y) any(str_detect(x,y))))%>% 
    lapply(function(x)any(unlist(x))) %>% 
    unlist()
  
  return(list(hit.target= hits %>% filter(hit.target),
              hit.related= false.hits %>% filter(hit.related)))
  
}