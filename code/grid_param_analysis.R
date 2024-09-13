grid_cluster_labels<-results_tbl('grid_cluster_labels_6_12_try_pasc_78', results_tag=FALSE) %>%
  dplyr::select(-index) %>%
  dplyr::select(-person_id) %>% collect
  #mutate(person_id=as.integer(person_id))

grid_cluster_labels_new<-results_tbl('grid_cluster_labels_new_6_12_try_pasc_78') %>%
  dplyr::select(-index) %>%
  dplyr::select(-person_id) %>% collect

n_persons=nrow(grid_cluster_labels_new)
grid_sum_old<-data.frame(params=grid_cluster_labels %>% colnames())
grid_props<-colSums(grid_cluster_labels==-1)/n_persons
grid_sum_old$prop_unclustered=grid_props
n_clust<-apply(grid_cluster_labels, 2, max)+1
grid_sum_old$n_clusters=n_clust


grid_sum<-data.frame(params=grid_cluster_labels_new %>% colnames())
grid_props<-colSums(grid_cluster_labels_new==-1)/n_persons
grid_sum$prop_unclustered=grid_props
n_clust<-apply(grid_cluster_labels_new, 2, max)+1
grid_sum$n_clusters=n_clust

params_keep<-grid_sum_old %>% filter(n_clust>=4, n_clust<=20, prop_unclustered<0.25)

grid_clust_keep<-grid_cluster_labels_new %>% dplyr::select(params_keep$params)

library(clevr)

#variation_info(grid_clust_keep$`min_percent: 0.01, n_neighbors: 20, min_samples: 1`, grid_clust_keep$`min_percent: 0.01, n_neighbors: 100, min_samples: 20`)

var_info<-data.frame(matrix(nrow=ncol(grid_clust_keep), ncol=ncol(grid_clust_keep)))
colnames(var_info)=colnames(grid_clust_keep)
rownames(var_info)=colnames(grid_clust_keep)

var_info_ref<-data.frame(matrix(nrow=ncol(grid_clust_keep), ncol=ncol(grid_clust_keep)))
colnames(var_info_ref)=colnames(grid_clust_keep)
rownames(var_info_ref)=colnames(grid_clust_keep)

var_info_prop<-data.frame(matrix(nrow=ncol(grid_clust_keep), ncol=ncol(grid_clust_keep)))
colnames(var_info_prop)=colnames(grid_clust_keep)
rownames(var_info_prop)=colnames(grid_clust_keep)



for (params_a in colnames(var_info)){
  print(params_a)
  for (params_b in rownames(var_info)){
    clusts_a<-grid_clust_keep %>% pull(params_a)
    clusts_b<-grid_clust_keep %>% pull(params_b)
    clusts_ab<-paste(clusts_a, clusts_b, sep='_')
    #var_info
    var_info[params_a, params_b]=
      variation_info(clusts_a, clusts_b)
#    var_info_ref[params_a, params_b]=variation_info(clusts_a, clusts_ab)
#    var_info_prop[params_a, params_b]=get_wallace_probs(clusts_a, clusts_b)
    #dist to common refinement
  }
}



get_wallace_probs<-function(clusts_a, clusts_b){
  n=length(clusts_a)
  denom=data.frame(clust_a=clusts_a) %>%
    group_by(clust_a) %>% summarize(j=n()) %>% 
    mutate(s=j*(j-1)/2) %>%
    ungroup %>% pull(s) %>% sum
  num=0
  for (i in 2:n){
    for (j in 1:(i-1)){
      if ((clusts_a[i]==clusts_a[j]) & (clusts_b[i]==clusts_b[j])){
        num=num+1
      }else{}
    }
  }
  
  prob=num/denom
  return(prob)
}

#library(qgraph)
#jpeg('example_forcedraw.jpg', width=1000, height=1000, unit='px')
#qgraph(as.matrix(var_info), layout='spring', vsize=3)


hclust<-hclust(as.dist(as.matrix(var_info)))
hclusts<-cutree(hclust, k=2)
#dev.off()
png('./hclust_tree.png', height=20, width=8, units='in', res=1000)
plot(hclust)
dev.off()



cmd_scale<-as.data.frame(cmdscale(as.matrix(var_info)))

data <- cmd_scale %>% dplyr::select(V1, V2)
kcluster<-kmeans(data, centers=2)
cmd_scale$params<-rownames(cmd_scale)
cmd_scale$clust<-hclusts#kcluster$cluster

cmd_scale %>% mutate(clust=as.factor(clust)) %>% 
  ggplot(aes(x=V1, y=V2, label=params,color=clust))+geom_point()

params_keep_2<-params_keep %>% left_join(cmd_scale %>% dplyr::select(params, clust), by='params')




#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 7
data <- cmd_scale %>% dplyr::select(V1, V2)
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
