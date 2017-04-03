set.seed(1)

#reset existing
file.remove(list.files("template", full.names = T))
file.remove(list.files("output", full.names = T))

#gather names of images to be presented
image_list <- list.files("img")

#create a limited number of templates with varied image orderings
num_templates <- 5

for(t in 1:num_templates){
  print(t)
  images <- sample(image_list, size = length(image_list), replace = F)
  template <- data.frame("file" = images, "status"=FALSE)
  write.csv(template, file.path("template", paste0("t", t,".csv")), row.names = F)
}

