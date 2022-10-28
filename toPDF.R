


library(tidyverse)
library(ggtext)

pt_inch <- function(x) {
  return(x * 0.0138888889)
}
path <- '/Users/yuyaarai/Documents/LocalSAGAS/20220516/SALAS_2/RESULTS'
times <- list.files(path, pattern="test_time", recursive=T, full.names=T)
deps <- list.files(path, pattern="test_dep", recursive=T, full.names=T)
machines <- list.files(path, pattern="test_machines",recursive=T, full.names=T)
pdfoutputs <- list.files(path, pattern="test_machines",recursive=T, full.names=T)
svgoutputs <- pdfoutputs
　

sort(times)
sort(deps)
sort(machines)
sort(pdfoutputs)
sort(svgoutputs)


for (i in 1:length(pdfoutputs)){
  pdfoutputs[i] <- gsub("/rawresults/", "/pdfs/", pdfoutputs[i])
  pdfoutputs[i] <- gsub("test_machines", "test_pdf", pdfoutputs[i])
  pdfoutputs[i] <- gsub("\\.tsv", ".pdf", pdfoutputs[i])
  
  
  svgoutputs[i] <- gsub("/rawresults/", "/pdfs/", svgoutputs[i])
  svgoutputs[i] <- gsub("test_machines", "test_svg", svgoutputs[i])
  svgoutputs[i] <- gsub("\\.tsv", ".svg", svgoutputs[i])
}


# check
for (i in 1:5){
  print(times[i])
  print(deps[i])
  print(machines[i])
  print(pdfoutputs[i])
  print(svgoutputs[i])
}


Right <- 50

tes_n = 25
Per = TRUE
tes_from = 19
if(Per){
  tes_from = 1
  tes_n = length(times)
}





Right <- 50

for (i in tes_from:tes_n) {    

  
  #for (i in 1:5) {              # for (ループ変数 in ベクトルやリスト)
  # ベクトルやリストの要素が空にならない限り式が繰り返される
  # 文字列の置換をおこなうことで自動的に出力先のディレクトリを指定する
  
  
  # Define path
  path_scheduling_result <-times[i]
  path_dependency <- deps[i]
  path_machine <-machines[i]
  path_output_pdf <-  pdfoutputs[i]
  path_output_svg <-  svgoutputs[i]
  # Read data
  df1 <- read_tsv(path_scheduling_result)
  df_dep <- read_tsv(path_dependency)
  df_machine <- read_tsv(path_machine)
  if(i == 1){
    print(colnames(df_dep))
    print(head(df_machine))
    print(head(df1))
  }
  colnames(df_dep)
  
  
  for (Raw in 1:nrow(df_machine)){
    name <- df_machine[Raw, 3]
    dup <- df_machine[Raw, 3]
    
    if(ncol(df_machine) >= 4){
    for (Col in 4:ncol(df_machine)){
      if(is.na(df_machine[Raw, Col])){
        next
      }
      if(dup == df_machine[Raw, Col]){
        next
      }
      dup = df_machine[Raw, Col]
      name <- paste(name, dup, sep = " ")
    }
    }
    df_machine[Raw, 3] <- name
    
    
  }
  
  df_machine <- df_machine[1:3]
  
  Title <- gsub("/.*/", "", times[i])
  Title <- gsub("_..\\.tsv", "", Title)
  Title <- gsub("test_time_\\d*_\\d*_Loop\\d_", "", Title)
  Title <- gsub("_N\\d{1,2}_.*", "", Title)
  
  
  
  
  Wratio <- 1.5
  Hratio <- 1.5
  
  
  machine_name_size <- 100/length(unique(df_machine$Machine_ID))
  
  Figpath <- ""
  
  if(grepl(pattern = "1500msec", times[i])){
    W <- pt_inch(370)
    H <- pt_inch(100)
    Title <- gsub(".*Gu2016", "Gu2016", times[i])
    Figpath <- "F2"
    
    
    
    Right <- 110
    
  }  
  
  else if(grepl(pattern = "qPCR_RNAseq", times[i])){
      W <- pt_inch(250)
      H <- pt_inch(150)
    Title <- gsub(".*qPCR_RNAseq", "qPCR_RNAseq", times[i])
    Figpath <- "F6"
    
    Right <- 3170
  }  
  
  else if(grepl(pattern = "Gu2016", times[i])){
      W <- pt_inch(250)
      H <- pt_inch(150)
    Title <- gsub(".*Gu2016", "Gu2016", times[i])
    Figpath <- "F3"
    
    Right <- 390
  }  
  
  else if(grepl(pattern = "RNAseq", times[i])){
      W <- pt_inch(250)
      H <- pt_inch(150)
    Title <- gsub(".*RNAseq", "RNAseq", times[i])
    Figpath <- "F5"
    
    Right <- 2420
    
  }  
  
  else if(grepl(pattern = "qPCR", times[i])){
      W <- pt_inch(250)
      H <- pt_inch(150)
    Title <- gsub(".*qPCR", "qPCR", times[i])
    Figpath <- "F4"
    
    Right <- 240
    
  }  
  
  Title <- gsub("\\.tsv", "", Title)
  Title <- gsub("_", " ", Title)
  Title <- gsub("min ", " min", Title)
  Title <- gsub("msec", " msec,", Title)
  Title <- gsub("SA", "SA", Title)
  Title <- gsub("SG", "SAGAS", Title)
  Title <- gsub("SM", "SA-Mod", Title)
  Title <- gsub("GR", "Greedy", Title)
  
  Search = list("SAGAS", "SA-Mod","SA",  "Greedy")
  for (s in 1:4){
    
    if (grepl(Search[s], Title)){
      
      Title <- gsub(Search[s], "", Title)
      Title <- paste(Search[s], Title, sep = "\n")
      break
      
    }
    
    
    
  }
  
  cat(Title)
  
  machine_name_size <- 40/length(unique(df_machine$Machine_ID))
  
  
  
  cat(machine_name_size)
  
  # Preprocess data
  df_dep %>%
    left_join(df1, by=c("Job_ID", "Operation_ID_1"="Operation_ID")) %>%
    left_join(df1, by=c("Job_ID", "Operation_ID_2"="Operation_ID")) -> df_dep
  
  # Calculate last end time
  last_end_time <- max(df1$End)
  
  # Plot
  g <- df1 %>%
    ggplot(aes(x=Start,  y=factor(Machine_ID))) +
    geom_segment(
      data=df_dep,
      aes(x=End.x, xend=Start.y, y=factor(Machine_ID.x), yend=factor(Machine_ID.y)),
      linetype="33",
      size=0.1
    ) +
    geom_segment(aes(xend=End, yend=factor(Machine_ID), color = factor(Job_ID)), size  =  0.5 * H) +
    # geom_label(aes(label=Job_ID, x=Start, y=factor(Machine_ID)), hjust = 1, vjust=-0.3, size=2, label.r=unit(0.08, "lines")) +
    theme_bw() +
    scale_y_discrete(
      breaks=factor(df_machine$Machine_ID),
      labels=df_machine$Machine_name,
      limits=rev(factor(unique(df_machine$Machine_ID)))
    ) +
    scale_x_continuous(limits = c(0, Right))+
    labs(x="Time (min)", y="Machine_ID", color = "Job ID", title = sprintf("%s%s: %.1f min", Title,"", last_end_time)) +
    #annotate("text",x=-Inf,y=Inf,label=sprintf("Total: %.1f min.", last_end_time),hjust=-.2,vjust=2) +
    theme(
      
      plot.title = element_text(size = 12, colour = "black", hjust = 0.5),
      axis.text.x = element_text(
      size = 8,
      colour = "black"
    ),
    axis.text.y = element_text(
      size = machine_name_size,
      colour = "black"
      
    ),
    axis.title.y = element_blank(),
    axis.title.x = element_text(
      size = 10,
      colour = "black"
    ),
          #legend.position = "none",
          
          panel.border = element_rect(color = "black",
                                      fill = NA,
                                      size = 0.5),
          legend.key.height= unit(0.01, 'inch'),
          legend.key.width= unit(0.1, 'inch'),
          legend.text = element_text(size = 6),
          legend.title = element_text(color="black", size = 6),
    panel.grid.major.x =element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(colour = "black"),
          
    )+
    guides(col = guide_legend(ncol = 1))
  
  
  
  if (grepl("FAILED", times[i])){
    
    g <- df1 %>%
      ggplot(aes(x=Start,  y=factor(Machine_ID))) +
      geom_segment(
        data=df_dep,
        aes(x=End.x, xend=Start.y, y=factor(Machine_ID.x), yend=factor(Machine_ID.y)),
        linetype="33",
        size=0.0
      ) +
      geom_segment(aes(xend=End, yend=factor(Machine_ID), color = factor(Job_ID)), size  =  0.0) +
      # geom_label(aes(label=Job_ID, x=Start, y=factor(Machine_ID)), hjust = 1, vjust=-0.3, size=2, label.r=unit(0.08, "lines")) +
      theme_bw() +
      scale_y_discrete(
        breaks=factor(df_machine$Machine_ID),
        labels=df_machine$Machine_name,
        limits=rev(factor(unique(df_machine$Machine_ID)))
      ) +
      scale_x_continuous(limits = c(0, Right))+
      labs(x="Time (min)", y="Machine_ID", color = "Job ID", title = sprintf("%s%s", Title,"")) +
      #annotate("text",x=-Inf,y=Inf,label=sprintf("Total: %.1f min.", last_end_time),hjust=-.2,vjust=2) +
      theme(
        
        plot.title = element_text(size = 12, colour = "black", hjust = 0.5),
        axis.text.x = element_text(
          size = 8,
          colour = "black"
        ),
        axis.text.y = element_text(
          size = machine_name_size,
          colour = "black"
          
        ),
        axis.title.y = element_blank(),
        axis.title.x = element_text(
          size = 10,
          colour = "black"
        ),
        legend.position = "none",
        
        panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 0.5),
        legend.key.height= unit(0.01, 'inch'),
        legend.key.width= unit(0.1, 'inch'),
        legend.text = element_text(size = 6),
        legend.title = element_text(color="black", size = 6),
        panel.grid.major.x =element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = "black"),
        
      )+
      guides(col = guide_legend(ncol = 1))+
      annotate("text",x=200,y=3.5,label="FAILED", size = 12)
  }
  
  
  
  
  
  
  
  
  plot(g)
  # Save plot
  ggsave(filename = path_output_pdf, plot = g, width = W, height = H)
  ggsave(filename = path_output_svg, plot = g, width = W, height = H)
  
  
  if(grepl(pattern = "N5", times[i])){
    ggsave(filename = gsub("/scheduling/.*/test_svg", paste0("/scheduling/P/",Figpath ,"/test_svg"), path_output_svg), plot = g, width = W, height = H)
  }
  
  if(grepl(pattern = "1500msec", times[i])){
    ggsave(filename = gsub("/scheduling/.*/test_svg", paste0("/scheduling/P/",Figpath ,"/test_svg"), path_output_svg), plot = g, width = W, height = H)
  }
  #ggsave(filename = gsub("/scheduling/.*/test_", "/V/test_", path_output_pdf), plot = g, width = 8, height = 4)
  #ggsave(filename = gsub("/scheduling/.*/test_", "/V/test_", path_output_svg), plot = g, width = 8, height = 4)
  
}

path_output_svg

