#server.R

library(lattice)
source("src/cnvgram.R")
library(psych)
library(ggplot2)
library(OmicCircos)
library(rtracklayer)
options(stringsAsFactors = FALSE);
shinyServer(function(input,output){
  
  dataValues <- reactive({
    
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data<-read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    data$count<-data$weight
    data$count=1
    
    return(data)
  })
  ##DT显示表内容
  output$table <- DT::renderDataTable(DT::datatable({
    dataValues() 
  }, rownames = FALSE))
  ###cnv画图内容
  #output$plot1 <- renderPlot({
    
    ##data
    ##psych包里  describe(mtcars[vars]) 统计函数 
    
    output$summary<-renderTable({
      ##要处理准备的数据集
      if(input$var=="software")
      {describe(dataValues())}
      else if(input$var=="gene"){  return(NULL)    }
      #attach(dataValues())
      # if(input$var=="software")
      # {describe((software)) }
      # else if(input$var=="chr"){describe(chromosome) }
      # else if(input$var=="TYPE"){describe(TYPE)}
      # else if(input$var=="gene"){describe(gene)}
      # else if(input$var=="cn"){describe(cn)}
      # else if(input$var=="start"){describe(start)}
      # else if(input$var=="log2"){describe(log2)}
      # else if(input$var=="end"){describe(end)}
      # else if(input$var=="SIZE"){describe(SIZE)}
      # else if(input$var=="probes"){describe(probes)}
      # else{return(NULL)}
      #ba<-data.frame(ab)
     # rownames(ba)<-c("chromosome",	"start",	"end",	"SIZE",	"gene",	"log2",	"cn",	"TYPE",	"depth",	"probes",	"weight",	"software")
     # return(ba)
      ##软件变量
      #z1<-input$softvar
      #if(z1=="all")
      #describe(dataValues)
      #else{
       # subcnvdata<-cnvdata[cnvdata$software==z1]
       # describe(subcnvdata$software)
      #}
    })
    output$summaryplot<-renderPlot({
      #attach(dataValues())
      if(input$var=="software")
      {
        ggplot(dataValues())+geom_bar(aes(x =factor(software),y=count,fill=factor(TYPE)),stat="identity")
      }
      else if(input$var=="gene"){
        ggplot(dataValues())+geom_bar(aes(x =factor(gene),y=count,fill=factor(software)),stat="identity")+theme(axis.text.x = element_text(size = 8,color = "black", face = "bold", vjust = 1, hjust = 1, angle = 60)) 
      }
      else if(input$var=="SIZE"){ggplot(dataValues())+geom_bar(aes(x =factor(SIZE),y=count,fill=factor(software)),stat="identity")+theme(axis.text.x = element_text(size = 8,color = "black", face = "bold", vjust = 1, hjust = 1, angle = 60)) 
      }
      else if(input$var=="cn"){ggplot(dataValues())+geom_bar(aes(x =factor(cn),y=count,fill=factor(software)),stat="identity")+theme(axis.text.x = element_text(size = 8,color = "black", face = "bold", vjust = 1, hjust = 1, angle = 60)) 
      }
      else if(input$var=="TYPE"){ggplot(dataValues())+geom_bar(aes(x =factor(TYPE),y=count,fill=factor(software)),stat="identity")+theme(axis.text.x = element_text(size = 8,color = "black", face = "bold", vjust = 1, hjust = 1, angle = 60)) 
      }
      else{return(NULL)}
      
    })
     ###circos图 
    output$plotcircos <-renderPlot({
      if(is.null(dataValues()))
        return(NULL)
      #cname<-c("ID","chr","po","NAME","cn","color")
      datanew<-data.frame(dataValues())
      ID<-seq(1,length(datanew))
      chr<-datanew$chromosome
      po<-datanew$start
      NAME<-datanew$gene
      #if(dataValues()$log2=="-")
      #{cn<-dataValues()$cn    }
      #else{cn<-dataValues()$log2}
      cn<-datanew$log2
      color="#33FF0080"
      #datanew<-data.frame(ID,chr,po,NAME,cn,color)
      #datanew[datanew$cn=="-"]=3
      colors <- rainbow(10, alpha=0.5);
      par(mar=c(2,2,2,2));
      plot(c(1,800), c(1,800), type="n", axes=FALSE, xlab="", ylab="", main="");
      circos(R=400, cir="hg19", W=10,   type="chr", lwd=5, side="out", cex=4,print.chr.lab=TRUE, scale=TRUE);
      circos(R=350, cir="hg19", W=40,  mapping= datanew,  col.v=4,   type="h", B=FALSE, lwd=3, cutoff=0,col=colors [ 4 ] );
      circos(R=310, cir="hg19", W=40,  mapping= datanew,  col.v=4,   type="s", B=FALSE, lwd=3, cutoff=0,col=colors [ 7 ]);
      circos(R=250, cir="hg19", W=40,  mapping= datanew,  col.v=4,   type="b3", B=TRUE, lwd=3, cutoff=0,col=colors [ 9 ]);
      zoom <- c(7, 7, 0, 154143883, 0,360);
      circos(R=200, cir="hg19", W=40,  mapping= datanew,  col.v=4,   type="s", B=FALSE, lwd=3, cutoff=0,col=colors [ 9 ],zoom=zoom);
      
    })
    #chr1:10000-2000
    
    observeEvent(input$submit, {
      shinyjs::alert("程序正在计算中，速度比较慢，请耐心等待一会，大约一分钟。")
      region<-input$CNV_reigon
      #region<-c("chr1:17380437-20915212") #这种格式直接就可以
      #cnvdata<-gsub("\\:|\\-","    ",region,perl=TRUE)
      session <- browserSession("UCSC")
      genome(session) <- "hg19"
      export(region, "Track12.bed")
      restoredTrack2 <- import("Track12.bed")
      track(session, "Track") <-  restoredTrack2
      browserView(session,restoredTrack2*-5, dense="Track",pack=c("DECIPHER","RefSeq Genes","ClinGen CNVs","ClinVar Variants",
                         "GeneReviews","OMIM Genes","DGV Struct Var") )
    })
    
      
      
      
      
    })
    
    #hide=c("UCSC Genes","GTEx Gene","ENCODE Regulation...","Conservation","Common SNPs(150)","RepeatMasker"),
  #output$getdata<- dataValues() 
  














# 利用lattice包中的绘图函数
#output$splom <- renderPlot({
#splom(mtcars[c(1, 3:7)], groups = mtcars$cyl,
#pscales = 0,pch=1:3,col=1:3,
#varnames = c("Miles\nper\ngallon", "Displacement\n(cu. in.)",
#            "Gross\nhorsepower", "Rear\naxle\nratio",
#           "Weight", "1/4 mile\ntime"),
# key = list(columns = 3, title = "Number of Cylinders",
# text=list(levels(factor(mtcars$cyl))),
# points=list(pch=1:3,col=1:3)))
#})

#output$wireframe <- renderPlot({
# wireframe(volcano, shade = TRUE,
#           aspect = c(61/87, 0.4),
#            light.source = c(10,0,10))
#})

#set.seed(122)
#histdata <- rnorm(500)