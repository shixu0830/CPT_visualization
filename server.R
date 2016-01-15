
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$table_tittle = renderText({
    "The input data"
  })
  output$csvfile = renderTable({
    if(input$example==T){
      cpt = exampledata
    }else{
      inFile = input$file1
      if (is.null(inFile))
        return(NULL)
      cpt = read.csv(inFile$datapath, header=input$header)
    }
    head(cpt)
  })
  output$table_tittle2 = renderText({
    "The summary of the data"
  })
  output$csvfile_summary = renderTable({
    if(input$example==T){
      cpt = exampledata
    }else{
      inFile = input$file1
      if (is.null(inFile))
        return(NULL)
      cpt = read.csv(inFile$datapath, header=input$header)
    }
    cpt = QC(cpt)
    nonnumericind = grep("[A-Z,a-z]",as.character(cpt$code))
    if(length(nonnumericind)>0){
      cptnonnum=cpt[nonnumericind,];cptnum=cpt[-nonnumericind,]
      cpt = groupcode(cptnonnum,cptnum)
    }else{
      cpt = groupcode(NULL, cpt)
    }
    names(cpt)[which(names(cpt)=="site")]="Cohort"
    cptdt=data.table(cpt)
    tab = cptdt[,list(No.patients=length(unique(id)),
                      No.codes=length(unique(code)),
                      No.blocks=length(unique(codegrp.order)),
                      No.code.assignments=length(code),
                      No.block.assignments=length(codegrp.order)),
                by=c("Cohort")]
    tab
  })
  output$method_chosen = renderText({
    paste("Method for hypothesis testing:", input$p_val_methods)
  })
  output$method_chosen2 = renderText({
    if(input$alpha.penalty==0){
      RRmethod="Ridge Regression"
    }else if(input$alpha.penalty==1){
      RRmethod="The Lasso"
    }else{
      RRmethod=paste("Elastic net with alpha = ", input$alpha.penalty)
    }
    paste("Method for rate ratio estimation:", RRmethod)
  })
  output$manhattanplot <- renderPlot({#renderChart({
    if(input$example==T){
      cpt = exampledata
    }else{
      inFile = input$file1
      if (is.null(inFile))
        return(NULL)
      cpt = read.csv(inFile$datapath, header=input$header)
    }
    inputmethod = input$p_val_methods
    print("input data done")
    results = wrapup_Manhattan(cpt,inputmethod)
    results = plot_elements(results)
    Manhattanplot(results)
    
  })
  
  output$myChart <- renderChart3({
    if(input$example==T){
      cpt = exampledata
    }else{
      inFile = input$file1
      if (is.null(inFile))
        return(NULL)
      cpt = read.csv(inFile$datapath, header=input$header)
    }
    inputmethod = input$p_val_methods
    
    data = wrapup_RR(cpt,inputmethod)
    print("Now plotting")
    
    d1 <- dPlot(
      x = c("blocknum","Block","CPT","RR","Raw_data","p"),
      y = c("log2RR"),
      groups = c("level"),
      data = data,
      type = "bubble",
      size = list(const = 1),
      height = 700,#600,
      width = 1200,#1200,
      bounds = list(x=50, y=10, width=900, height=480),
      xlab = "Block",
      ylab = "Log2(RR)"
    )
    d1$yAxis(type= "addMeasureAxis", grouporderRule = "level" )
    d1$xAxis(type= "addCategoryAxis", outputFormat = NULL)
    
    mytitle = paste("RR using glmnet and P vals using ", inputmethod, sep="")
    d1$addParams(title=mytitle)
    d1$legend(
      x = 150,#900-150,
      y = 15,#480+10+10,
      width = 60,
      height = 700-500,
      horizontalAlign = "right"
    )
    
    d1$setTemplate(
      afterScript =
        "<script>
      myChart.axes.filter(function(ax){return ax.position == 'x'})[0].titleShape.text(opts.xlab)
      myChart.axes.filter(function(ax){return ax.position == 'y'})[0].titleShape.text(opts.ylab)
      // This is a critical step.  By doing this we orphan the legend. This
      // means it will not respond to graph updates.  Without this the legend
      // will redraw when the chart refreshes removing the unchecked item and
      // also dropping the events we define below.
      myChart.legends = [];
      // This block simply adds the legend title. I put it into a d3 data
      // object to split it onto 2 lines.  This technique works with any
      // number of lines, it isn\'t dimple specific.
      svg.selectAll('title_text')
      .data(['Click legend to','show/hide points:'])
      .enter()
      .append('text')
      .attr('x', 1050)  //499)
      .attr('y', function (d, i) { return 90 + i * 14; })
      .style('font-family', 'sans-serif')
      .style('font-size', '10px')
      .style('color', 'Black')
      .text(function (d) { return d; });
      // Get a unique list of level values to use when filtering
      var filterValues = dimple.getUniqueValues(data, 'level');
      // Get all the rectangles from our now orphaned legend
      l.shapes.selectAll('rect')
      // Add a click event to each rectangle
      .on('click', function (e) {
      // This indicates whether the item is already visible or not
      var hide = false;
      var newFilters = [];
      // If the filters contain the clicked shape hide it
      filterValues.forEach(function (f) {
      if (f === e.aggField.slice(-1)[0]) {
      hide = true;
      } else {
      newFilters.push(f);
      }
      });
      // Hide the shape or show it
      if (hide) {
      d3.select(this).style('opacity', 0.2);
      } else {
      newFilters.push(e.aggField.slice(-1)[0]);
      d3.select(this).style('opacity', 0.8);
      }
      // Update the filters
      filterValues = newFilters;
      // Filter the data
      myChart.data = dimple.filterData(data, 'level', filterValues);
      // Passing a duration parameter makes the chart animate. Without
      // it there is no transition
      myChart.draw(800);
      myChart.axes.filter(function(ax){return ax.position == 'x'})[0].titleShape.text(opts.xlab)
      myChart.axes.filter(function(ax){return ax.position == 'y'})[0].titleShape.text(opts.ylab)
      myChart.axes[0].titleShape.text('Block')
      });
      
      //myChart.svg.append('g')
      //.append('text')
      //.text('Info of method needed here')
      //.attr('x',10)
      //.attr('y',30)
      //.style('font-size','200%')
      </script>")
#     d1$addParams(dom = 'myChart') #dont use this when using renderChart3!!
    
    print("Plot set-up done")
    return(d1)
  })
})




