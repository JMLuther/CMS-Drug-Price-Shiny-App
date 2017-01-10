library(shiny)
library(dplyr)
library(stringr)
library(shinythemes)
library(ggiraph)
library(scales)
drugData <- readRDS(file="drugData.rds") %>% group_by(formulation)
drugData <- drugData %>% filter(!average_cost_per_unit=="NaN" & !total_spending==0) 
drugData %>% group_by(year) %>% filter(beneficiary_count!=0) %>% 
  arrange(desc(total_annual_spending_per_user)) %>% 
  mutate(rank_total_per_user=row_number()) -> drugData


# THis is the server info  
#________________________________________________________________________________________________________________  
shinyServer(function(input, output, session) {
  
  
  # return a list of UI elements
  output$my_output_UI <- renderUI({
    
    selectInput(inputId = "myselect", label="Select Brand Name", selected = "Pyrimethamine|Daraprim",
                choices = selections, multiple = TRUE, selectize=FALSE, 
                size=ifelse(length(selections)>=20, 20, length(selections)))
  })
  
  selections <- sort(c("Pyrimethamine|Daraprim", "Sofosbuvir|Sovaldi|Harvoni", "Adrenaclick|Epipen|Twinject",
                       "Colchicine|Colcrys|Mitigare", "Losartan|Cozaar", "Glatiramer|Copaxone|Glatopa",
                       "Atorvastatin|Lipitor", "Ursodeoxycholic|Ursodiol|Actigall|Urso 250|Urso Forte", 
                       "Metoclopramide|Reglan|Metozolv",
                       "Fortamet|Glucophage|Glucophage XR|Glumetza|Riomet",
                       "Penicillamine|Cuprimine", "Corticotropin|H.P. Acthar",
                       "Omeprazole|Prilosec",
                       "Propranolol|Inderal",
                       "Diclofenac|Pennsaid",
                       "Hydroxychloroquine|Plaquenil"))
  
  observeEvent(input$mybutton,{
    
    # Provide a list of Brand Names based on the search string
    # Updated by clicking the button
    searchRegex <- paste0(input$search1, collapse="|")
    test <- sort(unique(drugData$brand_name[union(
      grep(searchRegex, drugData$generic_name, ignore.case = T),
      grep(searchRegex, drugData$brand_name, ignore.case = T)) ]))
    if (is.na(test[1])) {
      selections <<- selections
    } else {
      selections <<- test
    }
    updateSelectInput(session, "myselect", choices =  selections, selected = selections[1])
  })
  
  observeEvent(input$myselect,{
    output$plot1 <- renderggiraph({
      selectRegex <- paste0(input$myselect, collapse = "|")
      fig1 <- ggplot(data=filter(drugData, grepl(selectRegex, brand_name, ignore.case=T)),
                    aes(year, average_cost_per_unit, group=formulation, 
                        label=brand_name, color=dataset,
                         tooltip=tip, use_jquery=TRUE,
                         data_id=formulation)) + 
#        geom_line(color="grey50") +
#        geom_point(color="grey40") + 
        scale_y_continuous(labels=scales::dollar, limits = c(0,NA)) +
        scale_x_continuous(limits = c(2011, 2016.5)) +
        labs(x="Year", y="Average cost/unit",
             subtitle="Cost per unit") +
        ggrepel::geom_text_repel(data= filter(drugData, grepl(selectRegex, brand_name, ignore.case=T)
                                              & year==2015), color="black", size=2.5, nudge_x = .2) +
        geom_line_interactive() +
        geom_point_interactive() + 
        expand_limits(x=2016.5) + guides(color=guide_legend(title="Coverage Type:")) +
        ggthemes::theme_tufte()
    ggiraph(code=print(fig1), hover_css="stroke:orange;stroke-width:3", 
            width = 0.6, selected_css = "stroke:orange;stroke-width:3", tooltip_offy = -200)
    })

    output$plot2 <- renderggiraph({
      selectRegex <- paste0(input$myselect, collapse = "|")
      fig2 <- ggplot(data=filter(drugData, grepl(selectRegex, brand_name, ignore.case=T)),
                     aes(year, total_spending, group=formulation, 
                         label=brand_name, color=dataset,
                         tooltip=tip, use_jquery=TRUE,
                         data_id=formulation)) + 
        #        geom_line(color="grey50") +
        #        geom_point(color="grey40") + 
        scale_y_continuous(labels=scales::dollar, limits = c(0,NA)) +
        scale_x_continuous(limits = c(2011, 2016.5)) +
        labs(x="Year", y="Total Medicaid Cost",
             subtitle="Total Cost") +
        ggrepel::geom_text_repel(data= filter(drugData, grepl(selectRegex, brand_name, ignore.case=T)
                                              & year==2015), color="black", size=2.5, nudge_x = .2) +
        geom_line_interactive() +
        geom_point_interactive() + 
        expand_limits(x=2016.5) + guides(color=guide_legend(title="Coverage Type:")) +
        ggthemes::theme_tufte()
      ggiraph(code=print(fig2), hover_css="stroke:orange;stroke-width:3", 
              width = 0.6, selected_css = "stroke:orange;stroke-width:3", tooltip_offy = -200)
    })
    
    output$plot3 <- renderggiraph({
      selectRegex <- paste0(input$myselect, collapse = "|")
      fig3 <- ggplot(data=filter(drugData, grepl(selectRegex, brand_name, ignore.case=T)),
                     aes(year, beneficiary_count, group=formulation, 
                         label=brand_name, color=dataset,
                         tooltip=tip, use_jquery=TRUE,
                         data_id=formulation)) + 
        #        geom_line(color="grey50") +
        #        geom_point(color="grey40") + 
        scale_y_continuous(limits = c(0,NA)) +
        scale_x_continuous(limits = c(2011, 2016.5)) +
        labs(x="Year", y="Total number of Patients", subtitle="Total Beneficiaries") +
        ggrepel::geom_text_repel(data= filter(drugData, grepl(selectRegex, brand_name, ignore.case=T)
                                              & year==2015), color="black", size=2.5, nudge_x = .2) +
        geom_line_interactive() +
        geom_point_interactive() + 
        expand_limits(x=2016.5) + guides(color=guide_legend(title="Coverage Type:")) +
        ggthemes::theme_tufte()
      ggiraph(code=print(fig3), hover_css="stroke:orange;stroke-width:3", 
              width = 0.6, selected_css = "stroke:orange;stroke-width:3", tooltip_offy = -200)
    })
    
    output$plot4 <- renderggiraph({
      fig4 <- drugData %>% group_by(formulation) %>% filter(!is.na(total_annual_spending_per_user)) %>% 
        filter(rank_total_per_user<=25) %>% 
        ggplot(aes(year, total_annual_spending_per_user, group=formulation, label=brand_name, color=dataset,
                   tooltip=tip, use_jquery=TRUE, data_id=formulation)) + 
        scale_y_continuous(labels=scales::dollar, limits = c(0,NA)) +
        scale_x_continuous(limits = c(2011, 2016.5)) +
        labs(x="Year", y="Total Cost per Beneficiary", subtitle="Top 25 Most expensive per Beneficiary") +
        # ggrepel::geom_text_repel(data= filter(drugData, !is.na(total_annual_spending_per_user) & 
        #                                                          rank_total_per_user<=25 & year==2015), 
        #                                       color="black", size=2.5, nudge_x = .2) +
        geom_line_interactive() +
        geom_point_interactive() + 
        expand_limits(x=2016.5) + guides(color=guide_legend(title="Coverage Type:")) +
        ggthemes::theme_tufte()
      ggiraph(code=print(fig4), hover_css="stroke:orange;stroke-width:3", 
              width = 0.6, selected_css = "stroke:orange;stroke-width:3", tooltip_offy = -200)
    })
    
    
    }
  )
  
})
