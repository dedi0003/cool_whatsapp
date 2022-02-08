library(shiny)
library(bslib)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggimage)
library(plotly)
library(textdata)
library(rwhatsapp)
library(lubridate)
library(tidytext)
library(kableExtra)
library(RColorBrewer)
library(knitr)
library(ggrepel)
library(stopwords)
library(rvest)
library(wordcloud)

# WE USE THE RVEST PACKAGE

# HTML PAGE FETCH EMOJI SENTIMENT RANKING 1.0
# url_base <- "http://kt.ijs.si/data/Emoji_sentiment_ranking/index.html"
# doc <- read_html(url_base)
# # SEARCH EMOJI TABLE AND PROCESS
# table_emojis <- doc %>%
#     html_node("#myTable") %>%
#     html_table() %>%
#     as_tibble()
#
# write_csv2(table_emojis, "table_emo.csv")

table_emojis <- read_csv2("table_emo.csv")

remover_words <- read_csv("id_stopwords.txt", col_names = FALSE) %>%
    as_tibble()

add_words <- c("gw","sticker omitted","image omitted",
               "media", "omitted", "yah", "y", "ya", "tidak",
               "image","sticker", "kh","lh","lah","Messages and calls are end-to-end encrypted. No one outside of this chat, not even WhatsApp, can read or listen to them.",
               "yg", "ya", "mas", "ga", "klo", "aja", "nya","nih", "kh", "kk", "jg", "km", "kak", "1","2","3","4","5","6","7","8","9", "10", "ane", "dh", "ad", "lu", "dn") %>%
    as_tibble()

remover_indo <- full_join(remover_words, add_words, by = c("X1" = "value"))

sentimen_emoji <- table_emojis %>%
    select(1,6:9) %>%
    set_names("char", "negative","neutral","positive","sent.score")


# GET POSITIVE / NEGATIVE FROM LEXICON PACKAGE

lexico_negpos <- readRDS("lexico_negpos.RDS")

palet <- brewer.pal(8,"Set1")[c(7,5,1,3,4,2,6,8)]

# VERIFYING HOW MANY MESSAGES WERE SENT DURING THE PERIOD OF TIME


# daily chat

days <- c("Mon","Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

paragraph_style <- ("text-align: justify; margin-left: 50px; margin-right: 50px")

# Define UI for application that draws a histogram
ui <- navbarPage(title = "Cool Whatsapp Chat Analyzer",
                 theme = bs_theme(bootswatch = "minty"),

                 tabPanel(title = "How to Use", br(),
                          div(style = paragraph_style,
                          "This site is a place where you can do whatsapp chat analysis easily
                          and conveniently. All you need to do is just uploading your text data exported from
                          your whatsapp mobile app. Please make sure that you already gave names to all of your contacts before exporting the .txt file
                          from the whatsapp app. "), br(),
                          div(style = paragraph_style,
                          "Steps to do: "), br(),
                          fluidRow(column(
                          p(style = paragraph_style,
                            paste0("1. Upload your file in .txt or .zip format"), br(),
                            paste0("2. Go to Frequency menu to analyze quantity of the messages been sent"), br(),
                                   paste0("3. Go to Emoji menu to analyze emoji that mostly used by senders"),br(),
                                          paste0("4. Go to Member's Rank menu to see the most active group's members"), br(),
                                                 paste0("5. Go to Wordclouds menu to check what words are commonly used by the group, as well as unique words of each sender"),br(),
                                                        paste0("6. Finally, you can go to Lover/Hater menu to evaluate the intimacy of your relationship (requires private chat instead of group chat)"), br(),
                            paste0("Please rest assured that no chat data is being sent to the server, in other words, all text processing will be done on your local computer.")
                            ), width = 6), br(),

                          column(
                          div(style = paragraph_style, HTML('<img src="https://img.tek.id/img/content/2021/08/23/44343/whatsapp-untuk-ipad-bisa-jadi-kenyataan-9QbNbaLwQJ.jpeg" width="400" height="250">'))
,width = 6)),br(),br(), br(),br(),
column("", width = 12),
div(style = paragraph_style, HTML('<marquee scrolldelay="100" bgcolor="pink" color:#000000>Welcome to Cool Whatsapp Chat Analyzer! You can do several analysis using this dashboard. I hope you enjoy it!</marquee>'))

                          ),

                 tabPanel(title = "Upload Data",
                          fluidPage(
                              sidebarLayout(
                                  sidebarPanel(
                                      fileInput("upload", "Choose txt file", accept = "text"),
                                      tags$hr()
                                  ),
                                  mainPanel(
                                      p("You can upload your text file (.txt or .zip) exported from whatsapp mobile app.", br(),
                                        paste0("1. Go to Group's Info on your whatsapp (or User's Info for private chat)"), br(),
                                        paste0("2. On Android phone, go to the top right menu with three dots, then select 'export chat' and save the file somewhere on your phone"), br(),
                                        paste0("3. On iPhone, go to the bottom of the group/user info, then select 'export chat' and save the file somewhere on your phone"), br(),
                                        paste0("4. Upload the file exported from whatsapp, either in .zip or .txt"),

                                        style = paragraph_style)

                                  )
                              )
                          )
                          ),
                 tabPanel(title = "Frequency",
                          fluidRow(box(title = "",  width = 12)),
                          fluidRow(
                                column(width = 1),
                                br(),
                                br(),
                                  column(plotOutput("chat_daily"), width = 5),
                                  column(plotOutput("chat_hour"), width = 5),
                                column(width = 1)

                              )
                          ),

                 tabPanel(title = "Emoji",
                          fluidPage(
                              sidebarLayout(
                                  sidebarPanel(
                                      uiOutput("choose"),
                                      tags$hr()
                                  ),
                                  mainPanel(
                                      fluidRow(
                                          column(p("Select sender name to see their favorite emoji!"), width =6),
                                               br(),
                                      column(
                                         plotOutput("emoji"), width = 7)
                                      )

                                  )
                              )
                          )
                 ),
                 tabPanel(title = "Member's Rank",
                          fluidRow(plotOutput("active")

                          )
                 ),
                 tabPanel(title = "Wordclouds",
                          fluidPage(
                              sidebarLayout(
                                  sidebarPanel(
                                      uiOutput("unique_user"),
                                      tags$hr()
                                  ),
                                  mainPanel(
                                      fluidRow(
                                          column(p("See what mostly typed words & unique words used by a specific sender!"), width = 12),
                                          br(),
                                         ),
                                      fluidRow(plotOutput("mostwords")),
                                      fluidRow(
                                          plotOutput("wordcloud"))

                                  )
                              )
                          )
                 ),

                 tabPanel(title = "Lover/Hater",
                          fluidPage(
                              sidebarLayout(
                                  sidebarPanel(
                                      fileInput("lohaupload", "Choose txt file", accept = "text"),
                                      tags$hr(),

                                          br(),

                                              p("Levels of intimacy are: ", br(),
                                                paste0("1. Lover"), br(),
                                                paste0("2. Friendly"), br(),
                                                paste0("3. Ordinary"), br(),
                                                paste0("4. Unfriendly/Hater"), br(),
                                                style = 'text-align: justify')



                                  ),
                                  mainPanel(
                                      fluidRow(
                                          column(p("This section can be used for analyzing private chat of 2 people to see if they are lovers,
                                                   just friends, or haters.", style = paragraph_style), width = 12),
                                          br(),
                                          hr(),
                                      ),
                                      fluidRow(column(
                                          plotOutput("emo_sentiment"), width = 12)
                                          ), br(),
                                      column("" , width = 12),
                                      fluidRow(
                                          column(
                                          textOutput("prediction"), width = 12),
                                          tags$head(tags$style("#prediction{
                                          text-align: justify;
                                          margin-left: 50px; margin-right: 50px;
                                          font-weight: bold;
                                          font-size = 16px;

                                 }"
                                          )
                                          )

                                  )

                              )
                          )
                 ))

)



# Define server logic required to draw a histogram
server <- function(input, output, session) {


global_data <-    reactive({
        req(input$upload)

        text_chat <- rwhatsapp::rwa_read(input$upload$datapath) %>% filter(!is.na(author)) %>%
            mutate(day = day(time), month = month(time,label = TRUE), hour= hour(time), year = year(time), date = date(time), wday = wday(time, label = TRUE)) %>%
            na.omit() %>%
            select(-source)

        return(text_chat)

    })


    output$choose <- renderUI({
        req(input$upload)

        text_chat <- global_data()

        selectInput("sender", "Choose sender: ", choices = text_chat$author)
    })

    output$unique_user <- renderUI({
        req(input$upload)

        text_chat <- global_data()

        selectInput("unique", "Choose sender: ", choices = text_chat$author)
    })



    output$chat_daily <- renderPlot({

        text_chat <- global_data()

        text_day <- text_chat %>%
            group_by(day, month, year, wday, hour) %>%
            summarise(n_chat = n())

        text_day %>%
            ggplot(aes(x = as.factor(wday), y = n_chat)) +
            geom_col(fill = "darkgreen") +
            scale_fill_brewer()+
            ylab("") + xlab("hari") +
            ggtitle("Messages sent daily") +
            theme_minimal() +
            ylim(0, 100)+
            scale_x_discrete(guide = guide_axis(n.dodge=3))+
            theme(axis.text.y = element_text(size = 14),
                  axis.text.x = element_text(size = 14),
                  axis.title.x = element_text(size = 14),
                  strip.text = element_text(size=14),
                legend.title = element_blank(),
                   legend.position = "bottom",
                   plot.title = element_text(size = 16, face = "bold"))+
            facet_wrap(.~ year, ncol = 2)

    })

    output$chat_hour <- renderPlot({
        req(input$upload)

        text_chat <- global_data()

        text_day <- text_chat %>%
            group_by(day, month, year, wday, hour) %>%
            summarise(n_chat = n())

        text_day %>%
            ggplot(aes(x = as.factor(hour), y = n_chat)) +
            geom_col(fill = "darkgreen") +
            scale_fill_brewer()+
            ylab("") + xlab("jam") +
            ggtitle("Messages sent hourly") +
            theme_minimal() +
            ylim(0, 100)+
            scale_x_discrete(guide = guide_axis(n.dodge=3))+
            theme( legend.title = element_blank(),
                   legend.position = "bottom",
                   axis.text.y = element_text(size = 14),
                   axis.text.x = element_text(size = 14),
                   axis.title.x = element_text(size = 14),
                   strip.text = element_text(size=14),
                   plot.title = element_text(size = 16, face = "bold"))+
            facet_wrap(.~ year, ncol = 2)

        })

    output$emoji <- renderPlot({

        req(input$sender)

        text_chat <- global_data()


        plotEmojis_member <- text_chat %>%
            unnest(emoji, emoji_name) %>%
            mutate( emoji = str_sub(emoji, end = 1)) %>% #
            count(author, emoji, emoji_name, sort = TRUE) %>%
            # PLOT TOP 10 EMOJIS PER USER
            group_by(author) %>%
            top_n(n = 10, n) %>%
            slice(1:8) %>%
            mutate(emoji_url = map_chr(emoji,
                                        ~paste0("https://abs.twimg.com/emoji/v2/72x72/",as.hexmode(utf8ToInt(.x)),".png")) )


        # PLOT DATA
       plotEmojis_member %>%
            filter(author == input$sender) %>%

            ggplot(aes(x = reorder(emoji, n), y = n)) +
            geom_col(show.legend = FALSE, width = .16, fill = "pink") +
            # USE TO FETCH AN EMOJI PNG IMAGE https://abs.twimg.com
            geom_image(aes(image=emoji_url), size = 0.1) +
            ylab("") +
            xlab("") +
            # facet_wrap(~author, ncol = 5, scales = "free") +
            ggtitle(paste0("Favourite emoji of ", input$sender))+
            theme_minimal() +
            theme(axis.text.y = element_text(size = 14),
                  axis.text.x = element_blank(),
                  axis.title.x = element_text(size = 14),
                  line = element_blank(),
                  plot.title = element_text(size = 16, face = "bold"))+
            coord_flip()


    })

    output$active <- renderPlot({

        text_print <- global_data()

        text_active <- text_print %>%
            group_by(author) %>%
            summarise(author, n = n()) %>%
            unique() %>%
            arrange(desc(n))


        text_active %>%
            ggplot(aes(x = reorder(author, n), y = n)) +
            geom_bar(stat = "identity", fill = ifelse(text_active$n == max(text_active$n), "red", "darkgreen")) +
            ylab("Messages") + xlab("") +
            coord_flip() +
            ggtitle("The most active members")+
            theme_minimal()+
            scale_y_discrete(guide = guide_axis(n.dodge=3))+
            scale_x_discrete(guide = guide_axis(n.dodge=3))+
            theme(axis.text.y = element_text(size = 14),
                 axis.text.x = element_blank(),
                 axis.title.x = element_text(size = 14),
                line = element_blank(),
                  plot.title = element_text(size = 16, face = "bold"))
    })


    output$mostwords <- renderPlot({
    # REMOVE WORDS WITHOUT RELEVANT MEANING, SUCH AS PRONOUNS, ETC.



    # WORD COUNT
    text_plot <- global_data()

    text_plot %>%
        unnest_tokens(input = text, output = word) %>%
        filter(!word %in% remover_indo$X1) %>%
        count(word) %>%
        # PLOT TOP 30 MOST USED WORDS IN CONVERSATION
        top_n(15,n) %>%
        arrange(desc(n)) %>%
        ggplot(aes(x=reorder(word,n), y=n, fill=n, color=n)) +
        geom_col(show.legend = FALSE, width = .1) +
        geom_point(show.legend = FALSE, size = 3) +
        scale_fill_gradient(low="#2b83ba",high="#d7191c") +
        scale_color_gradient(low="#2b83ba",high="#d7191c") +
        ggtitle("Most used words") +
        xlab("") +
        ylab("") +
        coord_flip() +
        theme_minimal()+
        theme(axis.text.y = element_text(size = 14),
              axis.text.x = element_text(size = 14),
              axis.title.x = element_text(size = 14),
              line = element_blank(),
              plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
              )
    })



    mostword <- reactive({
        req(input$upload)
        req(input$unique)

        text_chat <- rwhatsapp::rwa_read(input$upload$datapath) %>% filter(!is.na(author)) %>%
            mutate(day = day(time), month = month(time,label = TRUE), hour= hour(time), year = year(time), date = date(time), wday = wday(time, label = TRUE)) %>%
            na.omit() %>%
            select(-source)

        mostword <- text_chat %>%
            filter(author == input$unique) %>%
            unnest_tokens(input = text, output = word) %>%
            filter(!word %in% remover_indo$X1) %>%
            count(word) %>%
            # PLOT TOP 30 MOST USED WORDS IN CONVERSATION
            top_n(30,n) %>%
            arrange(desc(n))

        return(mostword)
    })


    output$wordcloud <- renderPlot({


        # table_emojis <- read_csv2("table_emo.csv")
        #
        # remover_words <- read_csv2("id_stopwords.txt", col_names = FALSE) %>%
        #     as_tibble()
        #
        # add_words <- c("gw","sticker omitted","image omitted",
        #                "media", "omitted", "yah", "y", "ya", "tidak",
        #                "image","sticker", "kh","lh","lah","Messages and calls are end-to-end encrypted. No one outside of this chat, not even WhatsApp, can read or listen to them.",
        #                "yg", "ya", "mas", "ga", "klo", "aja", "nya","nih", "kh", "kk", "jg", "km", "kak", "1","2","3","4","5","6","7","8","9", "10", "ane", "dh", "ad", "lu", "dn") %>%
        #     as_tibble()
        #
        # remover_indo <- dplyr::full_join(remover_words, add_words, by = c("X1" = "value"))

        # sentimen_emoji <- table_emojis %>%
        #     select(1,6:9) %>%
        #     set_names("char", "negative","neutral","positive","sent.score")

        # WORD COUNT
        wordie <- mostword() %>%
            as_tibble()

        # set.seed(1234) # for reproducibility
        layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
        par(mar=rep(0, 4))
        plot.new()

        text(x = 0.5, y = 0.5, paste0("Most unique words from ", input$unique), cex = 1.5, font = 2)

        wordcloud(words = wordie$word, freq = wordie$n, min.freq = 1,
                  max.words=100, random.order=FALSE, rot.per=0.35,
                  colors=brewer.pal(8, "Dark2"), scale=c(4,0.5))
    }
    )



    global_loha <-  reactive({
        req(input$lohaupload)

        loha_chat <- rwhatsapp::rwa_read(input$lohaupload$datapath) %>% filter(!is.na(author)) %>%
            mutate(day = day(time), month = month(time,label = TRUE), hour= hour(time), year = year(time), date = date(time), wday = wday(time, label = TRUE)) %>%
        select(-source) %>% na.omit()

        return(loha_chat)
    })

    emoji_user <- reactive({

    ### Remover words


    # EXTRACT EMOJI AND UNITE WITH FEELING
    chat_loha <- global_loha()

    emoji_chat <- chat_loha %>%
        unnest(emoji, emoji_name) %>%
        mutate( emoji = str_sub(emoji, end = 1)) %>%
        inner_join(sentimen_emoji, by=c("emoji"="char"))


    # EXTRACT EMOJIS

    emoji_sentimen_score <- chat_loha %>%
        select( emoji, emoji_name) %>%
        unnest( emoji, emoji_name) %>%
        mutate( emoji = str_sub(emoji, end = 1)) %>%
        mutate( emoji_name = str_remove(emoji_name, ":.*")) %>%
        distinct() %>%
        unnest_tokens(input=emoji_name, output=emoji_words) %>%
        inner_join(lexico_negpos, by=c("emoji_words"="word"))

    emoji_sentimen_user <- emoji_chat %>%
        group_by(author) %>%
        summarise(
            positive=mean(positive),
            negative=mean(negative),
            neutral=mean(neutral),
            balance=mean(sent.score)
        ) %>%
        arrange(desc(balance))


    return(emoji_sentimen_user)
    })

    # # CREATE TABLE OF 3 COLUMNS
    # bind_cols(
    #     slice(emoji_sentimen_score, 01:10),
    #     slice(emoji_sentimen_score, 11:20),
    #     slice(emoji_sentimen_score, 21:30)
    # )

    # kable(emoji_sentimimen_score) %>%
    #     kable_styling(full_width = F, font_size = 11)


    # OCCURRENCES OF FEELINGS BY EMOJIS, BY USER



    output$emo_sentiment <- renderPlot({

        emo_sen <- emoji_user()

        emo_sen %>%
            mutate( negative  = -negative,
                    neutral.positive =  neutral/2,
                    neutral.negative = -neutral/2) %>%
            select(-neutral) %>%
            arrange(desc(positive)) %>%
            head(10) %>%
            gather("sentiment","mean", -author, -balance) %>%
            mutate(sentiment = factor(sentiment, levels = c("negative", "neutral.negative", "positive", "neutral.positive"), ordered = T)) %>%
            ggplot(aes(x=reorder(author,balance), y=mean, fill=sentiment)) +
            geom_bar(position="stack", stat="identity", show.legend = F, width = .5) +
            scale_fill_manual(values = brewer.pal(4,"RdYlGn")[c(1,2,4,2)]) +
            xlab("")+
            ylab(" - Negative / Neutral / Positive +") +
            ggtitle("Senders' sentiments based on emoji") +
            coord_flip() +
            theme_minimal()+
            theme(axis.text.y = element_text(size = 14),
                  axis.text.x = element_text(size = 14),
                  axis.title.x = element_text(size = 14),
                  strip.text = element_text(size=14),
                  legend.title = element_blank(),
                  legend.position = "bottom",
                  plot.title = element_text(size = 16, face = "bold"))

    })

    output$prediction <- renderText({
        req(input$lohaupload)

        chat_loha <- global_loha()

        private_sum <- chat_loha %>%

            group_by(author) %>%
            summarise(chat = n()) %>%
        mutate(pct = chat/sum(chat))

        emo_sen <- emoji_user()
        loha <- left_join(emo_sen, private_sum)

        lover_hater <- loha %>%
            mutate(prediction = case_when(balance >= 0.4 & pct >= 0.5 & positive >= 0.65 ~ "lover",
                                          positive >= 0.6 & pct >= 0.55 ~ "aggressive",
                                         between(balance, 0.35, 0.4) & between(pct, 0.45, 0.5) ~ "friendly",
                                        balance <= 0.35  & negative > 0.1 | pct <= 0.4 ~ "unfriendly/hater",
                                          TRUE ~ "ordinary"))

        if(is.null(input$lohaupload)){
            paste0("")}
            else {
        paste0("From our calculation based on emoji sentiment and number of messages being sent, it's most likely that ",
                lover_hater$author[2], " is ", lover_hater$prediction[2], " meanwhile ", lover_hater$author[1],
          " is ", lover_hater$prediction[1], "."
        )
                }

    })

}

# Run the application
shinyApp(ui = ui, server = server)
