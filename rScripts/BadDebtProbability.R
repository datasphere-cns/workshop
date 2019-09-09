setwd("C:/workshopDS/datasets")

install.packages("src/caret_6.0-68.zip", lib=".", repos= NULL, verbose=TRUE)
library("caret", lib.loc=".", verbose=TRUE)
library(caret)
df<-read.table("SoloNumericAtt.csv",dec=".",sep=",",header=TRUE,colClasses=c("numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
str(df)
table(df$ENMORA)

head(df)

df$ENMORA <- as.factor(df$ENMORA)
set.seed(100)
trainDataIndex <- createDataPartition(df$ENMORA, p=0.7, list = F)
trainData <- df[trainDataIndex, ]
testData <- df[-trainDataIndex, ]

table(trainData$ENMORA)

# Down Sample
'%ni%' <- Negate('%in%')  # Creando funcion Not In
set.seed(100)
down_train <- downSample(x = trainData[, colnames(trainData) %ni% "Class"],
                         y = trainData$ENMORA)

table(down_train$ENMORA)


up_train <- upSample(x = trainData[, colnames(trainData) %ni% "Class"],
                         y = trainData$ENMORA)

table(up_train$ENMORA)




logitmod <- glm(ENMORA ~ COCINA_ELECTRICA+LAVANDERIA+AIREACONDICIONADO+PCT_USODIA+PCT_USONOCHE+LLAMADASACALLCENTER+REGION+ANTIGUEDAD_CLIENTE+USUARIO_CANAL_DIGITAL+PUNTO_PAGO,data=down_train,family="binomial")



pred <- predict(logitmod, newdata = testData, type = "response")
pred

y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$ENMORA


MC  <- table(y_act,y_pred)
MC

table(testData$ENMORA)
# Accuracy
mean(y_pred == y_act)



###aplicando a toda la base
total <- predict(logitmod, newdata = df, type = "response")
total_pred_num <- ifelse(total > 0.5, 1, 0)
total_pred <- factor(total_pred_num, levels=c(0, 1))
total_act <- df$ENMORA
write.table(total,file="probabilidad_mora.csv",row.names=FALSE)


#Operativizando el Modelo
save(logitmod, file = "baddebt_logit.rda")



#GUI
library(shiny)
library(shinydashboard)


server <- function(input, output) 
{

  load("baddebt_logit.rda") 
  output$Score <- renderText({
	
	df_funct <- cbind(input$Cocina,	input$Lavanderia,	input$Aire,	input$Porcentaje,	100-input$Porcentaje,	as.numeric(input$Mora),
						input$LLamadas,	as.numeric(input$Region),	input$Antiguedad,	as.numeric(input$App),	as.numeric(input$Punto)	)


	mydatanew<-data.frame(df_funct)
	names(mydatanew)<-c("COCINA_ELECTRICA","LAVANDERIA","AIREACONDICIONADO","PCT_USODIA","PCT_USONOCHE","ENMORA","LLAMADASACALLCENTER","REGION","ANTIGUEDAD_CLIENTE","USUARIO_CANAL_DIGITAL","PUNTO_PAGO")
	mydatanew[,'COCINA_ELECTRICA'] <- as.numeric(as.character(mydatanew[,'COCINA_ELECTRICA']))
	mydatanew[,'LAVANDERIA'] <- as.numeric(as.character(mydatanew[,'LAVANDERIA']))
	mydatanew[,'AIREACONDICIONADO'] <- as.numeric(as.character(mydatanew[,'AIREACONDICIONADO']))
	mydatanew[,'PCT_USODIA'] <- as.numeric(as.character(mydatanew[,'PCT_USODIA']))
	mydatanew[,'PCT_USONOCHE'] <- as.numeric(as.character(mydatanew[,'PCT_USONOCHE']))
	mydatanew[,'ENMORA'] <- as.numeric(as.character(mydatanew[,'ENMORA']))
	mydatanew[,'LLAMADASACALLCENTER'] <- as.numeric(as.character(mydatanew[,'LLAMADASACALLCENTER']))
	mydatanew[,'REGION'] <- as.numeric(as.character(mydatanew[,'REGION']))
	mydatanew[,'ANTIGUEDAD_CLIENTE'] <- as.numeric(as.character(mydatanew[,'ANTIGUEDAD_CLIENTE']))
	mydatanew[,'USUARIO_CANAL_DIGITAL'] <- as.numeric(as.character(mydatanew[,'USUARIO_CANAL_DIGITAL']))
	mydatanew[,'PUNTO_PAGO'] <- as.numeric(as.character(mydatanew[,'PUNTO_PAGO']))
	#mydatanew[,] <- sapply(mydatanew[,], as.numeric)
	
	Score <- round(predict(logitmod, newdata=mydatanew, type="response"), digits=2)*100
	
	})
}


ui <-fluidPage(

  titlePanel("DataSphere Risk% Calculator"),
  fluidRow(
    column(
	  sliderInput("Cocina",
                   "KWh Cocina",
                   20,90,48),
      sliderInput("Lavanderia",
                   "KWh Lavanderia",
                   20,90,80),
      sliderInput("Aire",
                   "KWh Aire Acondicionado",
                   20,90,41),
      sliderInput("Porcentaje",
                   "Porcentaje Uso 6am a 6pm %",
                   0.1,1,0.75), width = 4, heigth=4
			  
			  ),
	
	   column( 
	selectInput("Mora", "Ha estado en Mora?:", 
              choices = c("No" = 0, "Si" = 1)),
			  
  
      selectInput("LLamadas", "Cantidad Llamadas a Call Center:", 
              choices = c("Ninguna" = 0, "Una" = 1,"Dos"=2,"Tres o Mas"=3)),
			  		   
      selectInput("Region", "Zona Occidente/Central/Oriental:", 
              choices = c("Occidente" = 1, "Central" = 2,"Oriente"=3)),
			  
	numericInput("Antiguedad",
                   "Antiguedad en Meses",
                   49),	

	selectInput("App", "Utiliza nuestra App?:", 
              choices = c("Si" = 0, "No" = 1)), width = 4, heigth=4
			  
			  ),


  column( 			  
	
	selectInput("Punto", "Punto de Pago:", 
              choices = c("Banco" = 1, "Caja Express" = 2,"Agencias"=3,"Online"=4)),

      submitButton("Submit"), width = 4, heigth=4
	  

    ),


    mainPanel(

      h1(textOutput("Score"))
	 
	  


    )
  )
)


shinyApp(ui = ui, server = server)
