
library(data.table) # For Fast Data Loading
library(forcats) # To use the forcats function
library(dplyr)
library(ggplot2)
library(inspectdf) # Automatic Data Exploration Analysis
library(stringi) #For handling strings
library(stringr) #For handling strings
getwd()



###!!!!!
#Put path of the directory containing the data files
x <-paste("/Users/soheilmoattarmohammadiberenguer/Desktop/Entrega_Soheil_Moattar_Berenguer/Fifa_App",
          sep="")





#Se the specified path as new directory
setwd(x)
#
df=fread("FIFA22.csv",data.table=FALSE)




Fun_NA <- function(x) {
  tmp.Na<-sum(is.na(x)*100)/length(x)
  return(tmp.Na)
}



df%>% sapply(Fun_NA)

#get all columns cotopntaining NAs
df.Na<-df%>% sapply(Fun_NA)


contain_Na<-df.Na[df.Na>0]%>%names()

#We have many columns with a high percentage of NAs.
#Since our goal here is visualiztion rather than training a model,
#we can just drop these columns to have a *clean* dataframe.

df<-df%>%select(-c(all_of(contain_Na)))


#No we have a 16710 x 52 data frame


#######################################################
#Some rows have blank values in the Club column
df<-df[!df$Club=="",]


df2=fread("players_22.csv",data.table=FALSE)





# # Filtramos los datos para quedarnos solo con las 100
# #equipos mas valiosos
top100_clubs<-df2 %>%group_by(club_name,league_name)%>%
  dplyr::summarise(Value = sum(value_eur))%>%
  dplyr::arrange(desc(Value))%>%head(100)

# We will only use these column from the second dataframe
cols_to_keep<-c("sofifa_id","value_eur","wage_eur","league_name","club_name")




# #Este dataset solo tendra los jugadores que jueguen es estos 100 clubs
df_100<-df2[df2$club_name %in% top100_clubs$club_name ,cols_to_keep]

colnames(df_100)[which(colnames(df_100) == "sofifa_id")] <- 'ID'





df3<-merge(df_100, df, by = "ID")



# Descending reordering for a neater experience
data<-df3%>%arrange(desc(wage_eur))



#drop unnecessary duplicate columns

cols_to_drop<- c("Position")
data<-data%>%select(-c(all_of(cols_to_drop)))

#Now we have a 2893 x 56 dataset which will be used for our visualization


#data for EDA

data_eda<-data



rm(df)





data$`Contract Valid Until`<-as.factor(data$`Contract Valid Until`)


#levels(data$`Contract Valid Until`)[levels(data$`Contract Valid Until`)=="Dec 31, 2022"] <- "2022"



# renaming the factors
levels(data$`Contract Valid Until`) <- list("Free Agent"="Dec 31, 2022",
                    "Summer 2022"="2022",
                    "Summer 2022"="2022",
                    "2023"="Summer 2023",
                    "Free Agent"="nan",
                    "Free Agent"= "2021",
                    "Free Agent"= "2022",
                    "2023"="2023",
                    "2024"="2024",
                    "2025"="2025",
                    "2026"="2026",
                    "2027+"="2027",
                    "2027+"="2028",
                    "2027+"="2031"
                    )
print("Modified Levels of factor")
# 

data$`Release Clause`<-data%>%select(`Release Clause`)%>%pull(`Release Clause`)%>%
  str_replace(pattern = "[0-9]*K", replacement = "0")%>%
  #str_replace(pattern = "€", replacement = "")%>%
  #str_replace(pattern = "M", replacement = "")%>%
str_replace(pattern = "nan", replacement = "0 M")  #as.factor()


  
  data$SprintSpeed<-data%>%select(SprintSpeed)%>%pull(SprintSpeed)%>%
  str_replace(pattern = ".00", replacement = "")
  #str_replace(pattern = "€", replacement = "")%>%
  #str_replace(pattern = "M", replacement = "")%>%
  #str_replace(pattern = "nan", replacement = "0 M")%>%  #as.factor()
  
  


data$Name<-data%>%select(Name)%>%pull(Name)%>%
  #str_replace(pattern = " ", replacement = "")%>%
  as.factor()


cols_to_change<- c("value_eur","wage_eur")

#Divide Value by Million and Wages by Thousand





# 
# data[,cols_to_change]<-data%>%select(cols_to_change)%>%
#   mutate(value_eur= value_eur/1000000)%>%
#   mutate(wage_eur= wage_eur/1000)%>%as.factor()
# 


colnames(data)[colnames(data) == "Release Clause"] <- 'ReleaseClause'
colnames(data)[colnames(data) == "Best Position"] <- 'Position'



colnames(data)[colnames(data) == "league_name"] <- 'League'


# 

data%<>%mutate(Stamina=as.factor(Stamina))%>%
  mutate(Dribbling=as.factor(Dribbling))

data$Dribbling%>%class()


# 
# 
# Specifying List of certain attributes
playerAtt <- c("Name", "Age", "Nationality", "Overall", "Potential","Club", "League", "Value", "Wage","Position","Height", "Weight","Dribbling","Stamina","SprintSpeed", "ReleaseClause")
playerAttNUM <- c("Age", "Height", "Weight","Overall","Potential", "Dribbling","Stamina", "SprintSpeed")






# Creating shortened dataframes for specific attributes
fifa2 <- data[playerAtt]
fifaNUM <- data[playerAttNUM]

# colnames(fifaNUM )[colnames(fifaNUM ) == 'Value (Million €)'] <- 'Value(Million€)'
# 
# colnames(fifaNUM )[colnames(fifaNUM ) =='Wage (Thousand €)'] <- 'Wage(Thousand€)'
# colnames(fifaNUM )[colnames(fifaNUM ) == 'Release Clause (Million €)'] <- 'ReleaseClause(Million€)'



write.csv(data,"./data.csv", row.names = FALSE)


write.csv(data,"./fifa2.csv", row.names = FALSE)


write.csv(data,"./fifaNUM.csv", row.names = FALSE)

