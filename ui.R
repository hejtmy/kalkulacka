library(shiny)
library(shinyWidgets)

inline_numericInput=function(ni){
  tags$div( class="form-inline",ni)
}

shinyUI(fluidPage(titlePanel(img(src="logo.png"), windowTitle="Diagnostická kalkulačka"),
                  navbarPage(title = "Diagnostická kalkulačka", 
                             theme = shinythemes::shinytheme("cerulean"),
                             id = "panels",
                             
                             
# Home page ---------------------------------------------------------------
                             
tabPanel("Domů",
         withMathJax(),
         fluidRow(column(h2("Diagnostická kalkulačka"), 
                         p("Diagnostická kalkulačka zjednodušuje život všem psychologům, 
                           kteří se v praxi zabývají", strong("psychologickým testováním."), 
                           "Nechte se provést intervaly spolehlivosti, kritickými skóry a 
                           dalšími otázkami z psychometrické problematiky."),
                         p("Pro používání diagnostické kalkulačky jsou potřeba jen",
                           strong("minimální statistické znalosti."), 
                           "Podrobný návod vždy popíše, co, jak a kam zadat, a rovněž jak výsledky interpretovat."),
                         width = 5),
                  column(h2("Profesionální výpočty"), 
                         p("Diagnostickou kalkulačku vyvíjí", 
                           strong("psychometrický tým Katedry psychologie Fakulty sociálních studií."), 
                           "K veškerým výpočtům je k dispozici podrobný postup, kód této kalkulačk je", 
                           a("veřejně dostupný.", href="https://github.com/hynekcigler/kalkulacka")),
                         p("Ve stručnosti jsou jednotlivé postupy popsány i v zápatí každé kalkulačky tak, 
                           aby běžný uživatel měl alespoň orientační přehled nad všemi funkcemi."),
                         width = 5))),


# Confidence interval -----------------------------------------------------

tabPanel("Interval spolehlivosti", value = "CI",
         titlePanel("Výpočet chyby měření a intervalu spolehlivosti"),
  sidebarLayout(
  

# * sidebar CI ------------------------------------------------------------

  sidebarPanel(  
    radioButtons(inputId="CI_scale", label="Vyberte použité skóre:",
                 choices = list("IQ skóry (100, 15)" = "IQ",
                                "T skóry (50, 10)" = "T",
                                "z-skóry (0, 1)" = "z", 
                                "Vážené skóry (10, 3)" = "W",
                                "Percentily" = "P",
                                "jiné" = "jednotka")),
    conditionalPanel(condition = "input.CI_scale == 'jednotka'",
                     numericInput("CI_M_manual", "Zadejte průměr:", value = 100),
                     numericInput("CI_SD_manual", "Zadejte směrodatnou odchylku:", value = 15)),
    
    numericInput("CI_reliability", "reliabilita testu:", 0.8, min=0, max=1, step=0.05),  
    
    conditionalPanel(condition = "input.CI_scale == 'IQ'", 
                     numericInput("CI_score_IQ", "pozorované skóre:", 100)),
    conditionalPanel(condition = "input.CI_scale == 'T'", 
                     numericInput("CI_score_T", "pozorované skóre:", 50)),
    conditionalPanel(condition = "input.CI_scale == 'z'", 
                     numericInput("CI_score_z", "pozorované skóre:", 0)),
    conditionalPanel(condition = "input.CI_scale == 'W'", 
                     numericInput("CI_score_W", "pozorované skóre:", 10)),
    conditionalPanel(condition = "input.CI_scale == 'P'", 
                     numericInput("CI_score_P", "pozorované skóre:", 50)),
    conditionalPanel(condition = "input.CI_scale == 'jednotka'", 
                     numericInput("CI_score_jine", "pozorované skóre:", 100)),
    wellPanel(
      h5("Pokročilé nastavení"),
      radioButtons(inputId="CI_width", label="Šířka CI",
                   choices=list("90%" = 90,
                                "95%" = 95,
                                "99%" = 99,
                                "jiná" = "manual"),
                   selected=90),
      
      conditionalPanel(condition="input.CI_width == 'manual'",
                       numericInput("CI_widthManual", "zadejte (%)",
                                    value=95, min=0, max = 100)),
      checkboxInput(inputId="CI_regrese", label = "Regrese k průměru.", TRUE)
    )
  ), 


# * mainpanel CI ----------------------------------------------------------

  mainPanel(
    (htmlOutput("text_CI1", placeholder=NULL)),
    plotOutput("plot_CI"),
    h3("Nápověda"),
    HTML("<p>Tato kalkulačka vypočítá interval spolehlivosti jednoho skóre při jednom měření, 
         pokud znáte reliabilitu testu. </p>
         <p>Zvolte jednotky, ve kterých je test vyhodnocen, a zvolte rovněž reliabilitu testu, který používáte. 
         V pokročilém nastavení následně můžete zvolit šířku intervalu spolehlivosti. 
         Můžete si rovněž zvolit, zda chcete aplikovat regresi k průměru (tedy sestrojit interval spolehlivosti 
         kolem odhadu pravého skóre), nebo nikoli (a interval sestrojit okolo skóre pozorovaného). 
         Ačkoli regrese k průměru je doporučovaným postupem (Dudek, 1979; Cígler a Šmíra, 2015), jeho předpokladem je, že 
         klient byl &bdquo;náhodně&ldquo; vybrán z populace o daném průměru. Naneštěstí, tento předpoklad zpravidla 
         neplatí v běžném klinickém prostředí, v pedagogicko-psychologických poradnách apod. V takovém případě máte 
         na výběr v zásadě tři možnosti:</p>
         <ol><li>I přes to použít regresi k průměru. Takový závěr je &bdquo;konzervativnější&ldquo;. V takové situaci předpokádáte,
                 že klient pochází z běžné populace.</li>
             <li>Regresi k průměru nepoužít. Tento postup a-priori neklade žádné předpoklady o skóre klienta, může však vést 
                 k extrémnějším závěrům, interval spolehlivosti bude posunutý směrem k extrémům.</li>
             <li>Zvolit si apriorní rozložení sám/sama. V takovém případě jako typ skóre vyberte <em>jiné</em>. 
                 Jako směrodatnou odchylku uveďte směrodatnou odchylku používaných skórů (tedy např. 15 pro IQ či 10 pro T-skóre), 
                 jako střední hodnotu uveďte vámi očekávaný skór, kterého respondent dosáhne. Jinými slovy: tipněte si výkon klienta 
                 a tento svůj tip dosaďte na místo průměru. Tento poslední postup by měl vést k nejadekvátnějšímu odhadu intervalu 
                 spolehlivosti.</li></ol>
         <p>Pokud jste zadali skóre v percentilech, jsou tyto percentily prvně převedeny na z-skóre, 
         následně jsou provedeny veškeré výpočty a jejich výsledky (kromě standardní chyby měření) jsou převedeny zpět 
         na percentil.</p>
         <p><em><strong>Upozornění:</strong> Použitý výpočet je založen na postupu klasické testové teorie. 
         Výpočet není vhodný pro testy, které byly konstruované s využití teorie odpovědi na položku nebo Raschova modelu 
         (u nás např. Woodcock-Johnson či Krátký inteligenční test.</em></p>"),
    hr(),
    h3("Postup odhadu"),
    HTML("<p>Standardní chybu měření označovanou jako \\(SE\\) (&bdquo;Standard Error of measurement&ldquo;) 
    lze odhadnout pomocí vzorce
    $$SE = SD\\sqrt{1-r_{xx'}}$$
         kde \\(SD\\) je směrodatná odchylka (&bdquo;Standard Deviation&ldquo;) a 
         \\(r_{xx'}\\) je reliabilita testu.</p>", 
         "<p>Podle doporučení řady autorů (např. Dudek, 1979; Cígler a Šmíra, 2015) je vhodnějším postupem 
         konstruovat interval spolehlivosti nikoli kolem pozorovaného skóre, ale kolem odhadu skóre pravého, 
         které se nachází trochu blíže k průměrnému skóru. Tato funkce je v našem výpočtu implementována 
         (lze ji však vypnout v pokročilém nastavení). Odhad pravého skóre \\(E(T)\\) 
         (očekávaná hodnota pravého skóre při určitém pozorovaném skóre) lze vypočítat jako 
         $$E(T) = r_{xx'}X + (1-r_{xx'})M$$
         kde \\(r_{xx'}\\) je reliabilita, \\(M\\) průměrné skóre a \\(X\\) skóre pozorované. 
         Všimněte si, že pokud je reliabilita rovna jedné, člen \\(1-r_{xx'}\\) je roven nule, 
         a průměrné skóre nemá na výpočet vliv; naopak pokud by reliabilita byla nulová, pak by na výpočet nemělo vliv 
         pozorované skóre a odhad pravého skóre by byl shodný s populačním průměrem.
         </p>", 
         "<p>Posledním krokem je odhad vlastního intervalu spolehlivosti \\(CI\\) (&bdquo;Confidence Interval&ldquo;). 
         Ten je odhadnut okolo odhadu pravého skóre \\(E(T)\\)
         (je-li tato funkce vypnutá, pak přímo okolo skóre pozorovaného \\(X\\)) jako 
         $$CI_{w} = E(T) \\pm z_{w}SE$$
         kde \\(w\\) označuje šířku intervalu (např. v procentech) 
         a \\(z_{w}\\) je příslušný kvantil normální rozdělení. Pro běžné hodnoty je tento kvantil roven 
         \\(z_{90\\%}=1,64\\), \\(z_{95\\%}=1,96\\) a \\(z_{99\\%}=2,58\\).</p>"),
    
    HTML("<p>Autorem kalkulačky je Hynek Cígler (&copy; 2020) s mírným přispěním Martina Šmíry.</p>"),
    hr(),
    h4("Zdroje"),
    HTML("<ul><li>Cígler, H., & Šmíra, M. (2015). 
         Chyba měření a odhad pravého skóru: Připomenutí některých postupů Klasické testové teorie. 
         <i>Testfórum, 4</i>(6), 67-84. 
         doi:<a href='https://doi.org/10.5817/TF2015-6-104'>10.5817/TF2015-6-104</a></li>
         <li>Dudek, F.J. (1979). 
         The Continuing Misinterpretation of the Standard Error of Measurement. 
         <i>Psychological Bulletin 86</i>(2), 335-337. 
         doi:<a href='https://doi.org/10.5817/10.1037/0033-2909.86.2.335'>10.1037/0033-2909.86.2.335</a></li></ul>")
  )
)),


# Převod skórů ------------------------------------------------------------

tabPanel("Převod skórů", 
         titlePanel("Převod mezi skóry"), 
         sidebarLayout(

# * sidebar ---------------------------------------------------------------

           sidebarPanel(
             selectInput("test", "Vyberte vstupní skór", 
                         list( "z-skóre" = "z",
                               "IQ skóre" = "iq",
                               "T-skóre" = "t",
                               "standardní (Wechslerovo) skóre" = "wech",
                               "percentil" = "perc",
                               "sten" = "sten",
                               "stanin" = "stanin")),
             
             conditionalPanel(
               condition = "input.test == 'z'", 
               numericInput("zValue", "Zadejte z-skóre", value=0, step=.2)
             ),
             conditionalPanel(
               condition = "input.test == 't'", 
               numericInput("tValue", "Zadejte T-skóre", value=50, step=1)
             ),
             conditionalPanel(
               condition = "input.test == 'wech'", 
               numericInput("wechValue", "Standardní (Wechslerovo) skóre", value=10, step=1)
             ),
             conditionalPanel(
               condition = "input.test == 'perc'", 
               numericInput("percValue", "Zadejte percentil", value=50, min=1, max=100, step=1)
             ),
             conditionalPanel(
               condition = "input.test == 'sten'", 
               numericInput("stenValue", "Zadejte sten", value=5, min=1, max=10)
             ),
             conditionalPanel(
               condition = "input.test == 'iq'", 
               numericInput("iqValue", "Zadejte IQ skóre", value=100, min=1, max=200, step=1)
             ),
             conditionalPanel(
               condition = "input.test == 'stanin'", 
               numericInput("staninValue", "Zadejte stanin", value=5, min=1, max=9)
             ),
             
             checkboxInput("zaokrouhlit", "Zaokrouhlit skóre na celá čísla"),
             conditionalPanel(
               condition = "input.test == 'sten' || input.test == 'stanin'", 
               checkboxInput("procentoMasy", "Spočítat procento pod křivkou", value = T)
             )
             ),


# * mainpanel -------------------------------------------------------------


           mainPanel(
             tableOutput("trans_vystup"),
             textOutput("trans_warn"),
             plotOutput("trans_graf", width="auto"),
             h3("Nápověda"),
             HTML("<p>Tato kalkulačka převádí běžná skóre používaná v psychologické diagnostice. 
                  Do vstupního pole zadejte skóre a vyberte jeho typ. Pamatujte, že některé skóry mají možný rozsah!
                  Dále si můžete vybrat zaokrouhlení. Protože steny a staniny nejsou bodovým odhadem, ale z definice 
                  reprezentují určitý rozsah možných hodnot, lze při jejich výběru zobrazit i podíl respondentů, 
                  kteří mají určité stenové či staninové skóre.</p>", 
                  "<h4>Použité skóry a jejich parametry</h4>", 
                  "<table class='table shiny-table table- spacing-s' style='width:auto; text-align: center;'>
                  <thead><tr><th>skóre</th><th style='text-align: center;'>průměr</th>
                             <th style='text-align: center;'>směrodatná odchylka</th>
                             <th style='text-align: center;'>minimum</th>
                             <th style='text-align: center;'>maximum</th>
                             <th style='text-align: center;'>typ</th></tr></thead>
                  <tfoot><td colspan = 6 style='text-align: left; max-width: 0px;'>
                  <small><sup>a</sup> zpravidla.<br />
                  <em>Poznámky:</em> typ standardní &ndash; převod pomocí lineární transformace;
                  typ plošný &ndash; převod zpravidla pomocí plošné transformace za předpokladu normálního rozložení. 
                  NA &ndash; není definováno. Kromě z-skóre jsou všechny jednotky zaokrouhlovány zpravidla na celá čísla 
                  (včetně stenů a staninů, což vede k zaokrouhlení průměrného z-skóre 0 na &bdquo;nadprůměrnou&ldquo; 
                  stenovou hodnotu 6); z-skóre bývají zaokrouhlována na dvě desetinná místa.
                  </small></td></tfoot>
                  <tbody>
                  <tr><th>z-skóre</th><td>0</td><td>1</td><td>NA</td><td>NA</td><td>standardní</td></tr>
                  <tr><th>IQ skóre</th><td>100</td><td>15</td><td>NA</td><td>NA</td><td>standardní</td></tr>
                  <tr><th>percentil</th><td>NA</td><td>NA</td><td>0</td><td>100</td><td>plošný</td></tr>
                  <tr><th>T-skóre</th><td>50</td><td>10</td><td>NA</td><td>NA</td><td>standardní</td></tr>
                  <tr><th>Wechslerovo (vážené) skóre</th><td>10</td><td>3</td><td>0<sup>a</sup></td><td>20<sup>a</sup></td><td>standardní</td></tr>
                  <tr><th>steny</th><td>5,5</td><td>2</td><td>1</td><td>10</td><td>plošný</td></tr>
                  <tr><th>staniny</th><td>5</td><td>2</td><td>1</td><td>9</td><td>standardní</td></tr>

                  </tbody>
                  </table>"),
             hr(),
             HTML("<p>Autorem této kalkulačky je Martin Šmíra (&copy; 2014) s mírným přispěním Hynka Cíglera.</p>")
           )
         )),


# Složené skóre -----------------------------------------------------------

tabPanel("Složené skóre", 
         titlePanel("Práce se složeným skóre"), 

# * sidebar ---------------------------------------------------------------

         sidebarLayout(
           sidebarPanel(
             radioButtons(inputId="SS_scale", label="Vyberte použité skóre:",
                          choices = list("IQ skóry (100, 15)" = "IQ",
                                         "T skóry (50, 10)" = "T",
                                         "z-skóry (0, 1)" = "z", 
                                         "Vážené skóry (10, 3)" = "W",
                                         "Percentily" = "P",
                                         "jiné" = "jednotka")),
             conditionalPanel(condition = "input.SS_scale == 'jednotka'",
                              numericInput("SS_M_manual", "Zadejte průměr:", value = 100),
                              numericInput("SS_SD_manual", "Zadejte směrodatnou odchylku:", value = 15)),
             radioButtons(inputId="SS_apriori", label = "Typ apriorní informace:", 
                          choices = list("populační" = "populace", 
                                         "žádná" = "no", 
                                         "vlastní" = "user")),
             conditionalPanel(condition = "input.SS_apriori == 'user'",
                              numericInput("SS_odhad", "Váš odhad skóre:", value = NA)#,
                              # sliderInput("SS_jistota", 
                              #             label = "Jaká je vaše jistota (%)?", value = 50, 
                              #             min = 0, max = 100, step = 1, ticks = T),
                              # HTML("<p><strong>Míru jistoty nedoporučujeme měnit, pokud si nejste jisti, co děláte! 
                              #       Toto může silně ovlivnit odhadované výsledky.</strong><br />
                              #      <small>0 % &ndash; je použit desetinásobek směrodatné odchylky rozložení (velmi slabý prior).<br />
                              #      50 % &ndash; je použita směrodatná odchylka (běžný prior, shodné s nastavením apriorní informace na populační průměr).<br />
                              #      100 % &ndash; je použita desetina směrodatné odchylky (velmi silný prior).</small></p>")
                              ),

             tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                      numericInput("SS1", label = "skóre 1:", value = NA)),
             tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                      numericInput("RR1", label = "reliabilita", value = NA, min = 0, max = 1)),
             
             conditionalPanel("(input.SS1 !== null) && (input.RR1 > 0)", 
                              HTML("<p><small>Pokud u druhého a dalšího testu nezadáte reliabilitu, 
                                   použije se reliabilita prvního testu.</small></p>"), 
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("SS2", label = "skóre 2:", value = NA)),
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("RR2", label = "reliabilita", value = NA, min = 0, max = 1))), 
             
             conditionalPanel("input.SS2 !== null",
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("SS3", label = "skóre 3:", value = NA)),
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("RR3", label = "reliabilita", value = NA, min = 0, max = 1))),       
             
             conditionalPanel("input.SS3 !== null", 
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("SS4", label = "skóre 4:", value = NA)),
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("RR4", label = "reliabilita", value = NA, min = 0, max = 1))), 
             
             conditionalPanel("input.SS4 !== null", 
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("SS5", label = "skóre 5:", value = NA)),
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("RR5", label = "reliabilita", value = NA, min = 0, max = 1))), 
             
             conditionalPanel("input.SS5 !== null", 
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("SS6", label = "skóre 6:", value = NA)),
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("RR6", label = "reliabilita", value = NA, min = 0, max = 1))), 
             
             conditionalPanel("input.SS6 !== null", 
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("SS7", label = "skóre 7:", value = NA)),
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("RR7", label = "reliabilita", value = NA, min = 0, max = 1))), 
             
             conditionalPanel("input.SS7 !== null", 
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("SS8", label = "skóre 8:", value = NA)),
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("RR8", label = "reliabilita", value = NA, min = 0, max = 1))), 
             
             conditionalPanel("input.SS8 !== null", 
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("SS9", label = "skóre 9:", value = NA)),
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("RR9", label = "reliabilita", value = NA, min = 0, max = 1))), 
             
             conditionalPanel("input.SS9 !== null", 
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("SS10", label = "skóre 10:", value = NA)),
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("RR10", label = "reliabilita", value = NA, min = 0, max = 1))), 
             conditionalPanel("input.SS10 !== null", 
                              HTML("<p><em>Dosáhli jste maximálního počtu deseti testů.</em></p>")), 
             
             numericInput("SS_p", label = "Statistická významnost", value = .05, min = 0, max=1)


             
             
             
             
             
             ),

# * mainpanel -------------------------------------------------------------

           mainPanel(tableOutput("SS_table"),
                     p("wkejwk"),
                     plotOutput("SS_CIplot"),
                     tableOutput("SS_result"),
                     tableOutput("SS_x2")
                     
                     
                     
                     
                     )



         ))
), 
hr(),
HTML("<div style = 'margin-left: 30px; margin-bottom: 30px;'>&copy; 2020 Hynek Cígler & Martin Šmíra<br />
     Katedra psychologie, Fakulta sociálních studií<br />
     Masarykova univerzita</div>")))