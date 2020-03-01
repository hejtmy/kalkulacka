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
    hr(),
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

             tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                      numericInput("SS1", label = "skóre 1:", value = NA)),
             tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                      numericInput("RR1", label = "reliabilita", value = NA, min = 0, max = 1, step = .01)),
             
             conditionalPanel("(input.SS1 !== null) && (input.RR1 > 0)", 
                              HTML("<p><small>Pokud u druhého a dalšího testu nezadáte reliabilitu, 
                                   použije se reliabilita prvního testu.</small></p>"), 
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("SS2", label = "skóre 2:", value = NA)),
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("RR2", label = "reliabilita", value = NA, min = 0, max = 1, step = .01))), 
             
             conditionalPanel("input.SS2 !== null",
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("SS3", label = "skóre 3:", value = NA)),
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("RR3", label = "reliabilita", value = NA, min = 0, max = 1, step = .01))),       
             
             conditionalPanel("input.SS3 !== null", 
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("SS4", label = "skóre 4:", value = NA)),
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("RR4", label = "reliabilita", value = NA, min = 0, max = 1, step = .01))), 
             
             conditionalPanel("input.SS4 !== null", 
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("SS5", label = "skóre 5:", value = NA)),
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("RR5", label = "reliabilita", value = NA, min = 0, max = 1, step = .01))), 
             
             conditionalPanel("input.SS5 !== null", 
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("SS6", label = "skóre 6:", value = NA)),
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("RR6", label = "reliabilita", value = NA, min = 0, max = 1, step = .01))), 
             
             conditionalPanel("input.SS6 !== null", 
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("SS7", label = "skóre 7:", value = NA)),
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("RR7", label = "reliabilita", value = NA, min = 0, max = 1, step = .01))), 
             
             conditionalPanel("input.SS7 !== null", 
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("SS8", label = "skóre 8:", value = NA)),
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("RR8", label = "reliabilita", value = NA, min = 0, max = 1, step = .01))), 
             
             conditionalPanel("input.SS8 !== null", 
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("SS9", label = "skóre 9:", value = NA)),
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("RR9", label = "reliabilita", value = NA, min = 0, max = 1, step = .01))), 
             
             conditionalPanel("input.SS9 !== null", 
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("SS10", label = "skóre 10:", value = NA)),
                              tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
                                       numericInput("RR10", label = "reliabilita", value = NA, min = 0, max = 1, step = .01))), 
             conditionalPanel("input.SS10 !== null", 
                              HTML("<p><em>Dosáhli jste maximálního počtu deseti testů.</em></p>")), 
             
             checkboxInput("advanced", "Pokročilé možnosti"),
             conditionalPanel("input.advanced == 1", 
                              numericInput("SS_p", label = "Statistická významnost", value = .05, min = 0, max=1, step = .01),
                              radioButtons(inputId="SS_apriori", label = "Typ apriorní informace:", 
                                           choices = list("populační" = "populace", 
                                                          "žádná" = "no", 
                                                          "vlastní" = "user")),
                              conditionalPanel(condition = "input.SS_apriori == 'user'",
                                               numericInput("SS_odhad", "Váš odhad skóre:", value = NA)))


             
             
             
             
             
             ),

# * mainpanel -------------------------------------------------------------

           mainPanel(h3(textOutput("SS_warning1"), style = "color: red;"),
                     h3(textOutput("SS_warning2"), style = "color: red;"),
                     h4("Tabulka 1: Výsledné skóre"),
                     p({textOutput("SS_warning3", inline = T)}, 
                       style = "color: red;"),
                     tableOutput("SS_x2"),
                     p(tags$small("odhad – odhad výsledného skóre na základě všech testů. 
                          CI – výsledný interval spolehlivosti;", br(),
                          "x2, df, p – test dobré shody (s testovou statistikou x2 a počtem stupňů volnosti df), 
                          zda se jednotlivé testové výsledky neliší. Pokud je test signifikantní 
                          (p je menší než zvolená hodnota ", textOutput("SS_pout", inline = T),
                                  "), pak nelze výsledné kombinované skóre interpretovat.")),
                     fluidRow(column(h4("Tabulka 2: Chyby měření jednotlivých testů"),
                                     tableOutput("SS_table"),
                                     HTML("<p><small>X – pozorované skóre
                                          r – reliabilita testu; 
                                          SE – standardní chyba měření; 
                                          CI – interval spolehlivosti.<br />
                                          Žádný z údajů nezohledňuje ostatní testy, týká se vždy jen daného testu. 
                                          Při výpočtu není zohledněna regrese k průměru.</small></p>"),
                                     plotOutput("SS_CIplot"), 
                                     width = 6),
                              column(h4("Tabulka 3: Vývoj chyb měření"),
                                     tableOutput("SS_result"),
                                     HTML("<p><small>X &ndash; odhad skóre u n-tého testu; SE = chyba měření daného testu; 
                                          SE_true &ndash; chyba odhadu s využitím všech dosavadních testů; 
                                          CI &ndash; interval měření, které využívá všechny dosavadní testy.</small></p>"),
                                     plotOutput("SS_goplot"),
                                     width = 6)),

                     hr(),
                     h3("Nápověda"),
                     p("Tato kalkulačka nabízí možnost &bdquo;zkombinovat&ldquo; více testů tak, aby uživatel 
                          získal jediný skór s jediným intervalem spolehlivosti. Vyberte si požadovanou jednotku, kterou používáte 
                       (všechny testy musí být zadané ve shodných jednotkách) a zadejte skóre a reliabilitu prvního testu. 
                       Poté se zobrazí pole pro zadání pozorovaného skóre a reliability druhého testu, a tak dál; 
                       celkem lze zadat až 10 separátních testových výsledků."), 
                     p("V nabídce pokročilé možnosti máte zároveň možnost změnit hladinu spolehlivosti použitou pro všechny výpočty, 
                       a rovněž apriorní informaci.", strong("Nedoporučujeme měnit apriorní informaci, pokud si nejste zcela jisti, 
                                                        že víte, co děláte!"), 
                       "Apriorní informace určuje průměr populace, ze které je proband vybrán. Je-li vybrán z běžné populace, 
                       je nejvhodnějším řešením ponechat původní nastavení (populační apriorní informace). V tomto případě je 
                       použita regrese k průměrnému skóru. Tuto funkci lze vypnout nastavením žádné apriorní informace; skóre 
                       prvního testu v takovémto případě není přibližováno k průměrnému skóre. Třetí možnost je nastavit 
                       vlastní apriorní informaci. V takovém případě můžete ", em("„zpřesnit“"), 
                       "výsledky vyšetření vaším odhadem. Zadejte do apriorní informace váš expertní odhad, jakého výsledku 
                       by měl proband dosáhnout. Výsledný odhad bude o něco blíže k této hodnotě, než by odpovídalo naměřenému 
                       skóre."),
                     p("V tabulce 1 naleznete zkombinované výsledky jednotlivých testů. Ve sloupci odhad je výsledný bodový odhad 
                       respondentova skóre, ve sloupci CI naleznete příslušný interval spolehlivosti.", br(), 
                       "ve sloupcích x2, df a p jsou výsledky testu dobré shody ověřujícího, zda se jednotlivé testové výsledky 
                       statisticky významně neliší. Pokud ano, nelze kombinované skóre bezpečně interpretovat."),
                     p("V tabulce 2 jsou k dispozici výsledky pro jednotlivé pozorované skóry: X jsou právě tyto skóry, 
                       r zadané reliability, SE chyby měření a CI intervaly spolehlivosti. Upozorňujeme, že tyto intervaly spolehlivosti 
                       byly odhadnuty bez využití regrese k průměru; nedoporučujeme je individuálně interpretovat. V grafické 
                       podobě jsou tyto výsledky pak na gafu níže-"),
                     p("Tabulka 3 potom předkládá vývoj, jak se postupně chyby měření zužovaly. N prvním řádku jsou výsledky po 
                       vyhodnocení prvního testu; na druhém po vyhodnocení druhého testu a tak dále. X, SE, a r jsou 
                       pozorované skóre, chyba měření a reliabilita daného testu (bez zohlednění ostatních měření). 
                       Naopak ve sloupcích X, SE_true a CI je výsledná odhadovaná hodnota pravého skóre, jeho chyba odhadu 
                        a příslušný interval spolehlivosti, pokud bereme v úvahu výsledek daného testu i všechna předchozí měření."),
                     p(strong("Upozornění:"), 
                        "Použitý výpočet je založen na postupu klasické testové teorie. Výpočet není vhodný pro testy, 
                       které byly konstruované s využití teorie odpovědi na položku nebo Raschova modelu 
                       (u nás např. Woodcock-Johnson či Krátký inteligenční test."),
                     hr(),
                     checkboxInput("showmethod", "Zobrazit postup výpočtu", value = F), 
                     conditionalPanel(
                       "input.showmethod == 1", 
                       h3("Postup odhadu"),
                       p("Námi použitý postup kombinuje Bayesovský a tradiční, frekventistický způsob odhadu. 
                         Výpočet pracuje se z-skóry - všechny ostatní jednotky jsou úvodem převedeny právě na z-skór a po ukončení 
                         všech výpočtů jsou veškeré hodnoty transformovány zpět na původní škálu. 
                         V prvním kroku je proveden odhad pravého skóre E(T) podle vzorce 
                         $$E(T) = r_{xx'}X + (1-r_{xx'})M$$
                         kde \\(r_{xx'}\\) je reliabilita, \\(M\\) uživatelem zadaná apriorní informace a \\(X\\) skóre pozorované. 
                         Protože pracujeme se z-skóry, v případě použití populační apriorní informace platí  \\(M=0\\). v případě žádné apriorní informace 
                         je tento krok přeskočen a namísto odhadu pravého skóre je použit přímo pozorovaný skór."),
                       p("Následně je odhadnuta standardní chyba měření \\(SE\\) podle vzorce
                         $$SE = SD\\sqrt{1-r_{xx'}}$$
                         kde \\(SD\\) je směrodatná odchylka a \\(r_{xx'}\\) je reliabilita testu. Kromě toho je odhadnuta ještě i 
                         chyba odhadu pravého skóre \\(SE_{T}\\) jako 
                         $$SE_{T} = \\sqrt{r_{xx'}}SE$$", 
                         "pro účely dalších výpočtů. V případě druhého a dalších měření je použit odlišný postup. 
                         Odhad pravého skóre \\(i\\)-tého testu \\(E(T_{i})\\) je proveden jako posteriorní odhad podle vzorce 
                         $$E(T_{i}) = \\frac{SE_i^2}{SE_i^2 + SE_{T,i-1}^2}E(T_{i-1}) + \\frac{SE_{T,i-1}^2}{SE_i^2 + SE_{T,i-1}^2}X_i$$
                         kde \\(SE_i^2\\) je standardní chyba měření \\(i\\)-tého testu, \\(SE_{T,i-1}\\) je standardní chyba odhadu 
                         pravého skóre s využitím všech předchozích testů \\(i-1\\), \\(E(T_{i-1})\\) jeho bodový odhad a konečně \\(X_i\\) skóre 
                         pozorované v testu \\(i\\). Chyba tohoto odhadu je spočítána pomocí vzorce 
                         $$SE_{T, i} = \\sqrt{\\frac1{SE_{T, i}^2} + \\frac1{SE_{T, i-1}^2}}^{-1}$$"), 
                       p("S pomocí této chyby odhadu je zpětně odhadnuta reliabilita všech testů \\(r_{ii'}\\) jako 
                         $$r_{ii'} = 1 + \\frac{\\sqrt{1-4SE_{T, i}^2}}{2}$$ 
                         Tento vzorec bohužel není identifikovaný, pokud \\(r_{ii'} \\leq 0,5\\). V takovém případě je uživatel 
                         upozorněn."),
                       p("Poslední důležitý výpočet je statisticky významný rozdíl jednotlivých testů navzájem. Ten je odhadnut pomocí 
                         testu dobré shode, kdy testová statistika má hodnotu
                         
                         $$\\chi^2=\\sum_{i=1}^{n} \\frac{(X_i-E(X))^2}{SE_i^2}$$
                         
                         s přibližně \\(\\chi^2\\) rozložením a \\(n-1\\) stupni volnosti, kde \\(n\\) udává počet zadaných testů. 
                         \\(E(X)\\) je potom očekávané skóre, odhadnuté jako vážený průměr všech dílčích testů 
                         $$E(X)=\\frac{\\sum_{i=1}^{n} SE_i^{-2}X_i}{\\sum_{i=1}^{n}SE_i^{-2}}$$"))

                     )



         )),

# Rozdílové skóry -----------------------------------------------------------

tabPanel("Rozdílové skóry", 
         titlePanel("Statisticky a klinicky významná změna"))

), 
hr(),
div("©", textOutput("yearcopy", inline = T), "Hynek Cígler & Martin Šmíra", br(),
"Katedra psychologie, Fakulta sociálních studií", br(),
     "Masarykova univerzita | ", tags$a("kalkulacka@testforum.cz", href="mailto:kalkulacka@testforum.cz"), 
    style = 'margin-left: 30px; margin-bottom: 30px;')))