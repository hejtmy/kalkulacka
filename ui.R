library(shiny)

shinyUI(fluidPage(titlePanel(img(src="logo.png"), windowTitle="Diagnostická kalkulačka"),
                  navbarPage(title = "Diagnostická kalkulačka", 
                             theme = shinythemes::shinytheme("cerulean"),
                             
                             
# Home page ---------------------------------------------------------------
                             
tabPanel("Domů",
         withMathJax(),
         fluidRow(column(h2("Diagnostická kalkulačka"), 
                         p("Diagnostická kalkulačka zjednodušuje život všem psychologům, 
                           kteří se v praxi zabývají", strong("psychologickým testováním"), 
                           ". Nechte se provést intervaly spolehlivosti, kritickými skóry a 
                           dalšími otázkami z psychometrické problematiky."),
                         p("Pro používání diagnostické kalkulačky jsou potřeba jen",
                           strong("minimální statistické znalosti"), 
                           ". Podrobný návod vždy popíše, co a jak kam zadat, a jak výsledky interpretovat."),
                         width = 5),
                  column(h2("Profesionální výpočty"), 
                         p("Diagnostickou kalkulačku vyvíjí", 
                           strong("psychometrický tým Katedry psychologie Fakulty sociálních studií"), 
                           ". Dáváme k dispozici veškeré postupy našich výpočtů."),
                         p("Zvláště u některých odhadů, využívajících principy", 
                           strong("bayesovské statistiky"), 
                           "je důležitý samotný návrh výpočtu - jiné způsoby odhadu mohou 
                           poskytnout mírně odlišné výsledky. Bayesovská statistika, 
                           kterou používáme, je ovšem přesnější a poskytuje i odhady pravděpodobnosti, 
                           s jakou dané hypotézy platí."),
                         width = 5))),


# Confidence interval -----------------------------------------------------

tabPanel("Interval spolehlivosti",
         titlePanel("Výpočet chyby měření a intervalu spolehlivosti"),
  sidebarLayout(
  
  # Application title
    
  
  sidebarPanel(  
    radioButtons(inputId="CI_scale", label="",
                 choices = list("IQ skóry (100, 15)" = "IQ",
                                "T skóry (50, 10)" = "T",
                                "z-skóry (0, 1)" = "z", 
                                "Vážené skóry (10, 3)" = "W",
                                "Percentily" = "P",
                                "jiné" = "jednotka")),
    
    numericInput("CI_reliability", "reliabilita:", 0.8, min=0, max=1, step=0.05),  
    
    conditionalPanel(condition = "input.CI_scale == 'IQ'", 
                     numericInput("CI_score_IQ", "skór:", 100)),
    conditionalPanel(condition = "input.CI_scale == 'T'", 
                     numericInput("CI_score_T", "skór:", 50)),
    conditionalPanel(condition = "input.CI_scale == 'z'", 
                     numericInput("CI_score_z", "skór:", 0)),
    conditionalPanel(condition = "input.CI_scale == 'W'", 
                     numericInput("CI_score_W", "skór:", 10)),
    conditionalPanel(condition = "input.CI_scale == 'P'", 
                     numericInput("CI_score_P", "skór:", 50)),
    conditionalPanel(condition = "input.CI_scale == 'jednotka'", 
                     numericInput("CI_score_jine", "skór:", 100)),
    conditionalPanel(condition = "input.CI_scale == 'jednotka'",
                     numericInput("CI_M_manual", "Zadejte průměr:", value = 100),
                     numericInput("CI_SD_manual", "Zadejte směrodatnou odchylku:", value = 15)),
    wellPanel(
      h4("Pokročilé nastavení"),
      h5("Šířka intervalu spolehlivosti"),
      radioButtons(inputId="CI_width", label="",
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
  
  mainPanel(
    h5(verbatimTextOutput("text_CI1")),
    plotOutput("plot_CI"),
    h3("Nápověda"),
    HTML("<p>Tato kalkulačka vypočítá interval spolehlivosti jednoho skóre při jednom měření, 
         pokud znáte reliabilitu testu. </p>
         <p>Zvolte jednotky, ve kterých je test vyhodnocen, a zvolte rovněž reliabilitu testu, který používáte. 
         V pokročilém nastavení následně můžete zvolit šířku intervalu spolehlivosti. 
         Můžete si rovněž zvolit, zda chcete aplikovat regresi k průměru (tedy sestrojit interval spolehlivosti 
         kolem odhadu pravého skóre), nebo nikoli (a interval sestrojit okolo skóre pozorovaného).</p>
         <p>Pokud jste zadali skóre v percentilech, jsou tyto percentily prvně převedeny na z-skóre, 
         následně jsou provedeny veškeré výpočty a jejich výsledky (kromě standardní chyby měření) jsou převedeny zpět 
         na percentil.</p>
         <p><em><strong>Upozornění:</strong> Použitý výpočet je založen na postupu klasické testové teorie. 
         Výpočet není vhodný pro testy, které byly konstruované s využití teorie odpovědi na položku nebo Raschova modelu 
         (u nás např. Woodcock-Johnson či Krátký inteligenční test.</em></p>"),
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
         "Posledním krokem je odhad vlastního intervalu spolehlivosti \\(CI\\) (&bdquo;Confidence Interval&ldquo;). 
         Ten je odhadnut okolo odhadu pravého skóre \\(E(T)\\)
         (je-li tato funkce vypnutá, pak přímo okolo skóre pozorovaného \\(X\\)) jako 
         $$CI_{w} = E(T) \\pm z_{w}SE$$
         kde \\(w\\) označuje šířku intervalu (např. v procentech) 
         a \\(z_{w}\\) je příslušný kvantil normální rozdělení. Pro běžné hodnoty je tento kvantil roven 
         \\(z_{90\\%}=1,64\\), \\(z_{95\\%}=1,96\\) a \\(z_{99\\%}=2,58\\).",
         "<br />"),
    
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
)


),
                             tabPanel("Kritický skór"),
                             tabPanel("Převod skórů")
                  ), 
hr(),
HTML("<div style = 'margin-left: 30px;'>&copy; 2020 Hynek Cígler & Martin Šmíra<br />
     Katedra psychologie, Fakulta sociálních studií<br />
     Masarykova univerzita</div>")))