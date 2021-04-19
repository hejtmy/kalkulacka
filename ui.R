library(shiny)
library(shinyWidgets)

inline_numericInput=function(ni){
  tags$div( class="form-inline",ni)
}

year <- substr(Sys.Date(), 1, 4)

shinyUI(fluidPage(conditionalPanel("input.panels == 'title'",
                                   titlePanel(img(src="logo.png"), 
                                              windowTitle="Diagnostická kalkulačka")),
                  navbarPage(title = "Diagnostická kalkulačka", 
                             theme = shinythemes::shinytheme("cerulean"),
                             id = "panels",
                             
                             
# Home page ---------------------------------------------------------------
                             
tabPanel("Domů", 
         value = "title",
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
                           a("veřejně dostupný.", href="https://github.com/hynekcigler/kalkulacka", target="_blank")),
                         p("Ve stručnosti jsou jednotlivé postupy popsány i v zápatí každé kalkulačky tak, 
                           aby měl uživatel přehled nad způsobem výpočtu."),
                         width = 3),
                  column(a(img(src="logoFSScol.png", width="100%"), href="http://psych.fss.muni.cz", target="_blank"),
                         width = 4)),
         hr(),
         h3("Dostupné kalkulačky"),
         fluidRow(
           column(
             h4("Interval spolehlivosti"),
             p("Po zvolení jednotky, zadání naměřeného skóre a reliability testu máte k dispozici 
               interval spolehlivosti pro vašeho respondenta. Kromě běžného intervalu spolehlivosti měření 
               získáte navíc i interval predikce (v jakém rozmezí bude ležet skór retestu, pokud se výkon 
               klienta nezmění?) a rozdílu (v jakém rozmezí by ležel skór jiného probanda se stejnou úrovní 
               schopnosti?)."),
             h4("Převod skórů"), 
             p("Pokud potřebujete převést např. percentily na IQ skóry či T-skóry, nebo naopak např. 
               steny či staniny na percentily či vážené skóry používané v inteligenčních testech, 
               využijete právě tuto kalkulačku."), 
             hr(),
             h3("Další užitečné pomůcky"),
             p("Kromě kalkulaček dostupných na této stránce můžete využít i další jednoduché nástroje z naší dílny:"),
             HTML("
<ul>
<li><strong><a href='http://fssvm6.fss.muni.cz/height/', target='_blank'>Simulace měření výšky</a>:</strong> Co by se stalo, kdybychom měřili 
lidskou výšku psychologickými nástroji? Jak by se projevila chyba měření? Jednoduchá aplikace vám nasimuluje výsledky 
hypotetického dotazníku výšky na základě vaší skutečné výšky v centimetrech a zadané reliability testu. Kalkulačka je v angličtině.</li>
<li><strong><a href='http://fssvm6.fss.muni.cz/vyska/', target='_blank'>Skutečný dotazník výšky</a></strong>, který ilustruje běžné postupy měření 
v psychologii.</li>
<li><strong><a href='http://fssvm6.fss.muni.cz/norms/', target='_blank'>Výběrová chyba norem:</a></strong> Žádné normy nejsou bezchybné, 
vždy záleží na náhodě; na tom, jací lidé se dostali do vzorku. Že zvláště u malých norem a pro extrémní skóry může být výběrová 
chyba skutečně velká, to vám ukáže jednoduchá aplikace.</li>
</ul>
              "), width = 5),
           column(
             h4("Složené skóre"),
             p("Administrovali jste více testů, měřicích ten samý rys, a chcete se dozvědět „souhrnný“ výsledek – tedy 
               výsledek agregovaný napříč všemi měřeními? Prostý průměr není 
               nejlepší nápad, zvlášť, pokud každý test má jinou reliabilitu. Využijte raději kalkulačku složeného skóre – 
               navíc se dozvíte i to, zda se jednotlivá měření liší (a měří tedy zřejmě něco jiného), 
               nebo zda jsou jednotlivé naměřené skóry pravděpodobně stejné."),
             h4("Rozdílové skóry"),
             p("Máte více skórů a nevíte, zda se skutečně liší? Kalkulačka rozdílových skórů vám to jednoduše řekne. 
               Na výběr máte mezi rozdílem dvou osob nebo dvou výsledků u jediného člověka, tedy test-retest, nebo tzv. statisticky
               a klinicky významné rozdíly."), 
             width = 5)),
         p(strong("Kalkulačku citujte jako:"), paste0("Cígler, H., & Šmíra, M. (", year,")"), 
           em("Diagnostická kalkulačka"),
           "(Verze 0.1.1).", 
           "Masarykova univerzita.", 
           a("http://kalkulacka.testforum.cz", href = "http://kalkulacka.testforum.cz")),
         p(strong("Bugs and reports:"), "Případné chyby hlaste prostřednictvím rozhraní", 
           a("GitHub", href="https://github.com/hynekcigler/kalkulacka/issues", target="_blank"))
         
         
         ),


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
    h3("Interval spolehlivosti"),
    (htmlOutput("text_CI1", placeholder=NULL)),
    plotOutput("plot_CI"),
    h3("Intervaly spolehlivosti pro rozdíl"),
    tableOutput("text_CI2"),
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
         <p>Kromě toho kalkulačka spočítá i dva další intervaly spolehlivosti, které naleznete v tabulce. 
         <strong>Chyba rozdílu </strong>udává, jak široký je interval spolehlivosti pro výsledek jiného měření tím stejným
         testem za předpokladu, že se pravé skóry obou testů neliší. Pokud nový skór (např. výsledek jiné osoby) 
         leží mimo tento interval, jsou oba skóry statisticky významně odlišné. <br />
         <strong>Chyba predikce</strong> odhaduje, v jakém rozmezí by měl ležet retestový výsledek respondenta 
         za předpokladu, že se jeho pravý skóry mezi měřeními nezměnil. Pokud leží nový výsledek mimo 
         uvedený interval, výkon respondenta se změnil.</p>
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
         $$CI_w = E(T) \\pm z_{w}SE$$
         kde \\(w\\) označuje šířku intervalu (např. v procentech) 
         a \\(z_{w}\\) je příslušný kvantil normální rozdělení. Pro běžné hodnoty je tento kvantil roven 
         \\(z_{90\\%}=1,64\\), \\(z_{95\\%}=1,96\\) a \\(z_{99\\%}=2,58\\).</p>
         <p>Interval pro rozdíl \\(CI_{\\Delta}\\) je spočítán okolo pozorovaného skóre podle vzorce, tedy
         
         $$ CI_{\\Delta}=X \\pm z_{w} SE_{\\Delta} $$
         
         kde \\(SE_\\Delta\\) se je standardní chyba rozdílu odhadnutá s pomocí standardní chyby měření 
         (viz dřívější vzorce) jako
         $$SE_\\Delta=\\sqrt{2}SE$$
         Interval spolehlivosti predikce \\(CI_{pred}\\) je spočítán okolo odhadu pravého skóre 
         \\(E(T))\\) jako
         $$CI_{pred} = E(T) \\pm z_{w}SE_{pred}$$
         kde \\(SE_{pred}\\) je standardní chyba predikce:
         $$SE_{pred}=SD\\sqrt{1-r_{xx'}^2}$$
         </p>"),
    
    p(paste0("Autorem kalkulačky je Hynek Cígler (©", year, ") s mírným přispěním Martina Šmíry.")),
    hr(),
    h4("Zdroje"),
    HTML("<ul><li>Cígler, H., & Šmíra, M. (2015). 
         Chyba měření a odhad pravého skóru: Připomenutí některých postupů Klasické testové teorie. 
         <i>Testfórum, 4</i>(6), 67-84. 
         doi:<a href='https://doi.org/10.5817/TF2015-6-104', target='_blank'>10.5817/TF2015-6-104</a></li>
         <li>Dudek, F.J. (1979). 
         The Continuing Misinterpretation of the Standard Error of Measurement. 
         <i>Psychological Bulletin 86</i>(2), 335-337. 
         doi:<a href='https://doi.org/10.5817/10.1037/0033-2909.86.2.335', target='_blank'>10.1037/0033-2909.86.2.335</a></li></ul>")
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
             p(paste0("Autorem této kalkulačky je Martin Šmíra (", year, ") s mírným přispěním Hynka Cíglera."))
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
             
             checkboxInput("SS_advanced", "Pokročilé možnosti"),
             conditionalPanel("input.SS_advanced == 1", 
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
                     p("Tato kalkulačka nabízí možnost zkombinovat více testů tak, aby uživatel 
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
                         $$E(X)=\\frac{\\sum_{i=1}^{n} SE_i^{-2}X_i}{\\sum_{i=1}^{n}SE_i^{-2}}$$")
                       ),
                     hr(),
                     p(paste0("Autorem této kalkulačky je Hynek Cígler (©", year, ")."))
                     
                     

                     )



         )),

# Rozdílové skóry -----------------------------------------------------------

tabPanel(
  "Rozdílové skóry", 
  titlePanel("Statisticky a klinicky významná změna"),

# * sidebar ---------------------------------------------------------------

  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId="RS_scale", label="Vyberte použité skóre:",
                   choices = list("IQ skóry (100, 15)" = "IQ",
                                  "T skóry (50, 10)" = "T",
                                  "z-skóry (0, 1)" = "z", 
                                  "Vážené skóry (10, 3)" = "W",
                                  "Percentily" = "P",
                                  "jiné" = "jednotka")),
      conditionalPanel(condition = "input.RS_scale == 'jednotka'",
                       numericInput("RS_M_manual", "Zadejte průměr:", value = 100),
                       numericInput("RS_SD_manual", "Zadejte směrodatnou odchylku:", value = 15)),
      hr(),
      h4("1. test"),
      p(tags$small("Skóre 1. osoby, testu nebo pretestu.")),
      tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
               numericInput("RS_X1", label = "Skóre 1", value = NA)),
      tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
               numericInput("RS_r1", label = "reliabilita", value = NA, min = 0, max = 1, step = .01)),
      hr(),
      h4("2. Test"),
      p(tags$small("Skóre druhého testu."), br(), 
        tags$small("Pokud nezadáte reliabilitu druhého testu, bude použita reliabilita testu prvního.", br(),
                   "Pro účely test-retest rozdílu bude vždy použita reliabilita prvního testu")),
      tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
               numericInput("RS_X2", label = "Skóre 2", value = NA)),
      tags$div(style = "display: inline-block;vertical-align:top; width: 100px;", 
               numericInput("RS_r2", label = "reliabilita", value = NA, min = 0, max = 1, step = .01)),
      numericInput("RS_cor", label = "korelace testů", value = NA, min = 0, max = 1, step = .01),
      checkboxInput("RS_advanced", "Pokročilé možnosti"),
      conditionalPanel("input.RS_advanced == 1", 
                       numericInput("RS_p", label = "Statistická významnost", 
                                    value = .05, min = 0, max=1, step = .01), 
                       checkboxInput("RS_regM", label = "Použít regresi k průměru 
                                     u statisticky významného rozdílu", value = TRUE))
    ),

# * mainpanel --------------------------------------------------------------

    mainPanel(
      h4(textOutput("RS_warn1", inline = T),
         textOutput("RS_warn2", inline = T),
         textOutput("RS_warn3", inline = T), style="color: red;"),
      h4("Tabulka 1: Rozdíl mezi skóry"),
      tableOutput("RS_result"),
      p(tags$small(tags$strong("Pozor, u klinické významnosti záleží na pořadí testů!"), br(),
                   "E(T2) – očekávané skóre druhého testu podle testu prvního; 
                   CI – interval spolehlivosti pro druhý test; 
                   rozdíl – pozorovaný rozdíl mezi testy;
                   SE – standardní chyba rozdílu; 
                   z – testová statistika rozdílu;
                   p – statistická významnost. 
                   Pokud není vypnut regresní odhad pro statisticky významný rozdíl, 
                   nedává smysl interval spolehlivosti tohoto rozdílu.")),
      h4("Tabulka 2: Přehled skórů"),
      tableOutput("RS_cis"),
      p(tags$small("X – pozorované skóre v daném testu; 
                   T – odhad pravého skóre daného testu;
                   SE – standardní chyba měření;
                   CI – interval spolehlivosti měření (bez zohlednění regrese k průměru);
                   CI_reg – interval spolehlivosti měření (po zohlednění regrese k průměru)")),
      plotOutput("RS_plot", height=250),
      hr(),
      h3("Nápověda"),
      HTML("<p>Tato kalkulačka poskytuje informace o rozdílnosti dvou skórů. 
           Uživatel může použít celkem tři různé rozdílové skóry:</p>
           <ol><li><strong>Statisticky významný rozdíl:</strong> Testuje hypotézu, zda se dva skóry od sebe 
           dostatečně liší natolik, abychom mohli konstatovat, že je jeden vyšší než druhý. 
           Pokud je test signifikantní, znamená to, že s určitou mírou jistoty (typicky 5 %) jeden skór je vyšší než druhý.<br />
           Příklad 1: <em>Existuje rozdíl mezi skórem fluidní a krystalizované inteligence daného respondenta?</em><br />
           Příklad 2: <em>Dosáhla v inteligenčním testu vyššího skóre Anežka nebo Bedřich?</em></li>
           <li><strong>Klinicky významný rozdíl:</strong> Testuje hypotézu, zda se dva skóry od sebe liší více než u náhodně 
           vybraného respondenta z populace. Pokud je test signifikantní, znamená to, že větší rozdíl skórů má jen velmi malé 
           množství osob (typicky 5 %), což může být diagnosticky důležitá informace.<br />
           Příklad: <em>Je rozdíl fluidní a krystalizované inteligence daného člověka věcně významný?</em></li>
           <li><strong>Rozdíl test-retest (chyba predikce):</strong> Ověřuje, zda se výkon respondenta změnil v čase. 
           Pokud je test signifikantní, znamená to, že se výkon daného respondenta v retetu s určitou mírou jistoty (typicky 5 %) 
           změnil od prvního měření (pretestu).<br />
           Příklad: <em>Došlo u respondenta ke změně výkonu v čase mezi oběma měřeními?</em></li></ol>
           <p>Uživatel má k dispozici dvě tabulky. Zatímco však druhá tabulka obsahuje obyčejné intervaly spolehlivosti pro 
           jedno měření (s pozorovaným skóre, odhadem skóru pravého, standardní chybou měření a intervaly spolehlivosti 
           při zvážení a při nezvážení regrese k průměru), první tabulka poskytuje přímo výsledky rozdílových skórů.</p>
           <p>V prvním sloupci, E(T), je k dispozici očekávaná (resp. nejpravděpodobnější) hodnota druhého testu při zadaných 
           reliabilitách a skóre prvního testu. Okolo této hodnoty se pohybují naměřené skóry, přičemž odlišnost je způsobena 
           chybou měření. Ve druhém sloupci je k dispozici interval spolehlivosti pro druhý test. Pokud skóre druhého testu 
           leží vně tohoto intervalu, je rozdíl statisticky významný na zadané hladině spolehlivosti. Ve sloupci rozdíl je čistě jen rozdíl
           očekávané a pozorované hodnoty se standardní chybou zobrazenou ve sloupci SE. Ve sloupci z je testová statistika a 
           ve sloupci označeném jako p je statistická významnost tohoto rozdílu. Poslední sloupec pak poskytuje slovní interpretaci 
           případného rozdílu.</p>
           <p>Pro výpočet <strong>statisticky významného rozdílu</strong> je nutné zadat skór prvního a druhého měření 
           a reliabilitu testů. Pokud mají obě měření stejnou reliabilitu (např. porovnáváme výkon dvou osob 
           v jednom testu), stačí zadat reliabilitu pouze prvního testu, ta se použije i pro druhé měření.</p>
           <p>Pro Výpočet <strong>klinicky významného rozdílu</strong> je nutné zadat korelaci obou testů, reliability však nejsou nutné</p>
           <p>Pro výpočet <strong>rozdílu test-retest</strong> je potřeba zadat skóre obou testů a reliabilitu prvního testu. 
           V tomto případě je reliabilita druhého testu ignorovaná, protože předpokládáme, že jde o dvě měřením tím stejným testem 
           se shodnou reliabilitou. Pokud jste při retestu měřili jiným testem, použijte statisticky významný rozdíl.</p>
           <p>V pokročilých možnostech můžete změnit požadovanou hladinu statistické významnosti. 
           Je možné rovněž ovlivnit výpočet statisticky významného rozdílu. V původním nastavení je použit regresní postup 
           navržený Cíglerem a Šmírou (2015, vzorec 15). V tomto případě je srovnáván rozdíl pravých skórů. Tuto možnost lze 
           vypnout, pak jsou srovnávány přímo skóry pozorované. Nejste-li si jistí, kterou z variant použít, zvolte přednastavenou 
           možnost (a nechte políčko zatrhnuté). V takovém případě však není k dispozici interval spolehlivosti pro druhý test.</p>
           <p>Uživatel aplikace má k dispozici rovněž i graf zobrazující obě měření včetně jejich intervalu spolehlivosti 
           (bez zvážení regrese k průměru, která by mohla být v tomto případě matoucí).</p>"),
      hr(),
      h3("Postup výpočtu"),
      h4("Obecný postup všech výpočtů"),
      HTML("<p>Ve všech případech je spočítán rozdíl očekávaného \\(E(B|A)\\) a pozorovaného skóre \\(B\\) 
           ve druhém testu \\(X_{\\Delta}\\) 
           (s výjimkou statisticky významného rozdílu při použití regresní metody, viz níže). 
           Protože tento očekávaný rozdíl je vždy nula (\\(H_0: X_{\\Delta}=0\\)), testová statistika \\(z\\) je spočítána 
           za předpokladu normálního rozložení jako podíl tohoto rozdílu a standardní chyby příslušného rozdílu:
           $$z=\\frac{X_{\\Delta}}{SE}$$
           Pro tuto testovou statistiku je pak dohledána příslušná pravděpodovnost (ve všech případech je použit oboustranný test). 
           Standardní chyba je využita i pro výpočet intervalu spolehlivosti s příslušným kvantilem normálního rozložení \\(z_w\\) 
           podle vzorce
           $$CI_w=z_w SE$$</p>"),
      h4("Statisticky významný rozdíl"),
      HTML("<p>V případě použití regresní metody (Cígler a Šmíra, 2015, vzorec 15) je pozorovaný rozdíl spočítán jako 
           $$X_{\\Delta}=\\sqrt{r_{aa'}}(A-M)-\\sqrt{r_{bb'}}(B-M)$$
           kde \\(r_{aa'})\\ a \\(r_{bb'}\\) jsou reliability obou testů \\(A\\) a \\(B\\) a \\(M\\) je průměr použitých jednotek.</p>
           <p>Pokud není použita regresní metoda, je rozdíl spočítán jako prostý rozdíl pozorovaných skórů
           $$X_{\\Delta}=A-B$$</p>
           <p>Standardní chyba statisticky významného rozdílu je pak v obou případech odhadnuta jako 
           $$SE_{stat.} = SD\\sqrt(2-r_{aa'}-r_{bb'})$$
           SD je směrodatná odchylka použitých jednotek.</p>"),
      h4("Klinicky významný rozdíl"),
      HTML("<p>Očekávané skóre \\(E(B|A)\\) ve druhém testu je spočítáno s využitím skóre prvního testu \\(A\\) 
           a jejich korelace \\(r_{ab}\\) jako 
           $$E(B|A) = r_{ab}A + (1-r_{ab})M$$
           Standardní chyba klinického rozdílu je potom
           $$SE = SD\\sqrt{1-r_{ab}^2}$$</p>"),
      h4("Test-retest (chyba predikce)"),
      HTML("<p>Postup je analogický předchozímu příkladu s tím rozdílem, že namísto korelace obou testů je použita 
           reliabilita testu (společná pro oba testy). <br/>
           Očekávané skóre \\(E(B|A)\\) ve druhém testu je spočítáno s využitím skóre prvního testu \\(A\\) 
           a jejich společné reliability \\(r_{aa'}=r_{bb'}\\) jako 
           $$E(B|A) = r_{aa'}A + (1-r_{aa'})M$$
           Standardní chyba klinického rozdílu je potom
           $$SE = SD\\sqrt{1-r_{aa'}^2}$$</p>"),
      h3("Zdroje"),
      HTML("<p>Cígler, H., & Šmíra, M. (2015). 
         Chyba měření a odhad pravého skóru: Připomenutí některých postupů Klasické testové teorie. 
        <i>Testfórum, 4</i>(6), 67-84. 
        doi:<a href='https://doi.org/10.5817/TF2015-6-104', target='_blank'>10.5817/TF2015-6-104</a></p>"),
      hr(),
      p(paste0("Autorem kalkulačky je Hynek Cígler (©", year, ")."))
      
    )
  )
)




), 
hr(),
div(a(img(src="logoFSScol.png", width="180px", style="float: left; margin-top: -10px; margin-right: 20px;"), 
      href="http://psych.fss.muni.cz", target="_blank"),
"©", year, "Hynek Cígler & Martin Šmíra", br(),
"Katedra psychologie, Fakulta sociálních studií", br(),
     "Masarykova univerzita | ", tags$a("kalkulacka@testforum.cz", href="mailto:kalkulacka@testforum.cz"), 
    style = 'margin-left: 30px; margin-bottom: 50px;')))