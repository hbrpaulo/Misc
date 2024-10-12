mermaid('graph TD

    RefDummy(Há referência?) --> AnInd(Sem<br>referência)
    AnInd --> NoRef(Análise<br>individual)
    RefDummy --> Ref(Com<br>referência)

    Ref -->RefPont(Com referência<br> pontual)
    Ref --> RefInt(Com referência<br> intervalar)
    
      NoRef --> Trend(Tendência)
      RefPont --> Trend1(Tendência)
      RefInt --> Trend2(Tendência)
      
      Trend --> TrendG(Tendência<br>global.)
      Trend --> TrendF(Tendência<br>frag.)
      
      Trend1 --> TrendG1(Tendência<br>global.)
      Trend1 --> TrendF1(Tendência<br>frag.)
      
      Trend2 --> TrendG2(Tendência<br>global.)
      Trend2 --> TrendF2(Tendência<br>frag.)
      
      NoRef --> Saz(Sazonalidade)
      RefPont --> Saz1(Sazonalidade)
      RefInt --> Saz2(Sazonalidade)
      
      Saz --> SazG(Sazonalidade<br>global)
      Saz --> SazF(Sazonalidade<br>frag.)
      
      Saz1 --> SazG1(Sazonalidade<br>global)
      Saz1 --> SazF1(Sazonalidade<br>frag.)
      
      Saz2 --> SazG2(Sazonalidade<br>global)
      Saz2 --> SazF2(Sazonalidade<br>frag.)
      
      %% Aspectos específicos
      RefPont --> Dist(Distância entre<br>curva e a referência)
      
      RefInt --> Dist1(Distância entre<br>curva e a referência)
      RefInt --> Outsiders(Pontos fora<br>do intervalor)
      Outsiders --> Count(Contagem<br>Porcentagem)
      Outsiders --> Seq(Sequenciais)
      
      %% Aesthetics 
      style SazG fill:#2BD72F
      style SazG1 fill:#2BD72F
      style SazG2 fill:#2BD72F
      
      style SazF fill:#EEA35D
      style SazF1 fill:#EEA35D
      style SazF2 fill:#EEA35D
      
      style TrendG fill:#2BD72F
      style TrendG1 fill:#2BD72F
      style TrendG2 fill:#2BD72F
      
      style TrendF fill:#2BD72F
      style TrendF1 fill:#2BD72F
      style TrendF2 fill:#2BD72F
      
      style Trend fill:#2BD72F
      style Trend1 fill:#2BD72F
      style Trend2 fill:#EEA35D
') %>% print
