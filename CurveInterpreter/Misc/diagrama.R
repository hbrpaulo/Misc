library(DiagrammeR)
diagram <- mermaid('graph TD
  
    Begin(CurveInterpreter) --> RefDummy(Há referência?)
    RefDummy --> AnInd(Sem<br>referência)
    AnInd --> NoRef(Análise<br>individual)
    RefDummy --> Ref(Com<br>referência)

    Ref -->RefPont(Com referência<br>pontual)
    Ref --> RefInt(Com referência<br>intervalar)
    
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
      
      %% Specfics aspects
      
      RefInt --> Outsiders(Pontos fora<br>do intervalo)
      Outsiders --> Count_out(Contagem<br>Porcentagem)
      Outsiders --> Seq_out(Sequenciais)

      %% Comparisons

      TrendG --> Comp[Comparações<br>entre as partes]
      TrendF --> Comp
      SazG --> Comp
      SazF --> Comp
      
      %%Comparações<br>entre as partes
      
      TrendG1 --> Dist1[Distância entre<br>curva e a referência]
      TrendF1 --> Dist1
      SazG1 --> Dist1
      SazF1 --> Dist1

      TrendG2 --> Dist2[Distância entre<br>curva e a referência]
      TrendF2 --> Dist2
      SazG2 --> Dist2
      SazF2 --> Dist2     
      Seq_out --> Dist2
      Count_out --> Dist2
      
      Dist1 --> Comp1[Comparações<br>entre as partes]
      Dist2 --> Comp2[Comparações<br>entre as partes]
      
    %% Aesthetics
    
    style RefDummy fill:#2BD72F
    
    style NoRef fill:#2BD72F
    style AnInd fill:#2BD72F
    style Ref fill:#2BD72F
    style RefPont fill:#2BD72F
    style RefInt fill:#2BD72F
    
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
    style Trend2 fill:#2BD72F
    
    style Comp fill:#FFFFFF
    style Comp1 fill:#FFFFFF
    style Comp2 fill:#FFFFFF
    
    style Dist1 fill:#FFFFFF
    style Dist2 fill:#FFFFFF
')
