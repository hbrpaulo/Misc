mermaid('graph LR
    Begin[CurveInterpret] --100%--> RefDummy{Há referência?}
    RefDummy --10%--> NoRef(Sem referência)
    RefDummy --Total: 20% <br> métricas: 80%<br>interpretação: 0%--> Ref(Com referência)
    Ref --Total: 20% <br> métricas: 80%<br>interpretação: 0%-->RefInt(Com referência intervalar)
    Ref --Total: 20% <br> métricas: 80%<br>interpretação: 0%-->RefPont(Com referência pontual)
    
')
