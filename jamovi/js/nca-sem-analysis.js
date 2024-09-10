analysis:
  name: ncaSemAnalysis  # Nome único para a análise
  title: Análise de Necessidade via SEM
  type: NCA
  options:
    necessaryEffect:
      type: Array
      title: Efeitos Necessários
      items:
        type: Object
        properties:
          name:
            type: String
            title: Nome do Efeito
            default: "Motivation -> Performance"
          cause:
            type: String
            title: Causa
            default: Motivation
          effect:
            type: String
            title: Efeito
            default: Performance
    sufficientCause:
      type: Array
      title: Causas Suficientes
      items:
        type: String
      default: []  # Lista vazia por padrão
    model:
      type: String
      title: Modelo SEM
      default: |
        Motivation =~ Mot1 + Mot2 + Mot3
        Performance =~ Des1 + Des2 + Des3
        Performance ~ Motivation
  results:
    necessityThreshold:
      title: Limiar de Necessidade
      type: Number
    caseProportion:
      title: Proporção de Casos
      type: Number
    modelParameters:
      title: Parâmetros do Modelo
      type: Table
      columns:
        - name: parameter
          title: Parâmetro
        - name: estimate
          title: Estimativa
        - name: se
          title: Erro Padrão
        - name: pvalue
          title: Valor P
    fitIndices:
      title: Índices de Ajuste
      type: Table
      columns:
        - name: index
          title: Índice
        - name: value
          title: Valor