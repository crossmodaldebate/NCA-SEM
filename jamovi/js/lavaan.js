// lavaan.js

const R = require('r-script');

class Lavaan {
  constructor() {
    this.r = R();
  }

  // Executa o modelo Lavaan em R
  async run(model, data) {
    const script = `
      library(lavaan)
      model <- '${model}'
      data <- read.csv('${data}') // Usa o nome do arquivo de dados fornecido
      fit <- sem(model, data = data)
      list(
        parameterEstimates = parameterEstimates(fit),
        fitIndices = fitIndices(fit)
      )
    `;
    const result = await this.r.run(script);
    return result;
  }
}

module.exports = Lavaan;