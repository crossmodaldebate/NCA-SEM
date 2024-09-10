// nca-sem-results.js

class NCA_SEM_Results {
  constructor(results) {
    this.results = results;
  }

  getParameterEstimates() {
    return this.results.parameterEstimates;
  }

  getFitIndices() {
    return this.results.fitIndices;
  }

  getNCAResults() {
    return this.results.ncaResults;
  }

  getPathDiagram() {
    return this.results.pathDiagram;
  }

  // Formata os resultados para exibição (implementação específica da aplicação)
  formatResults() {
    const formattedResults = {};
    formattedResults.parameterEstimates = this.getParameterEstimates();
    formattedResults.fitIndices = this.getFitIndices();
    formattedResults.ncaResults = this.getNCAResults();
    formattedResults.pathDiagram = this.getPathDiagram();
    return formattedResults;
  }
}

module.exports = NCA_SEM_Results;