// Fish Quality Analysis Function
function analyzeFishQuality(temperature, pH, dissolvedOxygen, ammonia) {
  // Define the optimal ranges for each parameter
  const optimalRanges = {
      temperature: { min: 24, max: 30 },
      pH: { min: 6.5, max: 8.5 },
      dissolvedOxygen: { min: 5, max: 8 },
      ammonia: { max: 0.5 }
  };

  function calculateScore(value, min, max) {
      if (value >= min && value <= max) {
          return 100;
      } else if (value < min || value > max) {
          return 50;
      } else {
          return 0;
      }
  }

  // Calculate scores for each parameter
  const tempScore = calculateScore(temperature, optimalRanges.temperature.min, optimalRanges.temperature.max);
  const pHScore = calculateScore(pH, optimalRanges.pH.min, optimalRanges.pH.max);
  const oxygenScore = calculateScore(dissolvedOxygen, optimalRanges.dissolvedOxygen.min, optimalRanges.dissolvedOxygen.max);
  const ammoniaScore = ammonia <= optimalRanges.ammonia.max ? 100 : 0;

  // Calculate the overall Fish Quality Index (FQI) by averaging the scores
  const FQI = (tempScore + pHScore + oxygenScore + ammoniaScore) / 4;

  return {
      tempScore,
      pHScore,
      oxygenScore,
      ammoniaScore,
      FQI
  };
}

// Handle form submission
document.getElementById("waterQualityForm").addEventListener("submit", function(event) {
  event.preventDefault();

  const temperature = parseFloat(document.getElementById("temperature").value);
  const pH = parseFloat(document.getElementById("pH").value);
  const oxygen = parseFloat(document.getElementById("oxygen").value);
  const ammonia = parseFloat(document.getElementById("ammonia").value);

  // Get the analysis result
  const result = analyzeFishQuality(temperature, pH, oxygen, ammonia);

  // Display the result
  const resultDiv = document.getElementById("analysisResult");
  resultDiv.innerHTML = `
      <h3>Analysis Results</h3>
      <p>Temperature Score: ${result.tempScore}</p>
      <p>pH Score: ${result.pHScore}</p>
      <p>Oxygen Score: ${result.oxygenScore}</p>
      <p>Ammonia Score: ${result.ammoniaScore}</p>
      <p>Overall Fish Quality Index (FQI): ${result.FQI}</p>
  `;

  // Interpret the result
  if (result.FQI >= 90) {
      resultDiv.innerHTML += "<p>Fish health is optimal.</p>";
  } else if (result.FQI >= 70) {
      resultDiv.innerHTML += "<p>Fish health is good, but conditions could be improved.</p>";
  } else if (result.FQI >= 50) {
      resultDiv.innerHTML += "<p>Fish health is suboptimal, corrective actions recommended.</p>";
  } else {
      resultDiv.innerHTML += "<p>Fish health is poor, urgent intervention required.</p>";
  }
});