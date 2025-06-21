# correlacao_linear_bicicletas_londres
Estudo de correlação com a sensação térmica e com a contagem de compartilhamento de bicicletas
Dataset de compartilhamento de bicicletas em londres na URL
https://www.kaggle.com/datasets/hmavrodiev/london-bike-sharing-dataset
Historical data for bike sharing in London 'Powered by TfL Open Data'
/Dicionário das variáveis 
timestamp: campo de timestamp para agrupar os dados.
cnt: contagem de novas compartilhamentos de bicicletas.
t1: temperatura real em °C.
t2: temperatura em °C que "parece" (sensação térmica).
hum: umidade em porcentagem.
wind_speed: velocidade do vento em km/h.
weather_code: categoria do clima (código que representa as condições meteorológicas).
is_holiday: campo booleano - 1 se for feriado / 0 se não for feriado.
is_weekend: campo booleano - 1 se o dia for fim de semana.
season: campo categórico para estações meteorológicas: 0-primavera; 1-verão; 2-outono; 3-inverno.

//Em relação a Regressão Linear
t1: para cada aumento de 1 grau na temperatura, o número de viagens aumenta em média 75.72.
R-squared: aproximadamente 15.12% da variabilidade no número de viagens pode ser explicada pela temperatura.
p-value: < 2.2e-16. Esse p-valor muito baixo indica que o modelo de regressão é bem significativo, logo a temperatura é um preditor significativo do número de viagens.
