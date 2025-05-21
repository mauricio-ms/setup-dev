curl -s https://economia.awesomeapi.com.br/json/last/BTC-BRL | jq -r '.BTCBRL.bid' | awk '{printf "\uf10f %.2f\n", $1}'

