curl -s https://economia.awesomeapi.com.br/json/last/USD-BRL | jq -r '.USDBRL.bid' | awk '{printf "\uf155 %.2f\n", $1}'

