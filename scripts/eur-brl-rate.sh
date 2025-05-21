curl -s https://economia.awesomeapi.com.br/json/last/EUR-BRL | jq -r '.EURBRL.bid' | awk '{printf "\uf153 %.2f\n", $1}'

