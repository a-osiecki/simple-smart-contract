#!/usr/bin/bash

wallets="$PATH_TO_TESTNET/GeneratingAndRestoringWallets/wallets/*.json"
for wallet in $wallets;
do 
	printf "Restoring ${wallet##*/}\n"
	curl -H "content-type: application/json" -XPOST -d @$wallet localhost:8090/v2/wallets;
	printf "\n\n"
done


