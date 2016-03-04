wget http://mtgjson.com/json/AllSets.json.zip -O a.zip
unzip a.zip
rm -v a.zip
mv AllSets.json priv/

###

wget http://mtgjson.com/json/AllCards.json.zip -O a.zip
unzip a.zip
rm -v a.zip
mv AllCards.json priv/
