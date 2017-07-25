sentence=`echo "$@" | sed -e "s/ /\+/g"`
response=`curl -s --get "https://yoda.p.mashape.com/yoda?sentence=$sentence" \
  -H "X-Mashape-Key: NPubgfEaBjmshUMlctfWWGeogeW2p1uiYQTjsn290LX2rq796v" \
  -H "Accept: text/plain"`

echo $response

