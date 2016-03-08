var fs = require("fs")
var allSets = JSON.parse(fs.readFileSync("priv/AllSets.json"))

function stripCardInfo(set) {
  return set.cards.map( function(card) { return card.name } )
}

function transformAllSets(ss) {
  var allSetsTransformed = []

  for (var idSet in allSets) {
    if (allSets.hasOwnProperty(idSet)) {
      var s0 = allSets[idSet]
      var s1 = { booster: s0.booster
               , code:    idSet
               , name:    s0.name
               , cards:   stripCardInfo(s0) }
      allSetsTransformed.push(s1)
    }
  }

  return allSetsTransformed
}

console.log(JSON.stringify(transformAllSets(allSets)))
