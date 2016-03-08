var fs = require("fs")
var allCards = JSON.parse(fs.readFileSync("priv/AllCards.json"))

function stripCardInfo(set) {
  return set.cards.map( function(card) { return card.name } )
}

function transformAllCards(ss) {
  var allCardsTransformed = []

  for (var idCard in allCards) {
    if (allCards.hasOwnProperty(idCard)) {
      var c0 = allCards[idCard]
      var c1 = { name:          c0.name
               , rarity:        c0.rarity
               , oracle:        c0.text
               , manaCost:      c0.manaCost
               , cmc:           c0.cmc
               , types:         c0.types
               , subtypes:      c0.subtypes
               , colors:        c0.colors
               , colorRelation: c0.colorIdentity
               , power:         c0.power
               , toughness:     c0.toughness }
      allCardsTransformed.push(c1)
    }
  }

  return allCardsTransformed
}

console.log(JSON.stringify(transformAllCards(allCards)))
