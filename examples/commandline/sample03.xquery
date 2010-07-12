(:
   How many sold items cost more than 40?
:)

count(for $i in doc("auction")/site/closed_auctions/closed_auction
        where  $i/price/text() >= 40 
        return $i/price)
