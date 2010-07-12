(:
  Return the initial increases of all open auctions.
:)

for $b in doc("auction")/site/open_auctions/open_auction
return <increase>{ $b/bidder[position()=1]/increase/text() }</increase>
