data BookInfo = Book Int String [String]
				deriving (Show)

data MagazineInfo = Magazine Int String [String]
					deriving (Show)

myInfo = Book 9780135072455 "Algebra of Programming"
			["Richard Bird", "Oege de Moor"]

data BookReview = BookReview BookInfo CustomerId String

type CustomerId = Int

type ReviewBody = String

data BetterReview = BetterReview BookInfo CustomerId ReviewBody

data BillingInfo = CreditCard CardNumber CardHolder Address 
					| CashOnDelivery
					| Invoice CustomerId
					deriving (Show)

type CardNumber = Int

type CardHolder = String

type Address = String

bookId		(Book id _ _) = id

data Customer = Customer {
	customerId :: CustomerId
	, customerName :: String
	, customerAddress :: Address
} deriving (Show)
