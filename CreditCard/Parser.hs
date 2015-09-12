
data MessageType = Info
                 | Warning
                 | Error Int
 deriving (Show, Eq)
  
parseString :: String ->MessageType	
parseString x = case x of
		('I':_)->Info
		('W':_)->Warning
		('E':_)->Error x'
		where x' = read$tail x::Int
