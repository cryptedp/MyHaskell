module LogAnalysis where
import Log

  
parseArray :: [String] -> (MessageType, [String])
parseArray (x:xs) = case x of
		"I"-> (Info,xs)
		"W"-> (Warning,xs)
		"E"-> (Error x', drop 1 xs)
		where x' = read (xs!!0)::Int
			
parseMessage :: String -> LogMessage
parseMessage x = LogMessage messageType timeStamp message
	where
		(messageType,rest) = parseArray$words x
		timeStamp = read(rest!!0)::Int
		message = unwords $ drop 1 rest

parse :: String->[LogMessage]
--map parseMessage to each line of the String
parse x = map parseMessage$lines x

insert:: LogMessage->MessageTree->MessageTree

insert message leaf = Node leaf message Leaf
insert (Unknown _) tree = tree
insert message@(LogMessage _ lval _) tree@(Node leftTree tlm@(LogMessage _ tval _) rightTree)
	|lval >= tval = Node leftTree tlm (insert message rightTree)
	|otherwise = Node (insert message leftTree) tlm rightTree 
	
build:: [LogMessage]->MessageTree
build [] = Leaf
build (x:xs)= insert x (build xs)

inOrder:: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree message rightTree) = inOrder(leftTree)++[message]++inOrder(rightTree)


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logmessages = [text | x@(LogMessage message timeStamp text) <- inOrder (build logmessages), 
    case message of 
        Error n -> n >= 50 
        _ -> False ]
		


