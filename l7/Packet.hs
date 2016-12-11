module Packet (Packet, Segment, Sequence, getSupernet, getHypernet, parsePacket)
where

type Segment = (Bool, String)
type Packet = [Segment]

type Sequence = [String]

getSupernet :: Packet -> Sequence
getSupernet = getSequences id

getHypernet :: Packet -> Sequence
getHypernet = getSequences not

getSequences predicate packet =
  [ snd x | x <- packet, predicate $ fst x ]

parsePacket :: String -> Packet
parsePacket =
  g' True
  where
    g' _     []        = []
    g' state list
      | elem ']' list  = [(state, listHead)] ++ (g' (not state) listTail)
      | otherwise      = [(state, listHead)]
      where
        listHead = (takeWhile ((flip notElem) "[]") list)
        listTail = tail (dropWhile ((flip notElem) "[]") list)
