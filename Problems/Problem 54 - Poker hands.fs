module ``Problem 54 - Poker hands``

type Rank =  Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten 
             | Jack | Queen | King | Ace

type Suit = Spades | Hearts | Diamonds | Clubs

type Card = Rank * Suit

type Hand = Set<Card>

[<StructuralComparison>]
[<StructuralEquality>]
type HandRank =
    | HighCard of Rank list
    | OnePair of Rank * Rank list
    | TwoPairs of Rank * Rank * Rank
    | ThreeOfAKind of Rank * Rank list
    | Straight of Rank
    | Flush of Rank list
    | FullHouse of Rank * Rank
    | FourOfAKind of Rank * Rank
    | StraightFlush of Rank
    | RoyalFlush

let parseRank =
    function 
    | '2' -> Two
    | '3' -> Three
    | '4' -> Four
    | '5' -> Five
    | '6' -> Six
    | '7' -> Seven
    | '8' -> Eight
    | '9' -> Nine
    | 'T' -> Ten
    | 'J' -> Jack
    | 'Q' -> Queen
    | 'K' -> King
    | 'A' -> Ace
    | r -> sprintf "%c Not a rank!" r |> failwith

let parseSuit =
    function
    | 'S' -> Spades
    | 'H' -> Hearts
    | 'D' -> Diamonds
    | 'C' -> Clubs
    | s -> sprintf "%c Not a suit!" s |> failwith
    
let parseCard (card: string) = parseRank card.[0], parseSuit card.[1]

let parseHand (hand: string) =
    hand.Split([|' '|]) |> set |> Set.map parseCard |> (fun e -> Hand(e))

let validateHandRank (hand: Hand) =
    let isSameSuit = hand |> Seq.distinctBy snd |> Seq.length = 1
    
    let isStraight = hand |> Seq.map fst |> Seq.pairwise 
                     |> Seq.forall (fun (a,b) -> (compare b a) = 1)

    let highCard = hand |> Seq.maxBy fst |> fst

    let ranks = hand |> Seq.groupBy fst
                     |> Seq.map (fun (k,v) -> Seq.length v, k)
                     |> Array.ofSeq |> Array.sortBy fst |> Array.rev
    
    let kickers n = ranks.[n..] |> List.ofArray |> (List.map snd >> List.sort >> List.rev)

    match hand with
    | h when isSameSuit && hand |> (Set.map fst) = set [Ten; Jack; Queen; King; Ace] -> RoyalFlush
    | h when isSameSuit && isStraight -> StraightFlush(highCard)
    | h when fst ranks.[0] = 4 -> FourOfAKind(snd ranks.[0], snd ranks.[1])
    | h when fst ranks.[0] = 3 && fst ranks.[1] = 2 -> FullHouse(snd ranks.[0], snd ranks.[1])
    | h when isSameSuit -> Flush(kickers 0)
    | h when isStraight -> Straight(highCard)
    | h when fst ranks.[0] = 3 -> ThreeOfAKind(snd ranks.[0], kickers 1)
    | h when fst ranks.[0] = 2 && fst ranks.[1] = 2 ->
        match snd ranks.[0], snd ranks.[1] with
        | a, b when a > b -> TwoPairs(a, b, snd ranks.[2])
        | a, b -> TwoPairs(b, a, snd ranks.[2])
    | h when fst ranks.[0] = 2 -> OnePair(snd ranks.[0], kickers 1) 
    | _ -> HighCard(kickers 0)

let validateFile url = 
    use client = new System.Net.WebClient()
    client.DownloadString(System.Uri(url)).Split([|'\n'|])
    |> Array.filter (fun e -> e.Length <> 0)
    |> Array.map (fun e -> parseHand e.[..13], parseHand e.[15..])
    |> Array.map (fun (a,b) -> validateHandRank a, validateHandRank b)
    |> Array.filter (fun (a,b) -> a > b)
    |> Array.length

#if INTERACTIVE
#time
validateFile "https://projecteuler.net/project/resources/p054_poker.txt"
#time
#endif

open NUnit.Framework
open Swensen.Unquote

[<TestCase("TS JS QS KS AS")>]
let ``Royal Flush should be identified`` hand =
    test<@ parseHand hand |> validateHandRank = RoyalFlush @>

[<TestCase("9S TS JS QS KS")>]
let ``Straight Flush should be identified`` hand =
    test<@ parseHand hand |> validateHandRank = StraightFlush(King) @>

[<TestCase("2S 2H 2D 2C AS")>]
let ``Four of a Kind should be identified`` hand =
    test<@ parseHand hand |> validateHandRank = FourOfAKind(Two, Ace) @>

[<TestCase("2S 4S 6S TS AS")>]
let ``Flush should be identified`` hand =
    test<@ parseHand hand |> validateHandRank = Flush([Ace; Ten; Six; Four; Two]) @>

[<TestCase("2S 2H 2D 3S 3D")>]
let ``Full House should be identified`` hand =
    test<@ parseHand hand |> validateHandRank = FullHouse(Two, Three) @>

[<TestCase("2D 3S 4S 5S 6S")>]
let ``Straight should be identified`` hand =
    test<@ parseHand hand |> validateHandRank = Straight(Six) @>

[<TestCase("2S 2H 2D 3C AS")>]
let ``Three of a Kind should be identified`` hand =
    test<@ parseHand hand |> validateHandRank = ThreeOfAKind(Two, [Ace; Three]) @>

[<TestCase("2S 2H 3D 3C AS")>]
let ``Two Pairs should be identified`` hand =
    test<@ parseHand hand |> validateHandRank = TwoPairs(Three, Two, Ace) @>

[<TestCase("2S 2D 5C KS AS")>]
let ``One Pair should be identified`` hand =
    test<@ parseHand hand |> validateHandRank = OnePair(Two, [Ace;King;Five]) @>

[<TestCase("2S 3D 5C KS AS")>]
let ``High Card should be identified`` hand =
    test<@ parseHand hand |> validateHandRank = HighCard([Ace;King;Five;Three;Two]) @>