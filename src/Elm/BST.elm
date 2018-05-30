module BST exposing (..)


type Tree comparable
    = Leaf
    | Node (Tree comparable) comparable (Tree comparable)


empty : Tree comparable
empty =
    Leaf


singleton : comparable -> Tree comparable
singleton v =
    Node empty v empty


insert : comparable -> Tree comparable -> Tree comparable
insert x tree =
    case tree of
        Leaf ->
            singleton x

        Node left y right ->
            if x < y then
                Node (insert x left) y right
            else if x > y then
                Node left y (insert x right)
            else
                tree


search : comparable -> Tree comparable -> Bool
search x tree =
    case tree of
        Leaf ->
            False

        Node left y right ->
            if x < y then
                search x left
            else if x > y then
                search x right
            else
                True


getValue : Tree comparable -> Maybe comparable
getValue tree =
    case tree of
        Leaf ->
            Nothing
        Node _ v _ ->
            Just v

getLeft : Tree comparable -> Tree comparable
getLeft tree =
    case tree of
        Leaf ->
            Leaf
        Node l _ _ ->
            l

getRight : Tree comparable -> Tree comparable
getRight tree =
    case tree of
        Leaf ->
            Leaf
        Node _ _ r ->
            r

deleteMin : Tree comparable -> Tree comparable
deleteMin tree =
    case tree of
        Leaf ->
            Leaf
        Node Leaf _ r ->
            r
        Node l x r ->
            Node (deleteMin l) x r


delete : comparable -> Tree comparable -> Tree comparable
delete x tree =
    let
        delete_ l r =
            case (l, r) of
                (Leaf, r) -> r
                (l, Leaf) -> l
                _ -> 
                    case minimum r of
                        Just x_ -> Node l x_ (deleteMin r)
                        _ -> tree
    in
        case tree of
            Leaf ->
                Leaf
            Node l v r ->
                if x < v then
                    Node (delete x l) v r
                else if x > v then
                    Node l v (delete x r)
                else
                    delete_ l r


depth : Tree comparable -> Int
depth tree =
    case tree of
        Leaf ->
            0

        Node left _ right ->
            1 + max (depth left) (depth right)


map : (comparable -> comparable) -> Tree comparable -> Tree comparable
map func tree =
    case tree of
        Leaf ->
            tree

        Node left x right ->
            Node (map func left) (func x) (map func right)


fold : (comparable -> a -> a) -> a -> Tree comparable -> a
fold func acc tree =
    case tree of
        Leaf ->
            acc

        Node left x right ->
            fold func (fold func (func x acc) left) right


sum : Tree number -> number
sum tree =
    fold (+) 0 tree


product : Tree number -> number
product tree =
    fold (*) 1 tree


maximum : Tree comparable -> Maybe comparable
maximum tree =
    case tree of
        Leaf ->
            Nothing

        Node _ x r ->
            if r == Leaf then
                Just x
            else
                maximum r


minimum : Tree comparable -> Maybe comparable
minimum tree =
    case tree of
        Leaf ->
            Nothing

        Node l x _ ->
            if l == Leaf then
                Just x
            else
                minimum l


num : Tree comparable -> Int
num tree =
    case tree of
        Leaf ->
            0

        Node l x r ->
            num l + 1 + num r


leftNum : Tree comparable -> Int
leftNum tree =
    case tree of
        Leaf ->
            0

        Node l _ _ ->
            num l


rightNum : Tree comparable -> Int
rightNum tree =
    case tree of
        Leaf ->
            0

        Node _ _ r ->
            num r


fromList : List comparable -> Tree comparable
fromList list =
    List.foldl insert empty list


toList : Tree comparable -> List comparable
toList tree =
    fold (::) [] tree
        |> List.reverse
