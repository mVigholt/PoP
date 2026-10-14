open Canvas
open Tetris

let w : int = 300
let h : int = 600
let bw : int = 10
let bh : int = 20

let mutable aBoard = board (bw, bh)
let mutable aPiece = aBoard.newPiece()
let mutable gameOver = false

let move (x:int) (y:int) =
    aPiece <- aBoard.take()
    if aPiece.IsNone then None
    else
        let p = aPiece.Value.clone()
        let (i,j) = p.offset
        let mutable ii = i
        let mutable jj = j
        if (p.width + i + x) <= bw && (i + x) >= 0 then ii <- i+x
        if (p.height + j + y) <= bh then jj <- j+y
        p.offset <- (ii,jj)
        if (aBoard.put p) && not((ii,jj) = (i,j)) then Some aBoard
        else aBoard.put aPiece.Value |> ignore; None

let draw (w : int) (h : int) (s : board) : canvas =
    let c : canvas = create w h
    let b = s.board
    for x = 0 to s.width-1 do
        for y = 0 to s.height-1 do
        setFillBox c (toCanvasColor(b[x,y])) (x*w/s.width, y*h/s.height) (x*w/s.width + w/s.width, y*h/s.height + h/s.height)
    c

let react (s: board) (k: Canvas.key) : board option =
    match getKey k with
        | UpArrow when not(gameOver) ->
            aPiece <- aBoard.take()
            if aPiece.IsNone then None else 
                let p = aPiece.Value.clone()
                if p.height + (fst p.offset) <= bw && p.width + (snd p.offset) <= bh then
                    p.rotateRight() |> ignore
                if not(aBoard.put p) then aBoard.put aPiece.Value |> ignore 
                Some aBoard
        | DownArrow when not(gameOver) ->
            if move 0 1 = None then if aBoard.newPiece() = None then gameOver <- true;
            Some aBoard
        | LeftArrow when not(gameOver) ->
            move -1 0 |> ignore
            if move 0 1 = None then if aBoard.newPiece() = None then gameOver <- true;
            Some aBoard
        | RightArrow when not(gameOver) ->
            move 1 0 |> ignore
            if move 0 1 = None then if aBoard.newPiece() = None then gameOver <- true;
            Some aBoard
        | Space ->
            aBoard <- board(bw,bh)
            aPiece <- aBoard.newPiece()
            gameOver <- false
            Some aBoard
        | _ ->
            None

printfn("%A") aPiece.Value.height

runApp "Tetris" w h draw react (aBoard)