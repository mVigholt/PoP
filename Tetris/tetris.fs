module Tetris
open Canvas

type position = int * int

type Color =
    | Yellow
    | Cyan
    | Blue
    | Orange
    | Red
    | Green
    | Purple

let toCanvasColor (c : Color option) : Canvas.color =
    match c with
        | Some Yellow -> fromArgb(255, 225, 0, 255)//yellow
        | Some Cyan -> fromArgb(0, 200, 200, 255)//teal 
        | Some Blue -> fromArgb(0, 0, 255, 255)//blue
        | Some Orange -> fromArgb(255, 150, 0, 255)//orange
        | Some Red -> fromArgb(200, 0, 0, 255)//red
        | Some Green -> fromArgb(0, 200, 0, 255)//green
        | Some Purple -> fromArgb(200, 0, 200, 255)//purple
        | _ -> fromArgb(255, 255, 255, 255)//white

type tetromino (a:bool[,], c:Color, o:position) =
    let mutable _image = a
    let mutable _offset = o
    member this.col = c
    member this.image = _image
    member this.offset with get () = _offset and set (o) = _offset <- o
    member this.width = Array2D.length1(_image)
    member this.height = Array2D.length2(_image)
    member this.rotateRight() = 
        _image <- Array2D.init this.height this.width (fun i j -> _image[j,this.height-1-i])
    member this.clone() = tetromino(_image, c, _offset)
    override this.ToString() = (string c) //printfn "%A" p //printfn "%s" (string p) //printfn "%s" (p.ToString())

type square (o:position) =
    inherit tetromino(array2D([[true;true];[true;true]]), Yellow, o)

type straight (o:position) =
    inherit tetromino(array2D([[true;true;true;true]]), Cyan, o)

type t (o:position) =
    inherit tetromino(array2D([[true;true;true];[false;true;false]]), Purple, o)

type z (m:bool, o:position) =
    inherit tetromino(array2D(if m then [[true;true;false];[false;true;true]] else [[false;true;true];[true;true;false]]), (if m then Red else Green), o)

type l (m:bool, o:position) =
    inherit tetromino(array2D(if m then [[true;true;true];[true;false;false]] else [[true;false;false];[true;true;true]]), (if m then Blue else Orange), o)

type board ( w : int , h : int ) = 
    let _board : Color option [,] = Array2D.create w h None
    let mutable _active : tetromino option = None
    let mutable points = 0
    let mutable lastPiece : int = 0
    let rnd c = System.Random().Next c
    let rec randomPiece():tetromino =
        match rnd(8)+1  with
            | n when n = 1 && n <> lastPiece -> lastPiece <- n; z(false,(rnd(10-1),0))
            | n when n = 2 && n <> lastPiece -> lastPiece <- n; z(true,(rnd(10-1),0))
            | n when n = 3 && n <> lastPiece -> lastPiece <- n; l(false,(rnd(10-1),0))
            | n when n = 4 && n <> lastPiece -> lastPiece <- n; l(true,(rnd(10-1),0))
            | n when n = 5 && n <> lastPiece -> lastPiece <- n; t(rnd(10-1),0)
            | n when n = 6 && n <> lastPiece -> lastPiece <- n; straight(rnd(10-0),0)
            | n when n = 7 && n <> lastPiece -> lastPiece <- n; square(rnd(10-1),0)
            | _ -> lastPiece <- 0; randomPiece() 
    let drawBoard (board:Color option [,]) (t: tetromino) (io:bool) = 
        let p = Array2D.map (fun x -> if x then Some t.col else None) t.image
        let (x,y) = t.offset
        for i=0 to (t.width)-1 do
            for j=0 to (t.height)-1 do
                if p[i,j].IsSome then board[i+x,j+y] <- if io then p[i,j] else None
    let deleteRow() =
        let mutable row = false
        for j=0 to h-1 do
            row <- true
            for i=0 to w-1 do
                if _board[i,j] = None then row <- false
            if row then
                for l=j downto 0 do
                    for k=0 to w-1 do
                        if l=0 then _board[k,l] <- None
                        else _board[k,l] <- _board[k,l-1]
    override this.ToString() = "board"
    member this.width = w
    member this.height = h
    member this.board : Color option [,] = _board
    member this.take() : tetromino option = 
        let p = _active |> Option.bind (fun (x : tetromino) -> drawBoard _board x false; Some (x.clone()))      
        _active <- None; p
    member this.put(t:tetromino):bool =
        let mutable p = Some t
        for i=0 to (t.width-1) do
            for j=0 to (t.height-1) do
                if t.image[i,j] && not(_board[i+(fst t.offset),j+(snd t.offset)] = None) then p <- None
        if p.IsSome then drawBoard _board p.Value true; _active <- p; true
        else false
    member this.newPiece() : tetromino option = 
        deleteRow()
        let p : tetromino = randomPiece()
        if this.put(p) then points <- points + 1; _active else printfn "Game Over!\nYou placed %A pieces" points; None