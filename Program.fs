open System
open System.Windows.Forms
open System.Drawing


let SZEROKOSC = 800
let WYSOKOSCT = 600
let SKALA = 20

type Direction = Up | Down | Left | Right

[<Struct>]
type P(x: int, y: int) =
    member this.X = x
    member this.Y = y
    static member (+) ((l: P), (r: P)) = P(l.X+r.X, l.Y+r.Y)

type Zbieracz = {Tail: P list ; Wynik: int; Dir: Direction}

type Moneta = {Poz: P; Kolor: Color; Fun: Zbieracz -> Zbieracz}
let redMoneta =   {Poz = P(15,15); Kolor = Color.OrangeRed;
                  Fun = (fun zb -> {zb with Wynik = zb.Wynik+1})}
let greenMoneta = {Poz = P(15,15); Kolor = Color.HotPink;
                  Fun = (fun zb -> {zb with Wynik = zb.Wynik+3})}

let rng = new Random()

let dirToP = function
    | Up    ->  P(0, -1)
    | Down  ->  P(0, 1)
    | Left  ->  P(-1, 0)
    | Right ->  P(1, 0)

let keyToDir = function
    | 38 -> Some(Up)
    | 40 -> Some(Down)
    | 37 -> Some(Left)
    | 39 -> Some(Right)
    | _  -> None

let keyToDir2 = function
    | 0x57 -> Some(Up)
    | 0x53 -> Some(Down)
    | 0x41 -> Some(Left)
    | 0x44 -> Some(Right)
    | _  -> None

let OpuszczenieEkranu (n: P) =
    let przenies min max x =
        match x with
        | x when x > max -> min
        | x when x < min -> max
        | n -> n
    let x = przenies 0 (SZEROKOSC/SKALA - 1) n.X
    let y = przenies 0 (WYSOKOSCT/SKALA - 1) n.Y
    P(x,y)

let Ruch nw (zbieracz: Zbieracz) =
    let newHead = List.head zbieracz.Tail + (dirToP nw) |> OpuszczenieEkranu
    let newTail = newHead :: if nw = zbieracz.Dir
                             then List.tail zbieracz.Tail
                             else zbieracz.Tail
    { zbieracz with Tail = newTail; Dir = nw}


let randomMoneta inneMiejsce =
    let randPos() = P(rng.Next(0, SZEROKOSC/SKALA), rng.Next(0, WYSOKOSCT/SKALA))
    let mutable pos = randPos()
    while Option.isSome <| List.tryFind ((=) pos) inneMiejsce
        do pos <- randPos()
    if rng.Next(0, 10) >= 8
    then {greenMoneta with Poz = pos}
    else {redMoneta with Poz = pos}

let jedzenie moneta (zbieracz: Zbieracz) =
    if List.head zbieracz.Tail = moneta.Poz
    then (randomMoneta zbieracz.Tail, moneta.Fun zbieracz)
    else (moneta, zbieracz)

let hit (zbieracz: Zbieracz,zbieracz2:Zbieracz) =
    Option.isSome <| (List.tryFind ((=) (List.head (List.concat [zbieracz.Tail;zbieracz2.Tail]))) <| List.tail (List.concat [zbieracz.Tail;zbieracz2.Tail]))
let hit2 (zbieracz: Zbieracz,zbieracz2:Zbieracz) =
    Option.isSome <| (List.tryFind ((=) (List.head (List.concat [zbieracz2.Tail;zbieracz.Tail]))) <| List.tail (List.concat [zbieracz2.Tail;zbieracz.Tail]))

type myForm() =
    inherit Form()
    do base.DoubleBuffered <- true

let form = new myForm(Text = "Zbieracz", ClientSize = Size(SZEROKOSC, WYSOKOSCT))
let graphics = form.CreateGraphics()

let mutable nowy = Left
let mutable nowy2 = Right

let setNowyKierunek (e: KeyEventArgs) =
    let dir = keyToDir e.KeyValue
    if Option.isSome dir
    then nowy <- Option.get dir

let setNowyKierunek2 (ea: KeyEventArgs) =
    let dir2 = keyToDir2 ea.KeyValue
    if Option.isSome dir2
    then nowy2 <- Option.get dir2

form.KeyDown |> Observable.add setNowyKierunek
form.KeyDown |> Observable.add setNowyKierunek2

let draw moneta ((zbieracz: Zbieracz,zbieracz2:Zbieracz)) =
    use niebieski = new SolidBrush(Color.Blue)
    use zielony = new SolidBrush(Color.Green)
    use czarny = new SolidBrush(Color.Black)
    use kolormonety = new SolidBrush(moneta.Kolor)
    use Pen = new Pen(Brushes.Black)
    use font = new Font("Arial", 16.0f)

    graphics.FillRectangle(new SolidBrush(Color.FromArgb(80, 192, 192, 192)), 0, 0, SZEROKOSC, WYSOKOSCT)
    List.map (fun (e: P) -> graphics.FillEllipse(czarny,e.X*SKALA, e.Y*SKALA, SKALA-1, SKALA-1)) <| List.concat [zbieracz.Tail; zbieracz2.Tail]
    graphics.FillEllipse(kolormonety, moneta.Poz.X*SKALA, moneta.Poz.Y*SKALA, SKALA, SKALA)   
    graphics.FillRectangle(niebieski, (List.head zbieracz.Tail).X*SKALA, (List.head zbieracz.Tail).Y*SKALA, SKALA, SKALA)
    graphics.FillRectangle(zielony, (List.head zbieracz2.Tail).X*SKALA, (List.head zbieracz2.Tail).Y*SKALA, SKALA, SKALA)
    graphics.DrawString((zbieracz.Wynik).ToString(), font, czarny, PointF(0.0f,0.0f))
    graphics.DrawString((zbieracz2.Wynik).ToString(), font, czarny, PointF(775.0f,0.0f))

let rec Petla((zbieracz: Zbieracz),(zbieracz2:Zbieracz), moneta) = async {   
    let nastepny = zbieracz |> Ruch nowy
    let nastepny2 = zbieracz2 |> Ruch nowy2
    let moneta, nastepny = jedzenie moneta nastepny
    let moneta, nastepny2 = jedzenie moneta nastepny2
    draw moneta (nastepny,nastepny2)
    let wynik = zbieracz.Wynik + zbieracz2.Wynik

    do! Async.Sleep((100.0 - float wynik) |> int)

    if hit(zbieracz,zbieracz2) || hit2(zbieracz,zbieracz2)
    then MessageBox.Show("Wynik obu graczy: " + (zbieracz.Wynik + zbieracz2.Wynik).ToString()+Environment.NewLine +
                            "Gracz pierwszy (niebieski) :"+ zbieracz.Wynik.ToString() + Environment.NewLine + "Gracz drugi (zielony)" +
                                zbieracz2.Wynik.ToString() ) |> ignore
         Application.Exit()
    else return! Petla(nastepny,nastepny2, moneta)
    }

let zbieracz = { Tail = [P(0,0)]; Wynik = 0; Dir = nowy }
let zbieracz2 = { Tail = [P(0,29)]; Wynik = 0; Dir = nowy2 }

[<STAThread>]
do Async.Start(Petla(zbieracz,zbieracz2,redMoneta))
Application.Run(form)