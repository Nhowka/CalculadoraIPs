namespace CalculadoraIPs

open Xamarin.Forms
open System
open AppLogic

type CalcPage() = 
    inherit ContentPage()
    let layout = StackLayout()
    
    let (|Numeric|) def n = 
        match Int32.TryParse n with
        | false, _ -> def
        | _, i -> i
    
    let numericEntryValidator min max (entry : Entry) = 
        fun (e : TextChangedEventArgs) -> 
            entry.Text <- match e.NewTextValue with
                          | "" -> ""
                          | t when t.Contains "." -> t.Replace(".", "")
                          | t -> 
                              match System.Int32.TryParse t, System.Int32.TryParse e.OldTextValue with
                              | (false, _), (false, _) -> ""
                              | (false, _), _ -> e.OldTextValue
                              | (_, i), _ -> 
                                  match i with
                                  | b when b > max -> max
                                  | b when b < min -> min
                                  | b -> b
                                  |> string
    
    let newEntry min max = 
        let entry = Entry(Keyboard = Keyboard.Numeric, Placeholder = string max, PlaceholderColor = Color.Gray)
        entry.TextChanged.Add(numericEntryValidator min max entry)
        entry
    
    let ne = newEntry
    let stackIps = 
        StackLayout(Orientation = StackOrientation.Horizontal, HorizontalOptions = LayoutOptions.CenterAndExpand)
    let (ip1, ip2, ip3, ip4, cidr) = (ne 0 255, ne 0 255, ne 0 255, ne 0 255, ne 1 30)
    
    let rec tabEffect (entries : Entry list) = 
        match entries with
        | [] | [ _ ] -> ()
        | a :: (b :: _ as r) -> 
            a.Completed.Add(fun _ -> b.Focus() |> ignore)
            tabEffect r
    
    do tabEffect [ ip1; ip2; ip3; ip4; cidr ]
    
    do 
        for ip in [ ip1; ip2; ip3 ] do
            stackIps.Children.Add(ip)
            stackIps.Children.Add(Label(Text = "."))
    
    do stackIps.Children.Add(ip4)
    do stackIps.Children.Add(Label(Text = "/"))
    do stackIps.Children.Add(cidr)
    let infoLabel = 
        Label
            (HorizontalTextAlignment = TextAlignment.Center, VerticalTextAlignment = TextAlignment.Center, 
             LineBreakMode = LineBreakMode.WordWrap)
    let button = Button(Text = "Info",BackgroundColor=Color.Silver)
    
    do 
        button.Clicked.Add(fun _ -> 
            infoLabel.Text <- let (|N|) a = 
                                  let (Numeric 0 a) = a
                                  a
                              match ip1.Text, ip2.Text, ip3.Text, ip4.Text with
                              | N a, N b, N c, N d -> 
                                  ip1.Text <- string a
                                  ip2.Text <- string b
                                  ip3.Text <- string c
                                  ip4.Text <- string d
                                  let cid = 
                                      max (defaultArg (match Int32.TryParse cidr.Text with
                                                       | false, _ -> None
                                                       | _, i -> Some i) 0) (AppLogic.getDefaultCIDR a)
                                  cidr.Text <- cid |> string
                                  AppLogic.ipInfo a b c d cid)
    
    do layout.Children.Add(stackIps)
    do layout.Children.Add(button)
    do layout.Children.Add(infoLabel)
    do base.Content <- layout
