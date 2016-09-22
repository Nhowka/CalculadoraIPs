namespace CalculadoraIPs

open Xamarin.Forms
open System
open AppLogic

type CalcPage() as this = 
    inherit ContentPage()
    let outerLayout = Grid(VerticalOptions = LayoutOptions.CenterAndExpand)
    
    do 
        for _ in 1..2 do
            outerLayout.RowDefinitions.Add(RowDefinition(Height = GridLength.Star))
    
    let innerLayout = StackLayout(VerticalOptions = LayoutOptions.StartAndExpand)
    
    let (|Numeric|) def n = 
        match Int32.TryParse n with
        | false, _ -> def
        | _, i -> i
    
    let (|Parseable|_|) n = 
        match Int32.TryParse n with
        | false, _ -> None
        | _, n -> Some n
    
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
    let ipInfoLabel = 
        Label
            (HorizontalTextAlignment = TextAlignment.Center, VerticalTextAlignment = TextAlignment.Center, 
             LineBreakMode = LineBreakMode.WordWrap)
    let button = Button(Text = "Info", BackgroundColor = Color.Silver)
    
    do 
        button.Clicked.Add(fun _ -> 
            ipInfoLabel.Text <- let (|N|) a = 
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
                                    ipInfo a b c d cid)
    
    let gridHosts = Grid(HorizontalOptions = LayoutOptions.EndAndExpand, VerticalOptions = LayoutOptions.FillAndExpand)
    
    do 
        for _ in 1..3 do
            gridHosts.ColumnDefinitions.Add(ColumnDefinition(Width = GridLength.Star))
        
    
    do 
        for _ in 1..2 do
            gridHosts.RowDefinitions.Add(RowDefinition(Height = GridLength.Star))
        gridHosts.RowDefinitions.Add(RowDefinition(Height = GridLength(5., GridUnitType.Star)))
    
    do gridHosts.Children.Add(Label(Text = "Subredes:"), 0, 0)
    let entryNets = ne 1 (1 <<< 22)
    do gridHosts.Children.Add(entryNets, 1, 0)
    do gridHosts.Children.Add(Label(Text = "Hosts por subrede:"), 0, 1)
    let entryHosts = ne 1 ((1 <<< 24) - 2)
    do gridHosts.Children.Add(entryHosts, 1, 1)
    
    let listIps = 
        ListView(Header = "Faixas de IPs", 
                 ItemTemplate = DataTemplate((fun () -> 
                                    let text = TextCell()
                                    text.SetBinding(TextCell.TextProperty, "Item1.Dec")
                                    text.SetBinding(TextCell.DetailProperty, "Item2.Dec")
                                    text.SetBinding(TextCell.CommandParameterProperty, "{Binding}")
                                    text.Tapped.Add(fun _ -> 
                                        match text.BindingContext with
                                        | :? Tuple<IP,IP,IP,int> as bc->
                                            let ip,mask, cidr = bc.Item1, bc.Item3, bc.Item4
                                            this.DisplayAlert("Faixa de IP:", 
                                                                   (sprintf "Base: %s/%d\nBroadcast: %s/%d" 
                                                                        (ip.Prefix mask).Dec cidr (ip.Broadcast mask).Dec) cidr, 
                                                                   "Fechar")
                                                              |> Async.AwaitTask
                                                              |> Async.Start
                                        |_ -> ()
                                    
                                    )
                                    text :> obj)))
    
    do gridHosts.Children.Add(listIps, 0, 2)
    let buttonHosts = Button(Text = "Calcular", BackgroundColor = Color.Silver)
    do gridHosts.Children.Add(buttonHosts, 2, 3, 0, 2)
    
    do 
        buttonHosts.Clicked.Add(fun _ -> 
            match entryNets.Text, entryHosts.Text with
            | Parseable n, Parseable h -> 
                let list = AppLogic.ips h n
                if list |> List.isEmpty then 
                    this.DisplayAlert("Erro", "Quantidade de subredes e hosts maior que o possível", "Fechar")
                    |> Async.AwaitTask
                    |> Async.Start
                else listIps.ItemsSource <- list
            | Parseable _, _ -> 
                this.DisplayAlert("Erro", "Hosts vazio", "Fechar")
                |> Async.AwaitTask
                |> Async.Start
            | _, _ -> 
                this.DisplayAlert("Erro", "Subredes vazio", "Fechar")
                |> Async.AwaitTask
                |> Async.Start)
    
    do innerLayout.Children.Add(stackIps)
    do innerLayout.Children.Add(button)
    do innerLayout.Children.Add(ipInfoLabel)
    do outerLayout.Children.Add(innerLayout, 0, 0)
    do outerLayout.Children.Add(gridHosts, 0, 1)
    do base.Content <- outerLayout
