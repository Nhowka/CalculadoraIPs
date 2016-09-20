namespace CalculadoraIPs

open Xamarin.Forms

type CalcPage() =
    inherit ContentPage()

    let layout = StackLayout()

    do base.Content <- layout
    
    