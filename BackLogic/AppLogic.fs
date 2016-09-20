namespace CalculadoraIPs

module Utils = 
    let internal (|First0|) n = 
        seq { 
            for i in 7..-1..0 do
                yield (1 <<< i) &&& n = 0
        }
        |> Seq.tryFindIndex id

type Class = 
    | A
    | B
    | C
    | D
    | E
    | Local

type IP = 
    { A : int
      B : int
      C : int
      D : int }
    
    member x.Class = 
        match x.A with
        | 127 -> Local
        | Utils.First0(Some 0) -> A
        | Utils.First0(Some 1) -> B
        | Utils.First0(Some 2) -> C
        | Utils.First0(Some 3) -> D
        | _ -> E
    
    member x.Dec = sprintf "%i.%i.%i.%i" x.A x.B x.C x.D
    member x.Hex = sprintf "%02X:%02X:%02X:%02X" x.A x.B x.C x.D
    member x.IsPrivate = x.A = 10 || (x.A = 172 && (x.B >= 16 && x.B <= 31)) || (x.A = 192 && x.B = 168)
    
    static member FromInt i = 
        { A = (i >>> 24) &&& 0xFF
          B = (i >>> 16) &&& 0xFF
          C = (i >>> 8) &&& 0xFF
          D = i &&& 0xFF }
    
    member x.ToInt = (x.A <<< 24) ||| (x.B <<< 16) ||| (x.C <<< 8) ||| x.D
    member x.IsBase(mask : IP) = (x.ToInt &&& (~~~mask.ToInt)) = 0
    member x.IsBroadcast(mask : IP) = (x.ToInt ||| mask.ToInt) = -1
    member x.Prefix(mask : IP) = IP.FromInt(x.ToInt &&& mask.ToInt)
    member x.Broadcast(mask : IP) = IP.FromInt((-1 ^^^ mask.ToInt) ||| x.ToInt)

module AppLogic = 
    let getDefaultCIDR hA = 
        match hA with
        | Utils.First0(Some 0) -> 8
        | Utils.First0(Some 1) -> 16
        | Utils.First0(Some 2) -> 24
        | _ -> 32
    
    let cidrMask cidr = 
        let mask = ~~~0 ^^^ ((1 <<< (32 - cidr)) - 1)
        IP.FromInt mask
    
    let hostsPerNetwork cidr = (1 <<< (32 - cidr)) - 2
    let totalNetworks (ip : IP) cidr = (1 <<< cidr)
    
    let ipInfo hA hB hC hD cidr = 
        let ip = 
            { A = hA
              B = hB
              C = hC
              D = hD }
        
        let mask = cidrMask cidr
        match ip.Class with
        | A | B | C -> 
            let hosts = hostsPerNetwork cidr
            let totalNetworks = totalNetworks ip cidr
            sprintf "IP: %s/%i
Representação hexadecimal: %s
Classe: %A
Tipo de rede: %s
Utilizável: %s
Máscara: %s
Prefixo da rede: %s
Broadcast: %s
Hosts por rede: %u
Total de redes: %u
Total utilizável: %u" ip.Dec cidr ip.Hex ip.Class (if ip.IsPrivate then "Rede local privada"
                                                   else "Rede pública") (if ip.IsBase mask then "Não (IP Base)"
                                                                         elif ip.IsBroadcast mask then 
                                                                             "Não (IP de Broadcast)"
                                                                         else "Sim") mask.Dec (ip.Prefix mask).Dec 
                (ip.Broadcast mask).Dec hosts totalNetworks (hosts * totalNetworks)
        | D -> sprintf "IP: %s
Representação hexadecimal: %s
Classe: D (Multicast)" ip.Dec ip.Hex
        | E -> sprintf "IP: %s
Representação hexadecimal: %s
Classe: D (Reservado para testes)" ip.Dec ip.Hex
        | Local -> sprintf "IP: %s
Representação hexadecimal: %s
Classe: Endereço de Loopback" ip.Dec ip.Hex
