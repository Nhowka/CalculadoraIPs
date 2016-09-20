namespace CalculadoraIPs

module AppLogic=
    let getDefaultCIDR hA = 32
    let ipInfo hA hB hC hD cidr = sprintf "%X:%X:%X:%X/%i" hA hB hC hD cidr