#r "nuget: QRCoder"

open QRCoder
open System.Drawing
let gen = new QRCodeGenerator()


let size = Size(200,200)
let bgColor = "#ffffffff"
let fgColor = "#00000000"

let svgLogo =
    let logo = System.IO.File.ReadAllText @".\cards\img\logo.svg" 
    SvgQRCode.SvgLogo(logo, fillLogoBackground = true, iconSizePercent = 18, iconEmbedded = true)


let situation n =
    let data = 
        gen.CreateQrCode(
                PayloadGenerator.Url($"https://qr.transmission-s.com/situation-%d{n}"), 
                QRCodeGenerator.ECCLevel.Q)
    use code = new SvgQRCode()
    code.SetQRCodeData(data)
    code.GetGraphic( size,
                bgColor, fgColor , 
                logo = svgLogo,
                drawQuietZones = true, 
                sizingMode = SvgQRCode.SizingMode.ViewBoxAttribute)

