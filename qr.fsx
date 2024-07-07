#r "nuget: QRCoder"
open QRCoder

let gen = new QRCodeGenerator()
let data = gen.CreateQrCode(PayloadGenerator.Url("https://transmission-s.com"))
let logo = System.IO.File.ReadAllText @"C:\Users\jchassaing\Downloads\Transmission(s)_Logo_Féminin-Masculin.svg" 

let code = new SvgQRCode()
code.SetQRCodeData(data)
let str = code.GetGraphic( System.Drawing.Size( 200, 200) , logo = SvgQRCode.SvgLogo(logo, fillLogoBackground = true))

System.IO.File.WriteAllText(@"D:\dev\transmissions\paged\qr.svg", str)